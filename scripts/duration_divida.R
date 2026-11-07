# =====================================================================
# Duration, convexidade e DV01 da Dívida Pública Federal (em mercado)
#
# Entradas (dados/):
#   estoquedpf.csv              Tesouro Transparente, "Estoque da DPF":
#                               título, vencimento e valor, mensal (09/2017+)
#   precotaxatesourodireto.csv  taxas do Tesouro Direto (curvas pré, real, IGP-M)
#
# Método, título a título e mês a mês:
#   - taxa de desconto: a do Tesouro Direto no último dia do mês, interpolada
#     no vencimento do título dentro da curva do seu indexador (pré para
#     LTN/NTN-F, real para NTN-B, IGP-M para NTN-C); fora da faixa, a ponta.
#   - fluxos: LTN e NTN-B Principal sem cupom; NTN-F cupom de 10% a.a.;
#     NTN-B e NTN-C cupom de 6% a.a.; cupons semestrais.
#   - duration de Macaulay, modificada e convexidade a partir desses fluxos.
#   - LFT: duration de TAXA = 0 (o cupom acompanha a Selic todo dia), mas
#     entra no prazo até o vencimento e no risco de rolagem.
#   - dívida externa e títulos residuais (TDA, CVS etc.): fora da duration;
#     o script informa a cobertura.
#
# Saídas: duration_mensal.csv, duration_por_titulo_ultimo_mes.csv,
#         gráficos 23 e 24, e o choque de curva no console.
#
# Observação: o "Valor do Estoque" do Tesouro é contábil (pela taxa média de
# emissão), não a preço de mercado. Serve como peso; a perda de marcação a
# mercado calculada abaixo é uma aproximação de primeira e segunda ordem.
# =====================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2); library(scales)

# ---- 1. Estoque por título --------------------------------------------------
est <- read_delim("estoquedpf.csv", delim = ";",
                  locale = locale(decimal_mark = ",", grouping_mark = "."),
                  col_types = cols(.default = "c")) |>
  transmute(titulo = `Titulo/Contrato`,
            venc   = as.Date(`Vencimento do Titulo/Contrato`, "%d/%m/%Y"),
            valor  = as.numeric(sub(",", ".", `Valor do Estoque`, fixed = TRUE)) / 1e9,  # R$ bi
            mes    = as.Date(paste0("01/", `Mes do Estoque`), "%d/%m/%Y"),
            carteira = `Classe da Carteira`, tipo_divida = `Tipo de Divida`) |>
  filter(carteira == "Mercado", !is.na(mes), !is.na(valor)) |>
  mutate(tipo = case_when(
    grepl("^NTN-B Principal", titulo) ~ "NTN-B Principal",
    grepl("^NTN-B", titulo)           ~ "NTN-B",
    grepl("^NTN-F", titulo)           ~ "NTN-F",
    grepl("^NTN-C", titulo)           ~ "NTN-C",
    grepl("^LTN", titulo)             ~ "LTN",
    grepl("^LFT", titulo)             ~ "LFT",
    grepl("Externa", tipo_divida)     ~ "Externa",
    TRUE                              ~ "Outros"),
    indexador = case_when(
      tipo == "LFT" ~ "Selic (LFT)",
      tipo %in% c("LTN", "NTN-F") ~ "Prefixado",
      tipo %in% c("NTN-B", "NTN-B Principal", "NTN-C") ~ "Índice de preços",
      tipo == "Externa" ~ "Câmbio (externa)",
      TRUE ~ "Outros"))
cat("Estoque em mercado:", format(range(est$mes)), "\n")
print(est |> filter(mes == max(mes)) |> group_by(indexador) |>
        summarise(R_bi = round(sum(valor), 1), .groups = "drop"))
View(est)

# ---- 2. Curvas do Tesouro Direto no fim de cada mês -------------------------
td <- read_delim("precotaxatesourodireto.csv", delim = ";",
                 locale = locale(decimal_mark = ",", grouping_mark = "."),
                 show_col_types = FALSE) |>
  transmute(tipo = `Tipo Titulo`, venc = as.Date(`Data Vencimento`, "%d/%m/%Y"),
            data = as.Date(`Data Base`, "%d/%m/%Y"), taxa = `Taxa Compra Manha`) |>
  filter(!is.na(taxa), taxa > 0) |>
  mutate(curva = case_when(
    tipo %in% c("Tesouro Prefixado", "Tesouro Prefixado com Juros Semestrais") ~ "pre",
    tipo %in% c("Tesouro IPCA+", "Tesouro IPCA+ com Juros Semestrais",
                "Tesouro Educa+", "Tesouro Renda+ Aposentadoria Extra") ~ "real",
    tipo == "Tesouro IGPM+ com Juros Semestrais" ~ "igpm",
    TRUE ~ NA_character_)) |>
  filter(!is.na(curva), data >= as.Date("2017-08-01")) |>
  mutate(mes = as.Date(format(data, "%Y-%m-01")))
ultimo_dia <- td |> group_by(mes) |> summarise(data = max(data), .groups = "drop")
curvas <- td |> semi_join(ultimo_dia, by = c("mes", "data")) |>
  group_by(mes, curva, venc) |> summarise(taxa = mean(taxa), .groups = "drop")

taxa_no_venc <- function(m, cv, v) {
  cc <- curvas |> filter(mes == m, curva == cv) |> arrange(venc)
  if (nrow(cc) == 0 && cv == "igpm") cc <- curvas |> filter(mes == m, curva == "real") |> arrange(venc)
  if (nrow(cc) == 0) return(NA_real_)
  if (nrow(cc) == 1) return(cc$taxa)
  approx(as.numeric(cc$venc), cc$taxa, xout = as.numeric(v), rule = 2, ties = mean)$y
}

# ---- 3. Duration de cada título ---------------------------------------------
cupom_aa <- c("LTN" = 0, "NTN-B Principal" = 0, "NTN-F" = 0.10, "NTN-B" = 0.06, "NTN-C" = 0.06)
curva_de <- c("LTN" = "pre", "NTN-F" = "pre", "NTN-B" = "real",
              "NTN-B Principal" = "real", "NTN-C" = "igpm")

metricas <- function(tipo, venc, ref, y) {
  T <- as.numeric(venc - ref) / 365.25
  if (T <= 0) return(c(mac = 0, mod = 0, conv = 0, T = 0))
  c_aa <- cupom_aa[[tipo]]
  if (c_aa == 0) {
    t <- T; cf <- 1
  } else {
    t <- rev(seq(T, 0, by = -0.5)); t <- t[t > 0]
    cup <- (1 + c_aa)^0.5 - 1
    cf <- rep(cup, length(t)); cf[length(cf)] <- cf[length(cf)] + 1
  }
  pv <- cf / (1 + y)^t
  mac <- sum(t * pv) / sum(pv)
  c(mac = mac, mod = mac / (1 + y),
    conv = sum(t * (t + 1) * pv) / (sum(pv) * (1 + y)^2), T = T)
}

# fim de mês de referência
est <- est |> mutate(ref = as.Date(format(mes + 31, "%Y-%m-01")) - 1)

tit <- est |> filter(tipo %in% names(cupom_aa), venc > ref)
taxas <- tit |> distinct(mes, tipo, venc) |>
  rowwise() |> mutate(y = taxa_no_venc(mes, curva_de[[tipo]], venc) / 100) |> ungroup()
tit <- tit |> left_join(taxas, by = c("mes", "tipo", "venc")) |>
  rowwise() |>
  mutate(m = list(metricas(tipo, venc, ref, y))) |>
  ungroup() |>
  mutate(mac = sapply(m, `[[`, "mac"), mod = sapply(m, `[[`, "mod"),
         conv = sapply(m, `[[`, "conv"), anos = sapply(m, `[[`, "T")) |>
  select(-m)

lft <- est |> filter(tipo == "LFT", venc > ref) |>
  mutate(y = NA_real_, mac = 0, mod = 0, conv = 0, anos = as.numeric(venc - ref) / 365.25)

todos <- bind_rows(tit, lft)

# ---- 4. Agregados mensais --------------------------------------------------
pond <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])
dur_ind <- todos |>
  group_by(mes, indexador) |>
  summarise(estoque = sum(valor), mac = pond(mac, valor), mod = pond(mod, valor),
            conv = pond(conv, valor), prazo_venc = pond(anos, valor),
            vence_12m = 100 * sum(valor[anos <= 1]) / sum(valor),
            dv01 = sum(valor * mod, na.rm = TRUE) * 1e-4 * 1e3,   # R$ milhões por 1 p.b.
            .groups = "drop")
dur_tot <- todos |>
  group_by(mes) |>
  summarise(indexador = "Total doméstico (LFT com duration 0)",
            estoque = sum(valor), mac = pond(mac, valor), mod = pond(mod, valor),
            conv = pond(conv, valor), prazo_venc = pond(anos, valor),
            vence_12m = 100 * sum(valor[anos <= 1]) / sum(valor),
            dv01 = sum(valor * mod, na.rm = TRUE) * 1e-4 * 1e3, .groups = "drop")
dur <- bind_rows(dur_ind, dur_tot) |> arrange(mes, indexador)

cobertura <- est |> group_by(mes) |>
  summarise(cobertura = 100 * sum(valor[tipo %in% c(names(cupom_aa), "LFT")]) / sum(valor))
cat(sprintf("\nCobertura da duration (dívida em mercado): %.1f%% no último mês\n",
            tail(cobertura$cobertura, 1)))

ult <- max(dur$mes)
cat("\n== Último mês:", format(ult, "%m/%Y"), "==\n")
print(dur |> filter(mes == ult) |>
        mutate(across(c(estoque, mac, mod, conv, prazo_venc, vence_12m, dv01), \(x) round(x, 2))),
      width = 200)
write_csv2(dur, "duration_mensal.csv")
write_csv2(todos |> filter(mes == ult) |>
             select(titulo, tipo, indexador, venc, valor, y, mac, mod, conv, anos) |>
             arrange(indexador, venc),
           "duration_por_titulo_ultimo_mes.csv")

# ---- 5. Choque de curva: quanto o mercado perde na marcação ----------------
# Perda ~ valor x (ModD x dy - 0,5 x Conv x dy^2), por título.
choque <- function(dpre, dreal) {
  todos |> filter(mes == ult, tipo != "LFT") |>
    mutate(dy = if_else(tipo %in% c("LTN", "NTN-F"), dpre, dreal) / 100,
           perda = valor * (mod * dy - 0.5 * conv * dy^2)) |>
    group_by(indexador) |> summarise(perda_R_bi = sum(perda), .groups = "drop") |>
    mutate(cenario = sprintf("pré +%.1f p.p., real +%.1f p.p.", dpre, dreal))
}
cenarios <- bind_rows(
  choque(1.0, 1.0),
  choque(1.5, 1.2),     # ciclo de corte de 2023: 12 meses depois
  choque(3.0, 2.7),     # ciclo de corte de 2023: 24 meses depois
  choque(3.7, 3.7))     # modelo ilustrativo, curva desconfiada, canetada de 1 p.p.
cat("\n== Perda de marcação a mercado de quem carrega prefixados e NTN-B (R$ bi) ==\n")
print(cenarios |> mutate(perda_R_bi = round(perda_R_bi, 1)) |>
        pivot_wider(names_from = indexador, values_from = perda_R_bi))
write_csv2(cenarios, "choque_curva_marcacao.csv")

# ---- 6. Gráficos -----------------------------------------------------------
cores <- c("Prefixado" = "#2E86C1", "Índice de preços" = "#7D9D3C",
           "Selic (LFT)" = "#C0392B", "Total doméstico (LFT com duration 0)" = "grey20")
g1 <- dur |>
  select(mes, indexador, `Duration modificada (anos)` = mod,
         `Prazo até o vencimento (anos)` = prazo_venc, `Vence em 12 meses (%)` = vence_12m) |>
  pivot_longer(-c(mes, indexador)) |>
  filter(!(name == "Duration modificada (anos)" & indexador == "Selic (LFT)")) |>
  ggplot(aes(mes, value, colour = indexador)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = cores, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Duration e prazo da dívida pública em mercado",
       subtitle = "Ponderado pelo estoque. LFT tem duration de taxa zero: reprecifica pela Selic todo dia.",
       x = NULL, y = NULL,
       caption = "Fontes: Tesouro Transparente (Estoque da DPF) e Tesouro Direto (taxas)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2))
g1
ggsave("23_duration_divida.png", g1, width = 10, height = 9, dpi = 150)

g2 <- cenarios |> filter(indexador != "Outros") |>
  mutate(cenario = factor(cenario, unique(cenario))) |>
  ggplot(aes(cenario, perda_R_bi, fill = indexador)) +
  geom_col() +
  scale_fill_manual(values = cores, name = NULL) +
  labs(title = "Quanto quem carrega prefixados e NTN-B perde se a curva subir",
       subtitle = sprintf("Perda de marcação a mercado, R$ bilhões, estoque de %s", format(ult, "%m/%Y")),
       x = NULL, y = NULL,
       caption = "Aproximação por duration modificada e convexidade, título a título") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g2
ggsave("24_choque_curva.png", g2, width = 9, height = 5.5, dpi = 150)
