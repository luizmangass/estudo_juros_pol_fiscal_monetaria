# =====================================================================
# Composição a preço de mercado, duration, convexidade e DV01 da DPF
#
# Entradas (dados/):
#   estoquedpf.csv              Tesouro Transparente, "Estoque da DPF":
#                               título, vencimento, valor CONTÁBIL e QUANTIDADE,
#                               mensal (09/2017 em diante)
#   precotaxatesourodireto.csv  taxas e preços do Tesouro Direto
#
# Passo a passo:
#   1. Lê o estoque em mercado, título a título.
#   2. Monta as curvas do Tesouro Direto no último dia de cada mês
#      (taxa média entre compra e venda, sem o spread de um lado só).
#   3. Para cada título calcula a taxa no seu vencimento (interpolação na
#      curva do indexador) e o preço por unidade de valor nominal (cotação).
#   4. Valor nominal atualizado (VNA) de cada indexador no mês: sai dos
#      próprios títulos do Tesouro Direto (VNA = PU / cotação). Para LTN e
#      NTN-F o valor de face é R$ 1.000.
#   5. Valor de MERCADO = quantidade x VNA x cotação.
#   6. Duration de Macaulay, modificada, convexidade e DV01, ponderadas pelo
#      valor de mercado; composição por indexador a mercado e contábil.
#   7. Choques de curva: perda de marcação a mercado de quem carrega o estoque.
#
# Convenções: tempo em anos corridos (/365,25), cupons semestrais; LTN e NTN-B
# Principal sem cupom; NTN-F 10% a.a.; NTN-B e NTN-C 6% a.a. LFT tem duration
# de TAXA zero (o valor acompanha a Selic) e é precificada pela taxa do
# Tesouro Selic (ágio/deságio sobre a Selic).
# Dívida externa e títulos residuais (TDA, CVS etc.) ficam pelo valor contábil.
#
# Saídas: duration_mensal.csv, composicao_mercado_mensal.csv,
#         perfil_titulos_ultimo_mes.csv, choque_curva_marcacao.csv,
#         gráficos 23 (duration), 24 (choque) e 25 (composição a mercado).
# =====================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2); library(scales)

num <- function(x) as.numeric(sub(",", ".", x, fixed = TRUE))

# ---- 1. Estoque por título ---------------------------------------------------
est <- read_delim("estoquedpf.csv", delim = ";", col_types = cols(.default = "c")) |>
  transmute(titulo = `Titulo/Contrato`,
            venc   = as.Date(`Vencimento do Titulo/Contrato`, "%d/%m/%Y"),
            valor_contabil = num(`Valor do Estoque`) / 1e9,            # R$ bi
            quantidade     = num(`Quantidade do Estoque`),
            mes    = as.Date(paste0("01/", `Mes do Estoque`), "%d/%m/%Y"),
            carteira = `Classe da Carteira`, tipo_divida = `Tipo de Divida`) |>
  filter(carteira == "Mercado", !is.na(mes), !is.na(valor_contabil)) |>
  mutate(tipo = case_when(
           grepl("^NTN-B1", titulo)          ~ "NTN-B1",   # Educa+ e Renda+: fluxo amortizável, fica pelo contábil
           grepl("^NTN-B Principal", titulo) ~ "NTN-B Principal",
           grepl("^NTN-B", titulo)           ~ "NTN-B",
           grepl("^NTN-F", titulo)           ~ "NTN-F",
           grepl("^NTN-C", titulo)           ~ "NTN-C",
           grepl("^LTN", titulo)             ~ "LTN",
           grepl("^LFT ", titulo)            ~ "LFT",     # "LFT-TD..." (unidade fracionária do Tesouro Direto) fica em Outros
           grepl("Externa", tipo_divida)     ~ "Externa",
           TRUE                              ~ "Outros"),
         indexador = case_when(
           tipo == "LFT" ~ "Selic (LFT)",
           tipo %in% c("LTN", "NTN-F") ~ "Prefixado",
           tipo %in% c("NTN-B", "NTN-B Principal", "NTN-B1", "NTN-C") ~ "Índice de preços",
           tipo == "Externa" ~ "Câmbio (externa)",
           TRUE ~ "Outros"),
         ref = as.Date(format(mes + 31, "%Y-%m-01")) - 1)        # último dia do mês
cat("Estoque em mercado:", format(range(est$mes)), "\n")
View(est)

# ---- 2. Curvas do Tesouro Direto no fim de cada mês --------------------------
td <- read_delim("precotaxatesourodireto.csv", delim = ";",
                 locale = locale(decimal_mark = ",", grouping_mark = "."),
                 show_col_types = FALSE) |>
  transmute(tipo_td = `Tipo Titulo`, venc = as.Date(`Data Vencimento`, "%d/%m/%Y"),
            data = as.Date(`Data Base`, "%d/%m/%Y"),
            taxa = (`Taxa Compra Manha` + `Taxa Venda Manha`) / 2,
            pu   = (`PU Compra Manha` + `PU Venda Manha`) / 2) |>
  filter(!is.na(taxa), data >= as.Date("2017-08-01")) |>
  mutate(curva = case_when(
           tipo_td %in% c("Tesouro Prefixado", "Tesouro Prefixado com Juros Semestrais") ~ "pre",
           tipo_td %in% c("Tesouro IPCA+", "Tesouro IPCA+ com Juros Semestrais",
                          "Tesouro Educa+", "Tesouro Renda+ Aposentadoria Extra") ~ "real",
           tipo_td == "Tesouro IGPM+ com Juros Semestrais" ~ "igpm",
           tipo_td == "Tesouro Selic" ~ "selic", TRUE ~ NA_character_),
         # estrutura de fluxo equivalente no estoque (para achar o VNA)
         tipo = case_when(
           tipo_td == "Tesouro Prefixado" ~ "LTN",
           tipo_td == "Tesouro Prefixado com Juros Semestrais" ~ "NTN-F",
           tipo_td == "Tesouro IPCA+" ~ "NTN-B Principal",
           tipo_td == "Tesouro IPCA+ com Juros Semestrais" ~ "NTN-B",
           tipo_td == "Tesouro IGPM+ com Juros Semestrais" ~ "NTN-C",
           tipo_td == "Tesouro Selic" ~ "LFT", TRUE ~ NA_character_),
         mes = as.Date(format(data, "%Y-%m-01"))) |>
  filter(!is.na(curva))
fim <- td |> group_by(mes) |> summarise(data = max(data), .groups = "drop")
td_fim <- td |> semi_join(fim, by = c("mes", "data"))
curvas <- td_fim |> group_by(mes, curva, venc) |> summarise(taxa = mean(taxa), .groups = "drop")

taxa_no_venc <- function(m, cv, v) {
  cc <- curvas[curvas$mes == m & curvas$curva == cv, ]
  if (nrow(cc) == 0 && cv == "igpm") cc <- curvas[curvas$mes == m & curvas$curva == "real", ]
  if (nrow(cc) == 0) return(NA_real_)
  if (nrow(cc) == 1) return(cc$taxa)
  approx(as.numeric(cc$venc), cc$taxa, xout = as.numeric(v), rule = 2, ties = mean)$y
}

# ---- 3. Fluxos, cotação e métricas por título --------------------------------
cupom_aa <- c("LTN" = 0, "NTN-B Principal" = 0, "NTN-F" = 0.10, "NTN-B" = 0.06,
              "NTN-C" = 0.06, "LFT" = 0)
curva_de <- c("LTN" = "pre", "NTN-F" = "pre", "NTN-B" = "real",
              "NTN-B Principal" = "real", "NTN-C" = "igpm", "LFT" = "selic")

metricas <- function(tipo, venc, ref, y) {
  T <- as.numeric(venc - ref) / 365.25
  if (is.na(y) || T <= 0) return(c(cot = NA, mac = 0, mod = 0, conv = 0, T = max(T, 0)))
  c_aa <- cupom_aa[[tipo]]
  if (c_aa == 0) { t <- T; cf <- 1 } else {
    t <- rev(seq(T, 0, by = -0.5)); t <- t[t > 0]
    cf <- rep((1 + c_aa)^0.5 - 1, length(t)); cf[length(cf)] <- cf[length(cf)] + 1
  }
  pv <- cf / (1 + y)^t
  mac <- sum(t * pv) / sum(pv)
  if (tipo == "LFT") return(c(cot = sum(pv), mac = 0, mod = 0, conv = 0, T = T))
  c(cot = sum(pv), mac = mac, mod = mac / (1 + y),
    conv = sum(t * (t + 1) * pv) / (sum(pv) * (1 + y)^2), T = T)
}
aplica <- function(df) {
  m <- mapply(metricas, df$tipo, df$venc, df$ref, df$y)
  bind_cols(df, as_tibble(t(m)))
}

# ---- 4. VNA de cada estrutura no mês, a partir do Tesouro Direto --------------
td_vna <- td_fim |> filter(!is.na(tipo), tipo_td != "Tesouro Educa+",
                           tipo_td != "Tesouro Renda+ Aposentadoria Extra") |>
  mutate(ref = as.Date(format(mes + 31, "%Y-%m-01")) - 1, y = taxa / 100) |>
  filter(venc > ref) |>
  aplica() |>
  mutate(vna = pu / cot) |>
  mutate(fam = case_when(tipo %in% c("NTN-B", "NTN-B Principal") ~ "IPCA",
                         tipo == "NTN-C" ~ "IGPM", tipo == "LFT" ~ "SELIC",
                         TRUE ~ "PRE")) |>
  group_by(mes, fam) |> summarise(vna = median(vna), .groups = "drop")
# Para LTN e NTN-F o valor de face é R$ 1.000 (o VNA estimado deve sair perto disso)
cat("\nChecagem: VNA estimado dos prefixados (deveria ser ~1.000):\n")
print(summary(td_vna$vna[td_vna$fam == "PRE"]))
td_vna <- td_vna |> mutate(vna = if_else(fam == "PRE", 1000, vna))

# ---- 5. Valor de mercado título a título --------------------------------------
tit <- est |> filter(tipo %in% names(cupom_aa), venc > ref)
taxas <- tit |> distinct(mes, tipo, venc) |>
  mutate(y = mapply(function(m, tp, v) taxa_no_venc(m, curva_de[[tp]], v) / 100,
                    mes, tipo, venc))
tit <- tit |> left_join(taxas, by = c("mes", "tipo", "venc")) |> aplica() |>
  mutate(fam = case_when(tipo %in% c("NTN-B", "NTN-B Principal") ~ "IPCA",
                         tipo == "NTN-C" ~ "IGPM", tipo == "LFT" ~ "SELIC", TRUE ~ "PRE")) |>
  left_join(td_vna, by = c("mes", "fam")) |>
  mutate(valor_mercado = quantidade * vna * cot / 1e9,
         # Proteção: se a unidade do título for diferente da do Tesouro Direto
         # (razão mercado/contábil fora de 0,5 a 2), fica o valor contábil.
         razao = valor_mercado / valor_contabil,
         valor_mercado = if_else(is.na(razao) | razao < 0.5 | razao > 2,
                                 valor_contabil, valor_mercado),
         anos = T)
cat(sprintf("\nTítulos que ficaram pelo valor contábil (unidade ou preço fora do padrão): %d de %d\n",
            sum(tit$razao < 0.5 | tit$razao > 2, na.rm = TRUE) + sum(is.na(tit$razao)), nrow(tit)))

resto <- est |> filter(!(tipo %in% names(cupom_aa)) | venc <= ref) |>
  mutate(valor_mercado = valor_contabil, mac = NA, mod = NA, conv = NA,
         anos = pmax(as.numeric(venc - ref) / 365.25, 0))
todos <- bind_rows(tit, resto)

# Checagem: razão mercado/contábil por indexador no último mês
ult <- max(todos$mes)
cat("\nÚltimo mês:", format(ult, "%m/%Y"), " - mercado x contábil (R$ bi)\n")
print(todos |> filter(mes == ult) |> group_by(indexador) |>
        summarise(contabil = round(sum(valor_contabil), 1),
                  mercado = round(sum(valor_mercado), 1),
                  razao = round(mercado / contabil, 3), .groups = "drop"))

# ---- 6. Composição e duration por mês -----------------------------------------
comp <- todos |>
  group_by(mes, indexador) |>
  summarise(contabil = sum(valor_contabil), mercado = sum(valor_mercado), .groups = "drop") |>
  group_by(mes) |>
  mutate(part_contabil = 100 * contabil / sum(contabil),
         part_mercado  = 100 * mercado / sum(mercado)) |> ungroup()
write_csv2(comp, "composicao_mercado_mensal.csv")

dom <- todos |> filter(tipo %in% names(cupom_aa), venc > ref)
pond <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])
resumo <- function(df) df |>
  summarise(estoque_mercado = sum(valor_mercado), estoque_contabil = sum(valor_contabil),
            mac = pond(mac, valor_mercado), mod = pond(mod, valor_mercado),
            conv = pond(conv, valor_mercado), prazo_venc = pond(anos, valor_mercado),
            vence_12m = 100 * sum(valor_mercado[anos <= 1]) / sum(valor_mercado),
            dv01_R_mi = sum(valor_mercado * mod, na.rm = TRUE) * 1e-4 * 1e3,
            .groups = "drop")
dur <- bind_rows(
  dom |> group_by(mes, indexador) |> resumo(),
  dom |> group_by(mes) |> resumo() |> mutate(indexador = "Total doméstico (LFT com duration 0)")) |>
  arrange(mes, indexador)
write_csv2(dur, "duration_mensal.csv")

cat("\n== Duration no último mês (pesos a mercado) ==\n")
print(dur |> filter(mes == ult) |> mutate(across(where(is.numeric), \(x) round(x, 2))), width = 200)
cat(sprintf("Cobertura (títulos com preço de mercado): %.1f%% da dívida em mercado\n",
            100 * sum(dom$valor_mercado[dom$mes == ult]) / sum(todos$valor_mercado[todos$mes == ult])))

perfil <- dom |> filter(mes == ult) |>
  select(titulo, tipo, indexador, venc, quantidade, valor_contabil, valor_mercado,
         y, mac, mod, conv, anos) |> arrange(indexador, venc)
write_csv2(perfil, "perfil_titulos_ultimo_mes.csv")

# ---- 7. Choques de curva --------------------------------------------------------
choque <- function(dpre, dreal) {
  perfil |> filter(tipo != "LFT") |>
    mutate(dy = if_else(tipo %in% c("LTN", "NTN-F"), dpre, dreal) / 100,
           perda = valor_mercado * (mod * dy - 0.5 * conv * dy^2)) |>
    group_by(indexador) |> summarise(perda_R_bi = sum(perda), .groups = "drop") |>
    mutate(cenario = sprintf("pré +%.1f p.p., real +%.1f p.p.", dpre, dreal))
}
cenarios <- bind_rows(choque(1.0, 1.0), choque(1.5, 1.2), choque(3.0, 2.7), choque(3.7, 3.7))
cat("\n== Perda de marcação a mercado de quem carrega prefixados e NTN-B (R$ bi) ==\n")
print(cenarios |> mutate(perda_R_bi = round(perda_R_bi, 1)) |>
        pivot_wider(names_from = indexador, values_from = perda_R_bi))
write_csv2(cenarios, "choque_curva_marcacao.csv")

# ---- 8. Gráficos ---------------------------------------------------------------
cores <- c("Prefixado" = "#2E86C1", "Índice de preços" = "#7D9D3C",
           "Selic (LFT)" = "#C0392B", "Câmbio (externa)" = "#8E6C8A", "Outros" = "#AAB7B8",
           "Total doméstico (LFT com duration 0)" = "grey20")

g1 <- dur |>
  select(mes, indexador, `Duration modificada (anos)` = mod,
         `Prazo até o vencimento (anos, sem cupons)` = prazo_venc,
         `Vence em 12 meses (% do estoque)` = vence_12m) |>
  pivot_longer(-c(mes, indexador)) |>
  filter(!(name == "Duration modificada (anos)" & indexador == "Selic (LFT)")) |>
  ggplot(aes(mes, value, colour = indexador)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = cores, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Duration e prazo da dívida pública em mercado",
       subtitle = "Pesos a preço de mercado. LFT tem duration de taxa zero: reprecifica pela Selic todo dia.",
       x = NULL, y = NULL,
       caption = "Fontes: Tesouro Transparente (Estoque da DPF) e Tesouro Direto (taxas e preços)") +
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
       subtitle = sprintf("Perda de marcação a mercado, R$ bilhões, estoque a mercado de %s", format(ult, "%m/%Y")),
       x = NULL, y = NULL,
       caption = "Duration modificada e convexidade, título a título") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g2
ggsave("24_choque_curva.png", g2, width = 9, height = 5.5, dpi = 150)

ordem <- c("Selic (LFT)", "Prefixado", "Índice de preços", "Câmbio (externa)", "Outros")
g3 <- comp |>
  select(mes, indexador, `A preço de mercado` = part_mercado, `Contábil` = part_contabil) |>
  pivot_longer(c(`A preço de mercado`, `Contábil`), names_to = "base") |>
  mutate(indexador = factor(indexador, ordem)) |>
  ggplot(aes(mes, value, fill = indexador)) +
  geom_area(colour = "white", linewidth = 0.1) +
  facet_wrap(~base, ncol = 2) +
  scale_fill_manual(values = cores, name = NULL) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), expand = c(0, 0)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0, 0)) +
  labs(title = "Composição da dívida pública em mercado: contábil x preço de mercado",
       subtitle = "Participação no estoque, título a título",
       x = NULL, y = NULL,
       caption = "Fontes: Tesouro Transparente (Estoque da DPF) e Tesouro Direto") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g3
ggsave("25_composicao_mercado.png", g3, width = 10, height = 5.5, dpi = 150)
