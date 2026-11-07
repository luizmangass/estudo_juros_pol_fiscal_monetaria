# =====================================================================
# Por que a composição da dívida muda? E como o mercado reagiu aos
# ciclos de corte de juros?
#
# Parte A. Regressão: a mudança da participação de LFT, prefixados e
#          índice de preços nos 12 meses seguintes, contra as condições
#          de mercado de hoje (risco, volatilidade da curva, inclinação,
#          inflação implícita, desancoragem, primário estrutural).
# Parte B. Estudo de evento: três ciclos de corte da Selic
#          - Tombini, ago/2011: corte surpresa, visto como político
#          - Ilan, out/2016: corte com teto de gastos recém-aprovado
#          - Campos Neto, ago/2023: corte com arcabouço novo
#          e o que aconteceu com curva, risco, câmbio e composição.
#
# Entradas (dados/): precotaxatesourodireto.csv, bcdata.sgs.432_*.csv,
#   bcdata.sgs.3698.csv, focus_ipca_*.xls, resultado-fiscal-estrutural.xlsx
#   e, gerados antes: composicao_indexador.csv, risco_pais_emendado.csv
# =====================================================================
library(readxl); library(dplyr); library(tidyr); library(readr)
library(ggplot2); library(scales); library(sandwich); library(lmtest)

mes_de <- function(d) as.Date(format(d, "%Y-%m-01"))

# ---- 1. Curva diária do Tesouro Direto -----------------------------------
td <- read_delim("precotaxatesourodireto.csv", delim = ";",
                 locale = locale(decimal_mark = ",", grouping_mark = "."),
                 show_col_types = FALSE) |>
  transmute(tipo = `Tipo Titulo`, venc = as.Date(`Data Vencimento`, "%d/%m/%Y"),
            data = as.Date(`Data Base`, "%d/%m/%Y"), taxa = `Taxa Compra Manha`) |>
  filter(!is.na(taxa), taxa > 0) |>
  mutate(prazo = as.numeric(venc - data) / 365.25,
         classe = case_when(
           tipo %in% c("Tesouro Prefixado", "Tesouro Prefixado com Juros Semestrais") ~ "pre",
           tipo %in% c("Tesouro IPCA+", "Tesouro IPCA+ com Juros Semestrais") ~ "real",
           TRUE ~ NA_character_)) |>
  filter(!is.na(classe))

interp5 <- function(prazo, taxa) {
  if (min(prazo) > 5 || max(prazo) < 5) return(taxa[which.min(abs(prazo - 5))])
  approx(prazo, taxa, xout = 5, ties = mean)$y
}
curva_d <- td |>
  group_by(data, classe) |>
  summarise(t5 = interp5(prazo, taxa), .groups = "drop") |>
  pivot_wider(names_from = classe, values_from = t5) |>
  filter(!is.na(pre), !is.na(real)) |>
  arrange(data) |>
  mutate(implicita = 100 * ((1 + pre / 100) / (1 + real / 100) - 1),
         d_pre = pre - lag(pre))

curva_m <- curva_d |>
  group_by(mes = mes_de(data)) |>
  summarise(pre5 = last(pre), real5 = last(real), implicita5 = last(implicita),
            vol_pre5 = sd(d_pre, na.rm = TRUE), .groups = "drop")

# ---- 2. Demais séries mensais --------------------------------------------
le_sgs <- function(arq, nome) read_delim(arq, delim = ";", col_types = "cc",
    locale = locale(decimal_mark = ",")) |>
  transmute(data = as.Date(data, "%d/%m/%Y"),
            !!nome := as.numeric(sub(",", ".", valor, fixed = TRUE)))
selic <- list.files(pattern = "^bcdata\\.sgs\\.432_.*\\.csv$") |>
  lapply(le_sgs, nome = "selic") |> bind_rows() |> distinct(data, .keep_all = TRUE) |>
  group_by(mes = mes_de(data)) |> summarise(selic = last(selic), .groups = "drop")
cambio <- le_sgs("bcdata.sgs.3698.csv", "cambio") |> transmute(mes = data, cambio)
risco  <- read_csv2("risco_pais_emendado.csv", show_col_types = FALSE) |>
  transmute(mes = as.Date(mes), risco)

le_focus_ipca <- function(a) {
  cab <- read_excel(a, skip = 1, n_max = 0, .name_repair = "minimal") |> names()
  x <- read_excel(a, skip = 2, col_names = FALSE, guess_max = 100000)
  names(x) <- c("data", as.character(as.Date(as.numeric(cab[-1]), origin = "1899-12-30")))
  x |> mutate(data = as.Date(data)) |>
    pivot_longer(-data, names_to = "mes_ref", values_to = "v", values_transform = as.numeric) |>
    filter(!is.na(v), !is.na(data)) |> mutate(mes_ref = as.Date(mes_ref))
}
meta_ano <- c(setNames(rep(4.5, 14), 2005:2018), "2019" = 4.25, "2020" = 4.0,
              "2021" = 3.75, "2022" = 3.5, "2023" = 3.25, setNames(rep(3.0, 5), 2024:2028))
desanc <- list.files(pattern = "^focus_ipca_.*\\.xls$") |> lapply(le_focus_ipca) |>
  bind_rows() |> distinct(data, mes_ref, .keep_all = TRUE) |>
  mutate(m0 = mes_de(data),
         k = 12 * (as.integer(format(mes_ref, "%Y")) - as.integer(format(m0, "%Y"))) +
             as.integer(format(mes_ref, "%m")) - as.integer(format(m0, "%m"))) |>
  filter(k >= 1, k <= 12) |> group_by(data) |> filter(n() == 12) |>
  summarise(pi_e12 = 100 * (prod(1 + v / 100) - 1), .groups = "drop") |>
  group_by(mes = mes_de(data)) |> summarise(pi_e12 = last(pi_e12), .groups = "drop") |>
  mutate(a = as.integer(format(mes, "%Y")), m = as.integer(format(mes, "%m")),
         meta = ((12 - m) * meta_ano[as.character(a)] + m * meta_ano[as.character(a + 1)]) / 12,
         desanc = pi_e12 - meta) |> select(mes, pi_e12, desanc)

spe <- read_excel("resultado-fiscal-estrutural.xlsx", sheet = "Dados trimestrais", skip = 3) |>
  rename(tri = 1, estrutural = 5) |>
  transmute(tri = as.Date(tri), estrutural = as.numeric(estrutural)) |> filter(!is.na(tri))
# trimestre -> os três meses dele
spe_m <- spe |> rowwise() |>
  reframe(mes = seq(tri, by = "-1 month", length.out = 3), estrutural = estrutural)

comp <- read_csv2("composicao_indexador.csv", show_col_types = FALSE) |>
  mutate(mes = as.Date(data),
         ind = recode(indexador, "Selic (LFT)" = "LFT", "Prefixado (LTN + NTN-F)" = "PRE",
                      "Índice de preços (NTN-B, NTN-C)" = "IPCA", "Câmbio (DPFe)" = "CAMBIO",
                      "Demais" = "DEMAIS")) |>
  select(mes, ind, part) |> mutate(part = 100 * part) |>
  pivot_wider(names_from = ind, values_from = part)

base <- comp |>
  left_join(curva_m, by = "mes") |> left_join(selic, by = "mes") |>
  left_join(cambio, by = "mes") |> left_join(risco, by = "mes") |>
  left_join(desanc, by = "mes") |> left_join(spe_m, by = "mes") |>
  arrange(mes) |>
  mutate(inclinacao = pre5 - selic,
         juro_real = 100 * ((1 + selic / 100) / (1 + pi_e12 / 100) - 1))
View(base)
write_csv2(base, "base_composicao_mensal.csv")

# ---- Parte A. O que move a composição ------------------------------------
# Variável dependente: mudança da participação nos 12 meses SEGUINTES (p.p.).
# Regressores padronizados (desvio-padrão = 1) para comparar tamanhos.
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
dados_a <- base |>
  mutate(across(c(LFT, PRE, IPCA), \(x) lead(x, 12) - x, .names = "d12_{.col}"),
         z_risco = z(risco), z_vol = z(vol_pre5), z_incl = z(inclinacao),
         z_desanc = z(desanc), z_juro_real = z(juro_real), z_estrut = z(estrutural)) |>
  filter(!is.na(d12_LFT), !is.na(z_risco), !is.na(z_vol), !is.na(z_desanc),
         !is.na(z_estrut))
cat("\nAmostra da Parte A:", format(range(dados_a$mes)), "-", nrow(dados_a), "meses\n")

regs <- lapply(c("LFT", "PRE", "IPCA"), function(k) {
  f <- as.formula(paste0("d12_", k, " ~ z_risco + z_vol + z_incl + z_desanc + z_juro_real + z_estrut"))
  m <- lm(f, data = dados_a)
  ct <- coeftest(m, vcov = NeweyWest(m, lag = 12, prewhite = FALSE))
  tibble(titulo = k, variavel = rownames(ct)[-1], coef = ct[-1, 1], ep = ct[-1, 2],
         p = ct[-1, 4], r2 = summary(m)$r.squared)
}) |> bind_rows()
print(regs |> mutate(across(c(coef, ep, p, r2), \(x) round(x, 3))), n = Inf)
write_csv2(regs, "regressao_composicao.csv")

rot_var <- c(z_risco = "Risco-país", z_vol = "Volatilidade do juro longo",
             z_incl = "Inclinação (pré 5a - Selic)", z_desanc = "Desancoragem (IPCA esp. - meta)",
             z_juro_real = "Juro real ex-ante", z_estrut = "Primário estrutural")
rot_tit <- c(LFT = "LFT (Selic)", PRE = "Prefixados", IPCA = "Índice de preços")
gA <- regs |>
  mutate(variavel = factor(rot_var[variavel], rev(rot_var)),
         titulo = factor(rot_tit[titulo], rot_tit), sig = p < 0.10) |>
  ggplot(aes(coef, variavel, colour = sig)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_errorbarh(aes(xmin = coef - 1.645 * ep, xmax = coef + 1.645 * ep), height = 0.2) +
  geom_point(size = 2.5) +
  facet_wrap(~titulo) +
  scale_colour_manual(values = c(`TRUE` = "#C0392B", `FALSE` = "grey60"),
                      labels = c(`TRUE` = "p < 0,10", `FALSE` = "não significativo"), name = NULL) +
  labs(title = "O que move a composição da dívida nos 12 meses seguintes",
       subtitle = "Mudança da participação (p.p.) quando cada condição está 1 desvio-padrão acima da média. IC 90%.",
       x = "p.p. de participação", y = NULL,
       caption = "Fontes: Tesouro (RMD, Tesouro Direto), BCB (Focus, SGS), SPE, Ipeadata/Investing. Erros Newey-West.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
gA
ggsave("21_o_que_move_a_composicao.png", gA, width = 11, height = 5.5, dpi = 150)

# ---- Parte B. Estudo de evento: ciclos de corte -------------------------
ciclos <- tibble(
  ciclo = c("Tombini, ago/2011 (corte surpresa)", "Ilan, out/2016 (teto aprovado)",
            "Campos Neto, ago/2023 (arcabouço)"),
  inicio = as.Date(c("2011-08-01", "2016-10-01", "2023-08-01")))

ev <- lapply(seq_len(nrow(ciclos)), function(j) {
  t0 <- ciclos$inicio[j]
  base |>
    mutate(k = 12 * (as.integer(format(mes, "%Y")) - as.integer(format(t0, "%Y"))) +
               as.integer(format(mes, "%m")) - as.integer(format(t0, "%m"))) |>
    filter(k >= -6, k <= 30) |>
    mutate(ciclo = ciclos$ciclo[j])
}) |> bind_rows()

ref <- ev |> filter(k == -1) |>
  select(ciclo, selic0 = selic, pre50 = pre5, real50 = real5, imp0 = implicita5,
         risco0 = risco, cambio0 = cambio, desanc0 = desanc, LFT0 = LFT, PRE0 = PRE, IPCA0 = IPCA)
ev <- ev |> left_join(ref, by = "ciclo") |>
  transmute(ciclo, k,
            `Selic (p.p.)` = selic - selic0,
            `Prefixado 5 anos (p.p.)` = pre5 - pre50,
            `Juro real 5 anos, NTN-B (p.p.)` = real5 - real50,
            `Inflação implícita 5 anos (p.p.)` = implicita5 - imp0,
            `Desancoragem (p.p.)` = desanc - desanc0,
            `Risco-país (p.b.)` = risco - risco0,
            `Câmbio (%)` = 100 * (cambio / cambio0 - 1),
            `Participação LFT (p.p.)` = LFT - LFT0,
            `Participação prefixados (p.p.)` = PRE - PRE0) |>
  pivot_longer(-c(ciclo, k), names_to = "variavel", values_to = "valor") |>
  mutate(variavel = factor(variavel, unique(variavel)),
         ciclo = factor(ciclo, ciclos$ciclo))
write_csv2(ev, "evento_ciclos_corte.csv")

gB <- ggplot(ev, aes(k, valor, colour = ciclo)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey40") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~variavel, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = c("#C0392B", "#2E86C1", "#7D9D3C"), name = NULL) +
  scale_x_continuous(breaks = seq(-6, 30, 6)) +
  labs(title = "Três ciclos de corte da Selic: como o mercado reagiu",
       subtitle = "Variação em relação ao mês anterior ao primeiro corte (mês 0 = início do ciclo)",
       x = "Meses desde o primeiro corte", y = NULL,
       caption = "Fontes: BCB, Tesouro Direto, RMD, Focus, Ipeadata/Investing") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
gB
ggsave("22_ciclos_de_corte.png", gB, width = 11, height = 9, dpi = 150)

# Resumo numérico: 12 e 24 meses depois do início
print(ev |> filter(k %in% c(12, 24)) |>
        pivot_wider(names_from = k, values_from = valor, names_prefix = "mes_") |>
        mutate(across(starts_with("mes_"), \(x) round(x, 2))) |>
        arrange(variavel, ciclo), n = Inf)
