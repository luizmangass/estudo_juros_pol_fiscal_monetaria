# =====================================================================
# Por que a âncora fiscal funcionou e depois falhou?
# Separa três medidas e vê qual explica a perda de ancoragem:
#   1. Postura monetária = juro real ex-ante - juro neutro
#   2. Impulso fiscal    = queda do resultado primário ESTRUTURAL (SPE)
#   3. Desancoragem      = IPCA esperado 12 meses - meta
#
# Entradas (mesma pasta):
#   focus_ipca_2009_2017.xls / focus_ipca_2017_2026.xls  Focus, IPCA mensal esperado (mediana)
#   bcdata.sgs.432_*.csv                   meta Selic diária
#   resultado-fiscal-estrutural.xlsx       SPE/Fazenda
#   estimativas-do-hiato-do-produto-ifi.xlsx
#   precotaxatesourodireto.csv             taxas do Tesouro Direto (IPCA+), juro neutro 5a5a
#   risco_pais_emendado.csv                gerado por surpresas_copom.R
# =====================================================================
library(readxl); library(dplyr); library(tidyr); library(readr)
library(ggplot2); library(scales); library(sandwich); library(lmtest)

# ---- 1. IPCA esperado nos próximos 12 meses ------------------------------
# O Focus traz a expectativa de IPCA para cada mês. Para cada data,
# acumula os 12 meses seguintes ao mês corrente.
le_focus_ipca <- function(a) {
  cab <- read_excel(a, skip = 1, n_max = 0, .name_repair = "minimal") |> names()
  meses_ref <- as.Date(as.numeric(cab[-1]), origin = "1899-12-30")
  x <- read_excel(a, skip = 2, col_names = FALSE, guess_max = 100000)
  names(x) <- c("data", as.character(meses_ref))
  x |> mutate(data = as.Date(data)) |>
    pivot_longer(-data, names_to = "mes_ref", values_to = "ipca_m",
                 values_transform = as.numeric) |>
    filter(!is.na(ipca_m), !is.na(data)) |>
    mutate(mes_ref = as.Date(mes_ref))
}
focus <- list.files(pattern = "^focus_ipca_.*\\.xls$") |>
  lapply(le_focus_ipca) |> bind_rows() |>
  distinct(data, mes_ref, .keep_all = TRUE)

pi_e12 <- focus |>
  mutate(mes_data = as.Date(format(data, "%Y-%m-01")),
         k = (as.integer(format(mes_ref, "%Y")) - as.integer(format(mes_data, "%Y"))) * 12 +
             as.integer(format(mes_ref, "%m")) - as.integer(format(mes_data, "%m"))) |>
  filter(k >= 1, k <= 12) |>
  group_by(data) |>
  filter(n() == 12) |>
  summarise(pi_e12 = 100 * (prod(1 + ipca_m / 100) - 1), .groups = "drop") |>
  group_by(mes = as.Date(format(data, "%Y-%m-01"))) |>
  summarise(pi_e12 = last(pi_e12), .groups = "drop")      # último dado do mês

# ---- 2. Meta de inflação (CMN) -------------------------------------------
# A janela t+1..t+12 cai em parte no ano corrente (12 - m meses) e em parte
# no seguinte (m meses): a meta relevante é a média ponderada das duas.
# A partir de 2025 a meta é contínua, de 3%.
meta_ano <- c(setNames(rep(4.5, 14), 2005:2018),
              "2019" = 4.25, "2020" = 4.0, "2021" = 3.75, "2022" = 3.5,
              "2023" = 3.25, setNames(rep(3.0, 5), 2024:2028))
pi_e12 <- pi_e12 |>
  mutate(a = as.integer(format(mes, "%Y")), m = as.integer(format(mes, "%m")),
         meta = ((12 - m) * meta_ano[as.character(a)] +
                 m * meta_ano[as.character(a + 1)]) / 12,
         desancoragem = pi_e12 - meta) |>
  select(-a, -m)

# ---- 3. Selic e juro real ex-ante ----------------------------------------
selic <- list.files(pattern = "^bcdata\\.sgs\\.432.*\\.csv$") |>
  lapply(\(a) read_delim(a, delim = ";", col_types = "cc",
                         locale = locale(decimal_mark = ","))) |>
  bind_rows() |>
  transmute(data = as.Date(data, "%d/%m/%Y"),
            selic = as.numeric(sub(",", ".", valor, fixed = TRUE))) |>
  distinct(data, .keep_all = TRUE) |>
  group_by(mes = as.Date(format(data, "%Y-%m-01"))) |>
  summarise(selic = last(selic), .groups = "drop")

# ---- 4. Juro neutro: duas medidas ----------------------------------------
# (a) Mercado: juro real FORWARD 5 anos daqui a 5 anos (5a5a), tirado das
#     taxas do Tesouro IPCA+ (arquivo precotaxatesourodireto.csv). Olhar
#     5 anos à frente tira o ciclo monetário atual; sobra o juro real de
#     equilíbrio mais prêmios de prazo e de risco (por isso é um teto).
#       f(5,10) = [(1+r10)^10 / (1+r5)^5]^(1/5) - 1
#     r5 e r10 vêm de interpolação linear entre os vencimentos do dia.
# (b) IFI: juro real ex-ante de longo prazo no cenário base = 5,0%.
td <- read_delim("precotaxatesourodireto.csv", delim = ";",
                 locale = locale(decimal_mark = ",", grouping_mark = "."),
                 show_col_types = FALSE) |>
  transmute(tipo = `Tipo Titulo`, venc = as.Date(`Data Vencimento`, "%d/%m/%Y"),
            data = as.Date(`Data Base`, "%d/%m/%Y"), taxa = `Taxa Compra Manha`) |>
  filter(tipo %in% c("Tesouro IPCA+", "Tesouro IPCA+ com Juros Semestrais"),
         !is.na(taxa), taxa > 0) |>
  mutate(prazo = as.numeric(venc - data) / 365.25)

fwd_5a5a <- function(prazo, taxa) {
  if (min(prazo) > 5 || max(prazo) < 10) return(NA_real_)
  o <- order(prazo); prazo <- prazo[o]; taxa <- taxa[o]
  r5  <- approx(prazo, taxa, xout = 5,  ties = mean)$y / 100
  r10 <- approx(prazo, taxa, xout = 10, ties = mean)$y / 100
  100 * (((1 + r10)^10 / (1 + r5)^5)^(1 / 5) - 1)
}
neutro_mkt <- td |>
  group_by(data) |>
  summarise(fwd = fwd_5a5a(prazo, taxa), .groups = "drop") |>
  filter(!is.na(fwd)) |>
  group_by(mes = as.Date(format(data, "%Y-%m-01"))) |>
  summarise(neutro_5a5a = mean(fwd), .groups = "drop")
write_csv2(neutro_mkt, "juro_neutro_5a5a.csv")

# ---- 5. Fiscal estrutural (SPE) e hiato (IFI), trimestrais ---------------
spe <- read_excel("resultado-fiscal-estrutural.xlsx", sheet = "Dados trimestrais",
                  skip = 3) |>
  rename(tri = 1, convencional = 2, ciclico = 3, nao_recorrente = 4, estrutural = 5) |>
  mutate(tri = as.Date(tri)) |>
  filter(!is.na(tri)) |>
  select(tri, convencional, ciclico, nao_recorrente, estrutural) |>
  mutate(across(-tri, as.numeric))
hiato <- read_excel("estimativas-do-hiato-do-produto-ifi.xlsx", sheet = "Hiato do Produto",
                    skip = 1) |>
  rename(tri = 1, hiato_inf = 2, hiato = 3, hiato_sup = 4) |>
  mutate(tri = as.Date(tri), across(c(hiato_inf, hiato, hiato_sup), \(x) 100 * as.numeric(x))) |>
  filter(!is.na(tri)) |> select(tri, hiato, hiato_inf, hiato_sup)

risco <- read_csv2("risco_pais_emendado.csv", show_col_types = FALSE) |>
  transmute(mes = as.Date(mes), risco)

# ---- 6. Base mensal e trimestral -----------------------------------------
base_m <- pi_e12 |>
  inner_join(selic, by = "mes") |>
  left_join(neutro_mkt, by = "mes") |>
  left_join(risco, by = "mes") |>
  mutate(juro_real = 100 * ((1 + selic / 100) / (1 + pi_e12 / 100) - 1),
         postura_mercado = juro_real - neutro_5a5a,
         postura_ifi     = juro_real - 5.0) |>
  arrange(mes)
View(base_m)

# Trimestre identificado pelo último mês (mar, jun, set, dez), como no SPE
base_t <- base_m |>
  mutate(tri = as.Date(paste0(format(mes, "%Y-"),
                              sprintf("%02d", 3 * ceiling(as.integer(format(mes, "%m")) / 3)),
                              "-01"))) |>
  group_by(tri) |>
  summarise(across(c(juro_real, pi_e12, desancoragem, postura_mercado,
                     postura_ifi, risco, selic), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  inner_join(spe, by = "tri") |>
  left_join(hiato, by = "tri") |>
  arrange(tri) |>
  mutate(impulso_fiscal = -(estrutural - lag(estrutural, 4)))   # + = expansão em 12 meses
View(base_t)

# ---- 7. Gráfico: as três medidas lado a lado -----------------------------
eventos <- tibble(
  data = as.Date(c("2016-12-01", "2021-10-01", "2022-12-01", "2023-08-01", "2024-04-01", "2024-11-01")),
  rotulo = c("Teto", "PEC precatórios", "PEC Transição", "Arcabouço", "Metas revistas", "Pacote de gastos"))

painel <- bind_rows(
  base_m |> transmute(data = mes, serie = "Juro real ex-ante - neutro de mercado (NTN-B 5a5a)",
                      valor = postura_mercado, painel = "1. Postura monetária (p.p.)"),
  base_m |> transmute(data = mes, serie = "Juro real ex-ante - neutro (IFI, 5%)",
                      valor = postura_ifi, painel = "1. Postura monetária (p.p.)"),
  base_t |> transmute(data = tri, serie = "Impulso fiscal estrutural, 12 meses",
                      valor = impulso_fiscal, painel = "2. Impulso fiscal (% PIB, + = expansão)"),
  base_m |> transmute(data = mes, serie = "IPCA esperado 12m - meta",
                      valor = desancoragem, painel = "3. Desancoragem (p.p.)")) |>
  filter(!is.na(valor), data >= as.Date("2010-01-01"))

g1 <- ggplot(painel, aes(data, valor, colour = serie)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_vline(data = eventos, aes(xintercept = data), linetype = "dotted",
             colour = "grey50", inherit.aes = FALSE) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~painel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("#2E86C1", "#85C1E9", "#C0392B", "#7D9D3C"), name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Âncora fiscal: postura monetária, impulso fiscal e expectativas",
       subtitle = paste("Linhas pontilhadas: teto (dez/16), PEC dos precatórios (out/21),",
                        "PEC da Transição (dez/22), arcabouço (ago/23),\nrevisão das metas (abr/24), pacote de gastos (nov/24)"),
       x = NULL, y = NULL,
       caption = "Fontes: BCB (Focus, SGS 432), SPE/Fazenda, IFI, Tesouro Direto") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2))
g1
ggsave("16_ancora_tres_medidas.png", g1, width = 10, height = 9, dpi = 150)

# ---- 8. Quem explica a desancoragem e o risco? ---------------------------
# Trimestral. Postura e impulso defasados (1 a 4 trimestres: política age
# com defasagem). Controles: hiato. Erros Newey-West.
base_r <- base_t |>
  mutate(postura_1 = lag(postura_ifi), postura_4 = lag(postura_ifi, 4),
         impulso_1 = lag(impulso_fiscal), impulso_4 = lag(impulso_fiscal, 4),
         hiato_1 = lag(hiato))
nw <- \(m) coeftest(m, vcov = NeweyWest(m, lag = 4, prewhite = FALSE))

cat("\n== Desancoragem ~ postura monetária + impulso fiscal (defasagem de 1 trimestre) ==\n")
m1 <- lm(desancoragem ~ postura_1 + impulso_1 + hiato_1, data = base_r); print(nw(m1))
cat("\n== Mesma coisa, defasagem de 4 trimestres ==\n")
m4 <- lm(desancoragem ~ postura_4 + impulso_4 + hiato_1, data = base_r); print(nw(m4))
cat("\n== Risco-país ~ postura + impulso + estrutural (nível) ==\n")
mr <- lm(risco ~ postura_1 + impulso_1 + estrutural, data = base_r); print(nw(mr))

# ---- 9. Tabela por período -------------------------------------------------
periodos <- base_t |>
  mutate(periodo = cut(tri, as.Date(c("2010-01-01", "2014-07-01", "2017-01-01",
                                      "2020-03-01", "2022-01-01", "2023-01-01", "2027-01-01")),
                       labels = c("2010-14T2", "2014T3-2016", "2017-2019 (teto)",
                                  "2020-2021", "2022", "2023-2026"), right = FALSE)) |>
  filter(!is.na(periodo)) |>
  group_by(periodo) |>
  summarise(juro_real = mean(juro_real),
            neutro_mercado = mean(juro_real - postura_mercado, na.rm = TRUE),
            postura_ifi = mean(postura_ifi), postura_mercado = mean(postura_mercado, na.rm = TRUE),
            estrutural = mean(estrutural), impulso = mean(impulso_fiscal, na.rm = TRUE),
            desancoragem = mean(desancoragem), risco = mean(risco, na.rm = TRUE),
            .groups = "drop") |>
  mutate(across(-periodo, \(x) round(x, 2)))
print(periodos)
write_csv2(periodos, "ancora_por_periodo.csv")
write_csv2(base_t, "ancora_base_trimestral.csv")
