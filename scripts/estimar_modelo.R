# =====================================================================
# Estima, equação por equação, os parâmetros do modelo da canetada
# com os dados do estudo, antes e depois da quebra fiscal de 2014.
#
# Ideia: no lugar das expectativas racionais (não observadas) usamos as
# expectativas do Focus. Cada equação do modelo vira uma regressão
# trimestral (2010T1 a 2026T2), com erros Newey-West. Os parâmetros que
# mudam de regime têm um termo de interação com "pós-2014".
#
# Saídas:
#   parametros_estimados.csv  tabela de parâmetros (para o texto)
#   parametros_estimados.inc  arquivo lido pelo modelo no Dynare
#   base_modelo_trimestral.csv
#
# Rode com o diretório de trabalho em dados/ (o 00_rodar_tudo.R faz isso).
# Depende de risco_pais_emendado.csv (gerado por surpresas_copom.R).
# =====================================================================
library(readxl); library(dplyr); library(tidyr); library(readr)
library(sandwich); library(lmtest)

trimestre <- function(d) as.Date(paste0(format(d, "%Y-"),
  sprintf("%02d", 3 * ceiling(as.integer(format(d, "%m")) / 3)), "-01"))
le_sgs <- function(arq, nome) {
  read_delim(arq, delim = ";", col_types = "cc", locale = locale(decimal_mark = ",")) |>
    transmute(data = as.Date(data, "%d/%m/%Y"),
              !!nome := as.numeric(sub(",", ".", valor, fixed = TRUE)))
}
nw <- function(m) coeftest(m, vcov = NeweyWest(m, lag = 4, prewhite = FALSE))

# ---- 1. Séries mensais -------------------------------------------------
selic <- list.files(pattern = "^bcdata\\.sgs\\.432_.*\\.csv$") |>
  lapply(le_sgs, nome = "selic") |> bind_rows() |> distinct(data, .keep_all = TRUE)
ipca12 <- le_sgs("bcdata.sgs.13522.csv", "pi12")
cambio <- le_sgs("bcdata.sgs.3698.csv", "cambio")
dlsp   <- le_sgs("bcdata.sgs.4513.csv", "dlsp")
risco  <- read_csv2("risco_pais_emendado.csv", show_col_types = FALSE) |>
  transmute(data = as.Date(mes), risco)

# Focus: IPCA esperado 12 meses (mesma conta de ancora_fiscal.R)
le_focus_ipca <- function(a) {
  cab <- read_excel(a, skip = 1, n_max = 0, .name_repair = "minimal") |> names()
  x <- read_excel(a, skip = 2, col_names = FALSE, guess_max = 100000)
  names(x) <- c("data", as.character(as.Date(as.numeric(cab[-1]), origin = "1899-12-30")))
  x |> mutate(data = as.Date(data)) |>
    pivot_longer(-data, names_to = "mes_ref", values_to = "v", values_transform = as.numeric) |>
    filter(!is.na(v), !is.na(data)) |> mutate(mes_ref = as.Date(mes_ref))
}
pi_e12 <- list.files(pattern = "^focus_ipca_.*\\.xls$") |> lapply(le_focus_ipca) |>
  bind_rows() |> distinct(data, mes_ref, .keep_all = TRUE) |>
  mutate(m0 = as.Date(format(data, "%Y-%m-01")),
         k = 12 * (as.integer(format(mes_ref, "%Y")) - as.integer(format(m0, "%Y"))) +
             as.integer(format(mes_ref, "%m")) - as.integer(format(m0, "%m"))) |>
  filter(k >= 1, k <= 12) |> group_by(data) |> filter(n() == 12) |>
  summarise(pi_e12 = 100 * (prod(1 + v / 100) - 1), .groups = "drop")

# Focus: Selic média esperada nas próximas 8 reuniões (cerca de 1 ano)
focus_selic <- list.files(pattern = "^focus_selic_.*\\.xls$") |>
  lapply(\(a) read_excel(a, skip = 1, guess_max = 100000) |> rename(data = 1) |>
           mutate(data = as.Date(data)) |>
           pivot_longer(-data, names_to = "reuniao", values_to = "v",
                        values_transform = as.numeric) |> filter(!is.na(v))) |>
  bind_rows() |> distinct(data, reuniao, .keep_all = TRUE) |>
  mutate(ordem = as.integer(sub(".*/", "", reuniao)) * 10 +
                 as.integer(sub("R/.*", "", reuniao)))
selic_esp <- focus_selic |>
  group_by(data) |> arrange(ordem, .by_group = TRUE) |>
  slice_head(n = 8) |> filter(n() == 8) |>
  summarise(selic_esp12 = mean(v), .groups = "drop")

# Curva prefixada (Tesouro Direto): vértice mais próximo de 5 anos
td <- read_delim("precotaxatesourodireto.csv", delim = ";",
                 locale = locale(decimal_mark = ",", grouping_mark = "."),
                 show_col_types = FALSE) |>
  transmute(tipo = `Tipo Titulo`, venc = as.Date(`Data Vencimento`, "%d/%m/%Y"),
            data = as.Date(`Data Base`, "%d/%m/%Y"), taxa = `Taxa Compra Manha`) |>
  filter(tipo %in% c("Tesouro Prefixado", "Tesouro Prefixado com Juros Semestrais"),
         !is.na(taxa), taxa > 0) |>
  mutate(dist = abs(as.numeric(venc - data) / 365.25 - 5)) |>
  group_by(data) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
  select(data, pre5 = taxa)

# ---- 2. Fiscal e hiato (já trimestrais) --------------------------------
spe <- read_excel("resultado-fiscal-estrutural.xlsx", sheet = "Dados trimestrais", skip = 3) |>
  rename(tri = 1, estrutural = 5) |> mutate(tri = as.Date(tri), estrutural = as.numeric(estrutural)) |>
  filter(!is.na(tri)) |> select(tri, estrutural)
hiato <- read_excel("estimativas-do-hiato-do-produto-ifi.xlsx", sheet = "Hiato do Produto", skip = 1) |>
  rename(tri = 1, hiato = 3) |> mutate(tri = as.Date(tri), hiato = 100 * as.numeric(hiato)) |>
  filter(!is.na(tri)) |> select(tri, hiato)

# ---- 3. Base trimestral --------------------------------------------------
media_tri <- function(df, v) df |> mutate(tri = trimestre(data)) |>
  group_by(tri) |> summarise("{v}" := mean(.data[[v]], na.rm = TRUE), .groups = "drop")
fim_tri <- function(df, v) df |> mutate(tri = trimestre(data)) |> arrange(data) |>
  group_by(tri) |> summarise("{v}" := last(.data[[v]]), .groups = "drop")

meta_ano <- c(setNames(rep(4.5, 14), 2005:2018), "2019" = 4.25, "2020" = 4.0,
              "2021" = 3.75, "2022" = 3.5, "2023" = 3.25, setNames(rep(3.0, 5), 2024:2028))

q <- media_tri(selic, "selic") |>
  inner_join(fim_tri(ipca12, "pi12"), by = "tri") |>
  inner_join(media_tri(pi_e12, "pi_e12"), by = "tri") |>
  inner_join(media_tri(cambio, "cambio"), by = "tri") |>
  inner_join(media_tri(risco, "risco"), by = "tri") |>
  inner_join(fim_tri(dlsp, "dlsp"), by = "tri") |>
  inner_join(media_tri(selic_esp, "selic_esp12"), by = "tri") |>
  inner_join(media_tri(td, "pre5"), by = "tri") |>
  inner_join(spe, by = "tri") |>
  inner_join(hiato, by = "tri") |>
  filter(tri >= as.Date("2010-03-01")) |>
  arrange(tri) |>
  mutate(
    a = as.integer(format(tri, "%Y")), m = as.integer(format(tri, "%m")),
    meta = ((12 - m) * meta_ano[as.character(a)] + m * meta_ano[as.character(a + 1)]) / 12,
    desanc   = pi_e12 - meta,                                # desancoragem (p.p.)
    juro_real = 100 * ((1 + selic / 100) / (1 + pi_e12 / 100) - 1),
    gap_r    = juro_real - 5.0,                              # vs. neutro da IFI
    dcambio4 = 100 * (log(cambio) - log(lag(cambio, 4))),    # variação em 12 meses (%)
    risco_pp = risco / 100,                                  # p.b. -> p.p.
    tp       = pre5 - selic_esp12,                           # prêmio de prazo (p.p.)
    pos      = as.numeric(tri > as.Date("2014-06-01"))) |>
  select(-a, -m)
cat("Base trimestral:", format(range(q$tri)), "-", nrow(q), "trimestres\n")
View(q)

# ---- 4. Equações ---------------------------------------------------------
L <- function(x, k = 1) dplyr::lag(x, k)
est <- list()

# (a) Curva de Phillips (12 meses), com expectativas do Focus:
#     pi12_t - pi_e12_{t-4} = b_y * hiato_{t-1} + b_q * dcambio4_t + e
#     (pi_e12 de 4 trimestres antes = o que se esperava para estes 12 meses)
q <- q |> mutate(surpresa_pi = pi12 - L(pi_e12, 4))
m_pc <- lm(surpresa_pi ~ L(hiato) + dcambio4, data = q)
est$phillips <- nw(m_pc)

# (b) Curva IS:  hiato_t = c + rho_y*hiato_{t-1} - a_r*gap_r_{t-1} + e
m_is <- lm(hiato ~ L(hiato) + L(gap_r), data = q)
est$is <- nw(m_is)

# (c) Regra de Taylor: selic_t = c + rho_i*selic_{t-1} + b_pi*pi_e12_t + b_y*hiato_t
#     phi_pi = b_pi / (1 - rho_i), por regime
m_tr <- lm(selic ~ L(selic) + pi_e12 + hiato + pos + pi_e12:pos, data = q)
est$taylor <- nw(m_tr)

# (d) Prêmio de risco (p.p.): risco_t = c + rho_rp*risco_{t-1} + chi_b*dlsp_{t-1}
#                                      + chi_pi*desanc_t, com mudança pós-2014
m_rp <- lm(risco_pp ~ L(risco_pp) + L(dlsp) + desanc + pos + L(dlsp):pos + desanc:pos, data = q)
est$risco <- nw(m_rp)

# (e) Prêmio de prazo (p.p.): tp_t = c + rho_tp*tp_{t-1} + k_rp*risco_t + k_pi*desanc_t
m_tp <- lm(tp ~ L(tp) + risco_pp + desanc + pos + risco_pp:pos + desanc:pos, data = q)
est$prazo <- nw(m_tp)

# (f) Regra fiscal (Bohn com o estrutural):
#     estrutural_t = c + rho_s*estrutural_{t-1} + psi*dlsp_{t-1}, mudando pós-2014
m_fs <- lm(estrutural ~ L(estrutural) + L(dlsp) + pos + L(dlsp):pos, data = q)
est$fiscal <- nw(m_fs)

for (n in names(est)) { cat("\n====", n, "====\n"); print(est[[n]]) }

# Reação fiscal pós-2014 (soma dos coeficientes) e seu erro-padrão
V <- NeweyWest(m_fs, lag = 4, prewhite = FALSE)
g <- c("L(dlsp)", "L(dlsp):pos")
psi_pos <- sum(coef(m_fs)[g]); ep_pos <- sqrt(sum(V[g, g]))
cat(sprintf("\nReação do estrutural à dívida pós-2014: %.4f (EP %.4f, p = %.2f)\n",
            psi_pos, ep_pos, 2 * pnorm(-abs(psi_pos / ep_pos))))

# ---- 5. Do coeficiente ao parâmetro do modelo ----------------------------
cf <- function(m, k) unname(coef(m)[k])
sdres <- function(m) sd(residuals(m))
rho_i <- cf(m_tr, "L(selic)")
par <- tibble(
  parametro = c("b_y", "b_q", "a_r", "rho_i",
                "phi_pi", "rho_rp", "chi_b", "chi_pi",
                "rho_tp", "k_rp", "k_pi", "rho_s", "psi",
                "sd_e_y", "sd_e_pi", "sd_e_m", "sd_e_rp", "sd_e_tp", "sd_e_s"),
  pre2014 = c(cf(m_pc, "L(hiato)"), cf(m_pc, "dcambio4") * 4, cf(m_is, "L(gap_r)") * -1, rho_i,
              cf(m_tr, "pi_e12") / (1 - rho_i), cf(m_rp, "L(risco_pp)"), cf(m_rp, "L(dlsp)"), cf(m_rp, "desanc"),
              cf(m_tp, "L(tp)"), cf(m_tp, "risco_pp"), cf(m_tp, "desanc"),
              cf(m_fs, "L(estrutural)"), cf(m_fs, "L(dlsp)") / 4,
              sdres(m_is), sdres(m_pc), sdres(m_tr), sdres(m_rp), sdres(m_tp), sdres(m_fs) / 4),
  pos2014 = pre2014 + c(0, 0, 0, 0,
              cf(m_tr, "pi_e12:pos") / (1 - rho_i), 0, cf(m_rp, "L(dlsp):pos"), cf(m_rp, "desanc:pos"),
              0, cf(m_tp, "risco_pp:pos"), cf(m_tp, "desanc:pos"),
              0, cf(m_fs, "L(dlsp):pos") / 4,
              0, 0, 0, 0, 0, 0))
# Observações sobre o mapeamento:
#  b_q: a regressão usa variação de 12 meses do câmbio contra inflação de 12 meses;
#       no modelo o repasse entra na inflação anualizada do trimestre (x4).
#  psi: o estrutural é anual (% do PIB em 12 meses); no modelo o primário é um fluxo
#       trimestral, por isso dividido por 4. O mesmo vale para o desvio-padrão do choque.
par <- par |> mutate(across(c(pre2014, pos2014), \(x) round(x, 4)))
print(par, n = Inf)
write_csv2(par, "parametros_estimados.csv")
write_csv2(q, "base_modelo_trimestral.csv")
