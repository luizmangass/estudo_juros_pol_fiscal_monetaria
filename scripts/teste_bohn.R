# =====================================================================
# Teste de Bohn: o fiscal reage à dívida?
#   primário_t = a + rho * dívida_{t-1} + b * hiato_t + e_t
# rho > 0 e significativo  -> fiscal estabiliza a dívida (regime monetário)
# rho <= 0                 -> fiscal não reage (risco de dominância fiscal)
#
# Entradas (CSV baixados do SGS/BCB, na mesma pasta):
#   bcdata.sgs.5793.csv   primário consolidado, % PIB 12m (NFSP: + = déficit)
#   bcdata.sgs.4513.csv   DLSP, % PIB
#   bcdata.sgs.13762.csv  DBGG, % PIB (a partir de dez/2006)
#   bcdata.sgs.24364.csv  IBC-Br dessazonalizado
# =====================================================================
library(dplyr); library(tidyr); library(readr)
library(ggplot2); library(scales)
library(sandwich); library(lmtest); library(strucchange)

le_sgs <- function(codigo, nome) {
  read_delim(sprintf("bcdata.sgs.%d.csv", codigo), delim = ";",
             col_types = "cc", locale = locale(decimal_mark = ",")) |>
    transmute(mes = as.Date(data, "%d/%m/%Y"),
              !!nome := as.numeric(sub(",", ".", valor, fixed = TRUE)))
}

# ---- 1. Dados ---------------------------------------------------------
base_m <- le_sgs(5793, "primario_nfsp") |>
  full_join(le_sgs(4513, "dlsp"), by = "mes") |>
  full_join(le_sgs(13762, "dbgg"), by = "mes") |>
  full_join(le_sgs(24364, "ibc"), by = "mes") |>
  arrange(mes) |>
  mutate(primario = -primario_nfsp)        # + = superávit

# Hiato: IBC-Br em log menos tendência HP (lambda mensal = 129600)
hp <- function(y, lambda) {
  n <- length(y); D <- diff(diag(n), differences = 2)
  as.numeric(solve(diag(n) + lambda * crossprod(D), y))
}
ok <- !is.na(base_m$ibc)
base_m$hiato <- NA_real_
base_m$hiato[ok] <- 100 * (log(base_m$ibc[ok]) - hp(log(base_m$ibc[ok]), 129600))

# Trimestral (último mês do trimestre): o primário já é acumulado em
# 12 meses, então dados mensais teriam sobreposição demais.
base <- base_m |>
  filter(format(mes, "%m") %in% c("03", "06", "09", "12")) |>
  mutate(dlsp_1 = lag(dlsp), dbgg_1 = lag(dbgg),
         primario_1 = lag(primario)) |>
  filter(!is.na(primario), !is.na(dlsp_1), !is.na(hiato))
View(base)

nw <- function(m) coeftest(m, vcov = NeweyWest(m, lag = 4, prewhite = FALSE))

# ---- 2. Amostra inteira -----------------------------------------------
cat("\n== Bohn, DLSP, amostra inteira ==\n")
m_dlsp <- lm(primario ~ dlsp_1 + hiato, data = base); print(nw(m_dlsp))

cat("\n== Bohn, DBGG, amostra inteira ==\n")
m_dbgg <- lm(primario ~ dbgg_1 + hiato, data = filter(base, !is.na(dbgg_1)))
print(nw(m_dbgg))

# Com persistência do primário (versão mais exigente)
cat("\n== Bohn, DLSP, com primário defasado ==\n")
m_ar <- lm(primario ~ primario_1 + dlsp_1 + hiato, data = base); print(nw(m_ar))
rho_lp <- coef(m_ar)["dlsp_1"] / (1 - coef(m_ar)["primario_1"])
cat(sprintf("Resposta de longo prazo à dívida: %.3f\n", rho_lp))

# ---- 3. Quebra estrutural: quando a reação mudou? ---------------------
cat("\n== Teste de Chow sequencial (supF) para o coeficiente da dívida ==\n")
fs <- Fstats(primario ~ dlsp_1 + hiato, data = base, from = 0.15)
print(sctest(fs))
quebra <- base$mes[fs$breakpoint]
cat("Data estimada da quebra:", format(quebra, "%m/%Y"), "\n")

antes  <- lm(primario ~ dlsp_1 + hiato, data = filter(base, mes <= quebra))
depois <- lm(primario ~ dlsp_1 + hiato, data = filter(base, mes >  quebra))
cat("\nAntes da quebra:\n");  print(nw(antes))
cat("\nDepois da quebra:\n"); print(nw(depois))

# ---- 4. Janelas móveis (8 anos = 32 trimestres) -----------------------
jan <- 32
rolagem <- lapply(jan:nrow(base), function(k) {
  d <- base[(k - jan + 1):k, ]
  m <- lm(primario ~ dlsp_1 + hiato, data = d)
  ct <- nw(m)
  tibble(mes = d$mes[jan], rho = ct["dlsp_1", 1], ep = ct["dlsp_1", 2])
}) |> bind_rows()

g1 <- ggplot(rolagem, aes(mes, rho)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_ribbon(aes(ymin = rho - 1.96 * ep, ymax = rho + 1.96 * ep),
              fill = "#C0392B", alpha = 0.18) +
  geom_line(colour = "#C0392B", linewidth = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "O fiscal reage à dívida? Coeficiente de Bohn em janelas de 8 anos",
       subtitle = paste("Resposta do primário (% PIB) a +1 p.p. de dívida líquida/PIB",
                        "no trimestre anterior. Faixa: IC 95% (Newey-West).\n",
                        "Data = fim da janela."),
       x = NULL, y = NULL,
       caption = "Fonte: BCB (SGS 5793, 4513, 24364). Controle: hiato do IBC-Br (filtro HP).") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
g1
ggsave("09_bohn_janelas_moveis.png", g1, width = 10, height = 5.5, dpi = 150)

# ---- 5. Gráfico de dispersão por período ------------------------------
base <- base |>
  mutate(periodo = case_when(mes <= as.Date("2014-06-01") ~ "2003-2014",
                             mes <= as.Date("2019-12-01") ~ "2014-2019",
                             TRUE ~ "2020-2026"))
g2 <- ggplot(base, aes(dlsp_1, primario, colour = periodo)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  scale_colour_manual(values = c("#2E86C1", "#C0392B", "#7D9D3C"), name = NULL) +
  labs(title = "Primário x dívida líquida do trimestre anterior",
       x = "DLSP (% do PIB, t-1)", y = "Primário (% do PIB, 12 meses, + = superávit)",
       caption = "Fonte: BCB (SGS 5793 e 4513)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g2
ggsave("10_bohn_dispersao.png", g2, width = 9, height = 6, dpi = 150)
