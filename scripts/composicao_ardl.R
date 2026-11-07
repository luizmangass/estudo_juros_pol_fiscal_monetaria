# =====================================================================
# Composição da dívida como série temporal: ARDL em razões logarítmicas
#
# Substitui a regressão estática de composicao_divida.R (Parte A) no elo
# entre o modelo macro e a composição. Três mudanças:
#
# 1. Variáveis dependentes em razão logarítmica (dados composicionais):
#        y1 = log(LFT / prefixados)    y2 = log(IPCA / prefixados)
#    Assim as três fatias sempre somam o mesmo total e ficam entre 0 e 1.
# 2. Dinâmica própria: ARDL trimestral em forma de correção,
#        dy_t = c + a1*y1_{t-1} + a2*y2_{t-1} + soma_j (g1j*dy1_{t-j} + g2j*dy2_{t-j})
#               + b' x_{t-1} + e_t
#    com x = risco-país, juro real ex-ante, desancoragem, primário
#    estrutural, inclinação (pré 5a - Selic) e depreciação do câmbio em
#    4 trimestres. O número de defasagens p é o menor com resíduo branco
#    (Ljung-Box, 8 defasagens, p > 0,05) nas duas equações.
# 3. Incerteza: coeficientes e matriz de covariância Newey-West salvos
#    para sortear parâmetros, e resíduos salvos (aos pares) para bootstrap.
#
# Trimestral porque os vencimentos concentrados em certos meses (LTN em
# jan/abr/jul/out, NTN-B em mai/ago) criam serrilhado mensal: no mensal
# os resíduos ficam fortemente autocorrelacionados.
#
# Entradas (dados/): base_composicao_mensal.csv (de composicao_divida.R)
# Saídas: ardl_selecao.csv, ardl_coeficientes.csv, ardl_vcov.csv,
#   ardl_residuos.csv, ardl_ponto_partida.csv, ardl_multiplicadores.csv,
#   29_ardl_multiplicadores.png
# =====================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2)
library(sandwich); library(lmtest)

# ---- 1. Base trimestral --------------------------------------------------
tri_de <- function(d) as.Date(sprintf("%s-%02d-01", format(d, "%Y"),
                                      (as.numeric(format(d, "%m")) - 1) %/% 3 * 3 + 1))
bt <- read_csv2("base_composicao_mensal.csv", show_col_types = FALSE) |>
  mutate(mes = as.Date(mes), tri = tri_de(mes)) |>
  group_by(tri) |>
  summarise(n = n(), across(c(LFT, PRE, IPCA, risco, juro_real, desanc, estrutural,
                              inclinacao, cambio), \(x) mean(x)), .groups = "drop") |>
  filter(n == 3) |> arrange(tri) |>
  mutate(y1 = log(LFT / PRE), y2 = log(IPCA / PRE),
         dy1 = y1 - lag(y1), dy2 = y2 - lag(y2),
         dep = 100 * (log(cambio) - lag(log(cambio), 4)))
View(bt)

xs <- c("risco", "juro_real", "desanc", "estrutural", "inclinacao", "dep")
monta <- function(p, pmax = 4) {
  d <- bt
  for (j in seq_len(pmax)) {
    d[[paste0("dy1_", j)]] <- lag(d$dy1, j); d[[paste0("dy2_", j)]] <- lag(d$dy2, j)
  }
  d <- d |> mutate(across(all_of(c("y1", "y2", xs)), lag, .names = "l_{.col}"))
  d |> filter(if_all(c("dy1", "dy2", paste0("l_", c("y1", "y2", xs)),
                       paste0("dy1_", seq_len(pmax)), paste0("dy2_", seq_len(pmax))),
                     \(v) !is.na(v)))
}
formula_p <- function(dep, p) {
  rhs <- c("l_y1", "l_y2", if (p > 0) c(paste0("dy1_", 1:p), paste0("dy2_", 1:p)),
           paste0("l_", xs))
  as.formula(paste(dep, "~", paste(rhs, collapse = " + ")))
}

# ---- 2. Escolha das defasagens (mesma amostra para todos os p) -----------
d_all <- monta(4)
selecao <- expand_grid(p = 0:4, eq = c("dy1", "dy2")) |>
  rowwise() |>
  mutate(m = list(lm(formula_p(eq, p), data = d_all)),
         bic = BIC(m), r2 = summary(m)$r.squared,
         ljung_box_p = Box.test(resid(m), lag = 8, type = "Ljung-Box")$p.value) |>
  select(-m) |> ungroup()
cat("\nSeleção de defasagens (amostra comum):\n"); print(selecao, n = Inf)
write_csv2(selecao, "ardl_selecao.csv")
ok <- selecao |> group_by(p) |> summarise(branco = all(ljung_box_p > 0.05)) |> filter(branco)
p <- if (nrow(ok)) min(ok$p) else 2
cat("\nDefasagens escolhidas: p =", p, "\n")

# ---- 3. Estimação final ---------------------------------------------------
d <- monta(p, pmax = max(p, 1))
cat("Amostra:", format(range(d$tri)), "-", nrow(d), "trimestres\n")
mods <- list(dy1 = lm(formula_p("dy1", p), data = d), dy2 = lm(formula_p("dy2", p), data = d))
coefs <- lapply(names(mods), function(k) {
  m <- mods[[k]]; V <- NeweyWest(m, lag = 4, prewhite = FALSE)
  ct <- coeftest(m, vcov = V)
  tibble(eq = k, termo = rownames(ct), coef = ct[, 1], ep = ct[, 2], p = ct[, 4],
         r2 = summary(m)$r.squared,
         ljung_box_p = Box.test(resid(m), lag = 8, type = "Ljung-Box")$p.value)
}) |> bind_rows()
print(coefs |> mutate(across(where(is.numeric), \(x) signif(x, 3))), n = Inf)
write_csv2(coefs, "ardl_coeficientes.csv")

vcov_long <- lapply(names(mods), function(k) {
  V <- NeweyWest(mods[[k]], lag = 4, prewhite = FALSE)
  as_tibble(V, rownames = "termo1") |> pivot_longer(-termo1, names_to = "termo2", values_to = "v") |>
    mutate(eq = k)
}) |> bind_rows()
write_csv2(vcov_long, "ardl_vcov.csv")
write_csv2(tibble(tri = d$tri, e1 = resid(mods$dy1), e2 = resid(mods$dy2)), "ardl_residuos.csv")
cat(sprintf("Correlação entre os resíduos das duas equações: %.2f\n",
            cor(resid(mods$dy1), resid(mods$dy2))))

# Ponto de partida: participações no último trimestre completo
ult <- bt |> slice_tail(n = 1)
write_csv2(ult |> select(tri, LFT, PRE, IPCA, y1, y2), "ardl_ponto_partida.csv")

# ---- 4. Simulador do ARDL (usado também em reacao_mercado_estocastica.R) --
# Recebe desvios de x (matriz T x 6, na ordem de xs) e devolve desvios de
# y1 e y2. Sem intercepto: é a resposta ao desvio em relação à base.
simula_ardl <- function(xdev, cf1, cf2, p, eps = NULL) {
  Tn <- nrow(xdev); y <- matrix(0, Tn + 1, 2); dy <- matrix(0, Tn + p + 1, 2)
  for (t in 1:Tn) {
    xl <- if (t > 1) xdev[t - 1, ] else rep(0, length(xs))   # x entra defasado
    lagdy <- function(cf, k) if (p == 0) 0 else
      sum(cf[paste0("dy1_", 1:p)] * dy[t + p - (1:p), 1]) +
      sum(cf[paste0("dy2_", 1:p)] * dy[t + p - (1:p), 2])
    for (k in 1:2) {
      cf <- if (k == 1) cf1 else cf2
      dy[t + p, k] <- cf["l_y1"] * y[t, 1] + cf["l_y2"] * y[t, 2] + lagdy(cf, k) +
        sum(cf[paste0("l_", xs)] * xl) + (if (is.null(eps)) 0 else eps[t, k])
    }
    y[t + 1, ] <- y[t, ] + dy[t + p, ]
  }
  y[-1, , drop = FALSE]
}
# Converte desvios de y em mudança de participação (p.p. do total dos três)
para_fatias <- function(ydev, y0, total = 100) {
  y1 <- y0[1] + ydev[, 1]; y2 <- y0[2] + ydev[, 2]
  den <- 1 + exp(y1) + exp(y2)
  den0 <- 1 + exp(y0[1]) + exp(y0[2])
  cbind(LFT = total * (exp(y1) / den - exp(y0[1]) / den0),
        PRE = total * (1 / den - 1 / den0),
        IPCA = total * (exp(y2) / den - exp(y0[2]) / den0))
}

# Estabilidade: autovalores da forma companheira em (y, dy e suas defasagens)
cf1 <- coef(mods$dy1); cf2 <- coef(mods$dy2)
raizes <- function(cf1, cf2, p) {
  n <- 2 + 2 * max(p, 1); A <- matrix(0, n, n)
  for (k in 1:2) {
    cf <- if (k == 1) cf1 else cf2
    A[2 + k, 1:2] <- cf[c("l_y1", "l_y2")]
    for (j in seq_len(p)) A[2 + k, 2 + 2 * (j - 1) + 1:2] <- cf[c(paste0("dy1_", j), paste0("dy2_", j))]
  }
  A[1, ] <- A[3, ]; A[1, 1] <- A[1, 1] + 1
  A[2, ] <- A[4, ]; A[2, 2] <- A[2, 2] + 1
  if (p > 1) for (j in 2:p) { A[2 + 2 * (j - 1) + 1, 2 + 2 * (j - 2) + 1] <- 1
                              A[2 + 2 * (j - 1) + 2, 2 + 2 * (j - 2) + 2] <- 1 }
  sort(Mod(eigen(A)$values), decreasing = TRUE)
}
r <- raizes(cf1, cf2, p)
cat(sprintf("Maior raiz da parte autorregressiva: %.3f (abaixo de 1 = estável; perto de 1 = choques muito persistentes)\n", r[1]))

# ---- 5. Multiplicadores dinâmicos ----------------------------------------
# Cada x sobe 1 desvio-padrão por 4 trimestres e volta a zero.
dp_x <- sapply(xs, function(v) sd(bt[[v]], na.rm = TRUE))
total3 <- with(ult, LFT + PRE + IPCA)
mult <- lapply(seq_along(xs), function(k) {
  xdev <- matrix(0, 24, length(xs)); xdev[1:4, k] <- dp_x[k]
  f <- para_fatias(simula_ardl(xdev, cf1, cf2, p), c(ult$y1, ult$y2), total3)
  tibble(variavel = xs[k], trimestre = 1:24, LFT = f[, "LFT"], PRE = f[, "PRE"], IPCA = f[, "IPCA"])
}) |> bind_rows()
write_csv2(mult, "ardl_multiplicadores.csv")
cat("\nMudança da participação 8 trimestres depois de 1 dp por 4 trimestres (p.p.):\n")
print(mult |> filter(trimestre == 8) |> mutate(across(c(LFT, PRE, IPCA), \(x) round(x, 2))))

rot <- c(risco = "Risco-país", juro_real = "Juro real", desanc = "Desancoragem",
         estrutural = "Primário estrutural", inclinacao = "Inclinação da curva",
         dep = "Depreciação do câmbio")
g <- mult |> pivot_longer(c(LFT, PRE, IPCA), names_to = "titulo") |>
  mutate(variavel = factor(rot[variavel], rot),
         titulo = factor(titulo, c("LFT", "PRE", "IPCA"), c("LFT", "Prefixados", "IPCA"))) |>
  ggplot(aes(trimestre, value, colour = titulo)) +
  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = -Inf, ymax = Inf, fill = "grey50", alpha = 0.12) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_line(linewidth = 1) +
  facet_wrap(~variavel, ncol = 3) +
  scale_colour_manual(values = c("#C0392B", "#2E86C1", "#27AE60"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(title = "O que move a composição da dívida, com dinâmica (ARDL trimestral)",
       subtitle = "Mudança da participação (p.p.) quando cada variável sobe 1 desvio-padrão por 4 trimestres (área cinza)",
       x = "Trimestres", y = NULL,
       caption = sprintf("ARDL(%d) em log(LFT/pré) e log(IPCA/pré), %s a %s. Newey-West.",
                         p, format(min(d$tri), "%Y"), format(max(d$tri), "%Y"))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g
ggsave("29_ardl_multiplicadores.png", g, width = 10, height = 6.5, dpi = 150)
