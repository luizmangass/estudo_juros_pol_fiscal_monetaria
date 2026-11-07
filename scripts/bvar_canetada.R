# =====================================================================
# Contraprova: a canetada num VAR bayesiano (forma reduzida)
#
# O modelo do Dynare impõe estrutura (Phillips, UIP, regra de Taylor...)
# e calibra o que os dados não identificam. Aqui a pergunta é: o que os
# dados dizem SEM essa estrutura?
#
# VAR trimestral, 2 defasagens, 2010-2026, sete variáveis em nível:
#   primário estrutural, hiato, IPCA 12m, Selic, câmbio (100 x log),
#   risco-país (pb), pré 5 anos
# Prior de Minnesota (passeio aleatório, aperto 0,2) por observações
# artificiais, como em Bańbura, Giannone e Reichlin (2010). Posterior
# normal-Wishart, 1.000 sorteios (descarta os explosivos).
#
# Identificação recursiva (Cholesky) nessa ordem: fiscal, atividade e
# inflação não reagem à Selic no mesmo trimestre; câmbio, risco e juro
# longo reagem. A canetada é a sequência de choques monetários que deixa
# a Selic 1 p.p. abaixo da trajetória base por 4 trimestres (previsão
# condicional), igual ao experimento do Dynare.
#
# Limite: o VAR também é linear e, por construção, não separa uma
# canetada da reação normal do BC (crítica de Lucas). Serve de
# contraprova, não de substituto.
#
# Entradas (dados/): base_modelo_trimestral.csv (estimar_modelo.R) e
#   scripts/modelo/simulacoes_modelo_estimado.csv
# Saídas: bvar_canetada.csv, bvar_vs_dynare.csv, 30_bvar_vs_dynare.png
# =====================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2)

set.seed(2026)
vs <- c("estrutural", "hiato", "pi12", "selic", "lcambio", "risco", "pre5")
d <- read_csv2("base_modelo_trimestral.csv", show_col_types = FALSE) |>
  mutate(tri = as.Date(tri), lcambio = 100 * log(cambio)) |>
  select(tri, all_of(vs)) |> filter(if_all(all_of(vs), \(x) !is.na(x))) |> arrange(tri)
cat("\nAmostra do VAR:", format(range(d$tri)), "-", nrow(d), "trimestres\n")

p <- 2; n <- length(vs); lambda <- 0.2; H <- 24; Hc <- 4; corte <- 1; N <- 1000
Y <- as.matrix(d[, vs])
X <- cbind(do.call(cbind, lapply(1:p, function(j) Y[(p + 1 - j):(nrow(Y) - j), ])), 1)
Yt <- Y[(p + 1):nrow(Y), ]

# ---- 1. Prior de Minnesota por observações artificiais --------------------
sig <- sapply(1:n, function(i) { y <- Y[, i]; sd(resid(lm(y[-1] ~ y[-length(y)]))) })
Yd <- rbind(diag(sig) / lambda, matrix(0, n * (p - 1), n), diag(sig), matrix(0, 1, n))
Xd <- rbind(cbind(kronecker(diag(1:p), diag(sig)) / lambda, 0),
            matrix(0, n, n * p + 1),
            c(rep(0, n * p), 1e-4))
Ys <- rbind(Yt, Yd); Xs <- rbind(X, Xd)
XtXi <- solve(crossprod(Xs))
B_hat <- XtXi %*% crossprod(Xs, Ys)
S <- crossprod(Ys - Xs %*% B_hat)
gl <- nrow(Ys) - ncol(Xs)

companion_root <- function(B) {
  A <- matrix(0, n * p, n * p); A[1:n, ] <- t(B[1:(n * p), ])
  if (p > 1) A[(n + 1):(n * p), 1:(n * (p - 1))] <- diag(n * (p - 1))
  max(Mod(eigen(A, only.values = TRUE)$values))
}

# ---- 2. Canetada: previsão condicional em desvios da base -----------------
canetada <- function(B, Sigma) {
  A0 <- t(chol(Sigma)); im <- which(vs == "selic")
  Phi <- array(0, c(n, n, H)); Phi[, , 1] <- diag(n)            # MA: Phi_h
  Al <- lapply(1:p, function(j) t(B[((j - 1) * n + 1):(j * n), ]))
  for (h in 2:H) for (j in 1:min(p, h - 1)) Phi[, , h] <- Phi[, , h] + Al[[j]] %*% Phi[, , h - j]
  resp <- sapply(1:H, function(h) Phi[, , h] %*% A0[, im])      # n x H, por choque unitário
  e <- numeric(Hc)
  for (t in 1:Hc) {
    feito <- if (t > 1) sum(sapply(1:(t - 1), function(s) e[s] * resp[im, t - s + 1])) else 0
    e[t] <- (-corte - feito) / resp[im, 1]
  }
  sapply(1:H, function(t) {
    s <- 1:min(t, Hc); rowSums(sapply(s, function(k) e[k] * resp[, t - k + 1]))
  })
}

# ---- 3. Sorteios da posterior ---------------------------------------------
draws <- list(); tent <- 0
while (length(draws) < N && tent < 5 * N) {
  tent <- tent + 1
  Sigma <- solve(rWishart(1, gl, solve(S))[, , 1])
  B <- B_hat + t(chol(XtXi)) %*% matrix(rnorm(length(B_hat)), nrow(B_hat)) %*% chol(Sigma)
  if (companion_root(B) >= 1.0) next
  draws[[length(draws) + 1]] <- canetada(B, Sigma)
}
cat(sprintf("Sorteios estáveis: %d de %d\n", length(draws), tent))
central <- canetada(B_hat, S / gl)

arr <- simplify2array(draws)                                     # n x H x N
bv <- lapply(1:n, function(i) tibble(
  variavel = vs[i], trimestre = 1:H, central = central[i, ],
  p10 = apply(arr[i, , ], 1, quantile, 0.10), p50 = apply(arr[i, , ], 1, median),
  p90 = apply(arr[i, , ], 1, quantile, 0.90))) |> bind_rows()
write_csv2(bv, "bvar_canetada.csv")

# ---- 4. Comparação com o Dynare (pós-2014, canetada de 1 p.p.) ------------
arq_sim <- c("simulacoes_modelo_estimado.csv", "../scripts/modelo/simulacoes_modelo_estimado.csv")
sim <- read_csv(arq_sim[file.exists(arq_sim)][1], show_col_types = FALSE) |>
  filter(regime == 2, corte == 1) |>
  group_by(sim) |> arrange(trimestre, .by_group = TRUE) |>
  mutate(pi12 = (pi + lag(pi, 1, default = 0) + lag(pi, 2, default = 0) + lag(pi, 3, default = 0)) / 4,
         selic = i, lcambio = cambio_nominal, risco = 100 * rp, pre5 = il, estrutural = 4 * s) |>
  ungroup()
comp_vars <- c("selic", "pi12", "lcambio", "risco", "pre5", "estrutural")
dyn <- sim |> pivot_longer(all_of(comp_vars), names_to = "variavel") |>
  group_by(variavel, trimestre) |>
  summarise(central = value[sim == 0], p10 = quantile(value[sim > 0], 0.10),
            p90 = quantile(value[sim > 0], 0.90), .groups = "drop") |>
  mutate(fonte = "Modelo estrutural (Dynare, pós-2014)")
comp <- bind_rows(dyn, bv |> filter(variavel %in% comp_vars) |>
                    select(variavel, trimestre, central, p10, p90) |>
                    mutate(fonte = "VAR bayesiano (dados, 2010-2026)"))
write_csv2(comp, "bvar_vs_dynare.csv")

cat("\nCanetada de 1 p.p.: desvio médio em 1-4 e 5-8 trimestres (central)\n")
print(comp |> mutate(janela = cut(trimestre, c(0, 4, 8, 24), c("1-4", "5-8", "9-24"))) |>
        group_by(fonte, variavel, janela) |> summarise(media = round(mean(central), 2), .groups = "drop") |>
        pivot_wider(names_from = janela, values_from = media) |> arrange(variavel, fonte), n = Inf)

rot <- c(selic = "Selic (p.p.)", pi12 = "IPCA 12 meses (p.p.)", lcambio = "Câmbio (%, + = depreciação)",
         risco = "Risco-país (pb)", pre5 = "Juro longo prefixado (p.p.)",
         estrutural = "Primário estrutural (% PIB)")
g <- comp |> mutate(variavel = factor(rot[variavel], rot)) |>
  ggplot(aes(trimestre, group = fonte)) +
  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = -Inf, ymax = Inf, fill = "grey50", alpha = 0.12) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = fonte), alpha = 0.15) +
  geom_line(aes(y = central, colour = fonte), linewidth = 1) +
  facet_wrap(~variavel, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c("#C0392B", "#7D3C98"), name = NULL) +
  scale_fill_manual(values = c("#C0392B", "#7D3C98"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(title = "Canetada de 1 p.p.: modelo estrutural contra VAR bayesiano",
       subtitle = "Desvio em relação à trajetória sem canetada. Faixas de 80%.",
       x = "Trimestres após o início do corte", y = NULL,
       caption = paste("Faixa do Dynare: incerteza macro (choques). Faixa do VAR: incerteza dos parâmetros.",
                       "VAR(2) em nível, prior de Minnesota, Cholesky com a Selic depois de fiscal, hiato e inflação.",
                       sep = "\n")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g
ggsave("30_bvar_vs_dynare.png", g, width = 10, height = 8.5, dpi = 150)

# ---- 5. Composição da dívida com a macro do VAR ---------------------------
# Mesmo ARDL de composicao_ardl.R, agora alimentado pelas trajetórias do
# VAR (central e cada sorteio). Sem expectativas no VAR: a inflação
# corrente faz o papel da esperada (juro real = Selic - IPCA 12m).
if (file.exists("ardl_coeficientes.csv")) {
  xs <- c("risco", "juro_real", "desanc", "estrutural", "inclinacao", "dep")
  coefs <- read_csv2("ardl_coeficientes.csv", show_col_types = FALSE)
  p0 <- read_csv2("ardl_ponto_partida.csv", show_col_types = FALSE)
  pa <- max(c(0, as.integer(sub("dy[12]_", "", grep("^dy[12]_", coefs$termo, value = TRUE)))))
  cf <- lapply(c("dy1", "dy2"), function(e) { x <- coefs |> filter(eq == e); setNames(x$coef, x$termo) })
  y0 <- c(p0$y1, p0$y2); total3 <- p0$LFT + p0$PRE + p0$IPCA
  para_x <- function(m) {                     # m: n x H (desvios do VAR)
    g <- function(v) m[which(vs == v), ]
    lc <- g("lcambio")
    cbind(risco = g("risco"), juro_real = g("selic") - g("pi12"), desanc = g("pi12"),
          estrutural = g("estrutural"), inclinacao = g("pre5") - g("selic"),
          dep = lc - c(rep(0, 4), lc)[seq_along(lc)])
  }
  ardl <- function(xdev) {
    Tn <- nrow(xdev); y <- matrix(0, Tn + 1, 2); dy <- matrix(0, Tn + pa + 1, 2)
    for (t in 1:Tn) {
      xl <- if (t > 1) xdev[t - 1, ] else rep(0, ncol(xdev))
      for (k in 1:2) {
        c_ <- cf[[k]]
        ar <- if (pa == 0) 0 else sum(c_[paste0("dy1_", 1:pa)] * dy[t + pa - (1:pa), 1]) +
          sum(c_[paste0("dy2_", 1:pa)] * dy[t + pa - (1:pa), 2])
        dy[t + pa, k] <- c_["l_y1"] * y[t, 1] + c_["l_y2"] * y[t, 2] + ar + sum(c_[paste0("l_", xs)] * xl)
      }
      y[t + 1, ] <- y[t, ] + dy[t + pa, ]
    }
    y1 <- y0[1] + y[-1, 1]; y2 <- y0[2] + y[-1, 2]
    den <- 1 + exp(y1) + exp(y2); den0 <- 1 + exp(y0[1]) + exp(y0[2])
    cbind(LFT = total3 * (exp(y1) / den - exp(y0[1]) / den0), PRE = total3 * (1 / den - 1 / den0),
          IPCA = total3 * (exp(y2) / den - exp(y0[2]) / den0))
  }
  cen <- ardl(para_x(central))
  todos <- simplify2array(lapply(draws, \(m) ardl(para_x(m))))   # H x 3 x N
  comp_var <- lapply(c("LFT", "PRE", "IPCA"), function(k) tibble(
    titulo = k, trimestre = 1:H, central = cen[, k],
    p10 = apply(todos[, k, ], 1, quantile, 0.10), p90 = apply(todos[, k, ], 1, quantile, 0.90))) |>
    bind_rows()
  write_csv2(comp_var, "bvar_composicao.csv")
  cat("\nComposição com a macro do VAR (p.p.; central e faixa de 80%):\n")
  print(comp_var |> filter(trimestre %in% c(8, 24)) |> mutate(across(c(central, p10, p90), \(x) round(x, 2))))
}
