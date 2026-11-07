# =====================================================================
# Reação estocástica do mercado e da dívida a uma canetada
#
# Liga as 1.000 simulações do modelo estimado (rodar_modelo_estimado.m)
# a três coisas que o modelo macro não tem:
#
#   A. Composição: o ARDL trimestral de composicao_ardl.R, em razões
#      logarítmicas (LFT/pré e IPCA/pré), com dinâmica própria e com
#      risco, juro real, desancoragem, primário estrutural, inclinação e
#      câmbio como exógenas. Em cada simulação: (i) os coeficientes são
#      sorteados da distribuição estimada (Newey-West), descartando os
#      sorteios instáveis; (ii) soma-se um resíduo sorteado dos resíduos
#      históricos, aos pares (as duas equações têm resíduos correlacionados).
#      A simulação 0 é a central: coeficientes estimados, sem resíduo.
#   B. Marcação a mercado: o perfil título a título de duration_divida.R
#      (duration modificada e convexidade, a preço de mercado) aplicado
#      ao choque simulado nos juros longos prefixado e real.
#   C. Custo extra para o Tesouro: a LFT reprecifica na hora (estoque x
#      Selic); prefixados e IPCA só quando vencem e são rolados à taxa
#      nova (perfil de vencimentos). Resultado em R$ bilhões acumulados.
#
# Correspondência entre o modelo (desvios do estado estacionário) e as
# variáveis do ARDL:
#   risco      = rp x 100 (p.p. -> pontos-base)
#   juro_real  = i - média de pi nos 4 trimestres seguintes (ex-ante)
#   desanc     = média de pi nos 4 trimestres seguintes (expectativa 12m)
#   estrutural = 4 x s (primário trimestral -> anual, % do PIB)
#   inclinacao = il - i (pré 5 anos - Selic)
#   dep        = câmbio nominal hoje - câmbio nominal 4 trimestres antes (%)
#   juro longo real = il - média de pi nos 20 trimestres seguintes
#
# Entradas (dados/): ardl_coeficientes.csv, ardl_vcov.csv, ardl_residuos.csv,
#   ardl_ponto_partida.csv (composicao_ardl.R), perfil_titulos_ultimo_mes.csv
#   (duration_divida.R) e scripts/modelo/simulacoes_modelo_estimado.csv
# Saídas: reacao_mercado_resumo.csv, reacao_mercado_trajetorias.csv,
#   26_composicao_estocastica.png, 27_custo_tesouro_estocastico.png,
#   28_marcacao_estocastica.png
# =====================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2); library(scales)

arq_sim <- c("simulacoes_modelo_estimado.csv",
             "../scripts/modelo/simulacoes_modelo_estimado.csv")
arq_sim <- arq_sim[file.exists(arq_sim)][1]
if (is.na(arq_sim)) stop("Rode antes scripts/modelo/rodar_modelo_estimado.m (Octave/Dynare).")

sim <- read_csv(arq_sim, show_col_types = FALSE)
if (!"cambio_nominal" %in% names(sim))
  stop("simulacoes_modelo_estimado.csv sem câmbio: rode de novo rodar_modelo_estimado.m.")
regimes <- c("Pré-2014 (estimado)", "Pós-2014 (estimado)")
cores   <- c("#2E86C1", "#C0392B")
set.seed(2026)

# ---- 1. Trajetórias das variáveis de mercado ------------------------------
media_frente <- function(x, k) sapply(seq_along(x), function(t) {
  j <- (t + 1):min(t + k, length(x)); if (t == length(x)) x[t] else mean(x[j]) })

traj <- sim |>
  group_by(regime, corte, sim) |> arrange(trimestre, .by_group = TRUE) |>
  mutate(pi_e4  = media_frente(pi, 4),
         pi_e20 = media_frente(pi, 20),
         risco = 100 * rp, juro_real = i - pi_e4, desanc = pi_e4,
         inclinacao = il - i, estrutural = 4 * s,
         dep = cambio_nominal - lag(cambio_nominal, 4, default = 0),
         real_longo = il - pi_e20) |>
  ungroup()

# ---- A. Composição: ARDL com sorteio de parâmetros e de resíduos ----------
xs <- c("risco", "juro_real", "desanc", "estrutural", "inclinacao", "dep")
coefs <- read_csv2("ardl_coeficientes.csv", show_col_types = FALSE)
vc    <- read_csv2("ardl_vcov.csv", show_col_types = FALSE)
res   <- as.matrix(read_csv2("ardl_residuos.csv", show_col_types = FALSE)[, c("e1", "e2")])
p0    <- read_csv2("ardl_ponto_partida.csv", show_col_types = FALSE)
y0 <- c(p0$y1, p0$y2); total3 <- p0$LFT + p0$PRE + p0$IPCA
p  <- max(c(0, as.integer(sub("dy[12]_", "", grep("^dy[12]_", coefs$termo, value = TRUE)))))
cat(sprintf("\nARDL(%d); ponto de partida %s: LFT %.1f%%, pré %.1f%%, IPCA %.1f%%\n",
            p, format(p0$tri), p0$LFT, p0$PRE, p0$IPCA))

cf_hat <- lapply(c("dy1", "dy2"), function(e) { x <- coefs |> filter(eq == e); setNames(x$coef, x$termo) })
V <- lapply(c("dy1", "dy2"), function(e) {
  x <- vc |> filter(eq == e) |> select(-eq) |> pivot_wider(names_from = termo2, values_from = v)
  m <- as.matrix(x[, -1]); rownames(m) <- x$termo1; m[names(cf_hat[[1]]), names(cf_hat[[1]])] })

raiz_max <- function(cf1, cf2, p) {
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
  max(Mod(eigen(A)$values))
}
simula_ardl <- function(xdev, cf1, cf2, p, eps = NULL) {
  Tn <- nrow(xdev); y <- matrix(0, Tn + 1, 2); dy <- matrix(0, Tn + p + 1, 2)
  for (t in 1:Tn) {
    xl <- if (t > 1) xdev[t - 1, ] else rep(0, ncol(xdev))   # x entra defasado
    for (k in 1:2) {
      cf <- if (k == 1) cf1 else cf2
      ar <- if (p == 0) 0 else
        sum(cf[paste0("dy1_", 1:p)] * dy[t + p - (1:p), 1]) +
        sum(cf[paste0("dy2_", 1:p)] * dy[t + p - (1:p), 2])
      dy[t + p, k] <- cf["l_y1"] * y[t, 1] + cf["l_y2"] * y[t, 2] + ar +
        sum(cf[paste0("l_", xs)] * xl) + (if (is.null(eps)) 0 else eps[t, k])
    }
    y[t + 1, ] <- y[t, ] + dy[t + p, ]
  }
  y[-1, , drop = FALSE]
}
para_fatias <- function(ydev, y0, total) {
  y1 <- y0[1] + ydev[, 1]; y2 <- y0[2] + ydev[, 2]
  den <- 1 + exp(y1) + exp(y2); den0 <- 1 + exp(y0[1]) + exp(y0[2])
  cbind(LFT = total * (exp(y1) / den - exp(y0[1]) / den0),
        PRE = total * (1 / den - 1 / den0),
        IPCA = total * (exp(y2) / den - exp(y0[2]) / den0))
}
sorteia <- function() repeat {
  c1 <- MASS::mvrnorm(1, cf_hat[[1]], V[[1]]); c2 <- MASS::mvrnorm(1, cf_hat[[2]], V[[2]])
  if (raiz_max(c1, c2, p) < 1) return(list(c1, c2))
}

grupos <- traj |> group_by(regime, corte, sim) |> arrange(trimestre, .by_group = TRUE) |>
  group_split()
zs <- lapply(grupos, function(g) {
  xdev <- as.matrix(g[, xs])
  if (g$sim[1] == 0) { cf <- cf_hat; eps <- NULL } else {
    cf <- sorteia(); eps <- res[sample(nrow(res), nrow(g), replace = TRUE), , drop = FALSE] }
  f <- para_fatias(simula_ardl(xdev, cf[[1]], cf[[2]], p, eps), y0, total3)
  g |> mutate(part_d_LFT = f[, "LFT"], part_d_PRE = f[, "PRE"], part_d_IPCA = f[, "IPCA"])
}) |> bind_rows()

# ---- B e C. Perfil título a título (a preço de mercado) -------------------
perfil <- read_csv2("perfil_titulos_ultimo_mes.csv", show_col_types = FALSE) |>
  filter(indexador %in% c("Prefixado", "Índice de preços", "Selic (LFT)")) |>
  mutate(indexador = recode(indexador, "Índice de preços" = "IPCA"))
estoque_lft <- sum(perfil$valor_mercado[perfil$tipo == "LFT"], na.rm = TRUE)
fixos <- perfil |> filter(indexador %in% c("Prefixado", "IPCA"), !is.na(mod))
cat(sprintf("\nEstoque a mercado: LFT R$ %.0f bi | prefixado R$ %.0f bi | IPCA R$ %.0f bi\n",
            estoque_lft, sum(fixos$valor_mercado[fixos$indexador == "Prefixado"]),
            sum(fixos$valor_mercado[fixos$indexador == "IPCA"])))

# Agregados por indexador: soma(V x mod) e soma(V x conv) para a marcação
agg <- fixos |> group_by(indexador) |>
  summarise(Vmod = sum(valor_mercado * mod), Vconv = sum(valor_mercado * conv), .groups = "drop")
# Quanto de cada indexador vence em cada trimestre (é rolado à taxa nova)
venc_q <- fixos |> mutate(q = pmax(1, ceiling(anos * 4))) |>
  group_by(indexador, q) |> summarise(V = sum(valor_mercado), .groups = "drop")
Tq <- max(sim$trimestre)
vence <- function(ind) { v <- numeric(Tq); x <- venc_q |> filter(indexador == ind, q <= Tq)
  v[x$q] <- x$V; v }
v_pre <- vence("Prefixado"); v_ipca <- vence("IPCA")

custo <- traj |>
  group_by(regime, corte, sim) |> arrange(trimestre, .by_group = TRUE) |>
  mutate(
    # B. marcação imediata se o choque do trimestre batesse no estoque de hoje
    mtm_pre  = -(agg$Vmod[agg$indexador == "Prefixado"] * il / 100 -
                   0.5 * agg$Vconv[agg$indexador == "Prefixado"] * (il / 100)^2),
    mtm_ipca = -(agg$Vmod[agg$indexador == "IPCA"] * real_longo / 100 -
                   0.5 * agg$Vconv[agg$indexador == "IPCA"] * (real_longo / 100)^2),
    # C. custo extra de juros por trimestre (R$ bi)
    c_lft  = estoque_lft * i / 100 / 4,
    c_pre  = cumsum(v_pre[trimestre] * il) / 100 / 4,
    c_ipca = cumsum(v_ipca[trimestre] * real_longo) / 100 / 4,
    custo_acum = cumsum(c_lft + c_pre + c_ipca),
    lft_acum = cumsum(c_lft)) |>
  ungroup()

# ---- 3. Resumos com bandas ------------------------------------------------
faixa <- function(df, vars) df |> filter(sim > 0) |>
  pivot_longer(all_of(vars), names_to = "variavel") |>
  group_by(regime, corte, trimestre, variavel) |>
  summarise(p05 = quantile(value, .05), p25 = quantile(value, .25),
            p50 = median(value), p75 = quantile(value, .75),
            p95 = quantile(value, .95), .groups = "drop") |>
  left_join(df |> filter(sim == 0) |>
              pivot_longer(all_of(vars), names_to = "variavel", values_to = "central") |>
              select(regime, corte, trimestre, variavel, central),
            by = c("regime", "corte", "trimestre", "variavel"))

traj_res <- bind_rows(
  faixa(zs, c("part_d_LFT", "part_d_PRE", "part_d_IPCA")),
  faixa(custo, c("custo_acum", "lft_acum", "mtm_pre", "mtm_ipca")))
write_csv2(traj_res, "reacao_mercado_trajetorias.csv")

# Tabela-resumo: 2 anos (t = 8) e 6 anos (t = 24); marcação em 1 e 2 anos.
# Lembrete: o modelo é linear, então o efeito da canetada em si é a
# trajetória central; as bandas são a incerteza macro em volta dela.
pior_mtm <- custo |> filter(sim > 0, trimestre %in% c(4, 8)) |>
  transmute(regime, corte, sim, trimestre, mtm = mtm_pre + mtm_ipca)
resumo <- bind_rows(
  zs |> filter(sim > 0, trimestre %in% c(8, 24)) |>
    pivot_longer(c(part_d_LFT, part_d_PRE, part_d_IPCA), names_to = "medida") |>
    mutate(medida = paste0(sub("part_d_", "Participação ", medida), " (p.p.), ", trimestre / 4, " anos")),
  custo |> filter(sim > 0, trimestre %in% c(8, 24)) |>
    transmute(regime, corte, sim, value = custo_acum,
              medida = paste0("Custo extra acumulado (R$ bi), ", trimestre / 4, " anos")),
  pior_mtm |> transmute(regime, corte, sim, value = mtm,
                        medida = paste0("Marcação pré + IPCA (R$ bi), ", trimestre / 4, " ano(s)"))) |>
  group_by(regime = regimes[regime], corte, medida) |>
  summarise(p05 = quantile(value, .05), p50 = median(value), p95 = quantile(value, .95),
            prob_positivo = mean(value > 0), .groups = "drop") |>
  arrange(corte, medida, regime)
cat("\nReação estocástica (percentis de 1.000 simulações):\n")
print(resumo |> mutate(across(where(is.numeric), \(x) round(x, 2))), n = Inf, width = 200)
write_csv2(resumo, "reacao_mercado_resumo.csv")

# ---- 4. Gráficos ----------------------------------------------------------
leque <- function(df, titulo, sub, ylab, arq, rotulos, alt = 6) {
  g <- df |> mutate(regime = factor(regimes[regime], regimes),
                    variavel = factor(rotulos[variavel], rotulos),
                    corte = paste0("Canetada de ", corte, " p.p.")) |>
    ggplot(aes(trimestre, group = regime)) +
    annotate("rect", xmin = 0.5, xmax = 4.5, ymin = -Inf, ymax = Inf, fill = "grey50", alpha = 0.12) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
    geom_ribbon(aes(ymin = p05, ymax = p95, fill = regime), alpha = 0.12) +
    geom_ribbon(aes(ymin = p25, ymax = p75, fill = regime), alpha = 0.22) +
    geom_line(aes(y = central, colour = regime), linewidth = 1) +
    facet_grid(variavel ~ corte, scales = "free_y") +
    scale_colour_manual(values = cores, name = NULL) +
    scale_fill_manual(values = cores, name = NULL) +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    labs(title = titulo, subtitle = sub, x = "Trimestres após o início do corte", y = ylab,
         caption = "Faixa clara: 90% das simulações; escura: 50%. Área cinza: os 4 trimestres de juro abaixo da regra.") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  print(g); ggsave(arq, g, width = 10, height = alt, dpi = 150)
}

leque(traj_res |> filter(grepl("^part_", variavel)),
      "Canetada e composição da dívida: o que a dinâmica histórica prevê",
      "Mudança da participação (p.p.), ARDL com sorteio de parâmetros e resíduos sobre 1.000 simulações do modelo",
      NULL, "26_composicao_estocastica.png",
      c(part_d_LFT = "LFT (Selic)", part_d_PRE = "Prefixados", part_d_IPCA = "IPCA"), alt = 7.5)
leque(traj_res |> filter(variavel %in% c("custo_acum", "lft_acum")),
      "Canetada: economia agora, conta depois",
      "Custo extra de juros acumulado para o Tesouro, R$ bilhões (estoque a preço de mercado)",
      NULL, "27_custo_tesouro_estocastico.png",
      c(lft_acum = "Só LFT", custo_acum = "LFT + rolagens"))

g <- pior_mtm |> mutate(regime = factor(regimes[regime], regimes),
                        corte = paste0("Canetada de ", corte, " p.p."),
                        quando = paste0(trimestre / 4, " ano(s) depois")) |>
  ggplot(aes(mtm, fill = regime)) +
  geom_vline(xintercept = 0, colour = "grey40") +
  geom_density(alpha = 0.35, colour = NA) +
  facet_grid(quando ~ corte, scales = "free") +
  scale_fill_manual(values = cores, name = NULL) +
  labs(title = "Marcação a mercado do estoque de prefixados e IPCA",
       subtitle = "Ganho ou perda de marcação do estoque atual, R$ bilhões (negativo = perda para quem carrega os títulos)",
       x = NULL, y = NULL,
       caption = "Duration modificada e convexidade título a título, a preço de mercado (duration_divida.R).") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        axis.text.y = element_blank())
g
ggsave("28_marcacao_estocastica.png", g, width = 10, height = 7, dpi = 150)
