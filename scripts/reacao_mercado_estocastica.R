# =====================================================================
# Reação estocástica do mercado e da dívida a uma canetada
#
# Liga as 1.000 simulações do modelo estimado (rodar_modelo_estimado.m)
# a três coisas que o modelo macro não tem:
#
#   A. Composição: a regressão de composicao_divida.R diz quanto a
#      participação de LFT, prefixados e IPCA muda em 12 meses quando
#      risco, juro real, desancoragem, inclinação e primário estrutural
#      mudam 1 desvio-padrão. Aplico esses coeficientes às trajetórias
#      simuladas, trimestre a trimestre.
#   B. Marcação a mercado: o perfil título a título de duration_divida.R
#      (duration modificada e convexidade, a preço de mercado) aplicado
#      ao choque simulado nos juros longos prefixado e real.
#   C. Custo extra para o Tesouro: a LFT reprecifica na hora (estoque x
#      Selic); prefixados e IPCA só quando vencem e são rolados à taxa
#      nova (perfil de vencimentos). Resultado em R$ bilhões acumulados.
#
# Correspondência entre o modelo (desvios do estado estacionário) e as
# variáveis da regressão:
#   risco      = rp x 100 (p.p. -> pontos-base)
#   juro_real  = i - média de pi nos 4 trimestres seguintes (ex-ante)
#   desanc     = média de pi nos 4 trimestres seguintes (expectativa 12m)
#   inclinacao = il - i (pre 5 anos - Selic)
#   estrutural = 4 x s (primário trimestral -> anual, % do PIB)
#   vol_pre5   = não existe no modelo, fica em zero
#   juro longo real = il - média de pi nos 20 trimestres seguintes
#
# Entradas (dados/): base_composicao_mensal.csv, regressao_composicao.csv,
#   perfil_titulos_ultimo_mes.csv (gerados antes) e
#   scripts/modelo/simulacoes_modelo_estimado.csv (Octave/Dynare)
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
regimes <- c("Pré-2014 (estimado)", "Pós-2014 (estimado)")
cores   <- c("#2E86C1", "#C0392B")

# ---- 1. Desvios-padrão da amostra da regressão ---------------------------
base <- read_csv2("base_composicao_mensal.csv", show_col_types = FALSE) |>
  mutate(mes = as.Date(mes)) |> arrange(mes) |>
  mutate(d12 = lead(LFT, 12) - LFT) |>
  filter(!is.na(d12), !is.na(risco), !is.na(vol_pre5), !is.na(desanc), !is.na(estrutural))
dp <- c(risco = sd(base$risco), juro_real = sd(base$juro_real, na.rm = TRUE),
        desanc = sd(base$desanc), inclinacao = sd(base$inclinacao, na.rm = TRUE),
        estrutural = sd(base$estrutural))
cat("\nDesvios-padrão usados para padronizar:\n"); print(round(dp, 2))

beta <- read_csv2("regressao_composicao.csv", show_col_types = FALSE) |>
  select(titulo, variavel, coef) |>
  pivot_wider(names_from = variavel, values_from = coef)

# ---- 2. Trajetórias das variáveis de mercado ------------------------------
media_frente <- function(x, k) sapply(seq_along(x), function(t) {
  j <- (t + 1):min(t + k, length(x)); if (t == length(x)) x[t] else mean(x[j]) })

traj <- sim |>
  group_by(regime, corte, sim) |> arrange(trimestre, .by_group = TRUE) |>
  mutate(pi_e4  = media_frente(pi, 4),
         pi_e20 = media_frente(pi, 20),
         risco = 100 * rp, juro_real = i - pi_e4, desanc = pi_e4,
         inclinacao = il - i, estrutural = 4 * s,
         real_longo = il - pi_e20) |>
  ungroup()

# ---- A. Composição: coeficientes da regressão nas trajetórias -------------
# A regressão é d12(part) = soma(beta x z). Por trimestre, a participação
# anda 1/4 disso; a mudança acumulada é a soma ao longo da trajetória.
zs <- traj |>
  mutate(z_risco = risco / dp["risco"], z_juro_real = juro_real / dp["juro_real"],
         z_desanc = desanc / dp["desanc"], z_incl = inclinacao / dp["inclinacao"],
         z_estrut = estrutural / dp["estrutural"], z_vol = 0)
for (k in c("LFT", "PRE", "IPCA")) {
  cf <- beta |> filter(titulo == k)
  zs[[paste0("d_", k)]] <- with(zs, cf$z_risco * z_risco + cf$z_juro_real * z_juro_real +
                                  cf$z_desanc * z_desanc + cf$z_incl * z_incl +
                                  cf$z_estrut * z_estrut + cf$z_vol * z_vol) / 4
}
# As três regressões são estimadas separadamente e não somam zero. Imponho
# a soma: o que sobra é redistribuído pelo peso de cada indexador hoje
# (câmbio e demais ficam constantes).
peso <- read_csv2("composicao_mercado_mensal.csv", show_col_types = FALSE) |>
  filter(mes == max(mes)) |>
  mutate(k = recode(indexador, "Selic (LFT)" = "LFT", "Prefixado" = "PRE",
                    "Índice de preços" = "IPCA")) |>
  filter(k %in% c("LFT", "PRE", "IPCA"))
w <- setNames(peso$part_mercado / sum(peso$part_mercado), peso$k)
cat("\nPesos para a soma zero:\n"); print(round(w, 3))
sobra <- zs$d_LFT + zs$d_PRE + zs$d_IPCA
for (k in c("LFT", "PRE", "IPCA")) zs[[paste0("d_", k)]] <- zs[[paste0("d_", k)]] - w[k] * sobra

zs <- zs |> group_by(regime, corte, sim) |> arrange(trimestre, .by_group = TRUE) |>
  mutate(across(c(d_LFT, d_PRE, d_IPCA), cumsum, .names = "part_{.col}")) |> ungroup()

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
      "Canetada e composição da dívida: o que a regressão histórica prevê",
      "Mudança acumulada da participação (p.p.), 1.000 simulações. A regressão é a mesma nos dois regimes: a diferença vem do modelo.",
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
