# =====================================================================
# Gráficos do modelo estimado (lê as saídas de rodar_modelo_estimado.m)
#   17_modelo_estimado_corte_1pp.png  canetada de 1 p.p., pré x pós-2014
#   18_modelo_estimado_corte_2pp.png  canetada de 2 p.p.
#   19_validacao_risco.png            resposta do risco: modelo x dados
# Rode na pasta scripts/modelo (onde estão os CSVs do Octave).
# =====================================================================
library(dplyr); library(readr); library(ggplot2); library(scales)

regimes <- c("Pré-2014 (estimado)", "Pós-2014 (estimado)")
res <- read_csv("resultados_modelo_estimado.csv", show_col_types = FALSE) |>
  mutate(regime = factor(regime, 1:2, regimes))
rotulos <- c(i = "Selic (p.p.)", il = "Juro longo prefixado (p.p.)",
             pi = "Inflação anualizada (p.p.)",
             cambio_nominal = "Câmbio nominal (%, + = depreciação)",
             rp = "Prêmio de risco (p.p.)", b = "Dívida/PIB (p.p.)")
cores <- c("#2E86C1", "#C0392B")

graf <- function(k, arq) {
  g <- res |> filter(corte == k, variavel %in% names(rotulos)) |>
    mutate(variavel = factor(rotulos[variavel], rotulos)) |>
    ggplot(aes(trimestre, group = regime)) +
    annotate("rect", xmin = 0.5, xmax = 4.5, ymin = -Inf, ymax = Inf,
             fill = "grey50", alpha = 0.12) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
    geom_ribbon(aes(ymin = p10, ymax = p90, fill = regime), alpha = 0.16) +
    geom_line(aes(y = deterministico, colour = regime), linewidth = 1) +
    facet_wrap(~variavel, scales = "free_y", ncol = 2) +
    scale_colour_manual(values = cores, name = NULL) +
    scale_fill_manual(values = cores, name = NULL) +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    labs(title = sprintf("Canetada de %d p.p. na Selic: modelo estimado com os dados", k),
         subtitle = paste("Desvio em relação ao cenário sem corte. Faixa cinza: 4 trimestres de corte.",
                          "\nLinha: trajetória central. Sombra: 80% de 1.000 simulações.",
                          "Pós-2014 com o menor ajuste fiscal que estabiliza a dívida."),
         x = "Trimestres", y = NULL,
         caption = paste("Persistências, regra fiscal e choques estimados (2010-2026);",
                         "resposta do risco calibrada pelas surpresas do Copom.")) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  print(g)
  ggsave(arq, g, width = 10, height = 9, dpi = 150)
}
graf(1, "17_modelo_estimado_corte_1pp.png")
graf(2, "18_modelo_estimado_corte_2pp.png")

# ---- Validação: resposta do risco a +1 p.p. de surpresa ------------------
# Pontos: projeções locais de surpresas_copom.R (risco EMBI+/CDS emendado),
# por 1 p.p., em p.p.; barras = IC 90%. Linha: modelo.
dados <- tibble(
  regime = rep(regimes, each = 5),
  meses  = rep(c(0, 3, 6, 9, 12), 2),
  efeito = c(0.201, 0.387, 0.370, NA, 0.549, 0.159, 1.370, 1.240, NA, 1.940),
  ep     = c(0.351, 0.749, 0.700, NA, 0.565, 0.416, 0.609, 0.539, NA, 1.230)) |>
  filter(!is.na(efeito)) |> mutate(regime = factor(regime, regimes))
modelo <- read_csv("irf_risco_modelo.csv", show_col_types = FALSE) |>
  mutate(regime = factor(regime, 1:2, regimes), meses = 3 * (trimestre - 1))

g3 <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_line(data = modelo, aes(meses, rp, colour = regime), linewidth = 1) +
  geom_pointrange(data = dados, aes(meses, efeito, ymin = efeito - 1.645 * ep,
                                    ymax = efeito + 1.645 * ep, colour = regime),
                  position = position_dodge(width = 1)) +
  scale_colour_manual(values = cores, name = NULL) +
  scale_x_continuous(breaks = seq(0, 12, 3)) +
  labs(title = "O modelo reproduz a resposta do risco-país às surpresas do Copom?",
       subtitle = "Resposta a +1 p.p. de surpresa, em p.p. Linha: modelo. Pontos: dados (IC 90%).",
       x = "Meses após a reunião", y = NULL,
       caption = "Dados: projeções locais com surpresas Focus e risco EMBI+/CDS (surpresas_copom.R)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g3
ggsave("19_validacao_risco.png", g3, width = 9, height = 5.5, dpi = 150)
