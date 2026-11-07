# =====================================================================
# Gráficos da simulação da "canetada" (lê resultados_modelo.csv,
# gerado por rodar_modelo.m no Dynare)
# =====================================================================
library(dplyr); library(readr); library(ggplot2); library(scales)

res <- read_csv("resultados_modelo.csv", show_col_types = FALSE) |>
  mutate(regime = factor(regime, 1:2, c("Curva confiante", "Curva desconfiada")),
         corte  = factor(paste0("Corte de ", corte, " p.p."),
                         c("Corte de 1 p.p.", "Corte de 2 p.p.")))

rotulos <- c(
  i              = "Selic (p.p.)",
  il             = "Juro longo prefixado (p.p.)",
  pi             = "Inflação anualizada (p.p.)",
  cambio_nominal = "Câmbio nominal (%, + = depreciação)",
  ief            = "Custo médio da dívida (p.p.)",
  b              = "Dívida/PIB (p.p.)")

graf <- function(dados, titulo, arquivo) {
  g <- dados |>
    filter(variavel %in% names(rotulos)) |>
    mutate(variavel = factor(rotulos[variavel], rotulos)) |>
    ggplot(aes(trimestre, group = regime)) +
    annotate("rect", xmin = 0.5, xmax = 4.5, ymin = -Inf, ymax = Inf,
             fill = "grey50", alpha = 0.12) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
    geom_ribbon(aes(ymin = p10, ymax = p90, fill = regime), alpha = 0.18) +
    geom_line(aes(y = deterministico, colour = regime), linewidth = 1) +
    facet_wrap(~variavel, scales = "free_y", ncol = 2) +
    scale_colour_manual(values = c("#2E86C1", "#C0392B"), name = NULL) +
    scale_fill_manual(values = c("#2E86C1", "#C0392B"), name = NULL) +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    labs(title = titulo,
         subtitle = paste("Desvio em relação ao cenário sem corte. Faixa cinza:",
                          "Selic mantida abaixo da regra por 4 trimestres.\n",
                          "Linha: trajetória central. Sombra: 80% das 1.000 simulações",
                          "com choques aleatórios."),
         x = "Trimestres", y = NULL,
         caption = "Modelo novo-keynesiano com bloco fiscal, calibração ilustrativa") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  print(g)
  ggsave(arquivo, g, width = 10, height = 9, dpi = 150)
}

graf(filter(res, corte == "Corte de 1 p.p."),
     "Canetada de 1 p.p. na Selic", "07_modelo_corte_1pp.png")
graf(filter(res, corte == "Corte de 2 p.p."),
     "Canetada de 2 p.p. na Selic", "08_modelo_corte_2pp.png")

# Tabela-resumo: picos e efeito acumulado
res |>
  group_by(regime, corte, variavel) |>
  summarise(pico = deterministico[which.max(abs(deterministico))],
            tri_pico = trimestre[which.max(abs(deterministico))],
            t24 = deterministico[trimestre == 24], .groups = "drop") |>
  filter(variavel %in% names(rotulos)) |>
  arrange(variavel, regime, corte) |>
  print(n = Inf)
