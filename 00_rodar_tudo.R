# =====================================================================
# Estudo_Juros: roda todas as análises em ordem
#
# Abra este arquivo no RStudio e rode tudo (Ctrl+Shift+Enter).
# Passo a passo completo, com o significado de cada saída: GUIA.md.
# Os gráficos vão para graficos/ e as bases geradas para resultados/.
#
# Pacotes:
# install.packages(c("readxl","readr","dplyr","tidyr","stringr","ggplot2",
#                    "scales","sandwich","lmtest","strucchange","janitor","jsonlite"))
# =====================================================================

raiz <- "C:/Users/Computador/Desktop/Estudo_Juros"   # ajuste para onde clonou o repositório
if (!dir.exists(raiz)) raiz <- getwd()       # se a pasta mudar de lugar
setwd(file.path(raiz, "dados"))

# Para rodar sem abrir uma janela a cada View() dos scripts.
# Para inspecionar passo a passo, abra o script e rode bloco a bloco.
View <- function(...) invisible(NULL)

scripts <- c(
  "divida_publica.R",          # composição, fatores, custo implícito, corte de 1 p.p.
  "lft_regressao.R",           # LFT x nível e inclinação da curva, quebras
  "custo_primario_nominal.R",  # custo da dívida x primário e nominal
  "teste_bohn.R",              # reação fiscal à dívida, quebra de 2014
  "teste_blanchard.R",         # versão ingênua (só para comparação)
  "surpresas_copom.R",         # surpresas do Copom, risco-país emendado, Blanchard identificado
  "robustez_surpresas_5d.R",   # mesmo teste com Focus de 5 dias úteis (2021+)
  "ancora_fiscal.R",           # postura monetária x impulso fiscal x desancoragem
  "composicao_divida.R",       # o que move LFT/prefixados; ciclos de corte Tombini, Ilan, Campos Neto
  "duration_divida.R",         # composição e duration a preço de mercado, DV01, choque de curva
  "estimar_modelo.R",          # estima as equações do modelo (pré e pós-2014)
  "reacao_mercado_estocastica.R" # canetada -> composição, marcação e custo (usa as simulações do Dynare)
)

for (s in scripts) {
  message("\n==== ", s, " ====")
  source(file.path(raiz, "scripts", s), echo = FALSE, encoding = "UTF-8")
  graphics.off()
}

# Organiza as saídas
dir.create(file.path(raiz, "graficos"), showWarnings = FALSE)
dir.create(file.path(raiz, "resultados"), showWarnings = FALSE)
for (f in list.files(pattern = "\\.png$")) file.rename(f, file.path(raiz, "graficos", f))
gerados <- c("composicao_indexador.csv", "estoque_dpf_longo.csv",
             "fatores_variacao_mensal.csv", "fatores_variacao_anual.csv",
             "surpresas_copom.csv", "risco_pais_emendado.csv",
             "ancora_por_periodo.csv", "ancora_base_trimestral.csv",
             "parametros_estimados.csv", "base_modelo_trimestral.csv",
             "juro_neutro_5a5a.csv", "surpresas_5d_vs_30d.csv", "robustez_5d_lp.csv",
             "regressao_composicao.csv", "evento_ciclos_corte.csv", "base_composicao_mensal.csv",
             "duration_mensal.csv", "perfil_titulos_ultimo_mes.csv", "choque_curva_marcacao.csv",
             "composicao_mercado_mensal.csv", "reacao_mercado_resumo.csv",
             "reacao_mercado_trajetorias.csv")
for (f in intersect(gerados, list.files()))
  file.copy(f, file.path(raiz, "resultados", f), overwrite = TRUE)
if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")

message("\nPronto. Gráficos em graficos/ e bases em resultados/.")
message("O modelo (Dynare) roda à parte: ver scripts/modelo/ e GUIA.md.")
