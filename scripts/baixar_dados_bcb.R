# =====================================================================
# Baixa de uma vez todas as séries do BCB usadas nas análises e salva
# cada uma como bcdata.sgs.CODIGO.csv (o mesmo formato do site).
# Rode no seu computador; precisa de internet.
#
# Atalho manual: cole no navegador
#   https://api.bcb.gov.br/dados/serie/bcdata.sgs.CODIGO/dados?formato=csv
# trocando CODIGO pelo número (ex.: 13762).
# Séries diárias (Selic 432) aceitam no máximo 10 anos por pedido:
#   ...dados?formato=csv&dataInicial=01/01/2015&dataFinal=31/12/2024
# =====================================================================
library(jsonlite); library(dplyr); library(readr)

series <- c(
  # --- Fiscal
  primario_consolidado = 5793,   # NFSP primário, % PIB, 12m (+ = déficit)
  nominal_consolidado  = 5727,   # NFSP nominal, % PIB, 12m (+ = déficit)
  primario_inss        = 7870,   # NFSP primário INSS, % PIB, 12m
  dbgg_pib             = 13762,  # Dívida bruta do governo geral, % PIB
  dlsp_pib             = 4513,   # Dívida líquida do setor público, % PIB
  # --- Atividade
  ibcbr_dessaz         = 24364,  # IBC-Br com ajuste sazonal
  # --- Monetário e preços
  selic_meta           = 432,    # Meta Selic (diária)
  selic_mes_anual      = 4189,   # Selic acumulada no mês, anualizada
  ipca_mensal          = 433,    # IPCA, variação mensal
  ipca_12m             = 13522,  # IPCA, acumulado 12 meses
  # --- Câmbio
  cambio_medio_mensal  = 3698    # R$/US$ venda, média mensal
)

baixa_sgs <- function(codigo, inicio = as.Date("2000-01-01"), fim = Sys.Date()) {
  # Quebra em blocos de 10 anos (exigência das séries diárias)
  cortes <- seq(inicio, fim, by = "10 years")
  pedacos <- lapply(cortes, function(d0) {
    d1  <- min(seq(d0, by = "10 years", length.out = 2)[2] - 1, fim)
    url <- sprintf(paste0("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%d/",
                          "dados?formato=json&dataInicial=%s&dataFinal=%s"),
                   codigo, format(d0, "%d/%m/%Y"), format(d1, "%d/%m/%Y"))
    tryCatch(fromJSON(url), error = function(e) NULL)
  })
  bind_rows(pedacos) |> distinct()
}

for (nm in names(series)) {
  cod <- series[[nm]]
  x <- baixa_sgs(cod)
  if (nrow(x) == 0) { message("Falhou: ", nm, " (", cod, ")"); next }
  x$valor <- sub(".", ",", x$valor, fixed = TRUE)   # mesmo formato do site
  write_delim(x, sprintf("bcdata.sgs.%d.csv", cod), delim = ";")
  message(sprintf("%-22s %6d  %d obs  %s a %s", nm, cod, nrow(x),
                  x$data[1], x$data[nrow(x)]))
}

# ---- Expectativas Focus (IPCA 12 meses à frente) ----------------------
# Serviço Olinda do BCB. Não consegui testar daqui; se falhar, baixe em
# https://www3.bcb.gov.br/expectativas2/#/consultaSeriesEstatisticas
url_focus <- paste0(
  "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/",
  "ExpectativasMercadoInflacao12Meses?$format=json",
  "&$filter=Indicador%20eq%20'IPCA'%20and%20Suavizada%20eq%20'S'",
  "&$select=Data,Mediana,baseCalculo")
focus <- tryCatch(fromJSON(url_focus)$value, error = function(e) NULL)
if (!is.null(focus)) {
  write_csv2(focus, "focus_ipca_12m.csv")
  message("Focus IPCA 12m: ", nrow(focus), " obs")
} else message("Focus: falhou, baixe manualmente")
