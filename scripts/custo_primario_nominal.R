# =====================================================================
# Custo implícito da DPF x resultado primário x resultado nominal
#
# A planilha "receita-orcamentaria-da-uniao.csv" NÃO traz primário nem
# nominal (é só receita, de 1980 a 2017). Primário e nominal vêm das
# NFSP do Banco Central (SGS), baixadas direto pela API abaixo.
#
#   SGS 5793: NFSP sem desvalorização cambial, % PIB, acumulado 12 meses,
#             resultado PRIMÁRIO, setor público consolidado
#   SGS 5727: idem, resultado NOMINAL
#   Convenção das NFSP: positivo = DÉFICIT, negativo = superávit.
#   Juros nominais (% PIB) = nominal - primário.
#
# Entrada: fatores_variacao_mensal.csv (gerado por divida_publica.R)
# =====================================================================
library(dplyr); library(tidyr); library(readr)
library(ggplot2); library(scales); library(jsonlite)

# ---- 1. Séries do BCB --------------------------------------------------
# Lê o CSV baixado do BCB ("bcdata.sgs.XXXX.csv") se estiver na pasta;
# se não estiver, baixa direto da API.
sgs <- function(codigo, inicio = "01/01/2003") {
  arq <- sprintf("bcdata.sgs.%d.csv", codigo)
  if (file.exists(arq)) {
    x <- read_delim(arq, delim = ";", col_types = "cc",
                    locale = locale(decimal_mark = ","))
  } else {
    url <- sprintf(paste0("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%d/",
                          "dados?formato=json&dataInicial=%s"), codigo, inicio)
    x <- fromJSON(url)
  }
  tibble(mes   = as.Date(x$data, "%d/%m/%Y"),
         valor = as.numeric(sub(",", ".", x$valor, fixed = TRUE)))
}

nfsp <- sgs(5793) |> rename(primario_nfsp = valor) |>
  inner_join(sgs(5727) |> rename(nominal_nfsp = valor), by = "mes") |>
  mutate(
    # Troca o sinal para a leitura usual: positivo = superávit
    primario = -primario_nfsp,
    nominal  = -nominal_nfsp,
    juros    = nominal_nfsp - primario_nfsp)        # juros nominais, % PIB
View(nfsp)

# ---- 2. Custo implícito (mesmo cálculo de divida_publica.R) ------------
fat <- read_csv2("fatores_variacao_mensal.csv", show_col_types = FALSE) |>
  mutate(mes = as.Date(data)) |> arrange(mes) |>
  mutate(taxa_mes = juros / estoque_ant)
fat$custo_12m <- sapply(seq_len(nrow(fat)), function(i)
  if (i < 12) NA_real_ else 100 * (prod(1 + fat$taxa_mes[(i - 11):i]) - 1))

base <- nfsp |> inner_join(select(fat, mes, custo_12m), by = "mes")

# ---- 3. Gráfico em dois painéis (unidades diferentes) ------------------
painel_custo <- base |>
  transmute(mes, serie = "Custo implícito da DPF (% a.a.)", valor = custo_12m,
            painel = "Custo da dívida")
painel_fiscal <- base |>
  select(mes, `Primário (+ = superávit)` = primario,
         `Nominal (+ = superávit)` = nominal,
         `Juros nominais` = juros) |>
  pivot_longer(-mes, names_to = "serie", values_to = "valor") |>
  mutate(valor = if_else(serie == "Juros nominais", -valor, valor),
         serie = if_else(serie == "Juros nominais",
                         "Juros nominais (com sinal de despesa)", serie),
         painel = "Resultado fiscal, % do PIB (12 meses)")

g <- bind_rows(painel_custo, painel_fiscal) |>
  mutate(painel = factor(painel, c("Custo da dívida",
                                   "Resultado fiscal, % do PIB (12 meses)"))) |>
  ggplot(aes(mes, valor, colour = serie)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~painel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("#C0392B", "#7D3C98", "#2E86C1", "#7D9D3C"),
                      name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Custo da dívida e resultado fiscal",
       x = NULL, y = NULL,
       caption = "Fontes: Tesouro Nacional (RMD, Anexo 2.9) e BCB (SGS 5793 e 5727)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2))
g
ggsave("06_custo_primario_nominal.png", g, width = 10, height = 8, dpi = 150)

# ---- 4. Bônus: o que a planilha de receita mostra ----------------------
# A linha REFINANCIAMENTO é rolagem de dívida (emitir título novo para
# pagar o que vence). Quando alguém diz que "metade do orçamento vai para
# a dívida", está somando a rolagem, que não é gasto com juros.
rec <- read_delim("receita-orcamentaria-da-uniao.csv", delim = ";",
                  locale = locale(decimal_mark = ",", grouping_mark = "."),
                  show_col_types = FALSE) |>
  filter(!is.na(`ANO DE REFERENCIA`), `ANO DE REFERENCIA` >= 1995) |>
  transmute(ano = `ANO DE REFERENCIA`,
            refin_pct = 100 * REFINANCIAMENTO / (RECEITAS + REFINANCIAMENTO))
rec   # a partir de 1995 (antes disso, moedas diferentes e sem a linha)
