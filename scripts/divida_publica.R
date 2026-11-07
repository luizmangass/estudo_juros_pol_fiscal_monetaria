# =====================================================================
# Dívida Pública Federal: composição, custo e sensibilidade à Selic
# Base: Tesouro Nacional, RMD - Anexo 2.1 (Estoque da DPF) e
#       Anexo 2.9 (Fatores de variação da DPF)
#
# Rode bloco a bloco (Ctrl+Enter) e inspecione cada objeto com View().
# =====================================================================

# ---- 0. Pacotes ------------------------------------------------------
# install.packages(c("readxl","dplyr","tidyr","stringr","ggplot2","scales"))
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

# Garante leitura correta de acentos (Windows/Linux)
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")))

# Ajuste para a pasta onde estão os arquivos
arq_estoque <- "estoque-da-divida-publica-federal.xlsx"
arq_fatores <- "fatores-de-variacao-da-divida-publica-federal.xlsx"

# Converte "Dez/00" em 2000-12-01
meses <- c(jan = 1, fev = 2, mar = 3, abr = 4, mai = 5, jun = 6,
           jul = 7, ago = 8, set = 9, out = 10, nov = 11, dez = 12)
mes_para_data <- function(x) {
  m <- meses[str_to_lower(str_sub(x, 1, 3))]
  a <- 2000L + as.integer(str_sub(x, 5, 6))
  as.Date(sprintf("%d-%02d-01", a, m))
}

# =====================================================================
# PARTE A - ESTOQUE (Anexo 2.1), R$ bilhões
# =====================================================================

# ---- A1. Leitura crua ------------------------------------------------
# O anexo é uma tabela de apresentação: título, linhas em branco,
# hierarquia só pela indentação e notas de rodapé. Lemos tudo como texto.
raw <- read_excel(arq_estoque, sheet = "2.1", col_names = FALSE,
                  col_types = "text", .name_repair = "minimal")
names(raw) <- paste0("c", seq_len(ncol(raw)))

# Linha com as datas = a que tem "Dez/00" na coluna 2
i_head <- which(raw$c2 == "Dez/00")
cab    <- unlist(raw[i_head, -1])
col_ok <- str_detect(cab, "^[A-Za-z]{3}/\\d{2}$") %in% TRUE
datas  <- mes_para_data(cab[col_ok])
range(datas)

# ---- A2. Corpo da tabela + hierarquia --------------------------------
corpo <- raw[(i_head + 1):nrow(raw), c(TRUE, col_ok)]
names(corpo) <- c("rotulo", as.character(datas))

corpo <- corpo |>
  mutate(rotulo = str_squish(str_remove_all(rotulo, "[¹²³]"))) |>
  filter(!is.na(rotulo), rotulo != "", !str_detect(rotulo, "^(Obs|\\d )"))

# O mesmo nome (LTN, LFT...) aparece em dois blocos; o bloco vem da posição
corpo <- corpo |>
  mutate(bloco = case_when(
    str_detect(rotulo, "^DPF EM PODER DO P")     ~ "Total",
    rotulo == "DPMFi"                            ~ "DPMFi (público)",
    rotulo == "DPFe"                             ~ "DPFe",
    str_detect(rotulo, "BANCO CENTRAL")          ~ "Carteira do BC",
    TRUE ~ NA_character_)) |>
  fill(bloco)

# ---- A3. Formato longo e numérico ------------------------------------
estoque <- corpo |>
  pivot_longer(-c(rotulo, bloco), names_to = "data", values_to = "valor") |>
  mutate(data  = as.Date(data),
         valor = as.numeric(valor)) |>        # o Excel já guarda número puro
  filter(!is.na(valor))

View(estoque)

# ---- A4. Títulos da DPMFi em poder do público ------------------------
# Abertura por título só existe a partir de jan/2006
titulos_pub <- estoque |>
  filter(bloco == "DPMFi (público)",
         rotulo %in% c("LFT","LTN","NTN-B","NTN-B1","NTN-C","NTN-D","NTN-F",
                       "Dívida Securitizada","TDA","Demais"))

dpfe_tot <- estoque |> filter(rotulo == "DPFe") |>
  transmute(data, rotulo = "DPFe", valor)

total_pub <- estoque |> filter(bloco == "Total") |>
  select(data, total = valor)

# ---- A5. Composição por indexador ------------------------------------
indexador_de <- function(t) case_when(
  t == "LFT"                     ~ "Selic (LFT)",
  t %in% c("LTN", "NTN-F")       ~ "Prefixado (LTN + NTN-F)",
  t %in% c("NTN-B", "NTN-B1", "NTN-C") ~ "Índice de preços (NTN-B, NTN-C)",
  t %in% c("DPFe", "NTN-D")      ~ "Câmbio (DPFe)",
  TRUE                           ~ "Demais")

comp <- bind_rows(titulos_pub, dpfe_tot) |>
  filter(data >= as.Date("2006-01-01")) |>
  mutate(indexador = indexador_de(rotulo)) |>
  group_by(data, indexador) |>
  summarise(valor = sum(valor), .groups = "drop") |>
  left_join(total_pub, by = "data") |>
  mutate(part = valor / total)

# Checagem: participações somam ~100%?
comp |> group_by(data) |> summarise(soma = sum(part)) |> summary()

# Último mês
ultimo <- max(comp$data)
comp |> filter(data == ultimo) |> arrange(desc(part))

ordem <- c("Selic (LFT)", "Prefixado (LTN + NTN-F)",
           "Índice de preços (NTN-B, NTN-C)", "Câmbio (DPFe)", "Demais")
cores <- c("#C0392B", "#2E86C1", "#7D9D3C", "#8E6C8A", "#AAB7B8")

g1 <- ggplot(comp, aes(data, part, fill = factor(indexador, ordem))) +
  geom_area(colour = "white", linewidth = 0.1) +
  scale_fill_manual(values = cores, name = NULL) +
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0, 0)) +
  labs(title = "Composição da DPF em poder do público, por indexador",
       subtitle = "Participação no estoque",
       x = NULL, y = NULL,
       caption = "Fonte: Tesouro Nacional, RMD - Anexo 2.1") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g1
ggsave("01_composicao_indexador.png", g1, width = 10, height = 6, dpi = 150)

# Só a fatia Selic, que é o centro do argumento
g2 <- comp |> filter(indexador == "Selic (LFT)") |>
  ggplot(aes(data, part)) +
  geom_line(colour = "#C0392B", linewidth = 1) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Participação da LFT (Selic) na DPF em poder do público",
       x = NULL, y = NULL,
       caption = "Fonte: Tesouro Nacional, RMD - Anexo 2.1") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())
g2
ggsave("02_participacao_lft.png", g2, width = 10, height = 5, dpi = 150)

# =====================================================================
# PARTE B - FATORES DE VARIAÇÃO (Anexo 2.9), R$ milhões
# Δ estoque = emissão líquida (financiamento) + juros apropriados
# =====================================================================

# ---- B1. Leitura crua ------------------------------------------------
rawf <- read_excel(arq_fatores, sheet = "2.9", col_names = FALSE,
                   .name_repair = "minimal")
names(rawf) <- paste0("c", seq_len(ncol(rawf)))

i_hf  <- which(rawf$c1 == "Indicadores")
cabf  <- as.character(unlist(rawf[i_hf, -1]))
# O cabeçalho mistura meses ("Jan/07") e totais anuais (2007, 2008 ...)
eh_mes <- str_detect(cabf, "^[A-Za-z]{3}/\\d{2}$") %in% TRUE
eh_ano <- str_detect(cabf, "^\\d{4}$") %in% TRUE

linhas_f <- c(
  estoque_ant = "Estoque anterior",
  var_nominal = "Variação Nominal",
  emis_liq    = "I.1 - Emissão/Resgate Líquido",
  juros       = "I.2 - Juros  Apropriados",
  juros_dpmfi = "- Juros Apropriados da DPMFi",
  juros_dpfe  = "- Juros Apropriados da DPFe")

pega_linha <- function(padrao) {
  rotulos <- str_squish(str_remove(as.character(rawf$c1), "\\s*\\d+$"))
  which(str_detect(rotulos, fixed(str_squish(padrao))))[1]
}

monta <- function(cols_mask, nomes_col) {
  out <- lapply(names(linhas_f), function(k) {
    i <- pega_linha(linhas_f[[k]])
    tibble(periodo = nomes_col, serie = k,
           valor = as.numeric(unlist(rawf[i, -1][cols_mask])))
  })
  bind_rows(out) |> pivot_wider(names_from = serie, values_from = valor)
}

fat_mensal <- monta(eh_mes, cabf[eh_mes]) |>
  mutate(data = mes_para_data(periodo), .before = 1) |> select(-periodo)

fat_anual <- monta(eh_ano, cabf[eh_ano]) |>
  mutate(ano = as.integer(periodo), .before = 1) |> select(-periodo)

View(fat_mensal); View(fat_anual)

# Checagem contábil: emissão líquida + juros = variação nominal
# (diferenças pontuais vêm de ajustes do Tesouro, ex.: retificação de abr/2021)
fat_mensal |> mutate(dif = emis_liq + juros - var_nominal) |>
  filter(abs(dif) > 1) |> select(data, var_nominal, emis_liq, juros, dif)

# ---- B2. Quanto da dívida cresce por juros x por financiamento --------
anual_long <- fat_anual |>
  select(ano, `Juros apropriados` = juros,
         `Emissão líquida (financiamento)` = emis_liq) |>
  pivot_longer(-ano, values_to = "valor") |>
  mutate(valor = valor / 1e3)            # R$ bilhões

g3 <- ggplot(anual_long, aes(factor(ano), valor, fill = name)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = c("#C0392B", "#2E86C1"), name = NULL) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "De onde vem a variação da DPF",
       subtitle = "R$ bilhões por ano (2026 até o último mês disponível)",
       x = NULL, y = NULL,
       caption = "Fonte: Tesouro Nacional, RMD - Anexo 2.9") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
g3
ggsave("03_fatores_juros_x_emissao.png", g3, width = 10, height = 6, dpi = 150)

# ---- B3. Custo implícito: juros apropriados / estoque anterior --------
# Inclui atualização monetária (IPCA das NTN-B) e variação cambial da DPFe;
# é um custo nominal "contábil", não a taxa de emissão.
custo <- fat_mensal |>
  mutate(taxa_mes = juros / estoque_ant) |>
  arrange(data)

# Taxa acumulada em 12 meses (sem pacote extra)
tm <- custo$taxa_mes
custo$taxa_12m <- sapply(seq_along(tm), function(i)
  if (i < 12) NA_real_ else prod(1 + tm[(i - 11):i]) - 1)

g4 <- ggplot(filter(custo, !is.na(taxa_12m)), aes(data, taxa_12m)) +
  geom_line(colour = "#C0392B", linewidth = 1) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Custo implícito da DPF, acumulado em 12 meses",
       subtitle = "Juros apropriados / estoque do mês anterior",
       x = NULL, y = NULL,
       caption = "Fonte: Tesouro Nacional, RMD - Anexo 2.9. Inclui variação cambial da DPFe.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())
g4
ggsave("04_custo_implicito_12m.png", g4, width = 10, height = 5, dpi = 150)

# =====================================================================
# PARTE C - O NÚMERO DO ARTIGO: quanto um corte de Selic economiza
# =====================================================================
# Efeito imediato de -1 p.p. na Selic = estoque de LFT x 1 p.p. ao ano.
# Prefixados e NTN-B já emitidos não mudam: o custo está travado até
# o vencimento, e o das emissões novas é definido pelo mercado.
lft_hoje   <- titulos_pub |> filter(rotulo == "LFT", data == ultimo) |> pull(valor)
total_hoje <- total_pub   |> filter(data == ultimo) |> pull(total)
juros_12m  <- fat_mensal |> arrange(data) |> tail(12) |>
  summarise(sum(juros) / 1e3) |> pull()        # R$ bilhões

economia_1pp <- lft_hoje * 0.01

cat(sprintf(
"Mês de referência ........................ %s
Estoque DPF em poder do público ......... R$ %.0f bi
LFT (Selic) ............................. R$ %.0f bi (%.1f%%)
Juros apropriados nos últimos 12 meses .. R$ %.0f bi
Economia de 1 p.p. de Selic (12 meses) .. R$ %.1f bi (%.1f%% dos juros)
",
  format(ultimo, "%m/%Y"), total_hoje, lft_hoje, 100 * lft_hoje / total_hoje,
  juros_12m, economia_1pp, 100 * economia_1pp / juros_12m))

# Exporta as bases limpas para reutilizar (no modelo, no Excel etc.)
write.csv2(estoque,    "estoque_dpf_longo.csv",       row.names = FALSE)
write.csv2(comp,       "composicao_indexador.csv",    row.names = FALSE)
write.csv2(fat_mensal, "fatores_variacao_mensal.csv", row.names = FALSE)
write.csv2(fat_anual,  "fatores_variacao_anual.csv",  row.names = FALSE)
