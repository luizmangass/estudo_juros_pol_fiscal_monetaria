# =====================================================================
# Por que a participação de LFT voltou a subir?
# Testa se a fatia de LFT responde ao NÍVEL dos juros (hipótese "cortes
# do Tombini") ou à INCERTEZA / prêmio da curva (hipótese "fuga do
# prefixado").
#
# Entradas:
#   composicao_indexador.csv   (gerado por divida_publica.R)
#   precotaxatesourodireto.csv (Tesouro Transparente, taxas diárias)
# =====================================================================
library(dplyr); library(tidyr); library(readr); library(stringr)
library(ggplot2); library(scales)
library(sandwich); library(lmtest); library(strucchange)

# ---- 1. Participação de LFT (mensal) ---------------------------------
comp <- read_csv2("composicao_indexador.csv", show_col_types = FALSE) |>
  mutate(data = as.Date(data))
lft <- comp |> filter(indexador == "Selic (LFT)") |>
  transmute(mes = data, lft = 100 * part)           # em %

# ---- 2. Curva a partir das taxas do Tesouro Direto --------------------
# São taxas de varejo (têm spread), mas acompanham o mercado de perto.
td <- read_delim("precotaxatesourodireto.csv", delim = ";",
                 locale = locale(decimal_mark = ",", grouping_mark = "."),
                 show_col_types = FALSE) |>
  transmute(tipo  = `Tipo Titulo`,
            venc  = as.Date(`Data Vencimento`, "%d/%m/%Y"),
            data  = as.Date(`Data Base`, "%d/%m/%Y"),
            taxa  = `Taxa Compra Manha`) |>
  filter(!is.na(taxa), taxa > 0) |>
  mutate(prazo = as.numeric(venc - data) / 365.25)

pre <- c("Tesouro Prefixado", "Tesouro Prefixado com Juros Semestrais")
ipca <- c("Tesouro IPCA+", "Tesouro IPCA+ com Juros Semestrais")

curva_dia <- td |>
  group_by(data) |>
  summarise(
    # prefixado curto: o mais curto com pelo menos 6 meses
    pre_curto = taxa[tipo %in% pre & prazo >= 0.5][
                which.min(prazo[tipo %in% pre & prazo >= 0.5])][1],
    pre_longo = taxa[tipo %in% pre][which.max(prazo[tipo %in% pre])][1],
    real_longo = taxa[tipo %in% ipca][which.max(prazo[tipo %in% ipca])][1],
    .groups = "drop") |>
  arrange(data)

curva_mes <- curva_dia |>
  mutate(mes = as.Date(format(data, "%Y-%m-01")),
         d_longo = c(NA, diff(pre_longo))) |>
  group_by(mes) |>
  summarise(pre_curto  = last(pre_curto),
            pre_longo  = last(pre_longo),
            real_longo = last(real_longo),
            vol_longo  = sd(d_longo, na.rm = TRUE),   # volatilidade diária no mês
            .groups = "drop") |>
  mutate(inclinacao = pre_longo - pre_curto)

base <- inner_join(lft, curva_mes, by = "mes") |>
  arrange(mes) |>
  mutate(d_lft   = lft - lag(lft),
         d_curto = pre_curto - lag(pre_curto),
         d_incl  = inclinacao - lag(inclinacao),
         d_lft_1 = lag(d_lft),
         tombini = mes >= as.Date("2011-01-01") & mes <= as.Date("2016-05-01"))

View(base)

# ---- 3. Gráfico: fatia de LFT x nível x inclinação -------------------
g <- base |>
  select(mes, `Participação de LFT (%)` = lft,
         `Prefixado curto (% a.a.)` = pre_curto,
         `Inclinação: longo - curto (p.p.)` = inclinacao) |>
  pivot_longer(-mes) |>
  ggplot(aes(mes, value)) +
  annotate("rect", xmin = as.Date("2011-01-01"), xmax = as.Date("2016-05-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "grey40") +
  geom_line(colour = "#C0392B", linewidth = 0.8) +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "LFT, nível dos juros e inclinação da curva",
       subtitle = "Faixa cinza: gestão Tombini no BC (jan/2011 a mai/2016)",
       x = NULL, y = NULL,
       caption = "Fontes: Tesouro Nacional (RMD, Anexo 2.1) e Tesouro Direto") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
g
ggsave("05_lft_juros_inclinacao.png", g, width = 10, height = 8, dpi = 150)

# ---- 4. Regressão em primeiras diferenças -----------------------------
# Participações são estoques lentos e não estacionários: em nível, a
# regressão tende a ser espúria. Em diferença perguntamos: no mês em que
# o juro curto cai, ou a curva empina, ou a volatilidade sobe, o que
# acontece com a fatia de LFT?
m1 <- lm(d_lft ~ d_lft_1 + d_curto + d_incl + vol_longo, data = base)
coeftest(m1, vcov = NeweyWest(m1, lag = 6, prewhite = FALSE))

# Mesma coisa com 3 meses de defasagem acumulada (o estoque reage devagar)
base <- base |>
  mutate(d3_lft   = lead(lft, 3) - lft,
         vol_3m   = (vol_longo + lag(vol_longo) + lag(vol_longo, 2)) / 3)
m2 <- lm(d3_lft ~ d_curto + d_incl + vol_3m, data = base)
coeftest(m2, vcov = NeweyWest(m2, lag = 6, prewhite = FALSE))

# ---- 5. Quebras estruturais (Bai-Perron) na fatia de LFT --------------
# Onde a tendência da série muda? As datas estimadas batem com o Tombini,
# ou com 2015 (crise fiscal), 2020 (pandemia), 2024-26?
lft_ts <- ts(lft$lft, start = c(as.integer(format(min(lft$mes), "%Y")),
                                 as.integer(format(min(lft$mes), "%m"))),
             frequency = 12)
tt <- seq_along(lft_ts)
bp <- breakpoints(lft_ts ~ tt, h = 24)      # segmentos de pelo menos 2 anos
summary(bp)
datas_quebra <- lft$mes[bp$breakpoints]
datas_quebra
