# =====================================================================
# Surpresas de política monetária e teste de Blanchard identificado
#
# Surpresa do Copom = Selic decidida - Selic esperada pelo Focus
# (mediana para aquela reunião, no último dia antes da decisão).
# Só a parte NÃO esperada da decisão é um choque: a parte esperada já
# estava no preço do câmbio e do CDS antes da reunião.
#
# Entradas:
#   focus_selic_2008_2017.xls / focus_selic_2017_2026.xls  (Focus, Selic
#       por reunião, mediana; baixados do Sistema de Expectativas do BCB)
#   bcdata.sgs.432*.csv  (meta Selic diária, em blocos)
#   bcdata.sgs.3698.csv  (câmbio, média mensal)
#   cds.csv              (Investing.com, Brasil CDS 5 anos USD, mensal)
#   embi.csv             (Ipeadata, EMBI+ Brasil, até jul/2024)
# =====================================================================
library(readxl); library(dplyr); library(tidyr); library(readr)
library(stringr); library(ggplot2); library(scales)
library(sandwich); library(lmtest)

# ---- 1. Focus: Selic esperada por reunião --------------------------------
arqs_focus <- list.files(pattern = "^focus_selic_.*\\.xls$")
focus <- lapply(arqs_focus, function(a) {
  read_excel(a, skip = 1, guess_max = 100000) |>   # sem guess_max, colunas vazias no início viram lógicas
    rename(data = 1) |>
    mutate(data = as.Date(data)) |>
    pivot_longer(-data, names_to = "reuniao", values_to = "esperada",
                 values_transform = as.numeric) |>
    filter(!is.na(esperada))
}) |> bind_rows() |> distinct(data, reuniao, .keep_all = TRUE)

# Data da reunião = último dia em que o Focus ainda projeta aquela reunião
reunioes <- focus |>
  group_by(reuniao) |>
  slice_max(data, n = 1) |>
  ungroup() |>
  rename(data_reuniao = data) |>
  filter(data_reuniao < max(focus$data))      # só reuniões que já ocorreram

# ---- 2. Decisão efetiva (meta Selic, série 432) ---------------------------
meta <- list.files(pattern = "^bcdata\\.sgs\\.432.*\\.csv$") |>
  lapply(\(a) read_delim(a, delim = ";", col_types = "cc",
                         locale = locale(decimal_mark = ","))) |>
  bind_rows() |>
  transmute(data = as.Date(data, "%d/%m/%Y"),
            meta = as.numeric(sub(",", ".", valor, fixed = TRUE))) |>
  distinct(data, .keep_all = TRUE) |> arrange(data)

# A nova meta passa a valer no dia seguinte ao anúncio. O último dia do
# Focus pode ser a véspera do anúncio, então lemos a meta 7 dias depois
# (a reunião seguinte está sempre a mais de 40 dias).
decisao <- function(d) meta$meta[which(meta$data >= d + 7)[1]]
antes   <- function(d) meta$meta[tail(which(meta$data <= d), 1)]

surp <- reunioes |>
  rowwise() |>
  mutate(decidida = decisao(data_reuniao),
         anterior = antes(data_reuniao)) |>
  ungroup() |>
  mutate(variacao = decidida - anterior,
         surpresa = decidida - esperada) |>
  arrange(data_reuniao)

View(surp)
cat("\nReuniões:", nrow(surp), " | surpresas diferentes de zero:",
    sum(abs(surp$surpresa) > 0.01), "\n")
print(surp |> filter(abs(surpresa) >= 0.25) |>
        select(reuniao, data_reuniao, anterior, esperada, decidida, surpresa),
      n = Inf)

g1 <- ggplot(surp, aes(data_reuniao, surpresa)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_col(fill = "#C0392B", width = 25) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Surpresas do Copom: decisão menos o esperado pelo Focus",
       subtitle = "p.p.; positivo = Copom mais duro que o mercado esperava",
       x = NULL, y = NULL,
       caption = "Fontes: BCB, Focus (mediana, base 30 dias) e SGS 432") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
g1
ggsave("13_surpresas_copom.png", g1, width = 10, height = 4.5, dpi = 150)

# ---- 3. Base mensal ---------------------------------------------------------
le_sgs <- function(codigo, nome) {
  read_delim(sprintf("bcdata.sgs.%d.csv", codigo), delim = ";", col_types = "cc",
             locale = locale(decimal_mark = ",")) |>
    transmute(mes = as.Date(data, "%d/%m/%Y"),
              !!nome := as.numeric(sub(",", ".", valor, fixed = TRUE)))
}
# ---- Risco-país: EMBI+ até jul/2024, emendado com o CDS depois -----------
# O CDS do Investing tem meses "planos" (abertura = máxima = mínima, sem
# negociação registrada) em 2008-2012 e em 2022. O EMBI+ é limpo, mas foi
# descontinuado em jul/2024. Solução: EMBI+ como base e, depois de jul/2024,
# o CDS convertido para a escala do EMBI pela regressão na sobreposição
# (só meses de CDS com negociação).
num_br <- function(x) as.numeric(sub(",", ".", gsub("\\.", "", x), fixed = TRUE))
cds <- read_csv("cds.csv", col_types = cols(.default = "c")) |>
  transmute(mes = as.Date(Data, "%d.%m.%Y"), cds = num_br(`Último`),
            plano = Abertura == `Máxima` & Abertura == `Mínima`)
embi_m <- read_csv("embi.csv", col_types = "cc", col_names = c("data", "embi"),
                   skip = 1) |>
  mutate(dia = as.Date(data, "%d/%m/%Y"), embi = as.numeric(embi)) |>
  filter(!is.na(embi)) |>
  group_by(mes = as.Date(format(dia, "%Y-%m-01"))) |>
  summarise(embi = last(embi), .groups = "drop")          # fim do mês, como o CDS

sobrep <- inner_join(embi_m, cds, by = "mes") |> filter(!plano)
ponte  <- lm(embi ~ cds, data = sobrep)
cat(sprintf("\nPonte CDS -> EMBI: EMBI = %.1f + %.3f x CDS  (R2 = %.2f, n = %d)\n",
            coef(ponte)[1], coef(ponte)[2], summary(ponte)$r.squared, nrow(sobrep)))

risco <- full_join(embi_m, select(cds, mes, cds), by = "mes") |>
  arrange(mes) |>
  mutate(risco = if_else(!is.na(embi), embi,
                         coef(ponte)[1] + coef(ponte)[2] * cds),
         fonte = if_else(!is.na(embi), "EMBI+", "CDS convertido")) |>
  filter(!is.na(risco))

g_r <- ggplot(risco, aes(mes, risco, colour = fonte)) +
  geom_line(linewidth = 0.8) +
  geom_line(data = filter(cds, mes >= as.Date("2013-01-01")),
            aes(mes, cds), colour = "grey60", linewidth = 0.4, inherit.aes = FALSE) +
  scale_colour_manual(values = c("CDS convertido" = "#C0392B", "EMBI+" = "#2E86C1"),
                      name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Risco-país: EMBI+ emendado com o CDS de 5 anos",
       subtitle = "Pontos-base, fim do mês. Cinza: CDS original, para comparação.",
       x = NULL, y = NULL,
       caption = "Fontes: Ipeadata/JP Morgan (EMBI+) e Investing.com (CDS 5 anos USD)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g_r
ggsave("15_risco_pais_emendado.png", g_r, width = 10, height = 4.5, dpi = 150)
write_csv2(risco, "risco_pais_emendado.csv")

cds <- transmute(risco, mes, cds = risco)    # daqui em diante "cds" = série emendada

surp_m <- surp |>
  group_by(mes = as.Date(format(data_reuniao, "%Y-%m-01"))) |>
  summarise(surpresa = sum(surpresa), variacao = sum(variacao), .groups = "drop")

base <- le_sgs(3698, "cambio") |>
  inner_join(cds, by = "mes") |>
  left_join(surp_m, by = "mes") |>
  mutate(surpresa = coalesce(surpresa, 0), variacao = coalesce(variacao, 0)) |>
  filter(mes >= as.Date("2009-01-01")) |>      # Focus por reunião começa em dez/2008
  arrange(mes) |>
  mutate(lcambio = 100 * log(cambio),
         d_cambio = lcambio - lag(lcambio), d_cds = cds - lag(cds),
         pos2014 = as.numeric(mes > as.Date("2014-06-01")))
range(base$mes)

# ---- 4. Projeções locais com a surpresa ------------------------------------
H <- 12
lp <- function(var, choque) {
  lapply(0:H, function(h) {
    d <- base |>
      mutate(y = lead(.data[[var]], h) - lag(.data[[var]]),
             x = .data[[choque]], x_pos = x * pos2014,
             l1_dc = lag(d_cambio), l2_dc = lag(d_cambio, 2),
             l1_dd = lag(d_cds), l2_dd = lag(d_cds, 2))
    m <- lm(y ~ x + x_pos + pos2014 + l1_dc + l2_dc + l1_dd + l2_dd, data = d)
    V <- NeweyWest(m, lag = h + 1, prewhite = FALSE)
    b <- coef(m)["x"]; bc <- b + coef(m)["x_pos"]
    tibble(var = var, choque = choque, h = h,
           periodo = c("2009 a jun/2014", "Depois de jun/2014"),
           efeito = c(b, bc),
           ep = sqrt(c(V["x", "x"],
                       V["x", "x"] + V["x_pos", "x_pos"] + 2 * V["x", "x_pos"])))
  }) |> bind_rows()
}

res <- bind_rows(lp("lcambio", "surpresa"), lp("cds", "surpresa"),
                 lp("lcambio", "variacao"), lp("cds", "variacao")) |>
  mutate(var = recode(var, lcambio = "Câmbio (%, + = depreciação)",
                      cds = "Risco-país, EMBI+ emendado (p.b.)"),
         choque = recode(choque, surpresa = "Surpresa do Copom (identificado)",
                         variacao = "Variação da Selic (ingênuo)"))
print(filter(res, h %in% c(0, 3, 6, 12)), n = Inf)

g2 <- ggplot(filter(res, choque == "Surpresa do Copom (identificado)"),
             aes(h, efeito, colour = periodo, fill = periodo)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_ribbon(aes(ymin = efeito - 1.645 * ep, ymax = efeito + 1.645 * ep),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~var, scales = "free_y") +
  scale_colour_manual(values = c("#2E86C1", "#C0392B"), name = NULL) +
  scale_fill_manual(values = c("#2E86C1", "#C0392B"), name = NULL) +
  scale_x_continuous(breaks = seq(0, H, 3)) +
  labs(title = "Resposta a uma surpresa de +1 p.p. do Copom",
       subtitle = "Projeções locais, 2009-2026. Faixa: IC 90% (Newey-West).",
       x = "Meses após a reunião", y = NULL,
       caption = "Surpresa = decisão - mediana Focus. Risco-país: EMBI+ até jul/2024, depois CDS convertido. Controles: 2 defasagens de câmbio e risco.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g2
ggsave("14_blanchard_surpresas.png", g2, width = 10, height = 5.5, dpi = 150)

write_csv2(surp, "surpresas_copom.csv")
