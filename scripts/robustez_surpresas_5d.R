# =====================================================================
# Robustez: surpresas do Copom com a mediana Focus de 5 dias úteis
#
# A base de 30 dias pode carregar expectativas velhas e inflar a
# "surpresa". A base de 5 dias úteis da Selic por reunião só existe a
# partir de mar/2021, então o teste é:
#   1. comparar as surpresas 5d x 30d nas reuniões de 2021 em diante;
#   2. refazer o teste de Blanchard pós-2014 trocando as surpresas de
#      2021+ pela versão de 5 dias.
#
# Entradas (dados/): focus_selic_5d_2021_2026.xls, bcdata.sgs.432_*.csv,
#   bcdata.sgs.3698.csv; e, gerados por surpresas_copom.R:
#   surpresas_copom.csv, risco_pais_emendado.csv
# =====================================================================
library(readxl); library(dplyr); library(tidyr); library(readr)
library(ggplot2); library(sandwich); library(lmtest)

# ---- 1. Surpresas com a base de 5 dias -----------------------------------
f5 <- read_excel("focus_selic_5d_2021_2026.xls", skip = 1, guess_max = 100000) |>
  rename(data = 1) |> mutate(data = as.Date(data)) |>
  pivot_longer(-data, names_to = "reuniao", values_to = "esperada_5d",
               values_transform = as.numeric) |>
  filter(!is.na(esperada_5d))
reun5 <- f5 |> group_by(reuniao) |> slice_max(data, n = 1) |> ungroup() |>
  filter(data < max(f5$data)) |> select(reuniao, esperada_5d)

s30 <- read_csv2("surpresas_copom.csv", show_col_types = FALSE) |>
  mutate(data_reuniao = as.Date(data_reuniao))

comp <- s30 |> inner_join(reun5, by = "reuniao") |>
  mutate(surpresa_5d = decidida - esperada_5d) |>
  select(reuniao, data_reuniao, decidida, esperada_30d = esperada,
         esperada_5d, surpresa_30d = surpresa, surpresa_5d) |>
  arrange(data_reuniao)
cat("\nReuniões comparadas:", nrow(comp), "\n")
print(comp |> filter(abs(surpresa_30d) > 0.01 | abs(surpresa_5d) > 0.01), n = Inf)
cat(sprintf("\nCorrelação 5d x 30d: %.2f | surpresas não nulas: 30d = %d, 5d = %d\n",
            cor(comp$surpresa_30d, comp$surpresa_5d),
            sum(abs(comp$surpresa_30d) > 0.01), sum(abs(comp$surpresa_5d) > 0.01)))
write_csv2(comp, "surpresas_5d_vs_30d.csv")

# ---- 2. Blanchard pós-2014 com a série híbrida ---------------------------
surp_h <- s30 |> left_join(select(comp, reuniao, surpresa_5d), by = "reuniao") |>
  mutate(surpresa_h = coalesce(surpresa_5d, surpresa))

cambio <- read_delim("bcdata.sgs.3698.csv", delim = ";", col_types = "cc",
                     locale = locale(decimal_mark = ",")) |>
  transmute(mes = as.Date(data, "%d/%m/%Y"), cambio = as.numeric(sub(",", ".", valor)))
risco <- read_csv2("risco_pais_emendado.csv", show_col_types = FALSE) |>
  transmute(mes = as.Date(mes), risco)

mensal <- function(df, col) df |>
  group_by(mes = as.Date(format(data_reuniao, "%Y-%m-01"))) |>
  summarise(x = sum(.data[[col]]), .groups = "drop")

roda_lp <- function(col, H = 12) {
  base <- cambio |> inner_join(risco, by = "mes") |>
    left_join(mensal(surp_h, col), by = "mes") |>
    mutate(x = coalesce(x, 0)) |>
    filter(mes >= as.Date("2009-01-01")) |> arrange(mes) |>
    mutate(lc = 100 * log(cambio), d_c = lc - lag(lc), d_r = risco - lag(risco),
           pos = as.numeric(mes > as.Date("2014-06-01")))
  lapply(0:H, function(h) {
    d <- base |> mutate(y = lead(risco, h) - lag(risco), x_pos = x * pos,
                        l1c = lag(d_c), l2c = lag(d_c, 2), l1r = lag(d_r), l2r = lag(d_r, 2))
    m <- lm(y ~ x + x_pos + pos + l1c + l2c + l1r + l2r, data = d)
    V <- NeweyWest(m, lag = h + 1, prewhite = FALSE)
    tibble(h = h, efeito = coef(m)["x"] + coef(m)["x_pos"],
           ep = sqrt(V["x", "x"] + V["x_pos", "x_pos"] + 2 * V["x", "x_pos"]))
  }) |> bind_rows()
}
res <- bind_rows(roda_lp("surpresa") |> mutate(base = "Focus 30 dias"),
                 roda_lp("surpresa_h") |> mutate(base = "Focus 5 dias úteis a partir de 2021"))
cat("\nEfeito pós-2014 de +0,25 p.p. de surpresa no risco-país (p.b.):\n")
print(res |> filter(h %in% c(0, 3, 6, 12)) |>
        mutate(efeito_025 = round(efeito / 4, 1), ep_025 = round(ep / 4, 1),
               p = round(2 * pnorm(-abs(efeito / ep)), 3)) |>
        select(base, h, efeito_025, ep_025, p), n = Inf)
write_csv2(res, "robustez_5d_lp.csv")

g <- ggplot(res, aes(h, efeito / 4, colour = base, fill = base)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_ribbon(aes(ymin = (efeito - 1.645 * ep) / 4, ymax = (efeito + 1.645 * ep) / 4),
              alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("#C0392B", "#7D3C98"), name = NULL) +
  scale_fill_manual(values = c("#C0392B", "#7D3C98"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 12, 3)) +
  labs(title = "Robustez: o efeito no risco-país depende da base do Focus?",
       subtitle = "Resposta pós-2014 a uma surpresa de +0,25 p.p., em pontos-base. Faixa: IC 90%.",
       x = "Meses após a reunião", y = NULL,
       caption = "A base de 5 dias úteis da Selic por reunião só existe desde mar/2021.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g
ggsave("20_robustez_surpresas_5d.png", g, width = 9, height = 5.5, dpi = 150)
