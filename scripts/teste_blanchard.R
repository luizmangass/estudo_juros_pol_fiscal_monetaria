# =====================================================================
# Teste de Blanchard (2004) para o Brasil: subir juros aprecia ou
# deprecia o câmbio?
#
# Lógica: em regime normal, Selic maior -> câmbio aprecia (entra capital).
# Sob dominância fiscal, Selic maior -> dívida mais cara -> risco de
# calote sobe (EMBI) -> câmbio DEPRECIA -> inflação sobe.
#
# Método: projeções locais (Jordà, 2005), separando antes e depois da
# quebra fiscal de jun/2014 encontrada no teste de Bohn.
#   y_{t+h} - y_{t-1} = a_h + b_h dSelic_t + c_h (dSelic_t x pós2014)
#                       + controles defasados + e
#   b_h      = efeito antes de 2014
#   b_h+c_h  = efeito depois de 2014
#
# Entradas: bcdata.sgs.4189 (Selic), 3698 (câmbio), 4513 (DLSP),
#           13522 (IPCA 12m), embi.csv (Ipeadata, EMBI+ Risco-Brasil)
#
# CUIDADO: dSelic não é um choque exógeno. O BC sobe juros justamente
# quando câmbio e risco pioram, o que puxa b_h para cima. Os controles
# defasados atenuam, mas não resolvem. Ver seção 5.
# =====================================================================
library(dplyr); library(tidyr); library(readr)
library(ggplot2); library(scales)
library(sandwich); library(lmtest)

le_sgs <- function(codigo, nome) {
  read_delim(sprintf("bcdata.sgs.%d.csv", codigo), delim = ";",
             col_types = "cc", locale = locale(decimal_mark = ",")) |>
    transmute(mes = as.Date(data, "%d/%m/%Y"),
              !!nome := as.numeric(sub(",", ".", valor, fixed = TRUE)))
}

# ---- 1. Dados mensais ---------------------------------------------------
embi <- read_csv("embi.csv", col_types = "cc", col_names = c("data", "embi"),
                 skip = 1) |>
  mutate(dia = as.Date(data, "%d/%m/%Y"), embi = as.numeric(embi)) |>
  filter(!is.na(embi)) |>
  group_by(mes = as.Date(format(dia, "%Y-%m-01"))) |>
  summarise(embi = mean(embi), .groups = "drop")

base <- le_sgs(4189, "selic") |>
  inner_join(le_sgs(3698, "cambio"), by = "mes") |>
  inner_join(le_sgs(13522, "ipca12"), by = "mes") |>
  inner_join(le_sgs(4513, "dlsp"), by = "mes") |>
  inner_join(embi, by = "mes") |>
  filter(mes >= as.Date("2003-01-01")) |>
  arrange(mes) |>
  mutate(lcambio  = 100 * log(cambio),
         juro_real = selic - ipca12,
         d_selic  = selic - lag(selic),
         d_cambio = lcambio - lag(lcambio),
         d_embi   = embi - lag(embi),
         pos2014  = as.numeric(mes > as.Date("2014-06-01")))
range(base$mes)
View(base)

# ---- 2. Gráfico descritivo ----------------------------------------------
g0 <- base |>
  select(mes, `Selic (% a.a.)` = selic, `EMBI+ Brasil (p.b.)` = embi,
         `Câmbio (R$/US$)` = cambio, `DLSP (% PIB)` = dlsp) |>
  pivot_longer(-mes) |>
  ggplot(aes(mes, value)) +
  annotate("rect", xmin = as.Date("2014-06-01"), xmax = max(base$mes),
           ymin = -Inf, ymax = Inf, fill = "grey50", alpha = 0.10) +
  geom_line(colour = "#C0392B", linewidth = 0.8) +
  facet_wrap(~name, ncol = 2, scales = "free_y") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(title = "Selic, risco-país, câmbio e dívida",
       subtitle = "Faixa cinza: após a quebra da reação fiscal (jun/2014, teste de Bohn)",
       x = NULL, y = NULL,
       caption = "Fontes: BCB (SGS 4189, 3698, 4513) e Ipeadata/JP Morgan (EMBI+)") +
  theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
g0
ggsave("11_selic_embi_cambio.png", g0, width = 10, height = 7, dpi = 150)

# ---- 3. Projeções locais --------------------------------------------------
H <- 12
lp <- function(var) {
  lapply(0:H, function(h) {
    d <- base |>
      mutate(y = lead(.data[[var]], h) - lag(.data[[var]]),
             dsel_pos = d_selic * pos2014,
             l1_dc = lag(d_cambio), l2_dc = lag(d_cambio, 2),
             l1_de = lag(d_embi),   l2_de = lag(d_embi, 2),
             l1_ds = lag(d_selic),  l2_ds = lag(d_selic, 2),
             l1_dsp = lag(d_selic) * pos2014)
    m <- lm(y ~ d_selic + dsel_pos + pos2014 + l1_dc + l2_dc +
              l1_de + l2_de + l1_ds + l2_ds + l1_dsp, data = d)
    V <- NeweyWest(m, lag = h + 1, prewhite = FALSE)
    b  <- coef(m)["d_selic"];  vb <- V["d_selic", "d_selic"]
    bc <- b + coef(m)["dsel_pos"]
    vbc <- vb + V["dsel_pos", "dsel_pos"] + 2 * V["d_selic", "dsel_pos"]
    tibble(var = var, h = h,
           periodo = c("Até jun/2014", "Depois de jun/2014"),
           efeito = c(b, bc), ep = sqrt(c(vb, vbc)))
  }) |> bind_rows()
}

res <- bind_rows(lp("lcambio"), lp("embi")) |>
  mutate(var = recode(var, lcambio = "Câmbio (%, + = depreciação)",
                      embi = "EMBI+ (pontos-base)"))
print(filter(res, h %in% c(0, 3, 6, 12)), n = Inf)

g1 <- ggplot(res, aes(h, efeito, colour = periodo, fill = periodo)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_ribbon(aes(ymin = efeito - 1.645 * ep, ymax = efeito + 1.645 * ep),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~var, scales = "free_y") +
  scale_colour_manual(values = c("#2E86C1", "#C0392B"), name = NULL) +
  scale_fill_manual(values = c("#2E86C1", "#C0392B"), name = NULL) +
  scale_x_continuous(breaks = seq(0, H, 3)) +
  labs(title = "Resposta a +1 p.p. na Selic: câmbio e risco-país",
       subtitle = "Projeções locais, 2003 a jul/2024. Faixa: IC 90% (Newey-West).",
       x = "Meses após a alta", y = NULL,
       caption = paste("Controles: 2 defasagens de câmbio, EMBI e Selic.",
                       "Selic não é choque exógeno: ver ressalva no script.")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
g1
ggsave("12_blanchard_projecoes_locais.png", g1, width = 10, height = 5.5, dpi = 150)

# ---- 4. Versão Blanchard em nível: o risco depende da dívida e do juro? ---
# EMBI_t = a + b*dívida_{t-1} + c*juro_real_{t-1} + d*(dívida x juro real) + e
# d > 0: juro mais alto aumenta mais o risco quando a dívida é maior.
m_nivel <- lm(embi ~ lag(dlsp) * lag(juro_real), data = base)
coeftest(m_nivel, vcov = NeweyWest(m_nivel, lag = 12, prewhite = FALSE))

# ---- 5. Próximo passo de identificação -----------------------------------
# Trocar d_selic pela SURPRESA de política monetária:
#   surpresa_t = Selic decidida no Copom - Selic esperada no Focus
#   (véspera da reunião), ou a variação do DI1 de 1 mês no dia do Copom.
# Com isso, a resposta do câmbio deixa de estar contaminada pela reação
# do BC ao próprio câmbio.
