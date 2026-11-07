# Guia operacional do estudo

O que rodar, em que ordem, o que sai de cada passo, como ler cada saída e como o
argumento se encadeia. No fim há um roteiro para o artigo (Substack ou Medium) e
para o fio no Twitter. O roteiro traz a estrutura e os números, não o texto: o texto
é seu.

---

## 1. A tese em três frases

1. O custo da dívida é decidido por quem empresta. O Banco Central fixa a Selic, mas
   não fixa o prêmio que o mercado cobra para carregar dívida longa.
2. Esse prêmio depende da credibilidade fiscal. Quando o fiscal deixa de reagir à
   dívida, o juro alto passa a piorar o risco, e o BC perde parte da potência.
3. Por isso o fiscal decide em qual regime o BC opera: "passageira" (o fiscal ancora
   e o juro funciona) ou "refém" (o juro sobe, o risco sobe junto, e a dívida migra
   para LFT, que reprecifica na hora).

---

## 2. Preparação (uma vez só)

**R e RStudio.** No console do R:

```r
install.packages(c("readxl","readr","dplyr","tidyr","stringr","ggplot2",
                   "scales","sandwich","lmtest","strucchange","janitor","jsonlite"))
```

**Octave e Dynare** (só se quiser rodar o modelo de novo; as saídas dele já estão
no repositório):

1. Instale o GNU Octave (octave.org) e o Dynare 6 (dynare.org). No Windows, o
   instalador do Dynare indica a pasta, por exemplo `C:/dynare/6.x/matlab`.
2. No Octave: `pkg install -forge statistics` (uma vez) e, em toda sessão,
   `pkg load statistics` e `addpath('C:/dynare/6.x/matlab')`.

**Baixar o repositório.** Pelo botão *Code > Download ZIP* no GitHub, ou
`git clone https://github.com/luizmangass/estudo_juros_pol_fiscal_monetaria`.
Em `00_rodar_tudo.R`, ajuste a linha `raiz <-` para a pasta onde ele ficou.

---

## 3. Ordem de execução

| Etapa | Onde | O que fazer | Tempo |
| --- | --- | --- | --- |
| 1 | RStudio | Abrir `00_rodar_tudo.R` e rodar tudo (Ctrl+Shift+Enter) | 2 a 4 min |
| 2 (opcional) | Octave, na pasta `scripts/modelo` | `rodar_modelo_estimado` | 5 a 10 min |
| 3 (opcional) | RStudio, na pasta `scripts/modelo` | `graficos_modelo_estimado.R` | segundos |
| 4 (só se fez a 2) | RStudio | Rodar `00_rodar_tudo.R` de novo, para a reação estocástica usar as simulações novas | 2 a 4 min |

A etapa 1 sozinha já reproduz tudo, porque as simulações do Dynare estão salvas em
`scripts/modelo/simulacoes_modelo_estimado.csv`.

Para ver cada transformação, abra um script de `scripts/`, rode
`setwd(".../dados")` e vá bloco a bloco: os `View()` mostram a tabela em cada etapa.

A ordem importa porque um script usa o que o anterior gerou:

```
divida_publica.R ─┬─> lft_regressao.R
                  └─> custo_primario_nominal.R
teste_bohn.R
teste_blanchard.R
surpresas_copom.R ──> risco_pais_emendado.csv ─┬─> robustez_surpresas_5d.R
                                               ├─> ancora_fiscal.R
                                               └─> composicao_divida.R ──> base_composicao_mensal.csv
                                                                          └─> composicao_ardl.R ─────┐
duration_divida.R ──> perfil_titulos_ultimo_mes.csv, composicao_mercado_mensal.csv ─────────────────┤
estimar_modelo.R ──> parametros_estimados.csv ──> [Dynare] ──> simulacoes_modelo_estimado.csv ───────┤
                                                                                                    v
                                                                          reacao_mercado_estocastica.R
estimar_modelo.R ──> base_modelo_trimestral.csv ──> bvar_canetada.R (usa também o ARDL e as simulações)
```

---

## 4. A sequência lógica, passo a passo

Cada passo responde a uma pergunta. O passo seguinte só faz sentido depois da
resposta do anterior.

### Passo 1. Quanto custa a dívida e quem define esse custo?

- **Script:** `divida_publica.R`
- **Entradas:** `estoque-da-divida-publica-federal.xlsx` (Anexo 2.1 do RMD) e
  `fatores-de-variacao-da-divida-publica-federal.xlsx` (Anexo 2.9)
- **Saídas:**
  - `01_composicao_indexador.png`: participação de LFT, prefixados, IPCA e câmbio desde 2006.
  - `02_participacao_lft.png`: só a LFT.
  - `03_fatores_juros_x_emissao.png`: o que faz a dívida crescer (juros apropriados contra emissão líquida).
  - `04_custo_implicito_12m.png`: juros apropriados em 12 meses divididos pelo estoque.
  - `composicao_indexador.csv`, `fatores_variacao_*.csv`.
- **Como ler:** a dívida cresce por juros, não por emissão. Mais da metade é
  LFT, que paga a Selic do dia: o corte e a alta passam direto para o custo.
- **Números (set/2026):** DPF de R$ 9.383 bi. LFT 51,6%, IPCA 23,5%, prefixados
  21,1%, câmbio 3,7%. Juros em 12 meses de R$ 1.042 bi. 1 p.p. de Selic custa cerca de
  R$ 48 bi por ano. Custo implícito perto de 12,5%.

### Passo 2. A LFT cresce por causa do juro?

- **Script:** `lft_regressao.R`
- **Saída:** `05_lft_juros_inclinacao.png`, com as quebras de Bai-Perron.
- **Como ler:** a fatia de LFT sobe quando a curva fica cara e inclinada. O
  Tesouro foge do prefixado longo quando o mercado cobra prêmio alto por ele.
- **Números:** a LFT foi de cerca de 18% (2014) para 52%. Quebras em out/2014,
  set/2019, set/2021 e mar/2024. A subida não começa com os cortes de Tombini:
  começa quando o fiscal piora, em 2014.

### Passo 3. Juros contra primário

- **Script:** `custo_primario_nominal.R`
- **Saída:** `06_custo_primario_nominal.png`.
- **Como ler:** o déficit nominal é quase todo juros. O primário é pequeno perto
  da conta de juros.
- **Números:** juros nominais perto de 9% do PIB; primário com déficit de cerca de 0,5%.

### Passo 4. O fiscal reage à dívida? (teste de Bohn)

- **Script:** `teste_bohn.R`
- **Saídas:**
  - `09_bohn_janelas_moveis.png`: o coeficiente em janelas móveis.
  - `10_bohn_dispersao.png`: primário contra dívida, antes e depois da quebra.
- **Como ler:** um coeficiente positivo significa que o governo faz mais primário
  quando a dívida sobe, o que é a condição de solvência. Se ele chega a zero, a dívida
  deixa de ter âncora.
- **Números:** a quebra é de jun/2014. Antes, +0,074 (p < 0,001). Depois, −0,027
  (não significativo).

### Passo 5. O juro alto piora o risco? (Blanchard, 2004)

- **Scripts:** `teste_blanchard.R` (versão ingênua), `surpresas_copom.R` (versão
  identificada) e `robustez_surpresas_5d.R` (robustez).
- **Saídas:**
  - `11_selic_embi_cambio.png` e `12_blanchard_projecoes_locais.png`: versão ingênua, só para comparação. Ela tem viés, porque o BC sobe o juro justamente quando o risco já está subindo.
  - `13_surpresas_copom.png`: a surpresa de cada reunião (decisão menos a mediana do Focus).
  - `14_blanchard_surpresas.png`: resposta do risco-país à surpresa, antes e depois de 2014. É o gráfico central do estudo.
  - `15_risco_pais_emendado.png`: EMBI+ até jul/2024, emendado com o CDS.
  - `20_robustez_surpresas_5d.png`: o mesmo teste com o Focus de 5 dias úteis.
- **Como ler:** a surpresa é a parte da decisão que o mercado não esperava, então
  não carrega a reação do BC ao risco. Se o risco sobe depois de uma surpresa de alta,
  o juro mais alto está piorando a percepção de solvência.
- **Números:** 143 reuniões, 25 com surpresa. Depois de 2014, +0,25 p.p. de surpresa
  leva a +34 pb de risco em 3 meses (p = 0,03) e +31 pb em 6 meses. Com o Focus de 5
  dias: +36 pb (p = 0,03) e +34 pb (p = 0,01). Antes de 2014, nada significativo.

### Passo 6. Em que regime estamos? (âncora fiscal)

- **Script:** `ancora_fiscal.R`
- **Saídas:** `16_ancora_tres_medidas.png`, `ancora_por_periodo.csv`, `juro_neutro_5a5a.csv`.
- **Como ler:** o gráfico compara três medidas. A primeira é a postura monetária
  (juro real menos o neutro). A segunda é o impulso fiscal estrutural. A terceira
  é a desancoragem das expectativas (Focus 12 meses menos a meta). O neutro "de
  mercado" é o forward 5 anos daqui a 5 anos das NTN-B, o juro real que o mercado
  cobra no longo prazo.
- **Números (média por período):**

| Período | Juro real | Neutro de mercado | Postura (vs mercado) | Desancoragem | Risco (pb) |
| --- | --- | --- | --- | --- | --- |
| 2010-14T2 | 4,1 | 5,7 | −1,6 | 1,0 | 204 |
| 2014T3-2016 | 6,9 | 6,4 | +0,5 | 1,5 | 341 |
| 2017-2019 (teto) | 3,3 | 5,0 | −1,7 | −0,3 | 261 |
| 2020-2021 | 0,0 | 4,4 | −4,4 | 0,1 | 316 |
| 2022 | 6,9 | 5,9 | +1,0 | 2,0 | 298 |
| 2023-2026 | 8,6 | 6,5 | +2,1 | 1,2 | 228 |

- **Leitura:** desde 2023 o juro real está 2 p.p. acima do neutro de mercado e a
  desancoragem não zera: 2,8 em jan/25, 1,0 em dez/25 e 1,5 em set/26. Isso é um
  regime intermediário, "refém". O BC ainda consegue apertar, mas precisa de mais
  juro para o mesmo resultado. Não é dominância fiscal plena. O neutro de mercado
  hoje está perto de 7,4%, contra 5% da IFI.

### Passo 7. O que move a composição, e o que aconteceu nos ciclos de corte

- **Script:** `composicao_divida.R`
- **Saídas:**
  - `21_o_que_move_a_composicao.png` e `regressao_composicao.csv`: mudança da participação nos 12 meses seguintes contra as condições de hoje, com regressores padronizados.
  - `22_ciclos_de_corte.png` e `evento_ciclos_corte.csv`: os ciclos de 2011, 2016 e 2023.
- **Como ler a regressão:** cada coeficiente diz quantos p.p. a participação muda
  em 12 meses quando a variável sobe 1 desvio-padrão.

| | LFT | Prefixados | IPCA |
| --- | --- | --- | --- |
| Primário estrutural | **−2,9** | **+1,6** | **+1,8** |
| Juro real | **+1,6** | +0,4 | **−1,1** |
| Desancoragem | +0,8 | **−1,0** | −0,1 |
| Risco-país | +0,8 | −0,5 | +0,1 |

  Em negrito, p < 0,05. O primário estrutural é a variável que mais pesa: fiscal
  melhor tira o Tesouro da LFT.

- **Ciclos de corte, 12 meses depois:**

| | Selic | Pré 5 anos | Câmbio | Risco | Fatia LFT |
| --- | --- | --- | --- | --- | --- |
| Tombini, ago/2011 | −5,00 | −3,9 | +30% (+50% em 24m) | +24 (+94 em 24m) | −7,8 |
| Ilan, out/2016 | −6,75 | −2,0 | −2% | −76 | +5,1 |
| Campos Neto, ago/2023 | −3,25 | **+1,5** | +16% | +32 | +5,7 |

  Em 2023 a Selic caiu e o juro longo subiu (o real de 5 anos subiu 1,2), porque o
  corte veio sem âncora fiscal. Em 2016 o corte veio com o teto recém-aprovado, e o
  risco caiu.

### Passo 8. A dívida a preço de mercado: composição e duration

- **Script:** `duration_divida.R`
- **Entradas:** `estoquedpf.csv` (estoque por título e vencimento) e
  `precotaxatesourodireto.csv` (curva do dia).
- **O que faz:** o estoque do Tesouro é contábil, carregado pela taxa da emissão.
  O script marca cada título a mercado: valor = quantidade × VNA × cotação, com a
  cotação calculada pela curva do Tesouro Direto no último dia útil do mês. Depois
  calcula a duration de Macaulay e a modificada, a convexidade e o DV01 título a
  título. A LFT tem duration zero, porque reprecifica todo dia.
- **Saídas:**
  - `25_composicao_mercado.png` e `composicao_mercado_mensal.csv`: participação contábil contra a de mercado.
  - `23_duration_divida.png` e `duration_mensal.csv`: duration, convexidade, DV01 e a fatia que vence em 12 meses, por indexador.
  - `24_choque_curva.png` e `choque_curva_marcacao.csv`: perda de marcação num choque como o de 2023 e num choque duas vezes maior.
  - `perfil_titulos_ultimo_mes.csv`: título a título, no último mês.
- **Números (ago/2026, a mercado):**

| | Estoque (R$ bi) | Duration modificada | Vence em 12m | DV01 (R$ mi por pb) | Mercado/contábil |
| --- | --- | --- | --- | --- | --- |
| Prefixado | 1.824 | 2,13 | 24% | 389 | 0,97 |
| IPCA | 1.933 | 6,36 | 7% | 1.228 | 0,91 |
| LFT | 4.904 | 0 | 15% | 0 | 1,00 |
| Total doméstico | 8.661 | 1,87 | 15% | 1.618 | |

  Choque de 2023 (pré +1,5 p.p., real +1,2 p.p.): perda de marcação de cerca de
  R$ 193 bi (R$ 56 bi em prefixados e R$ 136 bi em IPCA). Choque dobrado: cerca de
  R$ 381 bi. Cobertura de 95,7% do estoque.
- **Como ler:** a duration total é baixa (1,9) porque mais da metade é LFT. Isso
  protege o mercado de perdas de marcação, mas joga o risco de juro inteiro no
  Tesouro. É o seguro que o Tesouro dá a quem empresta, e ele custa caro quando o
  juro sobe.

### Passo 9. O modelo: o que acontece com uma "canetada"

- **Scripts:** `estimar_modelo.R`, depois `scripts/modelo/rodar_modelo_estimado.m`
  (Octave/Dynare) e `graficos_modelo_estimado.R`.
- **O modelo:** economia aberta pequena com curva IS, curva de Phillips, câmbio
  (UIP), regra de Taylor, prêmio de risco, prêmio de prazo, juro longo, custo médio
  da dívida (52% reprecifica na hora, que é a LFT; o resto, em 16 trimestres) e regra
  fiscal. A "canetada" é a Selic 1 ou 2 p.p. abaixo da regra por 4 trimestres.
- **O que vem dos dados:** suavização da Selic, persistências e choques
  (`parametros_estimados.csv`). A resposta do risco ao juro real é calibrada para
  reproduzir o passo 5 (`19_validacao_risco.png`).
- **Saídas:**
  - `17_modelo_estimado_corte_1pp.png` e `18_modelo_estimado_corte_2pp.png`: as trajetórias, com bandas de 1.000 simulações.
  - `resultados_modelo_estimado.csv`, `calibracao_chi_r.csv`, `irf_risco_modelo.csv` e `simulacoes_modelo_estimado.csv` (as 1.000 trajetórias completas).
- **Achado central:** com a reação fiscal estimada depois de 2014 (0,007 contra
  0,050 antes), o modelo não tem trajetória estável. Para estabilizar com a
  sensibilidade de risco de hoje, a reação fiscal precisaria ser de 0,076, mais
  alta que a de antes de 2014. Esse é o "ajuste que o mercado precisa acreditar que
  virá".
- **Canetada de 1 p.p.:** depois de 2 anos, a inflação está +1,6 p.p. no regime
  pós-2014 (contra +1,0 no pré-2014). A Selic depois tem de subir +1,2 (contra +0,8)
  para corrigir. A dívida fica +0,7 p.p. do PIB (contra +0,6).
- Os gráficos 07 e 08 são a primeira versão do modelo, calibrada à mão. Ficam
  só para registro.

### Passo 10. A composição como série temporal (ARDL)

- **Script:** `composicao_ardl.R`
- **Por que existe:** a regressão do passo 7 é estática (sem a fatia defasada),
  não tem câmbio e cada indexador é estimado separado. Para simular trajetórias,
  é preciso um modelo de série temporal.
- **O modelo:** ARDL trimestral (2009-2026, 70 trimestres) em razões
  logarítmicas, y1 = log(LFT/pré) e y2 = log(IPCA/pré), em forma de correção.
  Os regressores são:
  - a fatia defasada em nível;
  - 2 defasagens das variações;
  - risco, juro real, desancoragem, primário estrutural, inclinação e
    depreciação do câmbio em 4 trimestres, todos defasados 1 trimestre.

  A razão logarítmica garante que as três fatias somem o total e fiquem entre 0
  e 1, sem precisar impor isso depois.
- **Escolhas e testes:**
  - É trimestral porque, no mensal, os vencimentos concentrados (LTN em
    jan/abr/jul/out, NTN-B em mai/ago) deixam os resíduos muito autocorrelacionados.
  - p = 2 é o menor número de defasagens com resíduo branco nas duas equações
    (Ljung-Box com 8 defasagens: p = 0,21 e 0,56; ver `ardl_selecao.csv`).
  - A maior raiz é 0,994: o sistema é estável, mas um choque na composição dura
    anos. Os erros são Newey-West.
- **Saídas:**
  - `29_ardl_multiplicadores.png` e `ardl_multiplicadores.csv`: mudança das fatias quando cada variável sobe 1 desvio-padrão por 4 trimestres.
  - `ardl_coeficientes.csv`, `ardl_vcov.csv`, `ardl_residuos.csv` e `ardl_ponto_partida.csv`: insumos da simulação.
- **Números (8 trimestres depois de 1 dp por 4 trimestres, p.p.):**

| Variável que sobe | LFT | Prefixados | IPCA |
| --- | --- | --- | --- |
| Primário estrutural (+1,4 p.p. do PIB) | **−4,1** | +2,5 | +1,6 |
| Juro real (+3,0 p.p.) | +3,1 | −1,6 | −1,6 |
| Inclinação (+1,9 p.p.) | +1,8 | −1,0 | −0,8 |
| Risco (+67 pb) | +1,2 | −0,7 | −0,5 |
| Desancoragem (+0,86 p.p.) | +0,4 | −0,8 | +0,4 |
| Depreciação do câmbio (+16% em 4 trimestres) | −0,7 | +0,5 | +0,2 |

  Os sinais são os da regressão estática, e o primário estrutural continua sendo
  o que mais pesa (p < 0,001 na equação da LFT). O câmbio entra com sinal pequeno
  e não significativo (p = 0,35).

### Passo 11. Reação estocástica do mercado e da dívida

- **Script:** `reacao_mercado_estocastica.R`
- **O que faz:** pega as 1.000 trajetórias do modelo e aplica três peças:
  1. **Composição:** o ARDL do passo 10, com duas fontes de incerteza a mais.
     Em cada simulação, os coeficientes são sorteados da distribuição estimada
     (descartando sorteios instáveis), e soma-se um resíduo histórico sorteado
     aos pares, porque as duas equações têm resíduos com correlação de 0,81.
  2. **Marcação a mercado:** a duration título a título do passo 8.
  3. **Custo extra de juros:** o perfil de vencimentos. A LFT reprecifica na
     hora; prefixados e IPCA só quando são rolados.
- **Como as variáveis do modelo viram regressores:**
  - risco = rp × 100;
  - juro real = Selic − inflação média esperada nos 4 trimestres seguintes;
  - desancoragem = essa inflação esperada;
  - primário = 4 × s;
  - inclinação = juro longo − Selic;
  - câmbio = variação do câmbio nominal em 4 trimestres.
- **Saídas:**
  - `26_composicao_estocastica.png`: as fatias de LFT, prefixados e IPCA.
  - `27_custo_tesouro_estocastico.png`: o custo extra acumulado.
  - `28_marcacao_estocastica.png`: o ganho ou a perda de marcação em 1 e 2 anos.
  - `reacao_mercado_resumo.csv` e `reacao_mercado_trajetorias.csv`.
- **Números, canetada de 1 p.p., regime pós-2014** (central; entre parênteses, a
  faixa de 90%):

| | 2 anos | 6 anos |
| --- | --- | --- |
| Fatia de LFT | −5,6 p.p. (−21 a +10) | +3,2 p.p. (−21 a +20) |
| Fatia de IPCA | +6,3 p.p. (−4 a +18) | −1,0 p.p. |
| Fatia de prefixados | −0,7 p.p. | −2,2 p.p. |
| Custo extra acumulado | −R$ 20 bi (−74 a +32) | +R$ 1 bi (−162 a +156) |

- **Como ler:**
  - **O custo é robusto:** a canetada economiza cerca de R$ 50 bi no primeiro ano,
    pela LFT. A correção da Selic devolve essa economia em uns 3 anos. No pós-2014,
    a conta da LFT fica positiva a partir do terceiro ano.
  - **A composição não é robusta:** a faixa de 90% vai de −21 a +10 p.p. e o
    sinal da LFT muda conforme a especificação. A regressão estática dizia +3 p.p.
    em 2 anos; o ARDL diz −5,6. O passo 12 mostra por quê: o resultado depende de
    quanto o risco e a inflação reagem à canetada, e o modelo exagera os dois.
    Eu não usaria um número de composição no texto.

### Passo 12. Contraprova: a mesma canetada num VAR bayesiano

- **Script:** `bvar_canetada.R`
- **Por que existe:** o modelo do Dynare impõe estrutura e calibra a Phillips, a
  IS e o câmbio. O VAR pergunta o que os dados dizem sem essa estrutura.
- **O modelo:**
  - VAR(2) trimestral em nível, 2010-2026 (66 trimestres), com primário
    estrutural, hiato, IPCA 12m, Selic, câmbio, risco e pré 5 anos.
  - Prior de Minnesota (passeio aleatório, aperto 0,2) e 1.000 sorteios da
    posterior.
  - Identificação recursiva: fiscal, atividade e inflação não reagem à Selic
    no mesmo trimestre; câmbio, risco e juro longo reagem.
  - A canetada é a sequência de choques que deixa a Selic 1 p.p. abaixo da base
    por 4 trimestres (previsão condicional).
- **Saídas:**
  - `30_bvar_vs_dynare.png` e `bvar_vs_dynare.csv`: as duas respostas lado a lado.
  - `bvar_canetada.csv`.
  - `bvar_composicao.csv`: o ARDL alimentado pela macro do VAR.
- **Números (média dos trimestres 5 a 8, canetada de 1 p.p.):**

| | Dynare pós-2014 | VAR |
| --- | --- | --- |
| IPCA 12m | +2,4 p.p. | +0,55 p.p. |
| Selic (correção) | +0,7 p.p. | −0,4 p.p. |
| Risco-país | −181 pb | +6 pb |
| Câmbio | +1,4% | +1,0% |
| Juro longo prefixado | −0,1 p.p. | 0,0 p.p. |

  Com a macro do VAR, a fatia de LFT cai 1,1 p.p. em 2 anos (faixa de 80%: −1,5
  a −0,7) e a de IPCA sobe 1,5.
- **Como ler:**
  - **Inflação:** os dois concordam no sinal (ela sobe e o câmbio deprecia), mas
    o modelo põe cerca de 4 vezes mais inflação que o VAR.
  - **Risco:** no modelo ele cai quase 200 pb. É o canal das surpresas (passo 5)
    aplicado ao contrário, de forma simétrica. No VAR, o risco não se mexe.
  - **Conclusão para o texto:** o modelo serve para mostrar o mecanismo e a
    diferença entre os regimes, não para dar o tamanho. O VAR diz que, nos dados
    médios de 2010 a 2026, um corte de 1 p.p. é "barato" no curto prazo. A conta
    aparece em outro lugar: na inflação que fica e na Selic que precisa voltar.
- **Limite:** o VAR também é linear e não separa uma canetada da reação normal do
  BC (crítica de Lucas). É uma contraprova, não um substituto.

---

## 5. Dicionário das bases geradas (`resultados/`)

| Arquivo | Uma linha é | Colunas principais |
| --- | --- | --- |
| composicao_indexador.csv | mês × indexador (contábil) | participação |
| composicao_mercado_mensal.csv | mês × indexador | contábil, mercado, part_contabil, part_mercado |
| duration_mensal.csv | mês × indexador | mac, mod, conv, prazo_venc, vence_12m, dv01_R_mi |
| perfil_titulos_ultimo_mes.csv | título | valor contábil e de mercado, taxa, mac, mod, conv, anos |
| choque_curva_marcacao.csv | cenário × indexador | perda_R_bi |
| fatores_variacao_*.csv | mês ou ano × fator | R$ bi |
| surpresas_copom.csv | reunião | decidida, esperada, surpresa |
| surpresas_5d_vs_30d.csv | reunião (2021+) | surpresa nas duas bases |
| risco_pais_emendado.csv | mês | risco (EMBI+ até jul/24, CDS convertido depois) |
| robustez_5d_lp.csv | horizonte × base | efeito, erro-padrão |
| juro_neutro_5a5a.csv | dia | forward real 5a5a |
| ancora_base_trimestral.csv / ancora_por_periodo.csv | trimestre / período | juro real, neutro, postura, impulso, desancoragem, risco |
| base_composicao_mensal.csv | mês | participações e todas as variáveis da regressão |
| regressao_composicao.csv | indexador × variável | coef, ep, p, r2 |
| evento_ciclos_corte.csv | ciclo × variável × horizonte | variação |
| parametros_estimados.csv | parâmetro × regime | valor, erro-padrão |
| base_modelo_trimestral.csv | trimestre | séries usadas na estimação |
| reacao_mercado_resumo.csv | regime × corte × medida | p05, p50, p95, prob_positivo |
| reacao_mercado_trajetorias.csv | regime × corte × trimestre × variável | central e percentis |
| ardl_selecao.csv | defasagens × equação | BIC, R², Ljung-Box |
| ardl_coeficientes.csv | equação × termo | coef, ep, p, r2, ljung_box_p |
| ardl_multiplicadores.csv | variável × trimestre | mudança das fatias de LFT, PRE e IPCA |
| bvar_canetada.csv | variável × trimestre | central, p10, p50, p90 |
| bvar_vs_dynare.csv | fonte × variável × trimestre | central, p10, p90 |
| bvar_composicao.csv | indexador × trimestre | central, p10, p90 |

---

## 6. Roteiro do artigo (Substack ou Medium)

Uma estrutura possível, com o gráfico e o número que sustentam cada parte.

1. **Abertura: o debate errado.** "Fiscal e monetário" é tratado como duas
   conversas. A pergunta é quem define o custo da dívida. *Gráfico 04. Número:
   1 p.p. de Selic = R$ 48 bi.*
2. **A dívida que reprecifica todo dia.** Metade é LFT; a duration total é 1,9.
   Por que o Tesouro chegou aqui. *Gráficos 01, 05 e 23. Números: LFT de 18% para
   52%; DV01; 15% vence em 12 meses.*
3. **Quem empresta decide.** O juro longo e o neutro de mercado. O BC fixa o curto,
   o mercado fixa o resto. *Gráfico 16. Número: neutro de mercado perto de 7,4%,
   contra 5% da IFI.*
4. **O fiscal parou de reagir em 2014.** O teste de Bohn. *Gráfico 09. Números:
   +0,074 antes, −0,027 depois.*
5. **Quando o juro alto piora o risco.** Surpresas do Copom e Blanchard.
   *Gráficos 13 e 14 (e o 20 como robustez). Número: +0,25 p.p. de surpresa dá
   +34 pb de risco depois de 2014.*
6. **Três cortes, três finais.** Tombini, Ilan e Campos Neto. *Gráfico 22 e a
   tabela do passo 7.*
7. **E se viesse uma canetada?** O modelo, os dois regimes e a reação estocástica.
   *Gráficos 18, 27 e 30. Números dos passos 9, 11 e 12. Deixe a composição de fora: não é robusta.*
8. **Onde estamos desde 2023.** O regime "refém", não a dominância plena. O que
   tiraria o país daqui: primário estrutural. Esse é o coeficiente que mais pesa
   na regressão da composição (−2,9 na LFT).
9. **Ressalvas e dados.** Link para o repositório.

## 7. Fio no Twitter

Um tuíte por ideia, cada um com um gráfico. A redação é sua; aqui vão o ponto e o
número de cada tuíte.

| # | Ponto | Gráfico | Número |
| --- | --- | --- | --- |
| 1 | Quem decide o custo da dívida não é o BC | 04 | 1 p.p. = R$ 48 bi/ano |
| 2 | Metade da dívida paga a Selic do dia | 01 ou 25 | LFT 52-54% |
| 3 | Duration de 1,9: o Tesouro segura o risco de juro | 23 | DV01 de R$ 1,6 bi por pb |
| 4 | O fiscal parou de reagir à dívida em 2014 | 09 | +0,074 → −0,027 |
| 5 | Depois de 2014, juro mais alto piora o risco | 14 | +34 pb por 0,25 p.p. |
| 6 | O mercado já cobra 7,4% real no longo prazo | 16 | contra 5% da IFI |
| 7 | 2023: a Selic caiu e o juro longo subiu | 22 | pré 5a +1,5 p.p. |
| 8 | Fiscal melhor tira o Tesouro da LFT | 21 | −2,9 p.p./ano por 1 dp |
| 9 | Canetada: economia agora, conta depois | 27 | −R$ 50 bi no ano 1; devolvido em 3 anos |
| 10 | O corte volta como inflação | 30 | IPCA +0,5 (VAR) a +2,4 p.p. (modelo) |
| 11 | Estamos em regime "refém", não em dominância plena | 16 | postura +2,1; desancoragem 1,5 |
| 12 | Dados e códigos abertos | link do repositório | |

---

## 8. Limitações (para ter na ponta da língua)

- **Surpresas:** só 25 das 143 reuniões tiveram surpresa, e o efeito depende de
  poucas reuniões. A robustez com 5 dias úteis só cobre 2021 em diante.
- **Risco-país:** a série é emendada: EMBI+ até jul/2024 e CDS convertido depois
  (R² de 0,83 na ponte).
- **Modelo:** as curvas IS e de Phillips e a reação à inflação são calibradas,
  não estimadas, porque as regressões deram sinal trocado por simultaneidade.
- **Simetria:** o modelo é linear, então trata corte e alta de forma simétrica, e
  os dados de 2011 e 2023 sugerem que não é assim (ver passo 10).
- **Composição:** o ARDL é um só para todo o período e não separa regimes. A
  resposta a uma canetada muda de sinal conforme a macro que o alimenta (passos
  11 e 12), por isso fica como exercício, não como resultado.
- **Tamanho da resposta macro:** o modelo põe cerca de 4 vezes mais inflação que o
  VAR e uma queda de risco que o VAR não vê. Use o modelo para o mecanismo e o VAR
  para a ordem de grandeza.
- **Marcação a mercado:** usa a curva do Tesouro Direto (taxa média de compra e
  venda), não a da ANBIMA. Educa+ e Renda+ (NTN-B1) ficam no valor contábil.

## 9. Como atualizar os dados

Baixe as mesmas bases (a tabela de fontes está no README), salve em `dados/` com os
mesmos nomes e rode a etapa 1. Para as séries do BCB, `scripts/baixar_dados_bcb.R`
baixa tudo de uma vez.
