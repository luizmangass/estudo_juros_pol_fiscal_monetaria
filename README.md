# estudo_juros_pol_fiscal_monetaria

O custo da dívida é definido por quem empresta, não por quem se endivida. O Banco Central controla só a ponta curta da curva; o resto depende da credibilidade fiscal. Por isso o fiscal decide em qual regime o BC opera: quando o fiscal deixa de estabilizar a dívida, a política monetária vira refém.

Dados, códigos e gráficos do artigo "Dinâmica do ciclo de juros e inflação:
por que a política monetária é refém, ou passageira, da política fiscal".

## Como rodar

1. Instale os pacotes (lista no topo de `00_rodar_tudo.R`).
2. Abra `00_rodar_tudo.R` no RStudio e rode tudo. Ele roda os 8 scripts em ordem,
   joga os gráficos em `graficos/` e as bases geradas em `resultados/`.
3. Para ver a transformação passo a passo, abra um script de `scripts/`, faça
   `setwd(".../estudo_juros_pol_fiscal_monetaria/dados")` e rode bloco a bloco (os `View()` mostram cada etapa).
4. Os modelos (Dynare) rodam à parte, no MATLAB ou Octave, dentro de `scripts/modelo/`:
   - versão estimada (principal): `rodar_modelo_estimado.m`, depois `graficos_modelo_estimado.R`;
   - versão ilustrativa (primeira versão): `rodar_modelo.m`, depois `graficos_modelo.R`.

A ordem importa: `divida_publica.R` gera bases que `lft_regressao.R` e
`custo_primario_nominal.R` usam, e `surpresas_copom.R` gera o risco-país
emendado que `ancora_fiscal.R` usa.

## Bases de dados usadas

| Arquivo em dados/ | O que é | Fonte | Usado em |
| --- | --- | --- | --- |
| estoque-da-divida-publica-federal.xlsx | Estoque da DPF por título, RMD Anexo 2.1 | Tesouro Nacional | divida_publica.R |
| fatores-de-variacao-da-divida-publica-federal.xlsx | Fatores de variação da DPF, RMD Anexo 2.9 | Tesouro Nacional | divida_publica.R, custo_primario_nominal.R |
| precotaxatesourodireto.csv | Taxas e preços diários do Tesouro Direto (inclui o juro neutro de mercado, forward 5a5a das NTN-B) | Tesouro Transparente | lft_regressao.R, ancora_fiscal.R, estimar_modelo.R |
| receita-orcamentaria-da-uniao.csv | Receita orçamentária da União (linha de refinanciamento) | Tesouro Transparente | custo_primario_nominal.R |
| bcdata.sgs.5793.csv | NFSP primário, % PIB, 12 meses | BCB, SGS | custo_primario_nominal.R, teste_bohn.R |
| bcdata.sgs.5727.csv | NFSP nominal, % PIB, 12 meses | BCB, SGS | custo_primario_nominal.R |
| bcdata.sgs.4513.csv | Dívida líquida do setor público, % PIB | BCB, SGS | teste_bohn.R, teste_blanchard.R |
| bcdata.sgs.13762.csv | Dívida bruta do governo geral, % PIB | BCB, SGS | teste_bohn.R |
| bcdata.sgs.24364.csv | IBC-Br dessazonalizado | BCB, SGS | teste_bohn.R |
| bcdata.sgs.7870.csv | NFSP primário do INSS, % PIB, 12 meses | BCB, SGS | Só no texto (déficit da Previdência) |
| bcdata.sgs.432_2008_2017.csv e _2018_2026.csv | Meta Selic diária | BCB, SGS | surpresas_copom.R, ancora_fiscal.R |
| bcdata.sgs.4189.csv | Selic acumulada no mês, anualizada | BCB, SGS | teste_blanchard.R |
| bcdata.sgs.3698.csv | Câmbio R$/US$, média mensal | BCB, SGS | teste_blanchard.R, surpresas_copom.R |
| bcdata.sgs.13522.csv | IPCA acumulado em 12 meses | BCB, SGS | teste_blanchard.R |
| embi.csv | EMBI+ Risco-Brasil, diário, até jul/2024 | Ipeadata / JP Morgan | teste_blanchard.R, surpresas_copom.R |
| cds.csv | CDS Brasil 5 anos USD, mensal | Investing.com | surpresas_copom.R |
| focus_selic_2008_2017.xls e _2017_2026.xls | Selic esperada por reunião do Copom, mediana | BCB, Focus | surpresas_copom.R |
| focus_ipca_2009_2017.xls e _2017_2026.xls | IPCA mensal esperado, mediana | BCB, Focus | ancora_fiscal.R |
| resultado-fiscal-estrutural.xlsx | Resultado primário estrutural | SPE / Ministério da Fazenda | ancora_fiscal.R |
| estimativas-do-hiato-do-produto-ifi.xlsx | Hiato do produto | IFI / Senado | ancora_fiscal.R |
| projecoes-ifi.xlsx | Cenário base da IFI (juro real de longo prazo de 5%) | IFI / Senado | Parâmetro de juro neutro em ancora_fiscal.R |

Nomes trocados em relação ao download original: `embi.csv` (ipeadata[07-11-2026-03-56].csv),
`cds.csv` (Brasil CDS 5 Anos USD - Visão Geral.csv), `focus_*` (Série de estatísticas.xls e (1) a (3)),
`bcdata.sgs.432_*` (bcdata.sgs.432 (2) e (3)).

## Bases consultadas e não usadas

Não entram nos scripts: `dotacao-orcamentaria-e-credito-empenhado-da-dpf-por-fonte-de-recursos--2015-2026.csv`,
`emissoes-diretas-de-titulos-da-dpmfi.xlsx`, `estoque-da-divida-publica-federal.csv` (cópia do xlsx),
`CRONOGRAMA_20261106_RM_D.CSV` (veio vazio), `nivel-adequado-das-reservas-internacionais.xlsx`,
`bcdata.sgs.22864.csv`, `bcdata.sgs.3692.csv`, `bcdata.sgs.5723.csv`,
`bcdata.sgs.432.csv` e `(1)` (só 2023), `bcdata.sgs.13762 (1).csv` (duplicado), `seriehistoricajul26.xlsx`.

## Scripts

| Script | O que faz | Gráficos |
| --- | --- | --- |
| divida_publica.R | Composição por indexador, fatores de variação, custo implícito, efeito de 1 p.p. | 01 a 04 |
| lft_regressao.R | Fatia de LFT contra nível e inclinação da curva, quebras de Bai-Perron | 05 |
| custo_primario_nominal.R | Custo da dívida contra primário, nominal e juros (% PIB) | 06 |
| teste_bohn.R | Reação do primário à dívida, quebra e janelas móveis | 09, 10 |
| teste_blanchard.R | Versão ingênua com a variação da Selic (só comparação) | 11, 12 |
| surpresas_copom.R | Surpresas do Copom, risco-país EMBI+CDS, teste identificado | 13 a 15 |
| ancora_fiscal.R | Postura monetária, impulso fiscal estrutural e desancoragem | 16 |
| estimar_modelo.R | Estima as equações do modelo pré e pós-2014 | resultados/parametros_estimados.csv |
| modelo/ (estimado) | Modelo com parâmetros estimados e risco calibrado pelas surpresas | 17 a 19 |
| modelo/ (ilustrativo) | Primeira versão, calibrada à mão | 07, 08 |
| baixar_dados_bcb.R | Baixa as séries do SGS de uma vez | |

## O modelo estimado, em uma página

- **Estimados nos dados** (2010-2026, trimestral): suavização da Selic, persistência do risco-país,
  do prêmio de prazo e do primário estrutural, reação do primário à dívida (pré e pós-2014) e o
  tamanho dos choques.
- **Calibrado pelas surpresas do Copom**: a resposta do risco-país ao juro real (canal de Blanchard),
  escolhida para reproduzir as projeções locais (gráfico 19).
- **Não identificados** (ficam calibrados): curva IS, curva de Phillips e reação da Selic à inflação.
  As regressões deram zero ou sinal trocado, por simultaneidade.
- **Achado central**: com a reação fiscal estimada depois de 2014 (0,007 p.p. de primário por
  p.p. de dívida, contra 0,050 antes), o modelo não tem trajetória estável. Com a sensibilidade do
  risco medida depois de 2014, a estabilidade exige reação de cerca de 0,076, acima da de antes de 2014.
