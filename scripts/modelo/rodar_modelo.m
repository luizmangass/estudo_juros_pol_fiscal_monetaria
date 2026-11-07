% =====================================================================
% Roda o modelo nos dois regimes e simula a "canetada": o BC mantém a
% Selic k p.p. abaixo do que a regra mandaria durante H trimestres e
% depois volta a seguir a regra de Taylor.
%
% Como: a cada trimestre escolhe-se o choque e_m que deixa a Selic
% exatamente no alvo, dado o estado da economia (o mercado é
% surpreendido a cada trimestre, como numa intervenção discricionária).
%
% Gera:
%   - trajetória determinística (sem outros choques)
%   - N simulações estocásticas, com todos os outros choques ativos
% Saída: resultados_modelo.csv, lido por graficos_modelo.R
%
% Requer Dynare (https://www.dynare.org) no MATLAB ou Octave:
%   addpath('C:/dynare/6.x/matlab')   % ajuste o caminho
%   rodar_modelo
% =====================================================================
cortes = [1 2];   % tamanho do corte, em p.p.
H      = 4;       % trimestres de canetada
T      = 24;      % trimestres simulados
N      = 1000;    % simulações estocásticas
vars   = {'i','il','tp','rp','pi','q','b','ief','y'};
saida  = {};

for regime = [1 2]
  eval(sprintf('dynare modelo_selic_fiscal -DREGIME=%d noclearall nolog', regime));
  nomes = cellstr(M_.endo_names);
  dr    = oo_.dr;
  ii    = find(strcmp(nomes, 'i'));
  iq    = find(strcmp(nomes, 'q'));
  ipi   = find(strcmp(nomes, 'pi'));
  iem   = find(strcmp(cellstr(M_.exo_names), 'e_m'));
  row_i = find(dr.order_var == ii);       % linha da Selic na regra de decisão
  L     = chol(M_.Sigma_e + 1e-12*eye(M_.exo_nbr), 'lower');
  randn('seed', 42);

  for k = cortes
    for n = 0:N                           % n = 0: determinístico
      y  = zeros(M_.endo_nbr, T);
      yp = zeros(M_.endo_nbr, 1);         % estado inicial = estacionário
      for t = 1:T
        if n == 0, e = zeros(M_.exo_nbr, 1); else, e = L*randn(M_.exo_nbr, 1); end
        st = yp(dr.state_var);
        if t <= H
          % escolhe e_m para a Selic ficar k p.p. abaixo de zero (= da regra)
          e(iem) = 0;
          sem = dr.ghx(row_i,:)*st + dr.ghu(row_i,:)*e;
          e(iem) = (-k - sem) / dr.ghu(row_i, iem);
        else
          e(iem) = 0;                    % volta à regra
        end
        yt = zeros(M_.endo_nbr, 1);
        yt(dr.order_var) = dr.ghx*st + dr.ghu*e;
        y(:, t) = yt;  yp = yt;
      end
      % câmbio nominal acumulado (%): variação do real + inflação trimestral
      fx = cumsum([y(iq,1), diff(y(iq,:))] + y(ipi,:)/4);
      Y  = [y; fx];
      if n == 0
        det = Y;  sims = zeros(size(Y,1), T, N);
      else
        sims(:, :, n) = Y;
      end
    end

    nomes2 = [nomes; {'cambio_nominal'}];
    for v = [vars, {'cambio_nominal'}]
      j  = find(strcmp(nomes2, v{1}));
      pr = prctile(squeeze(sims(j,:,:))', [10 50 90]);
      for t = 1:T
        saida(end+1, :) = {regime, k, t, v{1}, det(j,t), pr(1,t), pr(2,t), pr(3,t)};
      end
    end
  end
end

fid = fopen('resultados_modelo.csv', 'w');
fprintf(fid, 'regime,corte,trimestre,variavel,deterministico,p10,p50,p90\n');
for r = 1:size(saida, 1)
  fprintf(fid, '%d,%d,%d,%s,%.6f,%.6f,%.6f,%.6f\n', saida{r, :});
end
fclose(fid);
disp('resultados_modelo.csv gerado');
