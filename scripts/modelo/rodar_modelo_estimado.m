% =====================================================================
% Roda o modelo ESTIMADO (modelo_estimado.mod) em dois regimes:
%   1 = pré-2014 (parâmetros estimados até jun/2014)
%   2 = pós-2014 (parâmetros estimados depois de jun/2014)
%
% Passo 1. Calibra chi_r (resposta do risco-país ao juro real) para que o
%          modelo reproduza a resposta do risco a uma surpresa do Copom
%          medida em surpresas_copom.R (projeções locais, por 1 p.p.).
% Passo 2. Simula a canetada: Selic 1 ou 2 p.p. abaixo da regra por 4
%          trimestres, trajetória central + 1.000 simulações estocásticas.
%
% No pós-2014 a regra fiscal estimada (psi = 0,0018) não estabiliza a
% dívida: o modelo não tem trajetória estável (Blanchard-Kahn falha).
% Para simular, o script sobe psi até o menor valor que estabiliza
% (psi_min) e avisa. Esse "mínimo" é o ajuste fiscal implícito que o
% mercado precisa acreditar que virá.
%
% Requer Dynare no MATLAB ou Octave (no Octave: pkg load statistics).
% Saídas: resultados_modelo_estimado.csv, calibracao_chi_r.csv, irf_risco_modelo.csv,
%         simulacoes_modelo_estimado.csv (cada trajetória; sim = 0 é a central)
% =====================================================================
cortes = [1 2];  H = 4;  T = 24;  N = 1000;
vars   = {'i','il','tp','rp','pi','q','b','ief','y'};
% Alvos: resposta do risco-país (p.p.) a surpresa de +1 p.p., 3 e 6 meses
% depois (projeções locais com risco EMBI+/CDS emendado).
alvo   = {[0.39 0.37], [1.37 1.24]};
grade  = 0:0.05:4;
saida  = {};  calib = {};  irfs = {};
% Trajetórias individuais (para o modelo satélite da dívida em R)
vars_sim = {'i','il','pi','rp','s','b','tp','y','cambio_nominal'};
fsim = fopen('simulacoes_modelo_estimado.csv', 'w');
fprintf(fsim, 'regime,corte,sim,trimestre,%s\n', strjoin(vars_sim, ','));

for regime = [1 2]
  eval(sprintf('dynare modelo_estimado -DREGIME=%d noclearall nolog', regime));
  pn = cellstr(M_.param_names);

  % --- Calibração conjunta: chi_r pelas surpresas, psi mínimo estável ------
  % Para cada chi_r da grade, parte do psi estimado e sobe até o menor
  % valor com trajetória estável (no pré-2014 o estimado já basta).
  psi_est = M_.params(strcmp(pn, 'psi_est'));
  ie  = find(strcmp(cellstr(M_.exo_names), 'e_m'));
  irp = find(strcmp(cellstr(M_.endo_names), 'rp'));
  ii  = find(strcmp(cellstr(M_.endo_names), 'i'));
  melhor = Inf;
  for c = grade
    M_.params(strcmp(pn, 'chi_r')) = c;
    psi = psi_est;
    while true
      M_.params(strcmp(pn, 'psi')) = psi;
      [dr_, info] = resolve_modelo(M_, options_, oo_);
      if info(1) == 0 || psi > 0.05, break; end
      psi = psi + 0.0001;
    end
    if info(1) ~= 0, continue; end
    e = zeros(M_.exo_nbr, 1);  e(ie) = 1;
    y = zeros(M_.endo_nbr, 5);  yp = zeros(M_.endo_nbr, 1);
    for t = 1:5
      if t > 1, e(:) = 0; end
      yt = zeros(M_.endo_nbr, 1);
      yt(dr_.order_var) = dr_.ghx * yp(dr_.state_var) + dr_.ghu * e;
      y(:, t) = yt;  yp = yt;
    end
    resp = y(irp, 2:3) / y(ii, 1);          % por 1 p.p. de Selic no impacto
    erro = sum((resp - alvo{regime}).^2);
    if erro < melhor, melhor = erro; chi_ok = c; psi_ok = psi; resp_ok = resp; irf_ok = y(irp,1:5) / y(ii,1); end
  end
  M_.params(strcmp(pn, 'chi_r')) = chi_ok;
  M_.params(strcmp(pn, 'psi'))   = psi_ok;
  [dr, info] = resolve_modelo(M_, options_, oo_);
  fprintf('\nRegime %d: chi_r = %.2f, psi estimado %.4f, psi usado %.4f | risco 3m/6m: modelo %.2f/%.2f, dados %.2f/%.2f\n', ...
          regime, chi_ok, psi_est, psi_ok, resp_ok(1), resp_ok(2), alvo{regime}(1), alvo{regime}(2));
  irfs{regime} = irf_ok;
  calib(end+1, :) = {regime, psi_est, psi_ok, chi_ok, resp_ok(1), resp_ok(2), ...
                     alvo{regime}(1), alvo{regime}(2)};

  % --- Canetada --------------------------------------------------------------
  nomes = cellstr(M_.endo_names);
  iq  = find(strcmp(nomes, 'q'));  ipi = find(strcmp(nomes, 'pi'));
  row_i = find(dr.order_var == ii);
  L = chol(M_.Sigma_e + 1e-12*eye(M_.exo_nbr), 'lower');
  randn('seed', 42);
  for k = cortes
    for n = 0:N
      y = zeros(M_.endo_nbr, T);  yp = zeros(M_.endo_nbr, 1);
      for t = 1:T
        if n == 0, e = zeros(M_.exo_nbr, 1); else, e = L * randn(M_.exo_nbr, 1); end
        st = yp(dr.state_var);
        if t <= H
          e(ie) = 0;
          sem = dr.ghx(row_i, :) * st + dr.ghu(row_i, :) * e;
          e(ie) = (-k - sem) / dr.ghu(row_i, ie);
        else
          e(ie) = 0;
        end
        yt = zeros(M_.endo_nbr, 1);
        yt(dr.order_var) = dr.ghx * st + dr.ghu * e;
        y(:, t) = yt;  yp = yt;
      end
      fx = cumsum([y(iq,1), diff(y(iq,:))] + y(ipi,:)/4);
      Y = [y; fx];
      if n == 0, det = Y; sims = zeros(size(Y,1), T, N); else, sims(:,:,n) = Y; end
    end
    nomes2 = [nomes; {'cambio_nominal'}];
    jj = cellfun(@(v) find(strcmp(nomes2, v)), vars_sim);
    for n = 0:N
      if n == 0, Ysim = det; else, Ysim = sims(:, :, n); end
      for t = 1:T
        fprintf(fsim, '%d,%d,%d,%d', regime, k, n, t);
        fprintf(fsim, ',%.5f', Ysim(jj, t));
        fprintf(fsim, '\n');
      end
    end
    for v = [vars, {'cambio_nominal'}]
      j = find(strcmp(nomes2, v{1}));
      pr = prctile(squeeze(sims(j,:,:))', [10 50 90]);
      for t = 1:T
        saida(end+1, :) = {regime, k, t, v{1}, det(j,t), pr(1,t), pr(2,t), pr(3,t)};
      end
    end
  end
end

fclose(fsim);
fid = fopen('resultados_modelo_estimado.csv', 'w');
fprintf(fid, 'regime,corte,trimestre,variavel,deterministico,p10,p50,p90\n');
for r = 1:size(saida,1), fprintf(fid, '%d,%d,%d,%s,%.6f,%.6f,%.6f,%.6f\n', saida{r,:}); end
fclose(fid);
fid = fopen('calibracao_chi_r.csv', 'w');
fprintf(fid, 'regime,psi_estimado,psi_usado,chi_r,modelo_3m,modelo_6m,dados_3m,dados_6m\n');
for r = 1:size(calib,1), fprintf(fid, '%d,%.4f,%.4f,%.2f,%.3f,%.3f,%.3f,%.3f\n', calib{r,:}); end
fclose(fid);
fid = fopen('irf_risco_modelo.csv', 'w');
fprintf(fid, 'regime,trimestre,rp\n');
for r = 1:2, for t = 1:5, fprintf(fid, '%d,%d,%.4f\n', r, t, irfs{r}(t)); end, end
fclose(fid);
disp('resultados_modelo_estimado.csv, calibracao_chi_r.csv e irf_risco_modelo.csv gerados');

