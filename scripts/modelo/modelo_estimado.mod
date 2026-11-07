// =====================================================================
// Modelo da canetada, versão ESTIMADA com os dados do estudo.
// Mesma estrutura de modelo_selic_fiscal.mod, com três mudanças:
//
// 1. Persistências, regra fiscal e volatilidade dos choques vêm das
//    regressões de estimar_modelo.R (2010T1-2026T2, Newey-West).
// 2. O prêmio de risco passa a responder ao juro real (canal de Blanchard):
//        rp = rho_rp*rp(-1) + chi_b*b + chi_pi*pi + chi_r*(i - pi(+1)) + e_rp
//    chi_r é escolhido em rodar_modelo_estimado.m para reproduzir a
//    resposta do risco-país às SURPRESAS do Copom (surpresas_copom.R).
// 3. Os dois regimes deixam de ser hipotéticos: são pré e pós-2014.
//
// Parâmetros que os dados NÃO identificam (curva IS, curva de Phillips,
// reação à inflação na regra de Taylor) ficam calibrados como antes: as
// regressões deram coeficientes nulos ou com sinal trocado, o problema
// clássico de simultaneidade (o BC reage à economia que ele afeta).
//
// Unidades: juros, inflação e prêmios em p.p. anualizados; hiato e câmbio
// real em %; dívida (b) em p.p. do PIB; primário (s) em p.p. do PIB por
// trimestre.
// =====================================================================

@#ifndef REGIME
  @#define REGIME = 2
@#endif

var y pi i r q rp tp il ifix ief b s m;
varexo e_y e_pi e_rp e_m e_tp e_s;

parameters a_y a_r a_q b_pi b_y b_q th_q rho_q
           rho_i phi_pi phi_y rho_m
           rho_rp chi_b chi_pi chi_r
           rho_tp k_rp k_pi D M alpha
           bbar rg rho_s psi psi_est;

// --- Calibrados (não identificados nos dados) ----------------------------
a_y = 0.5;  a_r = 0.15; a_q = 0.03;
b_pi = 0.4; b_y = 0.10; b_q = 0.30;
th_q = 0.6; rho_q = 0.9;
phi_pi = 1.8; phi_y = 0.3; rho_m = 0.0;
chi_b = 0.02; chi_pi = 0.10;
k_rp = 1.0;  k_pi = 0.20;
D = 20; M = 16; alpha = 0.52; bbar = 0.78; rg = 0.03;

// --- Estimados (estimar_modelo.R) -----------------------------------------
rho_i  = 0.858;   // suavização da Selic
rho_rp = 0.666;   // persistência do risco-país
rho_tp = 0.582;   // persistência do prêmio de prazo
rho_s  = 0.895;   // persistência do primário estrutural

@#if REGIME == 1
  // Pré-2014: o primário reagia à dívida (Bohn com o estrutural, p = 0,01)
  psi_est = 0.0125;
  psi     = 0.0125;
  chi_r = 0.0;     // substituído pela calibração por surpresas
@#else
  // Pós-2014: a reação à dívida praticamente some (diferença p = 0,04)
  psi_est = 0.0018;   // estimado; com ele o modelo não tem trajetória estável
  psi     = 0.005;    // valor provisório só para o Dynare inicializar;
                      // rodar_modelo_estimado.m recomeça de psi_est
  chi_r = 0.0;     // substituído pela calibração por surpresas
@#endif

model(linear);
  y  = a_y*y(-1) + (1-a_y)*y(+1) - a_r*(i - pi(+1)) + a_q*q + e_y;
  pi = b_pi*pi(-1) + (1-b_pi)*pi(+1) + b_y*y + b_q*(q - q(-1)) + e_pi;
  q  = th_q*q(+1) + (1-th_q)*rho_q*q(-1) - (i - pi(+1))/4 + rp/4;
  rp = rho_rp*rp(-1) + chi_b*b + chi_pi*pi + chi_r*(i - pi(+1)) + e_rp;
  i  = rho_i*i(-1) + (1-rho_i)*(phi_pi*pi(+1) + phi_y*y) + m;
  m  = rho_m*m(-1) + e_m;
  r  = i - pi(+1);
  il = (1/D)*(i + tp) + (1-1/D)*il(+1);
  tp = rho_tp*tp(-1) + k_rp*rp + k_pi*pi + e_tp;
  ifix = (1-1/M)*ifix(-1) + (1/M)*il;
  ief  = alpha*i + (1-alpha)*ifix;
  b  = (1+rg/4)*b(-1) + bbar*(ief - pi - 4*(y - y(-1)))/4 - s;
  s  = rho_s*s(-1) + psi*b(-1) + e_s;
end;

// Desvios-padrão: e_m, e_rp, e_tp e e_s são os resíduos das regressões;
// e_y e e_pi ficam calibrados (equações não identificadas).
shocks;
  var e_y;  stderr 0.5;
  var e_pi; stderr 1.0;
  var e_rp; stderr 0.317;
  var e_m;  stderr 0.776;
  var e_tp; stderr 0.541;
  var e_s;  stderr 0.073;
end;

steady;
check;
stoch_simul(order=1, irf=12, nograph, noprint);
