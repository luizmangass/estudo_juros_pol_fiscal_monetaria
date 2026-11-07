// =====================================================================
// Modelo novo-keynesiano de economia pequena e aberta com bloco fiscal
// e curva de juros. Trimestral, em desvios do estado estacionário.
//
// Unidades: juros, inflação, prêmios em p.p. ANUALIZADOS;
//           hiato (y) e câmbio real (q, + = depreciação) em %;
//           dívida (b) e primário (s) em p.p. do PIB.
//
// Dois regimes (escolha na linha de comando com -DREGIME=1 ou 2):
//   1 = "Curva confiante": o prêmio de prazo reage pouco a risco e inflação.
//   2 = "Curva desconfiada": mesmo BC e mesma regra fiscal; o mercado cobra
//       muito mais prêmio de prazo quando a inflação e o risco sobem.
//
// Por que não há um regime de "dominância fiscal" de verdade?
// Porque o modelo deixa de ter equilíbrio. Testando a calibração:
//   - phi_pi abaixo de ~1,5, ou chi_pi acima de ~0,25 -> indeterminação
//     (expectativas de inflação sem âncora);
//   - chi_b alto com psi baixo -> solução explosiva (dívida sem teto).
// Esse é o resultado de Sargent-Wallace/Leeper: sob dominância fiscal o
// modelo linear não tem trajetória estável. Num texto, isso já é o ponto.
//
// A "canetada" é o choque e_m: um desvio persistente da regra de Taylor.
// O próprio BC volta a reagir à inflação pela regra, como o mercado
// espera que aconteça.
// =====================================================================

@#ifndef REGIME
  @#define REGIME = 1
@#endif

var y      // hiato do produto
    pi     // inflação (anualizada)
    i      // Selic
    r      // juro real ex-ante
    q      // câmbio real (+ = depreciação)
    rp     // prêmio de risco país
    tp     // prêmio de prazo
    il     // juro longo (prefixado ~ 5 anos)
    ifix   // custo médio travado do estoque prefixado/indexado
    ief    // custo efetivo médio da dívida
    b      // dívida/PIB
    s      // resultado primário
    m;     // desvio da regra (canetada)

varexo e_y e_pi e_rp e_m e_tp e_s;

parameters a_y a_r a_q b_pi b_y b_q th_q rho_q
           rho_i phi_pi phi_y rho_m
           rho_rp chi_b chi_pi
           rho_tp k_rp k_pi D M alpha
           bbar rg rho_s psi;

// --- Demanda e oferta --------------------------------------------------
a_y   = 0.5;    // persistência do hiato
a_r   = 0.15;   // sensibilidade do hiato ao juro real
a_q   = 0.03;   // efeito do câmbio real sobre o hiato
b_pi  = 0.4;    // inércia inflacionária
b_y   = 0.10;   // inclinação da curva de Phillips
b_q   = 0.30;   // repasse cambial (anualizado)
th_q  = 0.6;    // peso das expectativas racionais na paridade
rho_q = 0.9;    // reversão do câmbio real

// --- Política monetária ------------------------------------------------
rho_i = 0.8;
phi_y = 0.3;
rho_m = 0.0;    // canetada = surpresa a cada trimestre (ver rodar_modelo.m)

// --- Curva e dívida ----------------------------------------------------
rho_rp = 0.7;
rho_tp = 0.8;
D      = 20;    // vida do título longo, em trimestres
M      = 16;    // prazo médio de repactuação da parte não-Selic (~4 anos)
alpha  = 0.52;  // fatia LFT na DPF (set/2026)
bbar   = 0.78;  // dívida/PIB de referência
rg     = 0.03;  // r - g no estado estacionário (anual)
rho_s  = 0.7;

@#if REGIME == 1
  // Curva confiante
  phi_pi = 1.8;
  chi_b  = 0.02;  chi_pi = 0.10;
  k_rp   = 1.0;   k_pi   = 0.20;
  psi    = 0.040;
@#else
  // Curva desconfiada: mesmo BC e mesma regra fiscal, mas o mercado cobra
  // o dobro de prêmio de prazo por risco e 5x mais por inflação.
  phi_pi = 1.8;
  chi_b  = 0.02;  chi_pi = 0.10;
  k_rp   = 2.0;   k_pi   = 1.00;
  psi    = 0.040;
@#endif

model(linear);
  // IS híbrida
  y = a_y*y(-1) + (1-a_y)*y(+1) - a_r*(i - pi(+1)) + a_q*q + e_y;
  // Phillips híbrida com repasse cambial
  pi = b_pi*pi(-1) + (1-b_pi)*pi(+1) + b_y*y + b_q*(q - q(-1)) + e_pi;
  // Paridade descoberta real com prêmio de risco (juro externo constante).
  // Versão híbrida (como nos modelos semiestruturais de BCs): parte das
  // expectativas de câmbio olha para trás; rho_q < 1 puxa o câmbio real
  // de volta ao equilíbrio no longo prazo (paridade do poder de compra).
  q = th_q*q(+1) + (1-th_q)*rho_q*q(-1) - (i - pi(+1))/4 + rp/4;
  // Prêmio de risco: sobe com dívida e com inflação (credibilidade)
  rp = rho_rp*rp(-1) + chi_b*b + chi_pi*pi + e_rp;
  // Regra de Taylor + canetada
  i = rho_i*i(-1) + (1-rho_i)*(phi_pi*pi(+1) + phi_y*y) + m;
  m = rho_m*m(-1) + e_m;
  r = i - pi(+1);
  // Juro longo = média ponderada das Selic esperadas + prêmio de prazo
  il = (1/D)*(i + tp) + (1-1/D)*il(+1);
  tp = rho_tp*tp(-1) + k_rp*rp + k_pi*pi + e_tp;
  // Custo da dívida: a parte não-Selic reprecifica aos poucos (rolagem)
  ifix = (1-1/M)*ifix(-1) + (1/M)*il;
  ief  = alpha*i + (1-alpha)*ifix;
  // Dívida/PIB: juro efetivo real menos crescimento nominal, menos primário
  b = (1+rg/4)*b(-1) + bbar*(ief - pi - 4*(y - y(-1)))/4 - s;
  // Regra fiscal
  s = rho_s*s(-1) + psi*b(-1) + e_s;
end;

shocks;
  var e_y;  stderr 0.5;
  var e_pi; stderr 1.0;
  var e_rp; stderr 1.0;
  var e_m;  stderr 1.0;   // tamanho 1 = canetada de 1 p.p.
  var e_tp; stderr 0.3;
  var e_s;  stderr 0.3;
end;

steady;
check;
stoch_simul(order=1, irf=24, nograph, noprint);
