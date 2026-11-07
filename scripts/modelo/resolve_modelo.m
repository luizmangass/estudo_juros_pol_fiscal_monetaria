% Resolve o modelo; se não houver trajetória estável, devolve info <> 0
% (o try contorna um erro do Dynare 6 no Octave quando a condição falha).
function [dr, info] = resolve_modelo(M_, options_, oo_)
  try
    [dr, info] = resol(0, M_, options_, oo_.dr, oo_.steady_state, ...
                       oo_.exo_steady_state, oo_.exo_det_steady_state);
  catch
    dr = [];  info = 3;
  end
end
