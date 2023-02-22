function x = fista(H,g,lam,max_iter,x0)
    yold = x0;
    x = x0;
    L = 2 * norm(H);
%    pobj = 0.5 * x0' * H * x0 + x0' * g + lam * norm(x0,1)
    t = 1;
    for iteration = 1:max_iter
       ynew = wthresh( (x - (H * x + g)/L), 's', lam/L );
       t_old = t;
       t = (1 + sqrt(1 + 4 * t_old^2))/2;
       x = ynew + (t_old - 1)/ t * (ynew - yold);
       yold = ynew;
%       pobj = 0.5 * x' * H * x + x' * g + lam * norm(x,1)
    end
end