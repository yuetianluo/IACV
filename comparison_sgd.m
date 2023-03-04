function error_matrix = comparison_sgd(X, y, mu, n, p, M, step_size, iter_max, lambda, alpha_0)
  % initialization
  mu_0 =  zeros(p,1);
  pi_0 =  (1./(1+exp(-X * mu_0)));
  mu_LOO = cell(n,1);
  for z = 1:n
     mu_LOO{z} = zeros(2, p);
     mu_LOO{z}(1,:) = mu_0;
  end
  mu_LOO_est = cell(n,1);
  mu_LOO_NS_est = cell(n,1);
  mu_LOO_IJ_est = cell(n,1);

  for z = 1:n
     mu_LOO_est{z} = zeros(2, p);
     mu_LOO_est{z}(1,:) = mu_0;
     mu_LOO_NS_est{z} = mu_0;
     mu_LOO_IJ_est{z} = mu_0;
  end

  mu_t = mu_0;
  pi_t = pi_0;
  nu_t = pi_t - y;
  grad_t = X' * nu_t + lambda * mu_0;
  Hess_t = X' * diag( pi_t .* (1-pi_t) ) * X + lambda * eye(p);

  piLOO_t = cell(n,1);
  for z = 1:n
     piLOO_t{z} = pi_0;
  end
  IACV_time = 0;
  exact_cv_time = 0;
  % iteration start
  error_matrix = zeros(iter_max, 14);
  for iter = 1:iter_max
      if strcmp(step_size, 'epoch-doubling') 
          if iter <= 1000
              alpha_t = alpha_0;
          elseif iter <= 3000
             alpha_t = alpha_0/2;
          elseif iter <= 7000
             alpha_t = alpha_0/4; 
          elseif iter <= 15000
              alpha_t = alpha_0/8;
          elseif iter <= 31000
              alpha_t = alpha_0/16;
          elseif iter <= 63000
              alpha_t = alpha_0/32;
          elseif iter <= 127000 
              alpha_t = alpha_0/64;
          else
              alpha_t = alpha_0/128;
          end
      else
          alpha_t = alpha_0/iter;
      end
      select_index = binornd(1, M/n * ones(n,1) );
      select_index = find(select_index);
      
      mu_t_old = mu_t;
      pi_t_old = pi_t;
      our_time1_start = tic;
      grad_t_S = ( X(select_index,:) )' * nu_t(select_index) + lambda * mu_t_old;
      Hess_t_S = ( X(select_index,:) )' * diag( pi_t_old(select_index) .* (1 - pi_t_old(select_index)) ) * X(select_index,:)  + lambda * eye(p);
      our_time1 = toc(our_time1_start);

      %  compute hat_mu_t
      mu_t = mu_t - alpha_t * grad_t_S;
      pi_t = (1./(1+exp(-X * mu_t)));
      nu_t = pi_t - y;

      %
      grad_t = X' * nu_t + lambda * mu_t;
      Hess_t = X' * diag( pi_t .* (1-pi_t) ) * X + lambda * eye(p);  
      Hess_t_inv = inv(Hess_t);
      iter_IACV_err = zeros(n,1);
      iter_NS_err = zeros(n,1);
      iter_IJ_err = zeros(n,1);
      iter_base_err = zeros(n,1);
      
      grad_t_S_minus = zeros(p, n);
      Hess_t_S_minus = cell(n,1);
      select_index2 = cell(n,1);
      grad_t_minus2 = zeros(p,n);
      grad_t_minus = zeros(p,n);
      Hess_t_minus = cell(n,1);
      nu_tz = cell(n,1);
      if M > 1
          cv_time_start = tic;
           for z = 1:n
              nu_tz{z} = piLOO_t{z} - y;
              if ismember(z,select_index)
                  % compute hat_mu_LOO_t
                 select_index2{z} = setdiff(select_index,z);
                 grad_t_minus2(:,z) = (X(select_index2{z},:))' * nu_tz{z}(select_index2{z}) + (lambda * mu_LOO{z}(1,:))';
                 mu_LOO{z}(2,:) = (mu_LOO{z}(1,:))' - alpha_t * grad_t_minus2(:,z);
                 piLOO_t{z} = (1./(1+exp(-X * (mu_LOO{z}(2,:))' )));
                 mu_LOO{z}(1,:) = mu_LOO{z}(2,:);                
              else
                  % compute hat_mu_LOO_t
                 grad_t_minus2(:,z) = (X(select_index,:))' * nu_tz{z}(select_index) + (lambda * mu_LOO{z}(1,:))';
                 mu_LOO{z}(2,:) = (mu_LOO{z}(1,:))' - alpha_t * grad_t_minus2(:,z);
                 piLOO_t{z} = (1./(1+exp(- X * (mu_LOO{z}(2,:))' )));
                 mu_LOO{z}(1,:) = mu_LOO{z}(2,:);                
              end 
              iter_base_err(z) = norm(mu_LOO{z}(2,:) - mu_t');
          end   
          exact_cv_per_time = toc(cv_time_start);
          exact_cv_time = exact_cv_time + exact_cv_per_time;

          % To compute the time of different procedures, we compute the parfor separately for each procedure
          our_time2_start = tic;
          for z = 1:n
              if ismember(z,select_index)
                  % compute tilde_mu_LOO_t
                  grad_t_S_minus(:,z) = grad_t_S - (pi_t_old(z) - y(z)) * (X(z,:))';
                  Hess_t_S_minus{z} = Hess_t_S - (X(z,:))' * X(z,:) * pi_t_old(z) * (1 - pi_t_old(z));
                  mu_LOO_est{z}(2,:) = (mu_LOO_est{z}(1,:))'  - alpha_t * grad_t_S_minus(:,z) - alpha_t * Hess_t_S_minus{z} * ( (mu_LOO_est{z}(1,:))' - mu_t_old);
                  mu_LOO_est{z}(1,:) = mu_LOO_est{z}(2,:);             
              else
                  % compute tilde_mu_LOO_t
                  mu_LOO_est{z}(2,:) = (mu_LOO_est{z}(1,:))' - alpha_t * grad_t_S - alpha_t * Hess_t_S * (  (mu_LOO_est{z}(1,:))' - mu_t_old  );
                  mu_LOO_est{z}(1,:) = mu_LOO_est{z}(2,:);      
              end 
              iter_IACV_err(z) = norm(mu_LOO{z}(2,:) - mu_LOO_est{z}(2,:)) ;
          end
          our_time2 = toc(our_time2_start);
          IACV_per_time = (our_time1 + our_time2);
          IACV_time = IACV_time + IACV_per_time;          
          for z = 1:n
              grad_t_minus(:,z) = grad_t - (pi_t(z) - y(z)) * (X(z,:))';
              Hess_t_minus{z} = Hess_t - (X(z,:))' * X(z,:) * pi_t(z) * (1 - pi_t(z));
              mu_LOO_NS_est{z} =  mu_t - Hess_t_minus{z} \ grad_t_minus(:,z);
              mu_LOO_IJ_est{z} = mu_t - Hess_t_inv * grad_t_minus(:,z);
              iter_NS_err(z) = norm(mu_LOO{z}(2,:) - (mu_LOO_NS_est{z})') ;
              iter_IJ_err(z) = norm(mu_LOO{z}(2,:) - (mu_LOO_IJ_est{z})') ;
          end
      else
        cv_time_start = tic;
          for z = 1:n
              nu_tz{z} = piLOO_t{z} - y;
              if ismember(z,select_index)
                  % compute hat_mu_LOO_t
                  mu_LOO{z}(2,:) = (mu_LOO{z}(1,:))';
              else
                  % compute hat_mu_LOO_t
                 grad_t_minus2(:,z) = (X(select_index,:))' * nu_tz{z}(select_index) + (lambda * mu_LOO{z}(1,:))'; 
                 mu_LOO{z}(2,:) = (mu_LOO{z}(1,:))' - alpha_t * grad_t_minus2(:,z);
                 piLOO_t{z} = (1./(1+exp(-X * (mu_LOO{z}(2,:))' )));
                 mu_LOO{z}(1,:) = mu_LOO{z}(2,:);                
              end
              iter_base_err(z) = norm(mu_LOO{z}(2,:) - mu_t') ;            
          end 
          exact_cv_per_time = toc(cv_time_start);
          exact_cv_time = exact_cv_time + exact_cv_per_time;
          our_time2_start = tic;
          for z = 1:n
              nu_tz{z} = piLOO_t{z} - y;
              if ismember(z,select_index)
                  % compute tilde_mu_LOO_t
                  mu_LOO_est{z}(2,:) = (mu_LOO_est{z}(1,:))';
                  mu_LOO_est{z}(1,:) = mu_LOO_est{z}(2,:);
              else
                  % compute tilde_mu_LOO_t
                  mu_LOO_est{z}(2,:) = (mu_LOO_est{z}(1,:))' - alpha_t * grad_t_S - alpha_t * Hess_t_S * (  (mu_LOO_est{z}(1,:))' - mu_t_old  );
                  mu_LOO_est{z}(1,:) = mu_LOO_est{z}(2,:);              
              end
              iter_IACV_err(z) = norm(mu_LOO{z}(2,:) - mu_LOO_est{z}(2,:));       
          end 
          our_time2 = toc(our_time2_start);
          IACV_per_time = (our_time1 + our_time2);
          IACV_time = IACV_time + IACV_per_time;    
          for z = 1:n
              grad_t_minus(:,z) = grad_t - (pi_t(z) - y(z)) * (X(z,:))';
              Hess_t_minus{z} = Hess_t - (X(z,:))' * X(z,:) * pi_t(z) * (1 - pi_t(z));
              mu_LOO_NS_est{z} =  mu_t - Hess_t_minus{z} \ grad_t_minus(:,z);
              mu_LOO_IJ_est{z} = mu_t - Hess_t_inv * grad_t_minus(:,z);
              iter_NS_err(z) = norm(mu_LOO{z}(2,:) - (mu_LOO_NS_est{z})') ;
              iter_IJ_err(z) = norm(mu_LOO{z}(2,:) - (mu_LOO_IJ_est{z})') ;       
          end 
      end 

      % compute CV error
      CV = 0;
      NS_CV = 0;
      IJ_CV = 0;
      IACV_CV = 0;
      base_CV = 0;

      for z = 1:n
        pi_cv = 1/(1 + exp( - X(z,:) * (mu_LOO{z}(2,:))' ) );
        cv_loss = -  (y(z) * log( pi_cv ) + (1 - y(z)) * log( 1 - pi_cv ) );
        CV = CV + cv_loss;

        pi_ns = 1/(1 + exp( - X(z,:) * mu_LOO_NS_est{z} ) );
        ns_cv_loss = - ( y(z) * log(pi_ns) + (1 - y(z)) * log( 1 - pi_ns )  );
        NS_CV = NS_CV + ns_cv_loss;


        pi_ij = 1/(1 + exp( - X(z,:) * mu_LOO_IJ_est{z} ) );
        ij_cv_loss = - ( y(z) * log(pi_ij) + (1 - y(z)) * log( 1 - pi_ij )  );
        IJ_CV = IJ_CV + ij_cv_loss;

        pi_IACV = 1/(1 + exp( - X(z,:) * (mu_LOO_est{z}(2,:))' ) );
        our_cv_loss = - ( y(z) * log(pi_IACV) + (1 - y(z)) * log( 1 - pi_IACV )  );
        IACV_CV = IACV_CV + our_cv_loss;
             
        pi_base = 1/(1 + exp( - X(z,:) * mu_t ) );
        base_cv_loss = - ( y(z) * log(pi_base) + (1 - y(z)) * log( 1 - pi_base )  );
        base_CV = base_CV + base_cv_loss;      
      end


      CV = CV/n;
      NS_CV = NS_CV/n;
      IJ_CV = IJ_CV/n;
      IACV_CV = IACV_CV/n;
      base_CV = base_CV/n;

      err = norm(mu_t - mu)/norm(mu);
      iter_result = [iter, err, IACV_time, exact_cv_time, mean(iter_IACV_err), mean(iter_NS_err), mean(iter_IJ_err), mean(iter_base_err),norm(grad_t), CV,IACV_CV, NS_CV, IJ_CV,  base_CV];
      % if mod(iter,1000) == 0 
      %     array2table(iter_result)
      % end
      error_matrix(iter, :) = iter_result;
  end

end