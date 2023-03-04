function error_matrix = comparison_gd(X, y, mu, n, p, iter_max, lambda, alpha_t)
  % use the all-zero initialization
  mu_0 =  zeros(p,1);

  % initialization
  pi_0 =  (1./(1+exp(-X * mu_0)));
  mu_LOO = cell(n,1);
  for z = 1:n
     mu_LOO{z} = zeros(2, p);
     mu_LOO{z}(1,:) = mu_0;
  end
  mu_LOO_IACV_est = cell(n,1);
  mu_LOO_NS_est = zeros(p,n);
  mu_LOO_IJ_est = zeros(p,n);
  for z = 1:n
     mu_LOO_IACV_est{z} = zeros(2, p);
     mu_LOO_IACV_est{z}(1,:) = mu_0;
     mu_LOO_NS_est(:, z) = mu_0;
     mu_LOO_IJ_est(:, z)= mu_0;
  end

  mu_t = mu_0;
  pi_t = pi_0;
  nu_t = pi_t - y;
  grad_t = X' * nu_t + lambda * mu_t;
  Hess_t = X' * diag( pi_t .* (1-pi_t) ) * X + lambda * eye(p);

  piLOO_t = cell(n,1);
  for z = 1:n
     piLOO_t{z} = pi_0;
     piLOO_t{z}(z) = [];
  end
  IACV_time = 0;
  exact_cv_time = 0;
  error_matrix = zeros(iter_max, 14);
  for iter = 1:iter_max
      % compute IACV_mu_LOO_t
      our_time_start = tic;
      for z = 1:n
          grad_t_minusz = grad_t - (pi_t(z) - y(z)) * (X(z,:))';
          Hess_t_minusz = Hess_t - (X(z,:))' * X(z,:) * pi_t(z) * (1 - pi_t(z));
          mu_LOO_IACV_est{z}(2,:) = (mu_LOO_IACV_est{z}(1,:))'  - alpha_t * grad_t_minusz - alpha_t * Hess_t_minusz * ( (mu_LOO_IACV_est{z}(1,:))' - mu_t);
      	  mu_LOO_IACV_est{z}(1,:) = mu_LOO_IACV_est{z}(2,:);
      end
      % compute hat_mu_t
      mu_t = mu_t - alpha_t * grad_t;
      pi_t = (1./(1+exp(-X * mu_t)));   
      nu_t = pi_t - y;
      grad_t = X' * nu_t + lambda * mu_t;
      Hess_t = X' * diag( pi_t .* (1-pi_t) ) * X + lambda * eye(p);  
      IACV_per_time = toc(our_time_start);
      IACV_time = IACV_time + IACV_per_time;

      % compute hat_mu_LOO_t
      % main computational burden
      cv_time_start = tic;
      for z = 1:n
          nu_tz = piLOO_t{z} - y(1:end ~= z);
          Xminusz = X(1:end ~= z,:);
          grad_t_minusz = (Xminusz)' * nu_tz + (lambda * mu_LOO{z}(1,:))';
          mu_minusz_t = (mu_LOO{z}(1,:))' - alpha_t * grad_t_minusz;
          piLOO_t{z} = (1./(1+exp(-Xminusz * mu_minusz_t )));
          mu_LOO{z}(2,:) = mu_minusz_t;
          mu_LOO{z}(1,:) = mu_LOO{z}(2,:); 
      end 
      exact_cv_per_time = toc(cv_time_start);
      exact_cv_time = exact_cv_time + exact_cv_per_time;

      Hess_t_inv = inv(Hess_t);
      % compute mu_LOO_NS_est / mu_LOO_IJ_est
      for z = 1:n
          grad_t_minusz = grad_t - ((pi_t(z) - y(z)) * (X(z,:))');
          Hess_t_minusz = Hess_t - (X(z,:))' * X(z,:) * pi_t(z) * (1 - pi_t(z));
          mu_LOO_NS_est(:, z) =  mu_t - Hess_t_minusz \ grad_t_minusz;
          mu_LOO_IJ_est(:, z) = mu_t - Hess_t_inv * grad_t_minusz;
      end

      % compute CV
      IACV_CV = 0;
      NS_CV = 0;
      IJ_CV = 0;
      CV = 0;
      base_CV = 0;
      for z = 1:n
        pi_cv = 1/(1 + exp( - X(z,:) * (mu_LOO{z}(2,:))' ) );
        cv_loss = -  (y(z) * log( pi_cv ) + (1 - y(z)) * log( 1 - pi_cv ) );
        CV = CV + cv_loss;

        pi_ns = 1/(1 + exp( - X(z,:) * mu_LOO_NS_est(:, z) ) );
        ns_cv_loss = - ( y(z) * log(pi_ns) + (1 - y(z)) * log( 1 - pi_ns )  );
        NS_CV = NS_CV + ns_cv_loss;

        pi_ij = 1/(1 + exp( - X(z,:) * mu_LOO_IJ_est(:, z) ) );
        ij_cv_loss = - ( y(z) * log(pi_ij) + (1 - y(z)) * log( 1 - pi_ij )  );
        IJ_CV = IJ_CV + ij_cv_loss;

        pi_IACV = 1/(1 + exp( - X(z,:) * (mu_LOO_IACV_est{z}(2,:))' ) );
        IACV_cv_loss = - ( y(z) * log(pi_IACV) + (1 - y(z)) * log( 1 - pi_IACV )  );
        IACV_CV = IACV_CV + IACV_cv_loss;

        pi_base = 1/(1 + exp( - X(z,:) * mu_t ) );
        base_cv_loss = - ( y(z) * log(pi_base) + (1 - y(z)) * log( 1 - pi_base )  );
        base_CV = base_CV  + base_cv_loss;
      end

      CV = CV/n;
      IACV_CV = IACV_CV/n;
      base_CV = base_CV/n;
      NS_CV = NS_CV/n;
      IJ_CV = IJ_CV/n;

      err = norm(mu_t - mu)/norm(mu);

      % compute mu_LOO_t estimation error
	  	iter_IACV_err = 0;
	    iter_NS_err = 0;
	    iter_IJ_err = 0;
	    iter_base_err = 0;
	    for z = 1:n
	        iter_IACV_err = norm(mu_LOO{z}(2,:) - mu_LOO_IACV_est{z}(2,:)) + iter_IACV_err;
	        iter_NS_err = norm(mu_LOO{z}(2,:)' - mu_LOO_NS_est(:,z)) + iter_NS_err;
	        iter_IJ_err = norm(mu_LOO{z}(2,:)' - mu_LOO_IJ_est(:,z)) + iter_IJ_err;
	        iter_base_err = norm(mu_LOO{z}(2,:) - mu_t') + iter_base_err;
	    end
      iter_result = [iter, err,IACV_time, exact_cv_time, iter_IACV_err/n, iter_NS_err/n, iter_IJ_err/n, iter_base_err/n, norm(grad_t), CV, IACV_CV, NS_CV, IJ_CV, base_CV];
      error_matrix(iter, :) = iter_result;
  end
end