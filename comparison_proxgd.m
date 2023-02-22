function error_matrix = comparison_proxgd(X, y, mu, n, p, iter_max, lambda, alpha_t)
	% initialization
	mu_0 = zeros(p,1);
	pi_0 =  (1./(1+exp(-X * mu_0)));
	mu_LOO = cell(n,1);
	for z = 1:n
	   mu_LOO{z} = zeros(2, p);
	   mu_LOO{z}(1,:) = mu_0;
	end
	mu_LOO_est = cell(n,1);
	mu_LOO_NS_est = zeros(p,n);
	mu_LOO_IJ_est = zeros(p,n);

	for z = 1:n
	   mu_LOO_est{z} = zeros(2, p);
	   mu_LOO_est{z}(1,:) = mu_0;
	   mu_LOO_NS_est(:,z) = mu_0;
	   mu_LOO_IJ_est(:,z) = mu_0;
	end
	 
	mu_t = mu_0;
	pi_t = pi_0;
	nu_t = pi_t - y;
	grad_t = X' * nu_t;
	Hess_t = X' * diag( pi_t .* (1-pi_t) ) * X;

	piLOO_t = zeros((n-1),n);
	for z = 1:n
	   pi_tem = pi_0;
	   pi_tem(z) = [];
	   piLOO_t(:,z) = pi_tem;
	end
  IACV_time = 0;
  exact_cv_time = 0;
	% iteration start
	error_matrix = zeros(iter_max, 14);
	for iter = 1:iter_max
	    our_time_start = tic;
	    % compute IACV_mu_LOO_t
	    for z = 1:n
	        grad_t_minusz = grad_t - ((pi_t(z) - y(z)) * (X(z,:))');
	        Hess_t_minusz = Hess_t - (X(z,:))' * X(z,:) * pi_t(z) * (1 - pi_t(z));
	        mu_LOO_est{z}(2,:) = (mu_LOO_est{z}(1,:))'  - alpha_t * grad_t_minusz - alpha_t * Hess_t_minusz * ( (mu_LOO_est{z}(1,:))' - mu_t);
	        mu_LOO_est{z}(2,:) = wthresh((mu_LOO_est{z}(2,:))','s', lambda * alpha_t );
	        mu_LOO_est{z}(1,:) = mu_LOO_est{z}(2,:);
	    end
	    % compute hat_mu_t
	    mu_t =  wthresh( (mu_t - alpha_t * grad_t), 's', lambda * alpha_t);
	    pi_t = (1./(1+exp(-X * mu_t)));
	    nu_t = pi_t - y;
	    grad_t = X' * nu_t;
	    Hess_t = X' * diag( pi_t .* (1-pi_t) ) * X;     
      IACV_per_time = toc(our_time_start);
      IACV_time = IACV_time + IACV_per_time;

	    % compute hat_mu_LOO_t
	    % main computational burden
	    cv_time_start = tic;
	    for z = 1:n
	        nu_tz = piLOO_t(:,z) - y(1:end ~= z);
	        Xminusz = X(1:end ~= z,:);
	        grad_t_minusz = (Xminusz)' * nu_tz;
	        mu_minusz_t = (mu_LOO{z}(1,:))' - alpha_t * grad_t_minusz;
	        mu_minusz_t = wthresh(mu_minusz_t, 's', lambda * alpha_t );
	        piLOO_t(:,z) = (1./(1+exp(-Xminusz * mu_minusz_t )));
	        mu_LOO{z}(2,:) = mu_minusz_t;
	        mu_LOO{z}(1,:) = mu_LOO{z}(2,:); 
	    end   
      exact_cv_per_time = toc(cv_time_start);
      exact_cv_time = exact_cv_time + exact_cv_per_time;

	    % compute mu_LOO_NS_est / mu_LOO_IJ_est
	    for z = 1:n
	        grad_t_minusz = grad_t - ((pi_t(z) - y(z)) * (X(z,:))');
	        Hess_t_minusz = Hess_t - (X(z,:))' * X(z,:) * pi_t(z) * (1 - pi_t(z));
	        mu_LOO_NS_est(:,z) =  fista(Hess_t_minusz, (grad_t_minusz - Hess_t_minusz * mu_t), lambda, 60, mu_t);
	        mu_LOO_IJ_est(:,z) = fista(Hess_t, (grad_t_minusz - Hess_t * mu_t), lambda, 60, mu_t);
	    end

	    % compute iteration estimation error
	    iter_IACV_err = 0;
	    iter_NS_err = 0;
	    iter_IJ_err = 0;
	    iter_base_err = 0;
	    for z = 1:n
	        iter_IACV_err = norm(mu_LOO{z}(2,:) - mu_LOO_est{z}(2,:))^2 + iter_IACV_err;
	        iter_NS_err = norm(mu_LOO{z}(2,:)' - mu_LOO_NS_est(:,z))^2 + iter_NS_err;
	        iter_IJ_err = norm(mu_LOO{z}(2,:)' - mu_LOO_IJ_est(:,z))^2 + iter_IJ_err;
	        iter_base_err = norm(mu_LOO{z}(2,:) - mu_t')^2 + iter_base_err;
	    end
	    % compute CV
	    CV = 0;
	    NS_CV = 0;
	    IJ_CV = 0;
	    IACV_CV = 0;
	    base_CV = 0;
	    for z = 1:n
	      pi_cv = 1/(1 + exp( - X(z,:) * (mu_LOO{z}(2,:))' ) );
	      cv_loss = -  (y(z) * log( pi_cv ) + (1 - y(z)) * log( 1 - pi_cv ) );
	      CV = CV + cv_loss;

	      pi_ns = 1/(1 + exp( - X(z,:) * mu_LOO_NS_est(:,z) ) );
	      ns_cv_loss = - ( y(z) * log(pi_ns) + (1 - y(z)) * log( 1 - pi_ns )  );
	      NS_CV = NS_CV + ns_cv_loss;

	      pi_ij = 1/(1 + exp( - X(z,:) * mu_LOO_IJ_est(:,z) ) );
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
	    iter_result = [iter, err, IACV_time, exact_cv_time, sqrt(iter_IACV_err/n), sqrt(iter_NS_err/n),sqrt(iter_IJ_err/n),sqrt(iter_base_err/n), norm(grad_t + lambda * sign(mu_t)), CV,IACV_CV, NS_CV, IJ_CV,  base_CV];
	    error_matrix(iter, :) = iter_result;
	    % if mod(iter,100) == 0  || iter <= 20
	    %     array2table(iter_result)
	    % end
	end
end