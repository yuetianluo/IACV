
clear all
% set the number of nodes in the parallel program
core_num = 24;
parpool('local', core_num);

rng('default');
seed = 2030;
rng(seed);
% Setup
p = 20;
p_star = 5;
n = 1000;
iter_max = 7000;
lambda = 1e-6 * n;
alpha_0 = 0.5/n;
exp_num = 100;

final_error_matrix = cell(exp_num,1);
program_time_start = tic;
% Run the experiment in parallel
parfor exp_index = 1:exp_num
  X = randn(n,p);
  mu = zeros(p,1);
  select_index = randperm(p);
  select_index = select_index(1:p_star);
  mu(select_index) = normrnd(0,1, [1,p_star]);

  pi = (1./(1+exp(-X * mu)));
  y = binornd(1,pi);
  final_error_matrix{exp_index} = comparison_gd(X, y, mu, n, p, iter_max, lambda, alpha_0);
end
toc(program_time_start);

final_error = final_error_matrix{1};
for exp_index = 2:exp_num
  final_error = final_error_matrix{exp_index} + final_error;
end
final_error = final_error/exp_num;
% summary the results
NS_err = zeros( size(final_error,1), exp_num );
IJ_err = zeros( size(final_error,1), exp_num );
IACV_err = zeros( size(final_error,1), exp_num );
Base_err = zeros( size(final_error,1), exp_num );

IACV_time = zeros( size(final_error,1), exp_num );
exact_cv_time = zeros( size(final_error,1), exp_num );

NS_est_err = zeros( size(final_error,1), exp_num );
IJ_est_err = zeros( size(final_error,1), exp_num );
IACV_est_err = zeros( size(final_error,1), exp_num );
Base_est_err = zeros( size(final_error,1), exp_num );

for exp_index = 1: exp_num
  NS_err(:,exp_index) = abs(final_error_matrix{exp_index}(:,12) - final_error_matrix{exp_index}(:,10));
  IJ_err(:,exp_index) = abs(final_error_matrix{exp_index}(:,13) - final_error_matrix{exp_index}(:,10));
  IACV_err(:,exp_index) = abs(final_error_matrix{exp_index}(:,11) - final_error_matrix{exp_index}(:,10));
  Base_err(:,exp_index) = abs(final_error_matrix{exp_index}(:,14) - final_error_matrix{exp_index}(:,10));
  NS_est_err(:,exp_index) = final_error_matrix{exp_index}(:,6);
  IJ_est_err(:,exp_index) = final_error_matrix{exp_index}(:,7);
  IACV_est_err(:,exp_index) = final_error_matrix{exp_index}(:,5);
  Base_est_err(:,exp_index) = final_error_matrix{exp_index}(:,8);
  IACV_time(:,exp_index) = final_error_matrix{exp_index}(:,3);
  exact_cv_time(:,exp_index) = final_error_matrix{exp_index}(:,4);
end

final_error = [final_error(:, [1,2,9,10,11,12,13,14]), median(IACV_time,2), median(exact_cv_time,2), quantile(IACV_time,[0.25,0.75],2)  , quantile(exact_cv_time,[0.25,0.75],2), median(NS_est_err,2), median(IJ_est_err,2), median(IACV_est_err,2), median(Base_est_err,2), quantile(NS_est_err,[0.25,0.75],2), quantile(IJ_est_err, [0.25, 0.75 ],2), quantile(IACV_est_err,[0.25,0.75],2), quantile(Base_est_err,[0.25,0.75],2),  median(NS_err,2), median(IJ_err,2), median(IACV_err,2), median(Base_err,2), quantile(NS_err,[0.25,0.75],2), quantile(IJ_err, [0.25, 0.75 ],2), quantile(IACV_err,[0.25,0.75],2), quantile(Base_err,[0.25,0.75],2) ];

% output
filename = join(["GD_experiment","p", join(string(p),'_'), 'p_star', p_star, "n", join(string(n), '_'),"iter_max",  join(string(iter_max),'_'), "lambda", join(string(lambda),'-'), "alpha0", alpha_0, 'exp_num', exp_num, 'seed',seed  ],'_');
colNames = {'iter','mu_t_est_err', 'grad_norm','CV', 'IACV_CV', 'NS_CV', 'IJ_CV','base_CV', 'IACV_time', 'exact_cv_time', 'IACV_time_q1', 'IACV_time_q2', 'exact_cv_time_q1', 'exact_cv_time_q2', 'mu_t_NS_err', 'mu_t_IJ_err','mu_t_IACV_err', 'mu_t_base_err','mu_t_NS_q1','mu_t_NS_q2', 'mu_t_IJ_q1', 'mu_t_IJ_q2', 'mu_t_IACV_q1', 'mu_t_IACV_q2', 'mu_t_base_q1','mu_t_base_q2', 'NS_CV_err', 'IJ_CV_err', 'IACV_CV_err', 'Base_CV_err', 'NS_CV_err_q1', 'NS_CV_err_q2' 'IJ_CV_err_q1', 'IJ_CV_err_q2', 'IACV_CV_err_q1', 'IACV_CV_err_q2', 'Base_CV_err_q1', 'Base_CV_err_q2'};

sTable = array2table(final_error, 'VariableNames', colNames);
sTable( (1:99:iter_max),:)
writetable(sTable, join(['../results/', filename,'.csv'], ''));
