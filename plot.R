########## plot the results.
library(ggplot2)
library(grid)
library(gridExtra)
library(patchwork)
library(latex2exp)
library(plyr)
library(tidyverse)
library(ggpubr)

# GD case
Xgd.0.1 <- read.csv('GD_experiment_p_20_p_star_5_n_250_iter_max_7000_lambda_0.00025_alpha0_0.002_exp_num_100_seed_2030.csv', header = T)
xbreaks = c(1,100, 10000)
xlabels = c('1',TeX('$10^2$'),TeX('$10^4$'))
ybreaks = c(0.1,0.001, 0.00001,0.0000001)
ylabels = c(TeX('1e-1'),TeX('1e-3'), TeX('1e-5'), TeX('1e-7') )
p0.1 = ggplot(Xgd.0.1[c(seq(from = 1, to = 100, by = 1),seq(from = 101, to = nrow(Xgd.0.1), by = 10) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(1,10000)) +
  geom_line(aes(y = abs(IACV_CV_err)/CV, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = IACV_CV_err_q1/CV , ymax = IACV_CV_err_q2/CV, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(NS_CV_err)/CV, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = NS_CV_err_q1/CV , ymax = NS_CV_err_q2/CV, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(IJ_CV_err)/CV, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = IJ_CV_err_q1/CV , ymax = IJ_CV_err_q2/CV, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(Base_CV_err)/CV, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = Base_CV_err_q1/CV , ymax = Base_CV_err_q2/CV, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num (log scale)", y = "Model Assess Error") + 
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5),legend.position = 'none' )  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"), breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                                                                      labels = c('Baseline','NS','IJ', "IACV")) +
 scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"), breaks = c('Baseline','NS','IJ', "IACV"),
                   labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                   colour=guide_legend(keywidth = 3, keyheight = 1))
p0.1

Xgd.0.2 <- read.csv('GD_experiment_median_p_20_p_star_5_n_1000_iter_max_7000_lambda_0.001_alpha0_0.0005_exp_num_100_seed_2030.csv', header = T)
xbreaks = c(1,100,10000)
xlabels = c('1',TeX('$10^2$'),TeX('$10^4$'))
ybreaks = c(0.1,0.001, 0.00001,0.0000001)
ylabels = c(TeX('1e-1'),TeX('1e-3'), TeX('1e-5'), TeX('1e-7') )
p0.2 = ggplot(Xgd.0.2[c(seq(from = 1, to = 100, by = 1),seq(from = 101, to = nrow(Xgd.0.1), by = 10) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels) + scale_x_log10(breaks = xbreaks, labels = xlabels) +
  geom_line(aes(y = abs(IACV_CV_err)/CV, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = IACV_CV_err_q1/CV , ymax = IACV_CV_err_q2/CV, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(NS_CV_err)/CV, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = NS_CV_err_q1/CV , ymax = NS_CV_err_q2/CV, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(IJ_CV_err)/CV, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = IJ_CV_err_q1/CV , ymax = IJ_CV_err_q2/CV, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(Base_CV_err)/CV, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = Base_CV_err_q1/CV , ymax = Base_CV_err_q2/CV, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num (log scale)", y = "") + 
  theme( text = element_text(size=31), plot.title = element_text(size=32,hjust = 0.5), axis.title.y = element_blank() )  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                              labels = c('Baseline','NS','IJ', "IACV")) +
  scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                    labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                           colour=guide_legend(keywidth = 3, keyheight = 1))
p0.1 + p0.2

Xgd.0.1$mu_t_IACV_err[nrow(Xgd.0.1 )]
Xgd.0.2$mu_t_IACV_err[nrow(Xgd.0.2 )]

shift = 5
p0.3 = ggplot(Xgd.0.1[c(seq(from = shift, to = 100, by = 1),seq(from = 101, to = nrow(Xgd.0.1), by = 10) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(shift,10000)) +
  geom_line(aes(y = mu_t_IACV_err, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IACV_q1, ymax = mu_t_IACV_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_NS_err, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = mu_t_NS_q1 , ymax = mu_t_NS_q2, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_IJ_err, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IJ_q1 , ymax = mu_t_IJ_q2, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_base_err, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = mu_t_base_q1 , ymax = mu_t_base_q2, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="", y = "Approximation Error") + ggtitle('n = 250') +
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5), legend.position = 'none' )  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                                                                labels = c('Baseline','NS','IJ', "IACV")) +
  scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                    labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                           colour=guide_legend(keywidth = 3, keyheight = 1))

p0.4 = ggplot(Xgd.0.2[c(seq(from = shift, to = 100, by = 1),seq(from = 101, to = nrow(Xgd.0.2), by = 10) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(shift,10000)) +
  geom_line(aes(y = mu_t_IACV_err, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IACV_q1, ymax = mu_t_IACV_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_NS_err, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = mu_t_NS_q1 , ymax = mu_t_NS_q2, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_IJ_err, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IJ_q1 , ymax = mu_t_IJ_q2, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_base_err, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = mu_t_base_q1 , ymax = mu_t_base_q2, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="", y = "") + ggtitle('n = 1000') +
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5), legend.position = 'none' )  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                                                                labels = c('Baseline','NS','IJ', "IACV")) +
  scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                    labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                           colour=guide_legend(keywidth = 3, keyheight = 1))


p0.3 + p0.4


xbreaks <- c(3000, 6000)
xlabels <- c(TeX('$3 \\times 10^3$'),TeX('$6 \\times 10^3$'))
p0.5 = ggplot(Xgd.0.1[seq(from = 1, to = nrow(Xgd.0.1), by = 10), ], aes(x = iter)) + 
  geom_line(aes(y = IACV_time, colour = "IACV", linetype = 'IACV'),size=1.2)+ 
  geom_ribbon(aes( ymin = IACV_time_q1, ymax = IACV_time_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = exact_cv_time, colour = "Exact CV", linetype = 'Exact CV'),  size=1.2) +
  geom_ribbon(aes( ymin = exact_cv_time_q1, ymax = exact_cv_time_q2, color = 'Exact CV',fill = 'Exact CV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num", y = "Runtime (s)") + 
  scale_x_continuous( breaks = xbreaks, labels = xlabels) +
  theme( text = element_text(size=31),plot.title = element_text(size=34,hjust = 0.5),axis.title.y = element_text(size =30) , legend.position = "none")  + scale_color_manual(name = 'Method',values = c('purple', "black"),
                                                                                                                                                                             labels = c('Exact CV',"IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid"),
                        labels = c('Exact CV',"IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                colour=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_fill_manual(name = 'Method',values = c('purple', "black"),
                    labels = c('Exact CV',"IACV"))

p0.6 = ggplot(Xgd.0.2[seq(from = 1, to = nrow(Xgd.0.2), by = 10), ], aes(x = iter)) + 
  geom_line(aes(y = IACV_time, colour = "IACV", linetype = 'IACV'),size=1.2)+ 
  geom_ribbon(aes( ymin = IACV_time_q1, ymax = IACV_time_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = exact_cv_time, colour = "Exact CV", linetype = 'Exact CV'),  size=1.2) +
  geom_ribbon(aes( ymin = exact_cv_time_q1, ymax = exact_cv_time_q2, color = 'Exact CV',fill = 'Exact CV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num", y = "") + 
 scale_x_continuous( breaks = xbreaks, labels = xlabels) +
  theme( text = element_text(size=31),plot.title = element_text(size=34,hjust = 0.5),axis.title.y = element_text(size =30) , legend.position = "none")  + scale_color_manual(name = 'Method',values = c('purple', "black"),
                                                                                                                                                                             labels = c('Exact CV',"IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid"),
                        labels = c('Exact CV',"IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                colour=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_fill_manual(name = 'Method',values = c('purple', "black"),
                    labels = c('Exact CV',"IACV"))

p0.5 + p0.6

p_gd = ggarrange(p0.3, p0.4, p0.1,  p0.2, ncol = 2, nrow = 2,common.legend = TRUE, legend="right", align = 'v', heights = c(5.2,4.7))
p_time = ggarrange(p0.5, p0.6, ncol = 2, nrow = 1,common.legend = TRUE, legend="right")
p_gd_combine = ggarrange( p_gd,p_time, ncol = 1, nrow = 2,  heights = c(6,2.7), align = 'v')


############ Proximal GD case
Xpgd.1 <- read.csv('Prox_GD_experiement_p_20_p_star_5_n_250_iter_max_7000_lambda_0.00025_step-size_0.002_exp_num_24.csv', header = T)

xbreaks = c(1,100, 10000)
xlabels = c('1',TeX('$10^2$'),TeX('$10^4$'))
ybreaks = c(0.1,0.0001,0.0000001,0.0000000001)
ylabels = c(TeX('1e-1'),TeX('1e-4'), TeX('1e-7'), TeX('1e-10') )
pgd.1 = ggplot(Xpgd.1[c(seq(from = 1, to = 100, by = 1),seq(from = 101, to = nrow(Xpgd.1), by = 10) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(1,10000)) +
  geom_line(aes(y = abs(IACV_CV_err)/CV, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = IACV_CV_err_q1/CV , ymax = IACV_CV_err_q2/CV, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(NS_CV_err)/CV, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = NS_CV_err_q1/CV , ymax = NS_CV_err_q2/CV, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(IJ_CV_err)/CV, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = IJ_CV_err_q1/CV , ymax = IJ_CV_err_q2/CV, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(Base_CV_err)/CV, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = Base_CV_err_q1/CV , ymax = Base_CV_err_q2/CV, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num (log scale)", y = "Model Assess Error") + 
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5), legend.position = 'none' )  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                                                                labels = c('Baseline','NS','IJ', "IACV")) +
  scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                    labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                           colour=guide_legend(keywidth = 3, keyheight = 1))
pgd.1



Xpgd.2 <- read.csv('Prox_GD_experiement_p_20_p_star_5_n_1000_iter_max_7000_lambda_0.001_step-size_0.0005_exp_num_100.csv', header = T)

xbreaks = c(1,100, 10000)
xlabels = c('1',TeX('$10^2$'),TeX('$10^4$'))
ybreaks = c(0.1,0.0001,0.0000001,0.0000000001)
ylabels = c(TeX('1e-1'),TeX('1e-4'), TeX('1e-7'), TeX('1e-10') )
pgd.2 = ggplot(Xpgd.2[c(seq(from = 1, to = 100, by = 1),seq(from = 101, to = nrow(Xpgd.2), by = 10) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(1,10000)) +
  geom_line(aes(y = abs(IACV_CV_err)/CV, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = IACV_CV_err_q1/CV , ymax = IACV_CV_err_q2/CV, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(NS_CV_err)/CV, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = NS_CV_err_q1/CV , ymax = NS_CV_err_q2/CV, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(IJ_CV_err)/CV, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = IJ_CV_err_q1/CV , ymax = IJ_CV_err_q2/CV, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(Base_CV_err)/CV, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = Base_CV_err_q1/CV , ymax = Base_CV_err_q2/CV, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num (log scale)", y = "") +
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5))  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                                     labels = c('Baseline','NS','IJ', "IACV")) +
  scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                    labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                           colour=guide_legend(keywidth = 3, keyheight = 1))
pgd.2


shift = 5
pgd.3 = ggplot(Xpgd.1[c(seq(from = shift, to = 100, by = 1),seq(from = 101, to = nrow(Xpgd.1), by = 10) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(shift,10000)) +
  geom_line(aes(y = mu_t_IACV_err, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IACV_q1, ymax = mu_t_IACV_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_NS_err, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = mu_t_NS_q1 , ymax = mu_t_NS_q2, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_IJ_err, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IJ_q1 , ymax = mu_t_IJ_q2, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_base_err, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = mu_t_base_q1 , ymax = mu_t_base_q2, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="", y = "Approximation Error") + ggtitle('n = 250') +
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5), legend.position = 'none' )  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                                                                labels = c('Baseline','NS','IJ', "IACV")) +
  scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                    labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                           colour=guide_legend(keywidth = 3, keyheight = 1))

pgd.4 = ggplot(Xpgd.2[c(seq(from = shift, to = 100, by = 1),seq(from = 101, to = nrow(Xpgd.2), by = 10) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(shift,10000)) +
  geom_line(aes(y = mu_t_IACV_err, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IACV_q1, ymax = mu_t_IACV_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_NS_err, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = mu_t_NS_q1 , ymax = mu_t_NS_q2, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_IJ_err, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IJ_q1 , ymax = mu_t_IJ_q2, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_base_err, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = mu_t_base_q1 , ymax = mu_t_base_q2, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="", y = "") + ggtitle('n = 1000') +
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5), legend.position = 'none' )  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                                                                labels = c('Baseline','NS','IJ', "IACV")) +
  scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                    labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                           colour=guide_legend(keywidth = 3, keyheight = 1))


xbreaks <- c(3000, 6000)
xlabels <- c(TeX('$3 \\times 10^3$'),TeX('$6 \\times 10^3$'))
pgd.5 = ggplot(Xpgd.1[seq(from = 1, to = nrow(Xpgd.1), by = 10), ], aes(x = iter)) + 
  geom_line(aes(y = IACV_time, colour = "IACV", linetype = 'IACV'),size=1.2)+ 
  geom_ribbon(aes( ymin = IACV_time_q1, ymax = IACV_time_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = exact_cv_time, colour = "Exact CV", linetype = 'Exact CV'),  size=1.2) +
  geom_ribbon(aes( ymin = exact_cv_time_q1, ymax = exact_cv_time_q2, color = 'Exact CV',fill = 'Exact CV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num", y = "Runtime (s)") + 
  scale_x_continuous( breaks = xbreaks, labels = xlabels) +
  theme( text = element_text(size=31),plot.title = element_text(size=34,hjust = 0.5),axis.title.y = element_text(size =30) , legend.position = "none")  + scale_color_manual(name = 'Method',values = c('purple', "black"),
                                                                                                                                                                             labels = c('Exact CV',"IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid"),
                        labels = c('Exact CV',"IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                colour=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_fill_manual(name = 'Method',values = c('purple', "black"),
                    labels = c('Exact CV',"IACV"))

pgd.6 = ggplot(Xpgd.2[seq(from = 1, to = nrow(Xpgd.2), by = 10), ], aes(x = iter)) + 
  geom_line(aes(y = IACV_time, colour = "IACV", linetype = 'IACV'),size=1.2)+ 
  geom_ribbon(aes( ymin = IACV_time_q1, ymax = IACV_time_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = exact_cv_time, colour = "Exact CV", linetype = 'Exact CV'),  size=1.2) +
  geom_ribbon(aes( ymin = exact_cv_time_q1, ymax = exact_cv_time_q2, color = 'Exact CV',fill = 'Exact CV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num", y = "") + 
  scale_x_continuous( breaks = xbreaks, labels = xlabels) +
  theme( text = element_text(size=31),plot.title = element_text(size=34,hjust = 0.5),axis.title.y = element_text(size =30) , legend.position = "none")  + scale_color_manual(name = 'Method',values = c('purple', "black"),
                                                                                                                                                                             labels = c('Exact CV',"IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid"),
                        labels = c('Exact CV',"IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                colour=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_fill_manual(name = 'Method',values = c('purple', "black"),
                    labels = c('Exact CV',"IACV"))

pgd.5 + pgd.6

p_pgd = ggarrange(pgd.3, pgd.4, pgd.1,  pgd.2, ncol = 2, nrow = 2,common.legend = TRUE, legend="right", align = 'v', heights = c(5.2,4.7))
pgd_time = ggarrange(pgd.5, pgd.6, ncol = 2, nrow = 1,common.legend = TRUE, legend="right")
p_pgd_combine = ggarrange( p_pgd,pgd_time, ncol = 1, nrow = 2,  heights = c(6,2.7), align = 'v')









## SGD case
Xsgd.0 <- read.csv('SGD_experiement_p_20_p_star_5_n_1000_iter_max_100000_lambda_0.001_batch_size_100_step-size_epoch-doubling_alpha0_0.005_exp_num_100.csv', header = T)
xbreaks = c(10, 1000, 100000)
xlabels = c('10',TeX('$10^3$'),TeX('$10^5$') )
ybreaks = c(0.1,0.001,0.00001,0.0000001)
ylabels = c('1e-1', '1e-3', '1e-5','1e-7')
sgd.1 = ggplot(Xsgd.0[c(seq(from = 1, to = 100, by = 1),seq(from = 101, to = nrow(Xsgd.0), by = 50) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(1,100000)) +
  geom_line(aes(y = abs(IACV_CV_err)/CV, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = IACV_CV_err_q1/CV , ymax = IACV_CV_err_q2/CV, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(NS_CV_err)/CV, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = NS_CV_err_q1/CV , ymax = NS_CV_err_q2/CV, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(IJ_CV_err)/CV, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = IJ_CV_err_q1/CV , ymax = IJ_CV_err_q2/CV, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(Base_CV_err)/CV, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = Base_CV_err_q1/CV , ymax = Base_CV_err_q2/CV, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num (log scale)", y = "Model Assess Error") + 
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5), legend.position = 'none' )  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                                                                labels = c('Baseline','NS','IJ', "IACV")) +
  scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                    labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                           colour=guide_legend(keywidth = 3, keyheight = 1))
sgd.1



Xsgd.1 <- read.csv('SGD_experiement_p_20_p_star_5_n_1000_iter_max_100000_lambda_0.004_batch_size_400_step-size_epoch-doubling_alpha0_0.00125_exp_num_100.csv', header = T)
sgd.2 = ggplot(Xsgd.1[c(seq(from = 1, to = 100, by = 1),seq(from = 101, to = nrow(Xsgd.1), by = 50) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(1,100000)) +
  geom_line(aes(y = abs(IACV_CV_err)/CV, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = IACV_CV_err_q1/CV , ymax = IACV_CV_err_q2/CV, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(NS_CV_err)/CV, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = NS_CV_err_q1/CV , ymax = NS_CV_err_q2/CV, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(IJ_CV_err)/CV, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = IJ_CV_err_q1/CV , ymax = IJ_CV_err_q2/CV, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(Base_CV_err)/CV, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = Base_CV_err_q1/CV , ymax = Base_CV_err_q2/CV, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num (log scale)", y = "") +
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5) )  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                                      labels = c('Baseline','NS','IJ', "IACV")) +
  scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                    labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                           colour=guide_legend(keywidth = 3, keyheight = 1))
sgd.2



shift = 5
sgd.3 = ggplot(Xsgd.0[c(seq(from = shift, to = 100, by = 1),seq(from = 101, to = nrow(Xsgd.0), by = 50) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(shift,100000)) +
  geom_line(aes(y = mu_t_IACV_err, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IACV_q1, ymax = mu_t_IACV_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_NS_err, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = mu_t_NS_q1 , ymax = mu_t_NS_q2, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_IJ_err, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IJ_q1 , ymax = mu_t_IJ_q2, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_base_err, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = mu_t_base_q1 , ymax = mu_t_base_q2, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="", y = "Approximation Error") + ggtitle('K = 100') +
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5), legend.position = 'none' )  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                                                                labels = c('Baseline','NS','IJ', "IACV")) +
  scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                    labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                           colour=guide_legend(keywidth = 3, keyheight = 1))

sgd.4 = ggplot(Xsgd.1[c(seq(from = shift, to = 100, by = 1),seq(from = 101, to = nrow(Xsgd.1), by = 50) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(shift,100000)) +
  geom_line(aes(y = mu_t_IACV_err, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IACV_q1, ymax = mu_t_IACV_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_NS_err, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = mu_t_NS_q1 , ymax = mu_t_NS_q2, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_IJ_err, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = mu_t_IJ_q1 , ymax = mu_t_IJ_q2, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = mu_t_base_err, colour = "Baseline", linetype = 'Baseline'),  size=1.2)+
  geom_ribbon(aes( ymin = mu_t_base_q1 , ymax = mu_t_base_q2, color = 'Baseline',fill = 'Baseline'),alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="", y = "") + ggtitle('K = 400') +
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5), legend.position = 'none' )  + scale_color_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                                                                                                                                                                                labels = c('Baseline','NS','IJ', "IACV")) +
  scale_fill_manual(name = 'Method',values = c('Baseline' = "#00BA38",'NS' = "#F8766D", 'IJ' = '#619CFF', "IACV" = "black"),breaks = c('Baseline','NS','IJ', "IACV"),
                    labels = c('Baseline','NS','IJ', "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid","solid", "solid"),
                        labels = c('Baseline','NS','IJ', "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                           colour=guide_legend(keywidth = 3, keyheight = 1))


xbreaks <- c(40000, 80000)
xlabels <- c(TeX('$4 \\times 10^4$'),TeX('$8 \\times 10^4$'))
sgd.5 = ggplot(Xsgd.0[seq(from = 1, to = nrow(Xsgd.0), by = 50), ], aes(x = iter)) + 
  geom_line(aes(y = IACV_time, colour = "IACV", linetype = 'IACV'),size=1.2)+ 
  geom_ribbon(aes( ymin = IACV_time_q1, ymax = IACV_time_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = exact_cv_time, colour = "Exact CV", linetype = 'Exact CV'),  size=1.2) +
  geom_ribbon(aes( ymin = exact_cv_time_q1, ymax = exact_cv_time_q2, color = 'Exact CV',fill = 'Exact CV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num", y = "Runtime (s)") + 
  scale_x_continuous( breaks = xbreaks, labels = xlabels) +
  theme( text = element_text(size=31),plot.title = element_text(size=34,hjust = 0.5),axis.title.y = element_text(size =30) , legend.position = "none")  + scale_color_manual(name = 'Method',values = c('purple', "black"),
                                                                                                                                                                             labels = c('Exact CV',"IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid"),
                        labels = c('Exact CV',"IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                colour=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_fill_manual(name = 'Method',values = c('purple', "black"),
                    labels = c('Exact CV',"IACV"))

sgd.6 = ggplot(Xsgd.1[seq(from = 1, to = nrow(Xsgd.1), by = 50), ], aes(x = iter)) + 
  geom_line(aes(y = IACV_time, colour = "IACV", linetype = 'IACV'),size=1.2)+ 
  geom_ribbon(aes( ymin = IACV_time_q1, ymax = IACV_time_q2, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = exact_cv_time, colour = "Exact CV", linetype = 'Exact CV'),  size=1.2) +
  geom_ribbon(aes( ymin = exact_cv_time_q1, ymax = exact_cv_time_q2, color = 'Exact CV',fill = 'Exact CV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num", y = "") + 
  scale_x_continuous( breaks = xbreaks, labels = xlabels) +
  theme( text = element_text(size=31),plot.title = element_text(size=34,hjust = 0.5),axis.title.y = element_text(size =30) , legend.position = "none")  + scale_color_manual(name = 'Method',values = c('purple', "black"),
                                                                                                                                                                             labels = c('Exact CV',"IACV")) +
  scale_linetype_manual(name = 'Method',values = c("dashed","solid"),
                        labels = c('Exact CV',"IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                                colour=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_fill_manual(name = 'Method',values = c('purple', "black"),
                    labels = c('Exact CV',"IACV"))

sgd.5 + sgd.6

p_sgd = ggarrange(sgd.3, sgd.4, sgd.1,  sgd.2, ncol = 2, nrow = 2,common.legend = TRUE, legend="right", align = 'v', heights = c(5.2,4.7))
sgd_time = ggarrange(sgd.5, sgd.6, ncol = 2, nrow = 1,common.legend = TRUE, legend="right")
p_sgd_combine = ggarrange( p_sgd,sgd_time, ncol = 1, nrow = 2,  heights = c(6,2.7), align = 'v')





# The plot in the Introduction.
xbreaks = c(1,100, 10000)
xlabels = c('1',TeX('$10^2$'),TeX('$10^4$'))
ybreaks = c(0.1,0.001, 0.00001,0.0000001)
ylabels = c(TeX('1e-1'),TeX('1e-3'), TeX('1e-5'), TeX('1e-7') )
pintro.1 = ggplot(Xgd.0.2[c(seq(from = 1, to = 100, by = 1),seq(from = 101, to = nrow(Xgd.0.2), by = 10) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(1,10000)) +
  geom_line(aes(y = abs(IACV_CV_err)/CV, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = IACV_CV_err_q1/CV , ymax = IACV_CV_err_q2/CV, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(NS_CV_err)/CV, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = NS_CV_err_q1/CV , ymax = NS_CV_err_q2/CV, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(IJ_CV_err)/CV, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = IJ_CV_err_q1/CV , ymax = IJ_CV_err_q2/CV, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num (log scale)", y = "Model Assess Error") + 
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5), legend.position = 'none' )  + scale_color_manual(name = 'Method',values = c( 'NS' ="#F8766D", 'IJ' =  '#619CFF',"IACV" = "black" ), breaks = c('NS','IJ','IACV'),
                                                                                                                                                                                labels = c("NS",
                                                                                                                                                                                           "IJ",
                                                                                                                                                                                           "IACV")) +
  scale_fill_manual(name = 'Method',values = c( 'NS' ="#F8766D", 'IJ' =  '#619CFF',"IACV" = "black" ), breaks = c('NS','IJ','IACV'),
                    labels = c("NS",
                               "IJ",
                               "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("solid","solid", "solid"), 
                        labels = c("NS",
                                   "IJ",
                                   "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                   colour=guide_legend(keywidth = 3, keyheight = 1))

pintro.2 = ggplot(Xsgd.1[c(seq(from = 1, to = 100, by = 1),seq(from = 101, to = nrow(Xsgd.1), by = 50) ) , ], aes(x = iter )) + scale_y_log10(breaks = ybreaks, labels = ylabels)  + scale_x_log10( breaks = xbreaks, labels = xlabels, limits = c(1,100000)) +
  geom_line(aes(y = abs(IACV_CV_err)/CV, colour = "IACV", linetype = 'IACV'),  size=1.2) +
  geom_ribbon(aes( ymin = IACV_CV_err_q1/CV , ymax = IACV_CV_err_q2/CV, color = 'IACV',fill = 'IACV'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(NS_CV_err)/CV, colour = "NS", linetype = 'NS'),  size=1.2) + 
  geom_ribbon(aes( ymin = NS_CV_err_q1/CV , ymax = NS_CV_err_q2/CV, color = 'NS',fill = 'NS'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  geom_line(aes(y = abs(IJ_CV_err)/CV, colour = "IJ", linetype = 'IJ'),  size=1.2) +
  geom_ribbon(aes( ymin = IJ_CV_err_q1/CV , ymax = IJ_CV_err_q2/CV, color = 'IJ',fill = 'IJ'), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  labs(x ="Iter Num (log scale)", y = "") +
  theme( text = element_text(size=31), axis.title.y = element_text(size =30) , plot.title = element_text(size=32,hjust = 0.5) )  + scale_color_manual(name = 'Method',values = c( 'NS' ="#F8766D", 'IJ' =  '#619CFF',"IACV" = "black" ), breaks = c('NS','IJ','IACV'),
                                                                                                                                                      labels = c("NS",
                                                                                                                                                                 "IJ",
                                                                                                                                                                 "IACV")) +
  scale_fill_manual(name = 'Method',values = c( 'NS' ="#F8766D", 'IJ' =  '#619CFF',"IACV" = "black" ), breaks = c('NS','IJ','IACV'),
                    labels = c("NS",
                               "IJ",
                               "IACV")) +
  scale_linetype_manual(name = 'Method',values = c("solid","solid", "solid"), 
                        labels = c("NS",
                                   "IJ",
                                   "IACV")) + guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
                                                   colour=guide_legend(keywidth = 3, keyheight = 1))

pintro.1 + pintro.2


