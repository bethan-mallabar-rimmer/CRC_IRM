#required variables from analysis: https://github.com/bethan-mallabar-rimmer/CRC_IRM/blob/main/analysis.R
#rocauc_irm_40e_test and rocauc_irm_40e_train from Section 9

library(dplyr)
library(ggplot2)

#FIGURE 2
#========
rocauc_frame <- function(rocauc_irm) {
  rocaucs_list <- c(0.5)
  UB_list <- c(0.5)
  LB_list <- c(0.5)
  for (i in 1:length(rocauc_irm)) {
    rocaucs_list <- c(rocaucs_list, rocauc_irm[[i]]$ROC)
    UB_list <- c(UB_list, rocauc_irm[[i]]$U95)
    LB_list <- c(LB_list, rocauc_irm[[i]]$L95)
  }
  aucs <- data.frame(aucs = rocaucs_list,
                     var = c(0:length(rocauc_irm)),
                     U95 = UB_list,
                     L95 = LB_list)
  return(aucs)
}

rocauc_frame_40e_test <- rocauc_frame(rocauc_irm_test$p_40_ve)
rocauc_frame_40e_train <- rocauc_frame(rocauc_irm_train$p_40_ve)


#plot:
#----
#graph theme by Harry Green:
HGtheme=theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))
#plot iterative rocauc
rocauc_plot_e_train <- ggplot(data=rocauc_frame_40e_train, aes(x=var, y=aucs, ymin=L95, ymax=U95)) +
  geom_point(colour = '#00C896', size=3) + geom_line(linetype = "dashed") + geom_errorbar(width=0.1) +
  HGtheme + xlab("variables added to integrated risk model") + ylab("ROC AUC") +
  scale_x_continuous(breaks=0:length(rocauc_irm_train$p_40_ve), labels=c("none",names(rocauc_irm_train$p_40_ve))) +
  scale_y_continuous(breaks=seq(0.5,0.9,by=0.05)) + theme(plot.margin = unit(c(5.5, 5.5, 20, 30), "points"))

rocauc_plot_e_test <- ggplot(data=rocauc_frame_40e_test, aes(x=var, y=aucs, ymin=L95, ymax=U95)) +
  geom_point(colour = '#00C896', size=3) + geom_line(linetype = "dashed") + geom_errorbar(width=0.1) +
  HGtheme + xlab("variables added to integrated risk model") + ylab("ROC AUC") +
  scale_x_continuous(breaks=0:length(rocauc_irm_test$p_40_ve), labels=c("none",names(rocauc_irm_test$p_40_ve))) +
  scale_y_continuous(breaks=seq(0.5,0.9,by=0.05)) + theme(plot.margin = unit(c(5.5, 5.5, 20, 30), "points"))

rocauc_plot_e_train
rocauc_plot_e_test
