#required variables from analysis: https://github.com/bethan-mallabar-rimmer/CRC_IRM/blob/main/analysis.R
#rocauc_irm from Section 8

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
    UB_list <- c(UB_list, rocauc_irm[[i]]$UB)
    LB_list <- c(LB_list, rocauc_irm[[i]]$LB)
  }
  aucs <- data.frame(aucs = rocaucs_list,
                     var = c(0:length(rocauc_irm)),
                     UB = UB_list,
                     LB = LB_list)
  return(aucs)
}

rocauc_frame_40 <- rocauc_frame(rocauc_irm$p_40_ve)

#plot:
#----
#graph theme by Harry Green:
HGtheme=theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

rocauc_plot_40 <- ggplot(data=rocauc_frame_40, aes(x=var, y=aucs, ymin=LB, ymax=UB)) +
  geom_point(colour = 'black', size=1) + geom_line(linetype = "dashed") + geom_errorbar(width=0.1) +
  HGtheme + xlab("Variables added to integrated risk model") + ylab("ROCAUC") +
  scale_x_continuous(breaks=0:8, labels=c("none","age at\nfirst symptom", "& abdominal\npain",
                                          "& PRS","& sex","& ever\nsmoked","& rectal\nbloodloss",
                                          "& waist\ncircumference",
                                          "& change \nin bowel \nhabits")) +
  scale_y_continuous(breaks=seq(0.5,0.85,by=0.02)) + theme(plot.margin = unit(c(5.5, 5.5, 5.5, 5.5), "points"),
                                                           axis.text = element_text(size=8, family='Helvetica'),
                                                           axis.title = element_text(family='Helvetica'),
                                                           panel.spacing = unit(0,'cm'))

rocauc_plot_40
