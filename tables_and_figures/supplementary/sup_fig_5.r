#required variables from analysis: https://github.com/bethan-mallabar-rimmer/CRC_IRM/blob/main/analysis.R
#p_v from Section 6

library(dplyr)
library(ggplot)

HGtheme=theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

#find ROCAUC of model with and without PRS:
#install.packages("pROC")
library(pROC)
rocauc=function(form,dataframe,pt){
  logit <- glm(form, data = dataframe, family = "binomial")
  prob = predict(logit, newdata = dataframe, type = "response")
  if (pt == TRUE) {
    roc = roc(dataframe$case ~ prob, plot = TRUE, print.auc = TRUE, ci=TRUE)
  } else {
    roc = roc(dataframe$case ~ prob, plot = FALSE, print.auc = TRUE, ci=TRUE)}
  return(roc)
}
rocauc('case~sym_age+abdo_pain+grs+sex+rectal_bloodloss+change_bowel_habit', p_v$p_40_ve, TRUE) #with
rocauc('case~sym_age+abdo_pain+sex+rectal_bloodloss+change_bowel_habit', p_v$p_40_ve, TRUE) #without

#input values into plot:
boxval_40 <- data.frame(x=c("IRM","IRM\nexcluding PRS"), min=c(0.78,0.76), low=c(0.78,0.76), mid=c(0.8 ,0.78), top=c(0.81,0.8), max=c(0.81,0.8))

boxplot <- ggplot(boxval_40, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity") + ylim(c(0.75, 0.85)) + ylab("ROCAUC") + xlab("Risk model with and without PRS") + HGtheme
