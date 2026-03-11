#required variables from analysis: https://github.com/bethan-mallabar-rimmer/CRC_IRM/blob/main/analysis.R
#p_40_ve from Section 6

#FIGURE 3
#========
Get CRC incidence rate by GRS quintile
#===========================================
#Takes a binary variable and returns a proportionality test to find the confidence interval. Returns it as a string (adapted from HG's code)
binaryci <- function(binarylist){
  binarylist=binarylist[!is.na(binarylist)]
  test1=100*prop.test(sum(binarylist), length(binarylist), conf.level=0.95)$conf.int[c(1,2)]%>%as.numeric()%>%signif(4)
  test2=100*prop.test(sum(binarylist), length(binarylist), conf.level=0.95)$estimate%>%as.numeric()%>%signif(4)
  return(paste(as.character(test2),'% (',as.character(test1[1]),'%-',as.character(test1[2]),'%)',sep=''))
}

#get incidence rate for top and bottom quintile:
top_quin_inc_40e <- binaryci(p_40_ve$case[p_v$p_40_ve$grs_quintile==5])
bottom_quin_inc_40e <- binaryci(p_40_ve$case[p_v$p_40_ve$grs_quintile==1])

#6C. Cox proportional hazards modelling and survival curve
#==========================================================
#IMPORTANT: This section needs to be run on an up-to-date version of R if possible (as of 2023/24), otherwise package dependencies etc. cause lots of errors.

#Required packages
#==================
#install.packages('survminer')
#install.packages('ggpubr')
library(ggplot2)
library(dplyr)
library(ggpubr)
library(survminer)

#Create dataframe for survival analysis
#-----------------------------------------
x <- 2 #number of years to measure CRC incidence over
quin_df <- data.frame(grs_quintile = c(5,4,3,2,1)) #a dataframe containing 1 column with numbers 5-1 in rows

make_survframe <- function(p_xx_v) {
  #copy participant dataframe:
  rainbow_40 <- p_xx_v
  rainbow_40$death_t <- lubridate::time_length(difftime(rainbow_40$death_date, rainbow_40$sym_date),"days") #death_t = time between first symptom and death
  rainbow_40$t <- lubridate::time_length(difftime(rainbow_40$crc_date, rainbow_40$sym_date), "days") #t = time between first symptom and cancer
  
  #make dataframe for cumulative hazards plot
  survframe<- rainbow_40 %>% select('eid','sym_age','crc_age','grs','grs_quintile','zscore','death_t','t')
  survframe <- survframe %>% mutate(status=!is.na(crc_age)) #status is TRUE if they ever had CRC and FALSE if they didn't.
  survframe$t[is.na(survframe$t)] <- 729.5 #if participants did not get cancer, set t = 729.5 (mean length in days of 2 years)
  survframe$t[survframe$t>=729.5] <- 729.5 #if participants got cancer >=2 years after symptom then this is outside the time period I want to plot, set t = 2.
  survframe$t <- pmin(survframe$t,survframe$death_t,na.rm=TRUE) #set t to the minimum of t and the death date. t will then be: 729.5, if they lived 2 years cancer free OR time between symptom and cancer diagnosis date if they got cancer OR or time between symptom and death
  survframe$status[survframe$t>=729.5] <- FALSE #status currently = TRUE for anyone who got CRC. change to false if they got CRC after 2 years
  survframe$status <- as.integer(survframe$status) #change TRUE and FALSE in status to 1 and 0
  
  return(survframe)
}

survframe_e <- make_survframe(p_40_ve)

#Run Cox PH model
#---------------
library(survival)
res.cox_e <- coxph(Surv(t, status) ~ grs_quintile, data =  survframe_e)

#format Cox proportional hazards model for cumulative plot:
fit_stuff <- function(res.cox_x) {
  fit <- survfit(res.cox_x, newdata = quin_df)
  #turn 'upside down' for the plot (otherwise it plots highest GRS quintile in the lowest band on the graph)
  fit2 <- fit
  fit2$surv <- 1-fit2$surv
  fit2$lower <- 1-fit2$lower
  fit2$upper <- 1-fit2$upper
  return(fit2)
}

fit_e <- fit_stuff(res.cox_e)


#Plot cumulative hazards/surivival curve
#=======================================
rainbow_plot <- function(survframe_x, fit_x) {
  rainbow_surv <- ggsurvplot(fit_x, xlim = c(0, max(survframe_x$t)), conf.int = TRUE,
                             legend.labs=c("5","4","3","2","1"),
                             risk.table = TRUE, data=quin_df,
                             legend.title='PRS quintile:',
                             palette=c('#FF0018','#FFA52C','#008018','#0000F9','#86007D'),
                             ggtheme=HGtheme, ylab='CRC incidence rate', xlab='Time since first CRC symptom (years)')
  rainbow_surv$data.survplot$surv <- 1-rainbow_surv$data.survplot$surv
  rainbow_surv$data.survplot$lower <- 1-rainbow_surv$data.survplot$lower
  rainbow_surv$data.survplot$upper <- 1-rainbow_surv$data.survplot$upper
  rainbow_surv$plot$data$surv <- 1-rainbow_surv$plot$data$surv
  rainbow_surv$plot$data$lower <- 1-rainbow_surv$plot$data$lower
  rainbow_surv$plot$data$upper <- 1-rainbow_surv$plot$data$upper
  rainbow_surv$plot <- rainbow_surv$plot + scale_y_continuous(breaks = c(0,0.005, 0.01, 0.015,
                                                                         0.02), limits=c(0, 0.025),
                                                              labels = scales::percent) +
    scale_x_continuous(breaks = c(0, 364.75, 729.5), labels = c(0,1,2), expand = c(0.05,0.05,0.02,0.05))
  return(rainbow_surv$plot)
}

rainbow_plot_e <- rainbow_plot(survframe_e, fit_e)

rainbow_plot_e <- rainbow_plot_e + theme(axis.title = element_text(family='Helvetica'),
                                         axis.text = element_text(family='Helvetica'),
                                         legend.text = element_text(family='Helvetica'),
                                         legend.title = element_text(family='Helvetica'),
                                         legend.position = c(.71,.925),
                                         legend.direction = 'horizontal') +
  guides(fill = guide_legend(reverse = TRUE),
         colour = guide_legend(reverse = TRUE))

rainbow_plot_e
