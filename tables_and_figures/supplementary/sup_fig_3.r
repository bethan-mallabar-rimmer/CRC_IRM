#required variables from analysis: https://github.com/bethan-mallabar-rimmer/CRC_IRM/blob/main/analysis.R
#p_v from Section 6

p_v_all <- data.frame()
cohorts <- c('Full Cohort (all participants age 40-79)','40-49 (all)',
             '50-59 (all)','60-69 (all)','70-79 (all)','40-79 (female)',
             '40-79 (male)','40-49 (female)','40-49 (male)',
             '50-59 (female)','50-59 (male)','60-69 (female)',
             '60-69 (male)','70-79 (female)','70-79 (male)')
for (i in c(1,6,7,2,8,9,3,10,11,4,12,13,5,14,15)) {
  p_v[[i]]$cohort <- cohorts[i]
  p_v_all <- rbind(p_v_all, p_v[[i]])
}
p_v_all$cohort <- factor(p_v_all$cohort, levels=rev(c('Full Cohort (all participants age 40-79)',
                                                     '40-79 (female)','40-79 (male)',
                                                     '40-49 (all)','40-49 (female)','40-49 (male)',
                                                     '50-59 (all)','50-59 (female)','50-59 (male)',
                                                     '60-69 (all)','60-69 (female)','60-69 (male)',
                                                     '70-79 (all)','70-79 (female)','70-79 (male)')))

#install.packages('ggridges')
#install.packages('RColorBrewer')
library(ggridges)
library(RColorBrewer)
library(ggplot2)

HGtheme=theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

sdvec <- function(x, ...) {
  meanv <- mean(x)
  sdv <- sd(x)
  c(meanv - sdv, meanv, meanv + sdv)
}

ggplot(p_v_all, aes(y = cohort)) +
  geom_density_ridges(
    aes(x = grs,
        scale = 0.75,
        linetype = paste(cohort, case),
        fill = paste(cohort, case)),
    alpha = .7, colour='black',
    from = -1, to = 3,
    quantile_lines = TRUE, quantile_fun=function(x, ...) mean(x)
  ) +
  scale_y_discrete(expand = c(0, 0), labels = rep(c('male','female','all'),5)) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_linetype_cyclical(values = c(1,2)) +
  scale_fill_cyclical(values = c("lightgrey","lightgrey")) +
  HGtheme + coord_cartesian(clip = "off") +
  ylab("Cohort") + xlab("PRS")


#by cohorts
p_v_mf <- p_v_all[p_v_all$cohort == 'Full Cohort (all participants age 40-79)' |
                  p_v_all$cohort == '40-49 (all)' |
                  p_v_all$cohort == '50-59 (all)' |
                  p_v_all$cohort == '60-69 (all)' |
                  p_v_all$cohort == '70-79 (all)',]

p_v_f <- p_v_all[p_v_all$cohort == '40-79 (female)' |
                   p_v_all$cohort == '40-49 (female)' |
                   p_v_all$cohort == '50-59 (female)' |
                   p_v_all$cohort == '60-69 (female)' |
                   p_v_all$cohort == '70-79 (female)',]

p_v_m <- p_v_all[p_v_all$cohort == '40-79 (male)' |
                   p_v_all$cohort == '40-49 (male)' |
                   p_v_all$cohort == '50-59 (male)' |
                   p_v_all$cohort == '60-69 (male)' |
                   p_v_all$cohort == '70-79 (male)',]

mixed_sex <- ggplot(p_v_mf, aes(y = cohort)) +
  geom_density_ridges(
    aes(x = grs,
        scale = 0.75,
        linetype = paste(cohort, case),
        fill = paste(cohort, case)),
    alpha = .7, colour='black',
    from = -1, to = 3,
    quantile_lines = TRUE, quantile_fun=function(x, ...) mean(x)
  ) +
  scale_y_discrete(expand = expand_scale(add = c(0, 0.8)), labels = rev(c('all ages','40-49','50-59','60-69','70-79'))) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_linetype_cyclical(values = c(1,2)) +
  scale_fill_cyclical(values = c("lightgrey","lightgrey")) +
  HGtheme + coord_cartesian(clip = "off") +
  ylab("Age") + xlab("") + ggtitle("mixed") +
  theme(plot.title = element_text(size=9, family = 'Helvetica'),
        axis.title = element_text(family = 'Helvetica'),
        axis.text = element_text(size=9, family = 'Helvetica')) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

female <- ggplot(p_v_f, aes(y = cohort)) +
  geom_density_ridges(
    aes(x = grs,
        scale = 0.75,
        linetype = paste(cohort, case),
        fill = paste(cohort, case)),
    alpha = .7, colour='black',
    from = -1, to = 3,
    quantile_lines = TRUE, quantile_fun=function(x, ...) mean(x)
  ) +
  scale_y_discrete(expand = expand_scale(add = c(0, 0.8)), labels = c('','','','','')) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_linetype_cyclical(values = c(1,2)) +
  scale_fill_cyclical(values = c("lightgrey","lightgrey")) +
  HGtheme + coord_cartesian(clip = "off") +
  ylab("") + xlab("PRS") + ggtitle("female") +
  theme(plot.title = element_text(size=9, family = 'Helvetica'),
        axis.title = element_text(family = 'Helvetica'),
        axis.text = element_text(size=9, family = 'Helvetica')) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

male <- ggplot(p_v_m, aes(y = cohort)) +
  geom_density_ridges(
    aes(x = grs,
        scale = 0.75,
        linetype = paste(cohort, case),
        fill = paste(cohort, case)),
    alpha = .7, colour='black',
    from = -1, to = 3,
    quantile_lines = TRUE, quantile_fun=function(x, ...) mean(x)
  ) +
  scale_y_discrete(expand = expand_scale(add = c(0, 0.8)), labels = c('','','','','')) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_linetype_cyclical(values = c(1,2)) +
  scale_fill_cyclical(values = c("lightgrey","lightgrey")) +
  HGtheme + coord_cartesian(clip = "off") +
  ylab("") + xlab("") + ggtitle("male") +
  theme(plot.title = element_text(size=9, family = 'Helvetica'),
        axis.title = element_text(family = 'Helvetica'),
        axis.text = element_text(size=9, family = 'Helvetica')) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

#install.packages('cowplot')
library(cowplot)

plot_grid(mixed_sex, female, male, ncol = 3, rel_widths = c(1.2,1,1))
