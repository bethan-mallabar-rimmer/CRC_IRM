#required variables from analysis: https://github.com/bethan-mallabar-rimmer/CRC_IRM/blob/main/analysis.R
#grs from Section 6A

library(dplyr)
#import a table including participant IDs + all the lifestyle variables/chacteristics assessed in this study
#table was generated on the DNA Nexus UKBB research analysis platform
p <- read.csv('00_participant.csv',  header = T #all participants
w <- read.csv('UKBB_withdrawals-25.04.23.csv',  header = T) #withdrawals
#remove withdrawals:
p <- p %>% filter(!(eid %in% w$X1003904)) #502,405 participants

grs <- readRDS("~/6_GRS.rds") #GRS
all_grs <- p %>% filter(eid %in% grs$grs$eid) %>% left_join(grs$grs) #487,198 participants had a GRS

#label participants by ancestry (related)
afrr <- read.table('AFR_8k_subject_list.txt', header = TRUE) #list of African ancestry participants
eurr <- read.table('EUR_451k_subject_list.txt', header = TRUE) #list of European ancestry participants
sasr <- read.table('SAS_10k_subject_list.txt', header = TRUE) #list of South Asian ancestry participants
easr <- read.table('EAS_2550_subject_list.txt', header = TRUE) #list of East Asian ancestry participants
amrr <- read.table('AMR_470_subject_list.txt', header = TRUE) #list of Admixed American ancestry participants

all_grs_r <- all_grs
all_grs_r$ancestry <- NA
all_grs_r$ancestry[all_grs_r$eid %in% amrr$x] <- 'AMR'
all_grs_r$ancestry[all_grs_r$eid %in% afrr$n_eid] <- 'AFR'
all_grs_r$ancestry[all_grs_r$eid %in% easr$x] <- 'EAS'
all_grs_r$ancestry[all_grs_r$eid %in% eurr$n_eid] <- 'EUR'
all_grs_r$ancestry[all_grs_r$eid %in% sasr$n_eid] <- 'SAS'

all_grs_r <- all_grs_r[!is.na(all_grs_r$ancestry),] #472,451 particpants clustered into 1 of 5 ancestral populations


#label participants by ancestry (unrelated)
afr <- read.table('AFR_8k_subject_list_unrelated.txt', header = TRUE) #list of unrelated African ancestry participants
eur <- read.table('EUR_451k_subject_list_unrelated.txt', header = TRUE) #list of unrelated European ancestry participants
sas <- read.table('SAS_10k_subject_list_unrelated.txt', header = TRUE) #list of unrelated South Asian ancestry participants
eas <- read.table('EAS_2482_subject_list_unrelated.txt', header = TRUE) #list of unrelated East Asian ancestry participants
amr <- read.table('AMR_468_subject_list_unrelated.txt', header = TRUE) #list of unrelated Admixed American ancestry participants

all_grs$ancestry <- NA
all_grs$ancestry[all_grs$eid %in% amr$X4242729] <- 'AMR'
all_grs$ancestry[all_grs$eid %in% afr$n_eid] <- 'AFR'
all_grs$ancestry[all_grs$eid %in% eas$X4034781] <- 'EAS'
all_grs$ancestry[all_grs$eid %in% eur$n_eid] <- 'EUR'
all_grs$ancestry[all_grs$eid %in% sas$n_eid] <- 'SAS'

all_grs <- all_grs[!is.na(all_grs$ancestry),] #399,454 particpants clustered into 1 of 5 ancestral populations and were unrelated

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
  c(meanv - sdv, meanv + sdv)
}

ggplot(all_grs, aes(x = grs, y = ancestry, fill = ancestry)) +
  geom_density_ridges(quantile_lines=TRUE, quantile_fun=function(x, ...) mean(x), vline_color='black') +
  geom_density_ridges(quantile_lines=TRUE, quantile_fun=sdvec, vline_color='black',vline_linetype=2,fill=alpha("black", 0)) +
  scale_fill_brewer(palette="Pastel1") +
  theme_ridges() + 
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5, family='Helvetica', size=12),
        axis.title.y = element_text(hjust = 0.5, family='Helvetica', size=12),
        axis.text = element_text(family='Helvetica', size=8),
        panel.spacing = unit(0,'cm'),
        plot.margin = unit(c(5.5, 5.5, 5.5, 5.5),'points')) +
  xlab('PRS') + ylab('Ancestry') +
  scale_x_continuous(breaks=seq(-2,4,by=0.5), limits = c(-2.2, 4.2), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

#run ANOVA to determine whether there is a difference between means

#assumptions of ANOVA:
#1. independence. PRS measurements are independent between participants as related participants not included
#because related participants have been excluded
#2. normality. Less important in a large sample such as this (https://statsandr.com/blog/anova-in-r/#fn3)
#but density plot shows approx normal distribution in all groups
#3. homoscedasdicity. Tested with Levene's test:

#install.packages('car')
library(car)
leveneTest(grs ~ ancestry, all_grs)
#p < 2.2e-16
#i.e. variance is not equal between groups, assumption is not met
#so use Welch's ANOVA which does not rely on this assumption
oneway.test(grs ~ ancestry, all_grs, var.equal = FALSE)
#p < 2.2e-16. Group means not equal
#post-hoc testing (Games-Howell test, for Welch's ANOVA with uneven sample sizes)
#install.packages('rstatix')
library(rstatix)
ght <- games_howell_test(all_grs, grs ~ ancestry, conf.level = 0.95, detailed = FALSE)
