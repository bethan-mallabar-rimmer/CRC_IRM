#required variables from analysis: https://github.com/bethan-mallabar-rimmer/CRC_IRM/blob/main/analysis.R
#p_40_v and p_00_v from Section 4

#install.packages('patchwork')
library(dplyr)
library(ggplot2)
library(patchwork)
library(devtools)

#A
#===
#install some required packages with devtools:
source_url("https://raw.githubusercontent.com/hdg204/UKBB/main/UKBB_Health_Records_Public.R")
source_url("https://raw.githubusercontent.com/ID690016874/HPDM042/main/find-read-codes/find_read_codes.R") 

#get read 2/3 codes and descriptions for CRC
lkp2 <- read.csv('read2_lkp.csv') # download read 2 lookup table
lkp3 <- read.csv('read3_lkp.csv') # download read 3 lookup table
crc_codes_gp <- find_read_codes(c('B13','B14','B575','B1z','B803','B804','B902','BB5N'))
crc_codes_gp_filtered <- crc_codes_gp[! crc_codes_gp$code %in% c('B1z..','B1z0.','B1zy.','B1zz.','B902.','B9020','B902z'),]

#get all hospital episode statistic, GP, and cancer registry records for CRC
hes_records <- read_ICD10(c('C18','C19','C20','C21'))
gp_records <- read_GP(crc_codes_gp_filtered$code)
cr_records <- read_cancer(c('C18','C19','C20','C21'))

#get all death records for CRC
death <- read.csv('death_death.csv') #date of death - only columns used for analysis were participant ID and date of death formatted: year-month-day
death_cause <- read.csv('death_death_cause.csv') #cause of death - only columns used were participant ID and 'cause_icd10' which contains: ICD10code<space>description of code
#split death_cause column of causes into ICD10 code and description
library(stringr)
death_cause[c('code', 'cause')] <- str_split_fixed(death_cause$cause_icd10, ' ', 2)
#reformat codes into 3 characters to match other ICD10 codes
for (i in 1:nrow(death_cause)) {death_cause$code[i] <- substr(death_cause$code[i], 1, 3)}
#add date of death
death_cause <- left_join(death_cause,death[,c(2,6)]) #col 2 = ID, 6 = date of death
#filter to colorectal cancer only:
death_cause <- death_cause[death_cause$code %in% c('C18','C19','C20','C21'),]

#import a table including participant IDs + all the lifestyle variables/chacteristics assessed in this study
#table was generated on the DNA Nexus UKBB research analysis platform
UKBB_var <- read.csv('00_participant.csv') 

#add date of birth for all participants:
UKBB_var$dob <- 15 #actual DoB unknown for privacy reasons. assume 15th of each month to minimise error
UKBB_var$dob <- apply(UKBB_var[,c(22,3,4)], 1, paste, collapse = "-") #col 3 in UKBB_var table is birth month, col 4 is birth year, col 22 is the new 'dob' column added in the above line
UKBB_var$dob <- as.Date(UKBB_var$dob, format = "%d-%B-%Y")

hes_records <- left_join(hes_records,UKBB_var[,c(1,22)])
gp_records <- left_join(gp_records,UKBB_var[,c(1,22)])
cr_records <- left_join(cr_records,UKBB_var[,c(1,22)])
death_records <- left_join(death_cause,UKBB_var[,c(1,22)])

#add age at which event occurred
library(lubridate)
hes_records$age <- lubridate::time_length(difftime(hes_records$epistart,
                                                   hes_records$dob),
                                          "years")
hes_records$age <- as.integer(hes_records$age)

gp_records$age <- lubridate::time_length(difftime(gp_records$event_dt,
                                                  gp_records$dob),
                                         "years")
gp_records$age <- as.integer(gp_records$age)

cr_records$age <- lubridate::time_length(difftime(cr_records$date,
                                                  cr_records$dob),
                                         "years")
cr_records$age <- as.integer(cr_records$age)

death_records$age <- lubridate::time_length(difftime(death_records$date_of_death,
                                                     death_records$dob),
                                            "years")
death_records$age <- as.integer(death_records$age)

#exclude participants who requested their data be removed from UKBB:
withdraw <- read.csv('UKBB_withdrawals-25.04.23.csv',header = T)
hes_records <- filter(hes_records, !(eid %in% withdraw$X1003904))
hes_records <- hes_records[!duplicated(hes_records),]
gp_records <- filter(gp_records, !(eid %in% withdraw$X1003904))
gp_records <- gp_records[!duplicated(gp_records),]
cr_records <- filter(cr_records, !(eid %in% withdraw$X1003904))
cr_records <- cr_records[!duplicated(cr_records),]
death_records <- filter(death_records, !(eid %in% withdraw$X1003904))
death_records <- death_records[!duplicated(death_records),]

#AAA---PLOT THE GRAPH---AAA
#==========================
HGtheme=theme_bw()+ #a graph theme made by Harry Green @ Exeter:
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))
HGtheme2=theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#count mode age (e.g. age 70 occurs most, how many records from age 70?)
hr_max_c <- sum(hes_records$age == getmode(hes_records$age), na.rm=T)
#max density (e.g. what percentage of records from age 70?)
hr_max_d <- hr_max_c / nrow(hes_records)
#histogram
hr <- hes_records %>% ggplot(aes(x = age)) +
  HGtheme +
  geom_histogram(fill='lightgrey', alpha=0.8, colour=alpha('grey', 1), binwidth=1) + ylab('HES') +
  geom_density(aes(y = ..density.. * nrow(hes_records))) +
  xlab('age') + #scale_y_continuous(breaks=seq(0,0.08,by=0.02),
  #limits=c(0,0.08)) + 
  scale_x_continuous(breaks=seq(0,90,by=5),limits=c(0,90)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y.left = element_text(colour=alpha('black', 0.5)),
        axis.text.y.right = element_text(colour=alpha('black', 1)),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y = element_line(colour = alpha('black', 0.7), size=(0.1)),
        panel.grid.major = element_line(colour = alpha('grey', 1), size=(0.1))) +
  scale_y_continuous(
    breaks=seq(0,4250,by=1000),
    limits=c(0,4250),
    minor_breaks = seq(0, ((round(hr_max_d, digits=2))*81152.95), 811.5295),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./(3750/hr_max_d),
                        breaks=seq(0,round(hr_max_d, digits=2),by=0.01))
  ) + geom_vline(xintercept=40,linetype=2)

#count mode age (e.g. age 70 occurs most, how many records from age 70?)
cr_max_c <- sum(cr_records$age == getmode(cr_records$age), na.rm=T)
#max density (e.g. what percentage of records from age 70?)
cr_max_d <- cr_max_c / nrow(cr_records)
#histogram
cr <- cr_records %>% ggplot(aes(x = age)) +
  HGtheme +
  geom_histogram(fill='lightgrey', alpha=0.8, colour=alpha('grey', 1), binwidth=1) + ylab('cr') +
  geom_density(aes(y = ..density.. * nrow(cr_records))) +
  xlab('age') + #scale_y_continuous(breaks=seq(0,0.08,by=0.02),
  #limits=c(0,0.08)) + 
  scale_x_continuous(breaks=seq(0,90,by=5),limits=c(0,90)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y.left = element_text(colour=alpha('black', 0.5)),
        axis.text.y.right = element_text(colour=alpha('black', 1)),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y = element_line(colour = alpha('black', 0.7), size=(0.1)),
        panel.grid.major = element_line(colour = alpha('grey', 1), size=(0.1))) +
  scale_y_continuous(
    breaks=seq(0,450,by=100),
    limits=c(0,450),
    minor_breaks = seq(0, ((round(cr_max_d, digits=2))*8069.606), 80.69606),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./(400/cr_max_d),
                        breaks=seq(0,round(cr_max_d, digits=2),by=0.01))
  ) + geom_vline(xintercept=40,linetype=2)

#count mode age (e.g. age 70 occurs most, how many records from age 70?)
gp_max_c <- sum(gp_records$age == getmode(gp_records$age), na.rm=T)
#max density (e.g. what percentage of records from age 70?)
gp_max_d <- gp_max_c / nrow(gp_records)
#histogram
gr <- gp_records %>% ggplot(aes(x = age)) +
  HGtheme +
  geom_histogram(fill='lightgrey', alpha=0.8, colour=alpha('grey', 1), binwidth=1) + ylab('gp') +
  geom_density(aes(y = ..density.. * nrow(gp_records))) +
  xlab('age') + #scale_y_continuous(breaks=seq(0,0.08,by=0.02),
  #limits=c(0,0.08)) + 
  scale_x_continuous(breaks=seq(0,90,by=5),limits=c(0,90)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y.left = element_text(colour=alpha('black', 0.5)),
        axis.text.y.right = element_text(colour=alpha('black', 1)),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y = element_line(colour = alpha('black', 0.7), size=(0.1)),
        panel.grid.major = element_line(colour = alpha('grey', 1), size=(0.1))) +
  scale_y_continuous(
    breaks=seq(0,450,by=100),
    limits=c(0,450),
    minor_breaks = seq(0, (0.2044507 * 2201.02), 88.0408),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./(255/gp_max_d),
                        breaks=seq(0,0.2,by=0.04))
  ) + geom_vline(xintercept=40,linetype=2)

#count mode age (e.g. age 70 occurs most, how many records from age 70?)
death_max_c <- sum(death_records$age == getmode(death_records$age), na.rm=T)
#max density (e.g. what percentage of records from age 70?)
death_max_d <- death_max_c / nrow(death_records)
#histogram
dr <- death_records %>% ggplot(aes(x = age)) +
  HGtheme2 +
  geom_histogram(fill='lightgrey', colour=alpha('grey', 1), binwidth=1) + ylab('death') +
  geom_density(aes(y = ..density.. * nrow(death_records))) +
  xlab('age') + #scale_y_continuous(breaks=seq(0,0.08,by=0.02),
  #limits=c(0,0.08)) + 
  scale_x_continuous(breaks=seq(0,90,by=5),limits=c(0,90)) + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.minor.y = element_line(colour = alpha('black', 0.7), size=(0.1)),
        panel.grid.major = element_line(colour = alpha('grey', 1), size=(0.1)),
        axis.text.y.left = element_text(colour=alpha('black', 0.5)),
        axis.text.y.right = element_text(colour=alpha('black', 1)),) +
  scale_y_continuous(
    breaks=seq(0,150,by=50),
    limits=c(0,150),
    minor_breaks = seq(0, ((round(death_max_d, digits=2))+0.01)*1633.116, 32.66232),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./(112/death_max_d),
                        breaks=seq(0,((round(death_max_d, digits=2))+1),by=0.02))
  ) + geom_vline(xintercept=40,linetype=2)

sf1_a <- hr / cr / gr / dr
#axis labels were then added manually in powerpoint because plotting axis
#subtitles in ggplot is hard, particularly with two different axes...

#B
#===
sf1_b <- p_00_v %>% ggplot() +
  aes(x = sym_age, linetype = as.factor(case)) + HGtheme +
  geom_density(alpha = 0.4, fill='lightgrey') + scale_linetype_discrete(name="",labels=c("control","case")) +
  xlab('age at first CRC symptom') + geom_vline(xintercept = 40) + 
  scale_x_continuous(breaks=seq(0,80,by=10)) + scale_y_continuous(breaks=seq(0,0.06,by=0.01),
                                                                  limits=c(0,0.0605))


#C
#===
sf1_c <- p_40_v %>% ggplot() +
  aes(x = sym_age, linetype = as.factor(case)) + HGtheme +
  geom_density(alpha = 0.4, fill='lightgrey') + scale_linetype_discrete(name="",labels=c("control","case")) +
  xlab('age at first CRC symptom') + 
  scale_x_continuous(breaks=seq(0,80,by=10)) + scale_y_continuous(breaks=seq(0,0.06,by=0.01),
                                                                  limits=c(0,0.0605))
