#0.These libraries are used throughout the script, good to enable them from the beginning:
#========================================================================================
library(dplyr)
library(ggplot2)
library(devtools)

#install some required packages with devtools:
source_url("https://raw.githubusercontent.com/hdg204/UKBB/main/UKBB_Health_Records_Public.R") 
source_url("https://raw.githubusercontent.com/bethan-mallabar-rimmer/CRC_IRM/main/find_read_codes/find_read_codes.R") 
source_url("https://raw.githubusercontent.com/hdg204/Rdna-nexus/main/install.R")

#1A. Get read 2/3 codes and descriptions for CRC symptoms
#=======================================================
#import DISCO read codes for CRC symptoms, and lookup tables for all read codes
lkp2 <- read.csv('read2_lkp.csv') # download read 2 lookup table
lkp3 <- read.csv('read3_lkp.csv') # download read 3 lookup table
disco <- read.csv('00_DISCO_crc_symptoms.csv') # download CRC symptom read codes from DISCO (please contact the DISCO consortium University of Exeter for this file)

#reformat DISCO read codes to 5 characters, same as UKBB read code format:
#-------------------------------------------------------------------------
for (i in 1:nrow(disco)) {disco$readcode[i] <- substr(disco$readcode[i], 1, 5)}
for (i in 1:nrow(disco)) {
  if (nchar(disco$readcode[i]) == 4) {disco$readcode[i] <- paste(disco$readcode[i],'.',sep='')}
}
#check how many unique DISCO read codes there are now they've been cut to 5 characters
length(unique(disco$readcode)) #=168

#remove 'faecal occult blood test abnormal' (due to low numbers of UKBB participants having this symptom at the primary care stage)
disco <- disco[,-(colnames(disco) == 'fob')]

#find read codes in the lookup table which match or start with the DISCO read codes:
#-----------------------------------------------------------------------------------
find_all_symptom_codes <- find_read_codes(unique(disco$readcode)) #returns 255 read codes

#after looking through above table, the following codes do not appear relevant to CRC, remove:
sym_codes_filtered <- find_all_symptom_codes[!(find_all_symptom_codes$code %in% c('1961.','1964.','1966.','196A.',
                                                                                  '25C1.','25D1.','25E1.','X772q','44TB.',
                                                                                  '25F1.','2I18.','2I180','44TB0','44TB1',
                                                                                  '479..','4791.','4792.','4795.','479Z.',
                                                                                  'D00y.','D00y0','D00z0','D00z1','D00z2',
                                                                                  'D00..','D00y1','E2780',
                                                                                  'Eu50y','J680.','R0903','R090B','R090G','R0939',
                                                                                  'XaCIU','XabrE','XabrF','XaNxS')),]

#remove any duplicates:
sym_codes_filtered <- sym_codes_filtered[!duplicated(sym_codes_filtered$code),]


#import list of symptom codes for rectal bleeding found by Matt Barclay @ UCL:
#----------------------------------------------------------------------------
ucl_codes_rb <- read.csv('00_rectal_bleeding_comparison.csv') #import codes
ucl_codes_rb <- ucl_codes_rb[ucl_codes_rb$code_source == 'Matt',] #label with source

#each read code has 2 entries, one with a 'count' value (how many times recorded)
#for 2010 and the other for 2015. These aren't as relevant for this analysis, so
#sum/combine the different counts and then remove duplicated entries:
ucl_codes_rb <- ucl_codes_rb %>% group_by(read) %>% mutate(total_count = sum(count)) %>%
  select(!(c(code_source,year,count))) %>% distinct()

#add term descriptions to the UCL codes:
#-----------------------------------------
#run find_read_codes function again to simultaneously expand code list and add term descriptions:
find_ucl_codes <- find_read_codes(unique(ucl_codes_rb$read))
ucl_codes_rb <- ucl_codes_rb %>% rename(code=read) %>% full_join(find_ucl_codes)
#remove irrelevant codes
ucl_codes_rb_filtered <- ucl_codes_rb[!(ucl_codes_rb$code %in% c('XaPpf','XaJYy',
                                                                 'J680.','.I781',
                                                                 'T4762','8I78.',
                                                                 '4I78.')),]

#join all read codes from the 3 sources into one dataframe
#---------------------------------------------------------
sym_codes_filtered <- full_join(sym_codes_filtered, ucl_codes_rb_filtered[,c(1,3:4)])
#remove duplicates
sym_codes_filtered <- sym_codes_filtered[!duplicated(sym_codes_filtered$code),]

#remove any codes describing faecal occult blood test - these will be used for a separate variable later
#------------------------------------------------------
fob_positive <- c('4794.','4793.','4796.','686B.','XaNxT','XaPke')
fob_negative <- c('4792.','4795.','XaNxS')
fob_requested <- c('479..','4791.','479Z.')

sym_codes_filtered <- sym_codes_filtered[!sym_codes_filtered$code %in% c(fob_requested, fob_negative, fob_positive),]
#230 symptoms

#1B. count the number of read codes per each type of symptom:
#============================================================
#get filtered symptom codes which originally came from DISCO (the symptom 'type' for
#each of these read codes - e.g. abdominal pain, weightloss - is already filled in):
#-----------------------------------------------------------------------------------
disco_type <- disco[!duplicated(disco$readcode), -2]
disco_type <- disco_type[disco_type$readcode %in% sym_codes_filtered$code,]
#add new symptom type (abdominal rigidity):
disco_type <- dplyr::mutate(disco_type, 'rigidity' = rep(NA, nrow(disco_type)))

#get filtered symptom codes which didn't originally come from DISCO:
#-------------------------------------------------------------------
not_disco <- sym_codes_filtered$code[!(sym_codes_filtered$code %in% disco$readcode)]

#make a dataframe to fill in with symptom type for each code:
not_disco_type <- data.frame(disco_type[1:length(not_disco),])
not_disco_type$readcode <- not_disco
not_disco_type[,-1] <- NA

#sort all symptoms which didn't come from DISCO consortium into types:
#---------------------------------------------------------------------
not_disco_desc <- sym_codes_filtered[sym_codes_filtered$code %in% not_disco_type$readcode,]
#abdominal pain
abp <- c('1965.','1967.','1974.','25D5.','25D7.','R090D','Xa7xW','Xa83s','XaBDO',
         'XaBDQ','XaBDR','XaD2x','XaD2y','XaD2z','XaY2H','25E2.','25E3.','25E4.',
         '25E5.','25E6.','25E7.','25E8.','25E9.','25EA.')
#rigidity
rig <- c('25F2.','25FZ.')
#abdominal mass incl. pelvic mass
abm <- c('25J1.','25J2.','25J4.','25J6.','25JZ.','25K4.','25L1.','25LZ.','X304O',
         'XaBj4','XaDsr','R0933','R0934','R0935')
#change in bowel habits
cbh <- c('25JA.','XaI94')
#rectal blood loss
rbl <- c('J68z0','J68z2','X30Be','XaFt2','XaIMX','XaJuu','XaJuv','XaYVt',
         ucl_codes_rb_filtered$code[!(ucl_codes_rb_filtered$code %in% sym_codes_filtered) &
                                      !duplicated(ucl_codes_rb_filtered$code)])
#appetite loss
app <- 'Ua1iv'
#haemoglobin levels
haem <- 'XaBLm' 
#weight loss
wl <- c('XaIu3','XaIxC','XaJM4','XaKwR','XaXTs')

#fill in symptom type dataframe
#-------------------------------
not_disco_type$weightloss[not_disco_type$readcode %in% wl] <- 1
not_disco_type$loss_appetite[not_disco_type$readcode %in% app] <- 1
not_disco_type$abdo_mass[not_disco_type$readcode %in% abm] <- 1
not_disco_type$abdo_pain[not_disco_type$readcode %in% abp] <- 1
not_disco_type$rectal_bloodloss[not_disco_type$readcode %in% rbl] <- 1
not_disco_type$change_bowel_habit[not_disco_type$readcode %in% cbh] <- 1
not_disco_type$haemoglobin[not_disco_type$readcode %in% haem] <- 1
not_disco_type$rigidity[not_disco_type$readcode %in% rig] <- 1

#Add all CRC symptom read codes + types from DISCO and not from DISCO into 1 dataframe:
#--------------------------------------------------------------------------------------
symptom_type <- dplyr::filter(disco_type, weightloss==1)
symptom_type <- rbind(symptom_type, dplyr::filter(not_disco_type, weightloss==1),
                      dplyr::filter(disco_type, loss_appetite==1),
                      dplyr::filter(not_disco_type, loss_appetite==1),
                      dplyr::filter(disco_type, iron_def==1),
                      dplyr::filter(disco_type, abdo_mass==1),
                      dplyr::filter(not_disco_type, abdo_mass==1),
                      dplyr::filter(disco_type, abdo_pain==1),
                      dplyr::filter(not_disco_type, abdo_pain==1),
                      dplyr::filter(disco_type, rectal_bloodloss==1),
                      dplyr::filter(not_disco_type, rectal_bloodloss==1),
                      dplyr::filter(disco_type, change_bowel_habit==1),
                      dplyr::filter(not_disco_type, change_bowel_habit==1),
                      dplyr::filter(disco_type, haemoglobin==1),
                      dplyr::filter(not_disco_type, haemoglobin==1),
                      dplyr::filter(disco_type, rigidity==1),
                      dplyr::filter(not_disco_type, rigidity==1))
#remove any duplicates:
symptom_type <- symptom_type[!duplicated(symptom_type$readcode),]

#remove rigidity (later in the study it was found hardly any participants had this symptom)
#also remove one rigidity code accidentally typed as abdominal pain:
symptom_type <- symptom_type[!(symptom_type$readcode %in% c('25F2.','25F..','25FZ.')),-10]
sym_codes_filtered <- sym_codes_filtered[!(sym_codes_filtered$code %in% c('25F2.','25F..','25FZ.')),]

#differentiate between unintentional and unspecified-whether-unintentional weightloss:
symptom_type$unintentional_weightloss <- NA
symptom_type$unintentional_weightloss[symptom_type$readcode
                                      %in% c('1625.','1627.','1D1A.','2224.',
                                             '2287.','22A6.','R032.','R0348',
                                             'R2y4.','R2y40','R2y4z','XaIu3',
                                             'XaJM4','XaKwR','XaXTs')] <- 1
symptom_type$other_weightloss <- NA
symptom_type$other_weightloss[symptom_type$readcode %in% c('22A8.','XaIxC','1623.')] <- 1
symptom_type <- symptom_type[,c(1:2,10:11,3:9)] #reorder columns

#2A. Find UKBB participants with symptoms
#=======================================
p_sym <- read_GP(sym_codes_filtered$code)

#exclude participants who requested their data be removed from UKBB:
withdraw <- read.csv('UKBB_withdrawals-25.04.23.csv',header = T)
p_sym <- filter(p_sym, !(eid %in% withdraw$X1003904))
#the above csv file was provided by UKBB via email. This step is only important if you
#have an existing file with UKBB participant data, which includes participants who have
#requested to withdraw their data from the study. If you are creating a new participant
#data file, then participants who requested data withdrawal are already removed from UKBB
#and will not be in the new file.

#number each symptom record, as there are multiple records per participant
p_sym$no <- 1:nrow(p_sym)

#2B. Any participants with a read code for haemoglobin level were included
#Exclude participants with normal haemoglobin level or no measurement
#======================================================================
#dataframe of participants with haemoglobin level recorded:
exclude <- p_sym[p_sym$read_2 %in% c('44TC.','XaBLm') | p_sym$read_3 %in% c('44TC.','XaBLm'),]

#import a table including participant IDs + all the lifestyle variables/chacteristics assessed in this study
#table was generated on the DNA Nexus UKBB research analysis platform
UKBB_var <- read.csv('00_participant.csv') 

#use the above participant table to add participant sex to haemoglobin dataframe:
exclude <- left_join(exclude, UKBB_var[,c(1,5)]) #(sex was in column 5 of the table)

#find participants with low haemoglobin levels ('low' is sex-dependent)
#also only include participants with haemoglobin > 1 as < 1 may be a percentage:
exclude$value1 <- as.numeric(exclude$value1) #convert haemoglobin measure to numeric
include <- exclude[(exclude$p31 == 'Female' & exclude$value1 < 11 & exclude$value1 > 1),]
include <- rbind(include, exclude[(exclude$p31 == 'Male' & exclude$value1 < 13 & exclude$value1 > 1),])
include <- include[!is.na(include$eid),]
#remove any participants with a % symbol in the 'value3' column as these are likely to be percentage measurements
include <- include[!(include$value3 == '%'),]

#remove participant symptom records from p_sym, if the 'symptom' was haemoglobin level that is either healthy or not recorded:
exclude <- exclude[!(exclude$no %in% include$no),]
p_sym <- p_sym[!(p_sym$no %in% exclude$no),]

#2C. Exclude any symptoms which occurred before participants were 40 or 50
#==================================================================
#add date of birth for all participants:
UKBB_var$dob <- 15 #actual DoB unknown for privacy reasons. assume 15th of each month to minimise error
UKBB_var$dob <- apply(UKBB_var[,c(22,3,4)], 1, paste, collapse = "-") #col 3 in UKBB_var table is birth month, col 4 is birth year, col 22 is the new column called 'dob' added in the above line
UKBB_var$dob <- as.Date(UKBB_var$dob, format = "%d-%B-%Y")
p_sym <- left_join(p_sym, UKBB_var[,c(1,22)])

#add age at which symptom occurred
library(lubridate)
p_sym$sym_age <- lubridate::time_length(difftime(p_sym$event_dt,
                                                 p_sym$dob),
                                        "years")
p_sym$sym_age <- as.integer(p_sym$sym_age)  

#For analysis of whole cohort, include any symptoms occurring after age 40 or 50:
#===============================================================================
#version 1: exclude symptoms which occurred before participants were 50:
p_sym_50 <- p_sym[p_sym$sym_age >= 50,]
#version 2: exclude symptoms which occurred before participants were 40:
p_sym_40 <- p_sym[p_sym$sym_age >= 40,]

#2D. Include only symptom record with earliest date for each patient
#===================================================================
get_earliest_symptom <- function(p_sym_n) {
  p_sym_filtered <- p_sym_n
  p_sym_filtered[,4:5][p_sym_filtered[,4:5] == ''] <- NA #cols 4 and 5 of the participant symptom records table contain read 2 and 3 codes
  p_sym_filtered <- p_sym_filtered %>%
    #get earliest symptom for each patient
    group_by(eid) %>%
    slice_min(event_dt) %>%
    #remove duplicate records (where same symptoms appears multiple times on same date)
    distinct(eid, read_2, read_3, .keep_all = TRUE) 
  p_sym_filtered <- p_sym_filtered[,-9]
  
  return(p_sym_filtered)
}

p_sym_filtered_00 <- get_earliest_symptom(p_sym) #no age threshold
p_sym_filtered_40 <- get_earliest_symptom(p_sym_40) #age threshold for earliest symptom = 40
p_sym_filtered_50 <- get_earliest_symptom(p_sym_50) #age threshold for earliest symptom = 50

#3A. For symptomatic participants, find earliest occurrence of CRC
#================================================================
#get read 2/3 codes and descriptions for CRC
crc_codes_gp <- find_read_codes(c('B13','B14','B575','B1z','B803','B804','B902','BB5N'))
#exclude irrelevant codes (e.g. non cancer, or cancer of anus/anal canal or appendix)
crc_codes_gp_filtered <- crc_codes_gp[! crc_codes_gp$code %in% c('B1z..','B1z0.','B1zy.','B1zz.',
                                                                 'B902.','B9020','B902z',
                                                                 'B143.','BB5N2','B135.',
                                                                 'BB5Nz','BB5N.','BB5N0',
                                                                 'B9025','B142.','XaZfN',
                                                                 'XaFsw'),]

#list ICD10 cancer registry codes for CRC
#excluding cancers of appendix (C18.1) and anus/anal canal (C21)
crc_codes_icd10_filtered <- c('C18.0','C18.2','C18.3','C18.4',
                              'C18.5','C18.6','C18.7','C18.8',
                              'C18.9','C19','C20')

#find earliest occurrence of CRC for each patient
#below function adapted from Harry Green's function first_occurence
first_occurence_age <- function(ICD10='',GP='',OPCS='',cancer='',
                                lower_age_threshold=0, upper_age_threshold=150,
                                p=p_sym_filtered_00, read_lkp2 = lkp2,
                                read_lkp3 = lkp3) {
  #ICD10
  ICD10_records=read_ICD10(ICD10) %>% mutate(date=epistart) %>% select(eid,date,diag_icd10) %>% 
    mutate(source='HES') %>% rename('diagnosis'=diag_icd10)
  #OPCS
  OPCS_records=read_OPCS(OPCS) %>% mutate(date=opdate) %>% select(eid,date) %>% mutate(source='OPCS')
  #GP
  GP_records=read_GP(GP) %>% mutate(date=event_dt) %>% select(eid,date,read_2,read_3) %>% mutate(source='GP')
  #for GP, combine read 2 and 3 codes and add descriptions
  GP_records$read_2[GP_records$read_2==""] <- GP_records$read_3[GP_records$read_2==""]
  GP_records <- GP_records %>% select(eid,date,read_2,source) %>% rename('diagnosis'=read_2)
  #get read descriptions:
  read_descriptions <- data.frame(rc = unique(GP_records$diagnosis), rd=rep(NA,length(unique(GP_records$diagnosis))))
  for (i in 1:nrow(read_descriptions)) {
    if (sum(read_descriptions$rc[i] %in% lkp3$read_3) > 0) {
      read_descriptions$rd[i] <- lkp3$term_description[lkp3$read_3 == read_descriptions$rc[i]]
    }
    if (sum(read_descriptions$rc[i] %in% lkp2$read_2) > 0) {
      read_descriptions$rd[i] <- lkp2$term_description[lkp2$read_2 == read_descriptions$rc[i]]
    }
  }
  #add to GP 'diagnosis' column
  for (i in 1:nrow(GP_records)) {
    GP_records$diagnosis[i] <- paste(GP_records$diagnosis[i], read_descriptions$rd[read_descriptions$rc == GP_records$diagnosis[i]])
  }
  #Cancer Registry
  cancer_records = read_cancer(cancer) %>% select(eid,date,cancer) %>% mutate(source='Cancer_Registry') %>% rename('diagnosis'=cancer)
  #Join everything together:
  all_records = rbind(ICD10_records,OPCS_records) %>% rbind(GP_records) %>% rbind(cancer_records) %>% mutate(date=as.Date(date))
  all_records = left_join(all_records,p[,c(1,9)])
  all_records$crc_age = lubridate::time_length(difftime(all_records$date, all_records$dob), "years")
  all_records = filter(all_records, (crc_age >= lower_age_threshold) & (crc_age < upper_age_threshold))
  all_records = all_records %>% group_by(eid) %>% top_n(-1,date) %>% ungroup() %>% distinct()
  #some of these patient IDs are duplicated due to being diagnosed with more than
  #1 cancer on the same day, or having multiple diagnoses from different sources
  #maximum number of multiple diagnoses is 4
  #therefore add 3 extra columns, so each diagnosis and source is in a separate column:
  all_records <- data.frame(eid = all_records$eid, date = all_records$date,
                            diagnosis_1 = all_records$diagnosis,
                            diagnosis_2 = rep("",nrow(all_records)),
                            diagnosis_3 = rep("",nrow(all_records)),
                            diagnosis_4 = rep("",nrow(all_records)),
                            source_1 = all_records$source,
                            source_2 = rep("",nrow(all_records)),
                            source_3 = rep("",nrow(all_records)),
                            source_4 = rep("",nrow(all_records)),
                            crc_age = all_records$crc_age)
  duplicated_ids <- all_records$eid[duplicated(all_records$eid)]
  for (i in 1:length(duplicated_ids)) {
    if (sum(all_records$eid == duplicated_ids[i]) >= 2) {
      all_records$diagnosis_2[all_records$eid == duplicated_ids[i]][1] <- all_records$diagnosis_1[all_records$eid == duplicated_ids[i]][2]
      all_records$source_2[all_records$eid == duplicated_ids[i]][1] <- all_records$source_1[all_records$eid == duplicated_ids[i]][2]
    }
    if (sum(all_records$eid == duplicated_ids[i]) >= 3) {
      all_records$diagnosis_3[all_records$eid == duplicated_ids[i]][1] <- all_records$diagnosis_1[all_records$eid == duplicated_ids[i]][3]
      all_records$source_3[all_records$eid == duplicated_ids[i]][1] <- all_records$source_1[all_records$eid == duplicated_ids[i]][3]
    }
    if (sum(all_records$eid == duplicated_ids[i]) == 4) {
      all_records$diagnosis_4[all_records$eid == duplicated_ids[i]][1] <- all_records$diagnosis_1[all_records$eid == duplicated_ids[i]][4]
      all_records$source_4[all_records$eid == duplicated_ids[i]][1] <- all_records$source_1[all_records$eid == duplicated_ids[i]][4]
    }
    if (sum(all_records$eid == duplicated_ids[i]) > 4) {
      print("> 4 duplications! Something went wrong")
    }
  }
  #now the first record for each patient should contain details of all
  #diagnoses that occurred on the same day.
  #select only the first record:
  all_records <- distinct(all_records, eid, .keep_all=TRUE)
  return(all_records)
}

crc_00 <- first_occurence_age(ICD10 = crc_codes_icd10_filtered,
                              GP = crc_codes_gp_filtered$code,
                              cancer = crc_codes_icd10_filtered)
crc_40 <- first_occurence_age(ICD10 = crc_codes_icd10_filtered,
                              GP = crc_codes_gp_filtered$code,
                              cancer = crc_codes_icd10_filtered,
                              lower_age_threshold = 40,
                              p = p_sym_filtered_40)
crc_50 <- first_occurence_age(ICD10 = crc_codes_icd10_filtered,
                              GP = crc_codes_gp_filtered$code,
                              cancer = crc_codes_icd10_filtered,
                              lower_age_threshold = 50,
                              p = p_sym_filtered_50)

length(unique(crc_00$eid)) == nrow(crc_00) #= TRUE i.e. no duplicated records
length(unique(crc_40$eid)) == nrow(crc_40) #= TRUE i.e. no duplicated records
length(unique(crc_50$eid)) == nrow(crc_50) #= TRUE i.e. no duplicated records

#3B. Add crc data to participants table
#======================================
#required files:
death <- read.csv('death_death.csv') #date of death - only columns used for analysis were participant ID and date of death formatted: year-month-day
death_cause <- read.csv('death_death_cause.csv') #cause of death - only columns used were participant ID and 'cause_icd10' which contains: ICD10code<space>description of code

#split death_cause column of causes into ICD10 code and description
library(stringr)
death_cause[c('code', 'cause')] <- str_split_fixed(death_cause$cause_icd10, ' ', 2)
#reformat codes into 3 characters to match other ICD10 codes
for (i in 1:nrow(death_cause)) {death_cause$code[i] <- substr(death_cause$code[i], 1, 3)}
#add date of death
death_cause <- left_join(death_cause,death[,c(2,6)]) #col 2 = ID, 6 = date of death

#filter both tables to only include symptomatic participants:
library(dplyr)
death_00 <- filter(death, eid %in% p_sym_filtered_00$eid)
death_40 <- filter(death, eid %in% p_sym_filtered_40$eid)
death_50 <- filter(death, eid %in% p_sym_filtered_50$eid)

death_cause_00 <- filter(death_cause, eid %in% p_sym_filtered_00$eid)
death_cause_40 <- filter(death_cause, eid %in% p_sym_filtered_40$eid)
death_cause_50 <- filter(death_cause, eid %in% p_sym_filtered_50$eid)

#a function to add all the CRC data about participants to a dataframe 'p':
add_crc <- function(p_sym_filtered_xx, crc_xx, death_cause_xx) { 
  #create a table including id, DoB, symptom date and symptom age:
  p <- p_sym_filtered_xx[,colnames(p_sym_filtered_xx) %in% c("eid",
                                                             "dob",
                                                             "event_dt",
                                                             "sym_age" ) == TRUE]
  
  #add CRC date, source of info (e.g. GP, cancer registry) and age at diagnosis, and rename some stuff:
  p <- left_join(p, crc_xx) %>% rename('sym_date' = event_dt,
                                       'crc_date' = date,
                                       'crc_source_1' = source_1,
                                       'crc_source_2' = source_2,
                                       'crc_source_3' = source_3,
                                       'crc_source_4' = source_4)
  
  #list of participants with death cause being CRC (ICD10 code of C18, C19, or C20, excluding appendix cancers).
  icd10_death_eid <- death_cause_xx$eid[(death_cause_xx$code %in% unique(substr(crc_codes_icd10_filtered,1,3))) &
                                          (death_cause_xx$cause != "Appendix")]
  
  #add whether participants had CRC listed as cause of death:
  p$crc_death[p$eid %in% icd10_death_eid] <- 1
  p$crc_death[is.na(p$crc_death)] <- 0
  
  #add date and age of death for all participants
  p <- left_join(p, death_cause_xx[,colnames(death_cause_xx) %in% 
                                     c('eid','date_of_death') == TRUE]) %>% rename('death_date' = date_of_death)
  p$death_age <- lubridate::time_length(difftime(p$death_date,p$dob),"years")
  
  #if participants had CRC listed as a cause of death, but no other diagnosis of CRC
  #in health records, fill in CRC date and age with death date and age:
  cdo <- p$eid[p$crc_death == 1 & is.na(p$crc_date)]
  p$crc_date[p$eid %in% cdo] <- p$death_date[p$eid %in% cdo]
  p$crc_age[p$eid %in% cdo] <- p$death_age[p$eid %in% cdo]
  
  #add info on diagnosis
  icd10_death_crc_diagnosis <- death_cause_xx[(death_cause_xx$eid %in% cdo) & (death_cause_xx$code %in% c("C18","C19","C20")),]
  for (i in 1:length(cdo)) {
    p$diagnosis_1[p$eid %in% cdo[i]] <- icd10_death_crc_diagnosis$cause_icd10[icd10_death_crc_diagnosis$eid == cdo[i]][1]
    p$crc_source_1[p$eid %in% cdo[i]] <- 'Death'
    #if participant has two or more death records relating to CRC, record both:
    if (sum(icd10_death_crc_diagnosis$eid == cdo[i]) >= 2) {
      p$diagnosis_2[p$eid %in% cdo[i]] <- icd10_death_crc_diagnosis$cause_icd10[icd10_death_crc_diagnosis$eid == cdo[i]][2]
      p$crc_source_2[p$eid %in% cdo[i]] <- 'Death'
    }
    #if participant has three or more death records relating to CRC, record all three:
    if (sum(icd10_death_crc_diagnosis$eid == cdo[i]) >= 3) {
      p$diagnosis_3[p$eid %in% cdo[i]] <- icd10_death_crc_diagnosis$cause_icd10[icd10_death_crc_diagnosis$eid == cdo[i]][3]
      p$crc_source_3[p$eid %in% cdo[i]] <- 'Death'
    }
    #same if a participant has four or more:
    if (sum(icd10_death_crc_diagnosis$eid == cdo[i]) == 4) {
      p$diagnosis_4[p$eid %in% cdo[i]] <- icd10_death_crc_diagnosis$cause_icd10[icd10_death_crc_diagnosis$eid == cdo[i]][4]
      p$crc_source_4[p$eid %in% cdo[i]] <- 'Death'
    }
    #four is the maximum number of records which this code records (because the most CRC diagnoses
    #a participant had from GP, hospital, and cancer registry records was 4). As it's probably very
    #unlikely for a participant to have >4 death records relating to CRC, print a message
    if (sum(icd10_death_crc_diagnosis$eid == cdo[i]) > 4) {
      print(paste('participant',cdo[i],'had >4 death records with CRC as a cause. This may be due
                  to an error.'))
    }
  }
  
  #calculate time between first symptom and CRC in days:
  p$time_diff <- lubridate::time_length(difftime(p$crc_date, p$sym_date),"days")
  #calculate time between first symptom and death in days
  p$time_diff_death <- lubridate::time_length(difftime(p$death_date, p$sym_date),"days")
  
  #determine whether participants are cases or controls
  p$case[p$time_diff > 0 & p$time_diff <= 730] <- '1' #case
  p$case[p$time_diff > 730] <- '0' #control
  p$case[is.na(p$time_diff)] <- '0'
  p$case[p$time_diff <= 0] <- 'exclude' #exclude
  p$case[p$time_diff_death <= 730 & p$crc_death == 0] <- 'exclude'
  
  #test a 5 year cutoff rather than a 2 year cutoff for determining cases (this was in response to reviewer comments)
  p$case_5years <- p$case
  p$case_5years[p$case_5years == 0 & p$time_diff > 0 & p$time_diff <= (365.25*5)] <- '1'
  p$case_5years[p$time_diff_death <= (365.25*5) & p$crc_death == 0] <- 'exclude'
  
  #test a 10 year cutoff rather than a 2 year cutoff for determining cases (in response to reviewers)
  p$case_10years <- p$case
  p$case_10years[p$case_10years == 0 & p$time_diff > 0 & p$time_diff <= (365.25*10)] <- '1'
  p$case_10years[p$time_diff_death <= (365.25*10) & p$crc_death == 0] <- 'exclude'
  
  #reformat some stuff
  p$crc_age <- as.integer(p$crc_age)
  p$death_age <- as.integer(p$death_age)
  
  #remove duplicates
  p <- p[!duplicated(p$eid),]
  
  return(p)
}

#make dataframe of all symptomatic patients, symptom date/age, CRC date/age, death date/age,
#whether death was due to CRC, time between symptom and CRC, and whether case/control/exclude
p_00 <- add_crc(p_sym_filtered_00, crc_00, death_cause_00)
p_40 <- add_crc(p_sym_filtered_40, crc_40, death_cause_40)
p_50 <- add_crc(p_sym_filtered_50, crc_50, death_cause_50)

#3C: For overall cohort analysis: Decide which age threshold to use based on density plots
#comparing age at first symptom in cases/controls
#=========================================================================================
density0 <- p_00 %>% ggplot() +
  aes(x = sym_age, fill = as.factor(case)) +
  geom_density(alpha = 0.4) + scale_fill_discrete(name="",labels=c("control","case")) +
  xlab('age at first CRC symptom') + geom_vline(xintercept = 40) + geom_vline(xintercept = 50)

density40 <- p_40 %>% ggplot() +
  aes(x = sym_age, fill = as.factor(case)) +
  geom_density(alpha = 0.4) + scale_fill_discrete(name="",labels=c("control","case")) +
  xlab('age at first CRC symptom')

density50 <- p_50 %>% ggplot() +
  aes(x = sym_age, fill = as.factor(case)) +
  geom_density(alpha = 0.4) + scale_fill_discrete(name="",labels=c("control","case")) +
  xlab('age at first CRC symptom')

#going with age threshold 40 because it increases case to control ratio without excluding too many participants

#4A. exclude participants with pathogenic variants in Lynch syndrome genes
#=====================================================================
#genes are MLH1, MSH2 and MSH6
#import tables of participants with pathogenic variants (for a list of UKBB participants with pathogenic variants
#in the above genes, please contact authors of 'Influence of family history on penetrance of hereditary cancers
#in a population setting', Jackson et al., eClinicalMedicine 2023
l_mlh1 <- read.csv('0_MLH1_clin_inds.txt',sep = "",header = F)
l_msh2 <- read.csv('0_MSH2_clin_inds.txt',sep = "",header = F)
l_msh6 <- read.csv('0_MSH6_clin_inds.txt',sep = "",header = F)

#calculate the number of participants in our cohort with pathogenic Lynch syndrome variants:
sum(p_40$eid %in% l_mlh1$V1) #14
length(p_40$eid[p_40$eid %in% l_mlh1$V1 & p_40$case == '1']) #2 cases
length(p_40$eid[p_40$eid %in% l_mlh1$V1 & p_40$case == '0']) #9 controls
sum(p_40$eid %in% l_msh2$V1) #16
length(p_40$eid[p_40$eid %in% l_msh2$V1 & p_40$case == '1']) #2 cases
length(p_40$eid[p_40$eid %in% l_msh2$V1 & p_40$case == '0']) #11 controls
sum(p_40$eid %in% l_msh6$V1) #68
length(p_40$eid[p_40$eid %in% l_msh6$V1 & p_40$case == '1']) #0 cases
length(p_40$eid[p_40$eid %in% l_msh6$V1 & p_40$case == '0']) #63 controls
#calculate how many participants are expected to be left in the cohort, after excluding thsoe with pathogenic Lynch syndrome variants:
nrow(p_40) - (14 + 16 + 68) #67750

#exclude the participants:
p_40 <- p_40[!(p_40$eid %in% l_mlh1$V1) & 
                   !(p_40$eid %in% l_msh2$V1) &
                   !(p_40$eid %in% l_msh6$V1),]
nrow(p_40) #cohort N = 67750 - as expected - i.e. no participants had variants in more than one Lynch syndrome gene

#exclude participants diagnosed with other hereditary conditions causing CRC
#==========================================================================
#e.g. familial adenomatous polyposis, Gardner syndrome, Turcot syndrome,
#hereditary flat adenoma syndrome, hereditary nonpolyposis CRC, the
#hamartomatous polyposis syndromes, and hereditary mixed polyposis syndrome

#diagnoses were identified solely using Read codes in primary care records because:
#1. identifying pathogenic variants for the above conditions and excluding participants based
#on genetics is outside the scope of this study
#2. ICD-10 codes which include the above conditions (D13.9 and D12.6) are not specific enough,
#and also include other conditions such as any benign neoplasm of the digestive system.

#change column names of the lookup tables of Read 2 and 3 codes (section 1 of this analysis), so
#the tables can be joined with dplyr:
colnames(lkp2)[1] <- "read_code"
colnames(lkp3)[1] <- "read_code"

#search Read 3 lookup table for any of the conditions listed above:
crc_h <- lkp3[grepl('(familial\\sadenomatous\\spolyposis)|(familial\\spolyposis)|(adenomatous\\spolyposis\\scoli)|(hereditary\\sadenomatous\\spolyposis)|(hereditary\\spolyposis)|(polyposis\\ssyndrome)|(familial\\sflat\\sadenoma)|(hereditary\\sflat\\sadenoma)|(flat\\sadenoma\\ssyndrome)|(Gardner)|(Turcot)|(hereditary\\snonpolyposis)|(familial\\snonpolyposis)|(familial\\shamartomatous\\spolyposis)|(hereditary\\shamartomatous\\spolyposis)|(hamartomatous\\spolyposis\\ssyndrome)|(PTEN-hamartoma)|(PTEN\\shamartoma)|(juvenile\\spolyposis\\ssyndrome)|(familial\\sjuvenile\\spolyposis)|(hereditary\\sjuvenile\\spolyposis)|(Cowden)|(Bannayan-Riley-Ruvalcaba)|(Peutz-Jeghers)|(hereditary\\smixed\\spolyposis)|(familial\\smixed\\spolyposis)|(mixed\\spolyposis\\ssyndrome)',
                    lkp3$term_description,ignore.case=TRUE),]
#remove any Read 3 search results which do NOT refer to hereditary CRC syndromes (Gardnerella, Gardner-Diamond, and Adenocarcinoma in adenomatous polyposis coli)
crc_h <- crc_h[-(grep('(Gardnerella)|(Gardner-Diamond)|(Adenocarcinoma\\sin\\sadenomatous\\spolyposis\\scoli)',crc_h$term_description,ignore.case = TRUE)),]
#add any Read codes where the description is in the format 'See [different Read code]'
for (i in 1:nrow(crc_h)) {crc_h <- rbind.data.frame(crc_h, lkp3[grep(crc_h$read_code[i], lkp3$term_description),])}
#search Read 2 lookup table and join results together:
crc_h <- full_join(crc_h, lkp2[grepl('(familial\\sadenomatous\\spolyposis)|(familial\\spolyposis)|(adenomatous\\spolyposis\\scoli)|(hereditary\\sadenomatous\\spolyposis)|(hereditary\\spolyposis)|(polyposis\\ssyndrome)|(familial\\sflat\\sadenoma)|(hereditary\\sflat\\sadenoma)|(flat\\sadenoma\\ssyndrome)|(Gardner)|(Turcot)|(hereditary\\snonpolyposis)|(familial\\snonpolyposis)|(familial\\shamartomatous\\spolyposis)|(hereditary\\shamartomatous\\spolyposis)|(hamartomatous\\spolyposis\\ssyndrome)|(PTEN-hamartoma)|(PTEN\\shamartoma)|(juvenile\\spolyposis\\ssyndrome)|(familial\\sjuvenile\\spolyposis)|(hereditary\\sjuvenile\\spolyposis)|(Cowden)|(Bannayan-Riley-Ruvalcaba)|(Peutz-Jeghers)|(hereditary\\smixed\\spolyposis)|(familial\\smixed\\spolyposis)|(mixed\\spolyposis\\ssyndrome)',lkp2$term_description,ignore.case=TRUE),], by='read_code')
#remove any Read 2 search results which do NOT refer to hereditary CRC syndromes:
crc_h <- crc_h[-(grep('(Gardnerella)|(Gardner-Diamond)|(Adenocarcinoma\\sin\\sadenomatous\\spolyposis\\scoli)',crc_h$term_description.y,ignore.case = TRUE)),]

#find participants with diagnoses of hereditary CRC syndromes:
crc_h_eid <- read_GP(unique(crc_h$read_code))
length(p_40$eid[p_40$eid %in% crc_h_eid$eid]) #33 people have a CRC causing syndrome (primary care records only). This includes:
sum(as.integer(p_40$case[p_40$eid %in% crc_h_eid$eid]), na.rm=TRUE) #2 cases
nrow(p_40[(p_40$eid %in% crc_h_eid$eid) & (p_40$case == '0'),]) #29 controls

#remove participants diagnosed with hereditary CRC syndromes from cohort:
p_40 <- p_40[!(p_40$eid %in% crc_h_eid$eid),]

#4B. exclude participants which are neither cases nor controls (i.e. don't meet study inclusion criteria)
#=================================================================================================
#p_00_v <- p_00 %>% filter(!(case == 'exclude'))
p_40_v <- p_40 %>% filter(!(case == 'exclude'))
#p_50_v <- p_50 %>% filter(!(case == 'exclude'))

#4C. Add UKBB variables
#=====================
#import some tables of UKBB variables for all UKBB participants:
var <- read.csv('00_participant_variables.csv') #UKBB variables
fh <- read.csv('00_participant_FH.csv') #family history (the only reason it's in a separate file is because I decided to include it in
                                        #in the analysis at a later date)

#list of UKBB variable names/codes in the table vs. what they were renamed to, for reference:
#list of UKBB variable names/codes in the table vs. what I renamed them, for reference:
UKBB_var_names <- list('eid' = 'eid',
                       'age' = 'p21022',
                       'dob_m' = "p52",
                       'dob_y' = "p34",
                       'sex' = "p31",
                       'TDI' = "p189",
                       'BMI' = "p21001_i0",
                       'waist_circumference' = "p48_i0",
                       'ever_smoked' = "p20160_i0",
                       'delete_this' = "p20116_i1", #'delete' variables were included by accident or just not useful
                       'smoking_status' = "p20116_i0",
                       'delete' = "p1239_i0",
                       'delete' = "p3456_i0",
                       'alcohol_intake' = "p1558_i0",
                       'diabetes' = "p2443_i0",
                       'processed_meat_intake' = "p1349_i0",
                       'PC1' = "p22009_a1",
                       'PC3' = "p22009_a3",
                       'PC2' = "p22009_a2",
                       'PC4' = "p22009_a4",
                       'PC5' = "p22009_a5")

#renaming UKBB variables in the dataframe:
colnames(var) <- c('eid','recruitment_age','delete','delete','sex','TDI','BMI',
                   'waist_circumference','ever_smoked','delete_this',
                   'smoking_status','delete','delete','alcohol_intake',
                   'diabetes','processed_meat_intake','PC1','PC3','PC2',
                   'PC4','PC5')
#delete unnecessary variables:
var <- var[,-c(3,4,10,12,13)]

#add variables to the dataframe of participants (the one with all the CRC/symptom info)
add_variables <- function(p_xx) {
  p_xx_v <-  left_join(p_xx, var)
  
  #some variables had 'NA' if participants didn't answer whereas some had e.g.
  #'prefer not to answer'. Change to be compatible:
  p_xx_v$ever_smoked[p_xx_v$ever_smoked == ''] <- NA
  p_xx_v$smoking_status[p_xx_v$smoking_status == 'Prefer not to answer'] <- NA
  p_xx_v$alcohol_intake[p_xx_v$alcohol_intake == 'Prefer not to answer'] <- NA
  p_xx_v$diabetes[p_xx_v$diabetes == 'Prefer not to answer'] <- NA
  p_xx_v$diabetes[p_xx_v$diabetes == 'Do not know'] <- NA
  p_xx_v$processed_meat_intake[p_xx_v$processed_meat_intake == 'Prefer not to answer'] <- NA
  p_xx_v$processed_meat_intake[p_xx_v$processed_meat_intake == 'Do not know'] <- NA
  
  return(p_xx_v)
}

p_40_v <- add_variables(p_40_v)

#4D. Add family history
#=====================
add_fh <- function(fh, p_xx_v) {
  fh_temp <- filter(fh, eid %in% p_xx_v$eid)
  colnames(fh_temp) <- c('eid','fh_mat','fh_pat')
  #UKBB family history is formatted as a list of conditions which participants self-reported their parent/s had. Use grep to find entries containing bowel cancer:
  fh_temp$fh_mat[grep('Bowel cancer', fh_temp$fh_mat)] <- 1
  fh_temp$fh_pat[grep('Bowel cancer', fh_temp$fh_pat)] <- 1
  fh_temp[,2:3][fh_temp[,2:3] != 1] <- 0 #if participant did not report mother or father having bowel cancer, set value to 0
  
  p <- dplyr::left_join(p_xx_v, fh_temp)
  
  p$fh_mat<- as.integer(p$fh_mat)
  p$fh_pat<- as.integer(p$fh_pat)
  
  #new variable which is 0 if no parents had bowel cancer, 1 if only one parent had bowel cancer, or 2 if both:
  p$fh <- p$fh_mat + p$fh_pat
  
  return(p)
}

p_40_v <- add_fh(fh, p_40_v)

#4E. Add which symptom type each participant had
#==========================================
#symptom_type is a dataframe: row names contain read codes and column names contain the symptom category each read code falls
#into (e.g. abdominal pain, weight loss).
#read codes have a value of either 1 or NA for each category. change it to 1 or 0, then convert to an integer, so it can be
#used for maths later:
symptom_type[,-1] <- sapply(symptom_type[,-1], function(x) as.integer(x))
symptom_type[is.na(symptom_type)] <- 0

#add which symptom(s) each participant had as a variable:
add_symptoms <- function(p_xx_v, p_sym_filtered_xx) {
  #get read 2 and 3 codes for all participants
  p_st2 <- left_join(p_xx_v[,1], p_sym_filtered_xx) %>%
    distinct(eid, read_2)
  p_st3 <- left_join(p_xx_v[,1], p_sym_filtered_xx) %>%
    distinct(eid, read_3)
  
  #add symptom types to each read code
  p_st2 <- p_st2[!is.na(p_st2$read_2),]
  p_st2 <- rename(p_st2, 'readcode'=read_2)
  p_st2 <- left_join(p_st2, symptom_type)
  
  p_st3 <- p_st3[!is.na(p_st3$read_3),]
  p_st3 <- rename(p_st3, 'readcode'=read_3)
  p_st3 <- left_join(p_st3, symptom_type)
  
  #add together total number of each symptom type for each participant
  p_st <- rbind(p_st2, p_st3) #add read 3 codes table after read 2 codes table
  p_st <- p_st %>% 
    group_by(eid) %>% #for each participant
    summarise(across(2:11, sum)) #sum total number of times each symptom was recorded
  
  #if any value in the symptom columns is >1 (meaning this symptom was recorded more than once
  #for the participant), convert it to 1. We don't need to know how many times one symptom,
  #e.g. rectal bleeding, was recorded, just that it was.
  p <- left_join(p_xx_v, p_st)
  sym_start <- which(colnames(p) == "weightloss")
  sym_end <- ncol(p)
  p[,sym_start:sym_end][p[,sym_start:sym_end] > 1] <- 1
  p[,sym_start:sym_end][is.na(p[,sym_start:sym_end])] <- 0
  
  return(p)
}

p_40_v <- add_symptoms(p_40_v,p_sym_filtered_40)

#4F. Add haemoglobin levels
#==========================
haem <- read_GP(c('44TC.','XaBLm'))
haem$value1 <- as.numeric(haem$value1)
haem <- haem[haem$value1 < 20 & haem$value1 > 1,]
haem <- filter(haem, !is.na(value1))

add_haem <- function(p_xx_v) {
  p <- p_xx_v %>%
    left_join(haem[,colnames(haem) %in% c('eid','event_dt','value1') == TRUE]) %>%
    rename('haem_level'=value1, 'haem_date'=event_dt)
  
  #calculate time difference between haemoglobin measurement and first symptom:
  p$haem_diff <- lubridate::time_length(difftime(p$sym_date, p$haem_date),"days")
  #change all time difference measurements to a positive number
  p$haem_diff <- sqrt(p$haem_diff^2)
  
  #if a participant has multiple haemoglobin measures, only use the one closest
  #to symptom date (also remove some duplicated records that somehow snuck in here)
  p$haem_diff[is.na(p$haem_diff)] <- 0
  p <- p %>% group_by(eid) %>% top_n(-1, haem_diff) %>%
    distinct(.keep_all = TRUE)
  
  return(p)
}

p_40_v <- add_haem(p_40_v)
mean(p_40_v$haem_diff[!is.na(p_40_v$haem_date)], na.rm=TRUE) #=574
#i.e. these haemoglobin measurements were taken an average of 574 days away from first symptom (1.57 years)
sum(!is.na(p_40_v$haem_level))
#88 participants had a haemoglobin measurement

#4G. Add faecal occult blood test (within 8 weeks of symptom) as a variable
#==========================================================================
#This was in response to a reviewer request, to know whether faecal occult blood test result
#near symptoms was predictive of CRC - however there were only 22 participants with a positive
#FOB test within 8 weeks of symptom, 90 participants with a negative FOB test and 50,275 with
#no FOB test at all. This variable was therefore too underpowered for logistic regression.

add_fob_date <- function(fob_codes, p_xx_v) {
  sym_date <- p_xx_v[,colnames(p_xx_v) %in% c('eid','sym_date')]
  gp <- read_GP(fob_codes)
  gp <- gp[gp$eid %in% p_xx_v$eid,]
  gp <- gp[,colnames(gp) %in% c('eid','event_dt')]
  fob_date <- left_join(sym_date,gp)
  fob_date$time_diff <- lubridate::time_length(difftime(fob_date$event_dt, fob_date$sym_date),"days")
  fob_date <- na.omit(fob_date[fob_date$time_diff >= 0 & fob_date$time_diff < 57,])
  return(fob_date)
}

fob_positive <- c('4794.','4793.','4796.','686B.','XaNxT','XaPke')
fob_negative <- c('4792.','4795.','XaNxS')

fob_date_positive <- add_fob_date(fob_positive, p_40_v)
fob_date_negative <- add_fob_date(fob_negative, p_40_v)

p_40_v$fob_8weeks <- rep(NA, nrow(p_40_v))
p_40_v$fob_8weeks[p_40_v$eid %in% fob_date_positive$eid] <- 1
p_40_v$fob_8weeks[p_40_v$eid %in% fob_date_negative$eid] <- 0

#4H. Add info on whether CRC is left or right sided
#===================================================
#There are separate variable columns for left and right side in case someone has both

#get a list of all diagnoses:
unique(c(p_40_v$diagnosis_1, p_40_v$diagnosis_2, p_40_v$diagnosis_3, p_40_v$diagnosis_4))

#left for descending, sigmoid and rectum, splenic flexure
left <- c("C20 Malignant neoplasm of rectum","C18.7 Sigmoid colon","C18.6 Descending colon",
          "C19 Malignant neoplasm of rectosigmoid junction","C18.5 Splenic flexure",
          "B133. Malignant neoplasm of sigmoid colon","B8033 Carcinoma in situ of sigmoid colon",
          "B132. Malignant neoplasm of descending colon","B141. Malignant neoplasm of rectum",
          "B14.. Malignant neoplasm of rectum","B8041 Carcinoma in situ of rectum",
          "B140. Malignant neoplasm of rectosigmoid junction",
          "B8040 Carcinoma in situ of rectosigmoid junction",
          "B137. Malignant neoplasm of splenic flexure of colon")
#right for cecum and ascending colon, hepatic flexure
right <- c("C18.0 Caecum","C18.2 Ascending colon","C18.3 Hepatic flexure",
           "B130. Malignant neoplasm of hepatic flexure of colon",
           "B134. Malignant neoplasm of caecum","B136. Malignant neoplasm of ascending colon")
#unsure for transverse colon or unspecified
unsure <- c("C18.4 Transverse colon","B13.. Malignant neoplasm of colon","C18.9 Colon, unspecified",
            "B13z. Malignant neoplasm of colon NOS","B131. Malignant neoplasm of transverse colon",
            "B803z Carcinoma in situ of colon NOS","C18.8 Overlapping lesion of colon",
            "B803. Carcinoma in situ of colon")

p_40_v$left_sided_CRC <- rep(NA,nrow(p_40_v))
p_40_v$right_sided_CRC <- rep(NA,nrow(p_40_v))

#record diagnoses which are neither right nor left first, then replace with any left or right diagnoses.
#this way, if a patient has e.g. "Colon, unspecified" in Cancer Registry, but ""B14.. Malignant neoplasm of rectum"
#in their GP record, the more specific diagnosis will take precedence and the cancer would be recorded as
#left sided

#add unsure if right or left diagnoses:
p_40_v$left_sided_CRC[(p_40_v$case == 1) & ((p_40_v$diagnosis_1 %in% unsure) | 
                                              (p_40_v$diagnosis_2 %in% unsure) |
                                              (p_40_v$diagnosis_3 %in% unsure) |
                                              (p_40_v$diagnosis_4 %in% unsure))] <- 0
p_40_v$right_sided_CRC[(p_40_v$case == 1) & ((p_40_v$diagnosis_1 %in% unsure) | 
                                               (p_40_v$diagnosis_2 %in% unsure) |
                                               (p_40_v$diagnosis_3 %in% unsure) |
                                               (p_40_v$diagnosis_4 %in% unsure))] <- 0
#add left sided diagnoses:
p_40_v$left_sided_CRC[(p_40_v$case == 1) & ((p_40_v$diagnosis_1 %in% left) | 
                                              (p_40_v$diagnosis_2 %in% left) |
                                              (p_40_v$diagnosis_3 %in% left) |
                                              (p_40_v$diagnosis_4 %in% left))] <- 1
p_40_v$left_sided_CRC[(p_40_v$case == 1) & is.na(p_40_v$left_sided_CRC)] <- 0

#add right sided diagnoses:
p_40_v$right_sided_CRC[(p_40_v$case == 1) & ((p_40_v$diagnosis_1 %in% right) | 
                                               (p_40_v$diagnosis_2 %in% right) |
                                               (p_40_v$diagnosis_3 %in% right) |
                                               (p_40_v$diagnosis_4 %in% right))] <- 1
p_40_v$right_sided_CRC[(p_40_v$case == 1) & is.na(p_40_v$right_sided_CRC)] <- 0

#4I. Convert some categorical variables to factor variables
#========================================================
factorise <- function(p_xx_v) {
  p <- p_xx_v
  p$ever_smoked <- factor(p$ever_smoked)
  p$smoking_status <- factor(p$smoking_status, levels=c("Never","Previous","Current"))
  p$alcohol_intake <- factor(p$alcohol_intake,
                             levels = c('Never','Special occasions only',
                                        'One to three times a month',
                                        'Once or twice a week',
                                        'Three or four times a week',
                                        'Daily or almost daily'))
  p$processed_meat_intake <- factor(p$processed_meat_intake,
                                    levels = c('Never','Less than once a week',
                                               'Once a week','2-4 times a week',
                                               '5-6 times a week',
                                               'Once or more daily'))
  p$sex <- factor(p$sex, levels = c("Female", "Male"))
  p$diabetes <- factor(p$diabetes, levels = c("No", "Yes"))
  p$case <- as.numeric(p$case)
  return(p)
}

p_40_v <- factorise(p_40_v)

#5. Split cohort by ancestry, age, and sex
#==========================================
#whole cohort:
afr <- read.table('AFR_8k_subject_list_unrelated.txt', header = TRUE) #list of unrelated African ancestry participants
eur <- read.table('EUR_451k_subject_list_unrelated.txt', header = TRUE) #list of unrelated European ancestry participants
sas <- read.table('SAS_10k_subject_list_unrelated.txt', header = TRUE) #list of unrelated South Asian ancestry participants
eas <- read.table('EAS_2482_subject_list_unrelated.txt', header = TRUE) #list of unrelated East Asian ancestry participants
amr <- read.table('AMR_468_subject_list_unrelated.txt', header = TRUE) #list of unrelated Admixed American ancestry participants

#splitting by ancestry:
p_40_va <- filter(p_40_v, eid %in% afr$n_eid) #AFR: 7 cases in cohort
p_40_ve <- filter(p_40_v, eid %in% eur$n_eid) #EUR: 625 cases
p_40_vs <- filter(p_40_v, eid %in% sas$n_eid) #SAS: 5 cases
p_40_veas <- filter(p_40_v, eid %in% eas$n_eid) #EAS: 0 controls or cases
p_40_vamr <- filter(p_40_v, eid %in% amr$n_eid) #AMR: 0 controls or cases

#the analysis can only be run in participants with European ancestry due to
#insufficient sample size
#split the European cohort for sensitivity analyses:
#age
p_4049_ve <- filter(p_40_ve, sym_age >= 40 & sym_age <50) #40-49: 55 cases
p_5059_ve <- filter(p_40_ve, sym_age >= 50 & sym_age <60) #50-59: 180 cases
p_6069_ve <- filter(p_40_ve, sym_age >= 60 & sym_age <70) #60-69: 300 cases
p_7079_ve <- filter(p_40_ve, sym_age >= 70) #70-79: 90 cases

#sex
p_40f_ve <- filter(p_40_ve, sex == "Female") #female all ages: 257 cases
p_40m_ve <- filter(p_40_ve, sex == "Male") #male all ages: 368 cases

#age and sex
p_4049f_ve <- filter(p_4049_ve, sex == "Female") #female 40-49: 32 cases
p_4049m_ve <- filter(p_4049_ve, sex == "Male") #male 40-49: 23 cases

p_5059f_ve <- filter(p_5059_ve, sex == "Female") #female 50-59: 83 cases
p_5059m_ve <- filter(p_5059_ve, sex == "Male") #male 50-59: 97 cases

p_6069f_ve <- filter(p_6069_ve, sex == "Female") #female 60-69: 115 cases
p_6069m_ve <- filter(p_6069_ve, sex == "Male") #male 60-69: 185 cases

p_7079f_ve <- filter(p_7079_ve, sex == "Female") #female 70-79: 27 cases
p_7079m_ve <- filter(p_7079_ve, sex == "Male") #male 70-79: 63 cases


#put all data frames in a list
#=============================
p_v <- list(p_40_ve, p_4049_ve, p_5059_ve, p_6069_ve, p_7079_ve,
            p_40f_ve, p_40m_ve,
            p_4049f_ve, p_4049m_ve, p_5059f_ve, p_5059m_ve,
            p_6069f_ve, p_6069m_ve, p_7079f_ve, p_7079m_ve)
names(p_v) <- c('p_40_ve', 'p_4049_ve', 'p_5059_ve', 'p_6069_ve', 'p_7079_ve',
                'p_40f_ve', 'p_40m_ve',
                'p_4049f_ve', 'p_4049m_ve', 'p_5059f_ve', 'p_5059m_ve',
                'p_6069f_ve', 'p_6069m_ve', 'p_7079f_ve', 'p_7079m_ve')

#6A. Generate polygenic risk score (PRS) (in this code the nomenclature used was genetic risk score (GRS))
#====================================
#generate GRS for all UKBB participants using source_url("https://raw.githubusercontent.com/hdg204/Rdna-nexus/main/install.R")
#note this requires a tab-separated table of risk-associated variants, with the following columns: chr, bp, other, effect, weight
#(effect and other refer to the alleles which do or don't impact CRC risk, weight is the beta)
grs <- generate_grs('00_GRS_snplist.tsv')

#copy whole cohort dataframe and add GRS
p_grs <- filter(grs$grs, eid %in% p_v$p_40_ve$eid)
p_40_v <- left_join(p_v$p_40_ve, p_grs)

#get mean and standard deviation of PRS
gm <- mean(p_40_v$grs, na.rm = T)
gsd <- sd(p_40_v$grs, na.rm = T)

#add GRS and other related measures to cohort and subcohorts:
add_grs <- function(p_xx_v) {
  #add GRS to participants dataframe
  p_grs <- filter(grs$grs, eid %in% p_xx_v$eid)
  p <- left_join(p_xx_v, p_grs)
  
  #calculate z score (like GRS but scaled, so 1 unit increase = 1 standard deviation increase):
  p$zscore <- (p$grs - gm)/gsd
  
  #divide GRS into quintiles:
  scorequin <- quantile(p$grs, probs = seq(0, 1, 1/5), na.rm = T)
  p <- p %>% mutate(grs_quintile=NA)
  p$grs_quintile[p$grs<scorequin[2]] <- 1
  p$grs_quintile[p$grs<scorequin[3] & p$grs>scorequin[2]] <- 2
  p$grs_quintile[p$grs<scorequin[4] & p$grs>scorequin[3]] <- 3
  p$grs_quintile[p$grs<scorequin[5] & p$grs>scorequin[4]] <- 4
  p$grs_quintile[p$grs>scorequin[5]] <- 5
  
  return(p)
}

p_v <- lapply(p_v, function(x) add_grs(x))

#now check GRS distribution across age groups - it should be the same because GRS
#doesn't change with age
ggplot() + geom_density(aes(x=p_v$p_4049_ve$grs), alpha = 0.4) +
  geom_density(aes(x=p_v$p_5059_ve$grs), alpha = 0.4) +
  geom_density(aes(x=p_v$p_6069_ve$grs), alpha = 0.4) +
  geom_density(aes(x=p_v$p_7079_ve$grs), alpha = 0.4) +
  geom_density(aes(x=p_v$p_40_ve$grs), alpha = 0.4) +
  xlab('GRS')
#yes, almost exactly the same - nothing weird going on!

#comparing case v control GRS across all groups:
ggplot() + geom_density(aes(x=p_v$p_4049_ve$grs[p_v$p_4049_ve$case == 1]), alpha = 0.4, color='red') +
  geom_density(aes(x=p_v$p_4049_ve$grs[p_v$p_4049_ve$case == 0]), alpha = 0.4) +
  geom_density(aes(x=p_v$p_5059_ve$grs[p_v$p_5059_ve$case == 1]), alpha = 0.4, color='red') +
  geom_density(aes(x=p_v$p_5059_ve$grs[p_v$p_5059_ve$case == 0]), alpha = 0.4) +
  geom_density(aes(x=p_v$p_6069_ve$grs[p_v$p_6069_ve$case == 1]), alpha = 0.4, color='red') +
  geom_density(aes(x=p_v$p_6069_ve$grs[p_v$p_6069_ve$case == 0]), alpha = 0.4) +
  geom_density(aes(x=p_v$p_7079_ve$grs[p_v$p_7079_ve$case == 1]), alpha = 0.4, color='red') +
  geom_density(aes(x=p_v$p_7079_ve$grs[p_v$p_7079_ve$case == 0]), alpha = 0.4) +
  geom_density(aes(x=p_v$p_40_ve$grs[p_v$p_40_ve$case == 1]), alpha = 0.4, color='red') +
  geom_density(aes(x=p_v$p_40_ve$grs[p_v$p_40_ve$case == 0]), alpha = 0.4)

#comparing one cohort at a time:
d4049 <- ggplot() + geom_density(aes(x=p_v$p_4049_ve$grs[p_v$p_4049_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_4049_ve$grs[p_v$p_4049_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_4049_ve$grs[p_v$p_4049_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_4049_ve$grs[p_v$p_4049_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='All')
d5059 <- ggplot() + geom_density(aes(x=p_v$p_5059_ve$grs[p_v$p_5059_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_5059_ve$grs[p_v$p_5059_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_5059_ve$grs[p_v$p_5059_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_5059_ve$grs[p_v$p_5059_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='All')
d6069 <- ggplot() + geom_density(aes(x=p_v$p_6069_ve$grs[p_v$p_6069_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_6069_ve$grs[p_v$p_6069_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_6069_ve$grs[p_v$p_6069_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_6069_ve$grs[p_v$p_6069_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='All')
d7079 <- ggplot() + geom_density(aes(x=p_v$p_7079_ve$grs[p_v$p_7079_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_7079_ve$grs[p_v$p_7079_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_7079_ve$grs[p_v$p_7079_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_7079_ve$grs[p_v$p_7079_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='All')
d40 <- ggplot() + geom_density(aes(x=p_v$p_40_ve$grs[p_v$p_40_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_40_ve$grs[p_v$p_40_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_40_ve$grs[p_v$p_40_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_40_ve$grs[p_v$p_40_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey'))

d40f <- ggplot() + geom_density(aes(x=p_v$p_40f_ve$grs[p_v$p_40f_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_40f_ve$grs[p_v$p_40f_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_40f_ve$grs[p_v$p_40f_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_40f_ve$grs[p_v$p_40f_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='Female')
d40m <- ggplot() + geom_density(aes(x=p_v$p_40m_ve$grs[p_v$p_40m_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_40m_ve$grs[p_v$p_40m_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_40m_ve$grs[p_v$p_40m_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_40m_ve$grs[p_v$p_40m_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='Male')
d4049f <- ggplot() + geom_density(aes(x=p_v$p_4049f_ve$grs[p_v$p_4049f_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_4049f_ve$grs[p_v$p_4049f_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_4049f_ve$grs[p_v$p_4049f_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_4049f_ve$grs[p_v$p_4049f_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='Female')
d4049m <- ggplot() + geom_density(aes(x=p_v$p_4049m_ve$grs[p_v$p_4049m_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_4049m_ve$grs[p_v$p_4049m_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_4049m_ve$grs[p_v$p_4049m_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_4049m_ve$grs[p_v$p_4049m_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='Male')
d5059f <- ggplot() + geom_density(aes(x=p_v$p_5059f_ve$grs[p_v$p_5059f_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_5059f_ve$grs[p_v$p_5059f_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_5059f_ve$grs[p_v$p_5059f_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_5059f_ve$grs[p_v$p_5059f_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='Female')
d5059m <- ggplot() + geom_density(aes(x=p_v$p_5059m_ve$grs[p_v$p_5059m_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_5059m_ve$grs[p_v$p_5059m_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_5059m_ve$grs[p_v$p_5059m_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_5059m_ve$grs[p_v$p_5059m_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='Male')
d6069f <- ggplot() + geom_density(aes(x=p_v$p_6069f_ve$grs[p_v$p_6069f_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_6069f_ve$grs[p_v$p_6069f_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_6069f_ve$grs[p_v$p_6069f_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_6069f_ve$grs[p_v$p_6069f_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='Female')
d6069m <- ggplot() + geom_density(aes(x=p_v$p_6069m_ve$grs[p_v$p_6069m_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_6069m_ve$grs[p_v$p_6069m_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_6069m_ve$grs[p_v$p_6069m_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_6069m_ve$grs[p_v$p_6069m_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='Male')
d7079f <- ggplot() + geom_density(aes(x=p_v$p_7079f_ve$grs[p_v$p_7079f_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_7079f_ve$grs[p_v$p_7079f_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_7079f_ve$grs[p_v$p_7079f_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_7079f_ve$grs[p_v$p_7079f_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='Female')
d7079m <- ggplot() + geom_density(aes(x=p_v$p_7079m_ve$grs[p_v$p_7079m_ve$case == 1]), alpha = 0.4, linetype = 'dashed') +
  geom_density(aes(x=p_v$p_7079m_ve$grs[p_v$p_7079m_ve$case == 0]), alpha = 0.4) +
  geom_vline(xintercept = mean(p_v$p_7079m_ve$grs[p_v$p_7079m_ve$case == 1]), linetype='dashed') +
  geom_vline(xintercept = mean(p_v$p_7079m_ve$grs[p_v$p_7079m_ve$case == 0])) + xlab('PRS') +
  scale_x_continuous(breaks=seq(-1.5,3.5,by=0.5)) + theme(axis.text = element_text(size=6), axis.title = element_text(size = 10, colour = 'darkgrey')) +
  labs(subtitle='Male')

#install.packages('patchwork')
library(patchwork)

#display plots together:
gridfull <- d40 / (d40f + d40m) + plot_annotation(title = 'Full Cohort', theme = theme(plot.title = element_text(hjust = 0.5)))

grid4049 <- d4049 + d4049f + d4049m + plot_annotation(title = 'Age 40-49', theme = theme(plot.title = element_text(hjust = 0.5)))
grid5059 <- d5059 + d5059f + d5059m + plot_annotation(title = 'Age 50-59', theme = theme(plot.title = element_text(hjust = 0.5)))
grid6069 <- d6069 + d6069f + d6069m + plot_annotation(title = 'Age 60-69', theme = theme(plot.title = element_text(hjust = 0.5)))
grid7079 <- d7079 + d7079f + d7079m + plot_annotation(title = 'Age 70-79', theme = theme(plot.title = element_text(hjust = 0.5)))
gridsub <- grid4049 / grid5059 / grid6069 / grid7079 + plot_annotation(title = 'Subcohorts', theme = theme(plot.title = element_text(hjust = 0.5)))

prs_cohort_distribution <- gridfull / wrap_elements(full = gridsub) + plot_layout(ncol=1, nrow=3, heights=c(0.5,0.5,4))

#save to file:
pdf(file = "prs_cohort_distribution.pdf",
    width = 8.27,
    height = 11.67)
prs_cohort_distribution
dev.off()


#7A. Do logistic  regression testing
#==================================
#make a list with each variable plus its p value and odds ratio for predicting cases or controls:
#-------------------------
or_list <- function(p_xx_v, col_list) {
  lr <- replicate(length(col_list), list(OR = NA, L95 = NA, U95 = NA, p = NA), simplify = FALSE)
  
  #print cohort age
  print(paste0('age:',min(p_xx_v$sym_age),'-',max(p_xx_v$sym_age)))
  
  for (i in 1:length(col_list)) {
    print(i) #print column/iteration
    
    x <- colnames(p_xx_v)[col_list][i]
    print(paste0('case~',x))
    if (length(unique(na.exclude(p_xx_v[[col_list[i]]]))) != 1) { #if there's only one value in the column glm can't run
      basemod <- glm(noquote(paste0('case~',x)), data=p_xx_v, family=binomial) %>% summary()
      lr[[i]][[1]] <- round(exp(basemod$coefficients[2,1]),2)
      lr[[i]][[2]] <- round(exp(basemod$coefficients[2,1]-1.96*basemod$coefficients[2,2]),2)
      lr[[i]][[3]] <- round(exp(basemod$coefficients[2,1]+1.96*basemod$coefficients[2,2]),2)
      lr[[i]][[4]] <- (signif(basemod$coefficients[2,4],2))
    } else {
      print('only 1 value in column, returning NA')
      lr[[i]][[1]] <- NA
      lr[[i]][[2]] <- NA
      lr[[i]][[3]] <- NA
      lr[[i]][[4]] <- NA
    }
    names(lr)[i] <- x
  }
  return(lr)
}

lr_v <- lapply(p_v, function(x) or_list(x, c(4,7,16:23,26:45))) #indexing because some columns don't contain variables, & some variables don't contain enough data
#note: it turns out that logistic regression of the ordinal variables smoking status, alcohol intake and processed meat intake is
#only assessing the first level of the variable (e.g. comparing 'never smoked' to 'previously smoked', but not comparing 'never' to 'current',
#or 'previous' to 'current'.)
#This wasn't fixed at this stage in the pipeline. However, supplementary table 6 contains logistic regression results for all levels
#of ordinal variables. Additionally, no ordinal variable was included in the final integrated risk model, so this error does not affect
#final study results.

#make a list of only significant (p < 0.05 / 30, Bonferroni correction) variables:
lrp_v <- replicate(length(lr_v), list(NA))

#following function only works if every cohort has same number of variables (all have 30)
temp <- unlist(lr_v)
for (i in 1:length(lr_v)) { #for all 30 cohorts
  x <- c()
  print(names(lr_v)[i])
  v <- length(lr_v[[i]]) #for all 30 variables
  for (j in 1:v) {
    print(names(lr_v[[i]][j]))
    #if p-value of variable < 0.05/30, append this variable number to a vector called 'x' of variables to keep
    n <- v*4*(i-1) + j*4
    if (!is.na(temp[n]) & temp[n] < (0.05/v)) {x <- c(x,j); print('YES'); print(temp[v*4*(i-1) + j*4])}
    else {print('NO'); print(temp[v*4*(i-1) + j*4])}
    print(paste0('X IS ',x))
    lrp_v[[i]] <- lr_v[[i]][x] #add logistic regression results for the cohort to lrp_v, only including variables with sig p values
  }
}
names(lrp_v) <- names(lr_v)

#8. Get ROCAUC measures for signifcantly associated variables
#=========================================================
# Takes a formula and a data frame and returns a roc object (Harry Green's code)
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

#add ROCAUC values to list of logistic regression results for significantly associated variables
#-----------------------------------------------------------------------------------------------
add_rocauc <- function(p_x, lr_x, lrp_x) {
  for (i in 1:length(lr_x)) { #30 cohorts
    chrt <- names(lr_x)[i]
    print(paste0(i,' ',chrt))
    for (j in 1:length(lr_x[[i]])) {  #30 variables
      var <- names(lr_x[[i]][j])
      if (!is.na(lr_x[[i]][[j]][[1]]) & #if it was possible to do logistic regression on this variable previously, then it should be possible to do ROCAUC
          !is.na(p_v[[i]][,which(colnames(p_v[[i]]) == var)][p_v[[i]][5] == 1]) #check if cases exist
      ) {
        print(paste0(j,' ',var))
        basemod <- rocauc(noquote(paste0('case~',var)), p_x[[i]], F) #calculate ROC AUC
        lr_x[[i]][[j]][[5]] <- round(basemod$ci[2],2) #add ROCAUC
        lr_x[[i]][[j]][[6]] <- round(basemod$ci[1],2)
        lr_x[[i]][[j]][[7]] <- round(basemod$ci[3],2)
      } else {
        print('only 1 value in column, returning NA')
        lr_x[[i]][[j]][[5]] <- NA
        lr_x[[i]][[j]][[6]] <- NA
        lr_x[[i]][[j]][[7]] <- NA
      }
      #names(lr_x[[i]][[j]][1:6]) <- c('OR','ORL95','ORU95','ROCAUC','ROCL95','ROCU95')
      #no idea why renaming doesn't work
    }
  }
  return(lr_x)
}

rocauc_v <- add_rocauc(p_v,lr_v)

#8A. Build the risk model by adding variables which cause biggest jump in ROCAUC
#==============================================================================
rocauc_jump <- function(lrp_xx, p_xx_v) {
  #character vector of named variables:
  factors <- names(lrp_xx)
  #create empty list:
  rocp <- replicate(length(lrp_xx), list(ROC = 0, LB = 0, UB = 0), simplify = FALSE)
  
  for (i in 1:length(lrp_xx)) { #e.g., for i in 1:7
    for (j in 1:length(factors)) { #'factors' decreases in size with each loop, so this will be 1:7, then 1:6, then 1:5...
      
      #print out which model is currently being tested + the ROCAUC of that model:
      print(paste0(names(rocp)[1:i-1],collapse='+'))
      print(factors[j])
      print(paste0('case~',paste0(names(rocp)[1:i-1],collapse='+'),'+',factors[j]))
      
      baseauc <- rocauc(paste0('case~',paste0(names(rocp)[1:i-1],collapse='+'),'+',factors[j]), p_xx_v, FALSE)
      print(baseauc$ci[2])
      print('-------------------')
      
      if (((baseauc$ci[2] == rocp[[i]]$ROC) == TRUE) && ((names(rocp)[i] != factors[j]) == TRUE)) {
        warning(paste('ROC AUC of',factors[j],'matched',names(rocp)[i]))
        #this warning means two different risk models share the highest ROCAUC (so far), so one of them isn't being included in the calculation
        
      } else if ((baseauc$ci[2] > rocp[[i]]$ROC) == TRUE) {
        rocp[[i]]$ROC <- baseauc$ci[2]
        rocp[[i]]$LB <- baseauc$ci[1]
        rocp[[i]]$UB <- baseauc$ci[3]
        names(rocp)[i] <- factors[j]
        maxf <- factors[j]
      }
    }
    #remove the factor with the best ROC AUC from factors
    factors <- factors[factors != maxf]
  }
  return(rocp)
}

rocauc_irm <- replicate(length(lr_v), list(NA))

for (i in 1:length(p_v)) {
  #get list of variables significantly associated with logistic regression, excluding FOB and smoking status
  #FOB excluded because it doesn't make sense to exclude an existing (and outdated) diagnostic test in the risk model
  #smoking status excluded because logistic regression showed people who previously smoked were more at risk of CRC
  #in the two-year period then people who never smoked or reported currently smoking. This doesn't make much sense
  #and might be due to underlying bias in this variable.
  x <- lrp_v[[i]][which(names(lrp_v[[i]]) != 'fob' & names(lrp_v[[i]]) != 'smoking_status')]
  
  if (length(x) != 0) {
    #do rocauc jump calculation for remaining variables
    rocauc_irm[[i]] <- rocauc_jump(x,p_v[[i]])
  } else {rocauc_irm[[i]] <- NA}
  
}
names(rocauc_irm) <- names(lrp_v)

#8B plot the increase in ROCAUC as more variables are added to risk model
#======================================================================
#create data frame to use in plot:
#----------------------------------
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

rocauc_frame_40e <- rocauc_frame(rocauc_irm$p_40_ve)

#plot:
#----
#graph theme by Harry Green:
HGtheme=theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))
rocauc_plot_e <- ggplot(data=rocauc_frame_40e, aes(x=var, y=aucs, ymin=LB, ymax=UB)) +
  geom_point(colour = '#00C896', size=3) + geom_line(linetype = "dashed") + geom_errorbar(width=0.1) +
  HGtheme + xlab("variables added to integrated risk model") + ylab("ROC AUC") +
  scale_x_continuous(breaks=0:8, labels=c("none","age at\nfirst symptom", "& abdominal\npain",
                                          "& PRS","& sex","& ever\nsmoked","& rectal\nbloodloss",
                                          "& waist\ncircumference",
                                          "& change in\nbowel habits")) +
  scale_y_continuous(breaks=seq(0.5,0.85,by=0.05)) + theme(plot.margin = unit(c(5.5, 5.5, 20, 30), "points"))


#9 Test different models with Akaike information criterion (AIC)
#================================================================
#install.packages('AICcmodavg')
library(AICcmodavg)

#list for all possible logistic regression formulas
aic_irm <- replicate(length(lr_v), list(NA))
#list for results of AIC testing on all possible models
aic_models <- replicate(length(lr_v), list(NA))

#get all possible formulas for all variables for every cohort (it only took 10 nested for/if loops...)
for (i in 1:length(p_v)) { #15
  print(names(p_v[i]))
  
  vlen <- length(lrp_v[[i]][which(names(lrp_v[[i]]) != 'fob' & names(lrp_v[[i]]) != 'smoking_status')])
  
  #if more than 0 significant variables:
  if (vlen != 0) {
    form_l <- sapply((1:vlen), function(x)
      combn(names(lrp_v[[i]][which(names(lrp_v[[i]]) != 'fob' & names(lrp_v[[i]]) != 'smoking_status')]), x))
    
    if (length(form_l) != 0) { #if more than 0 significant variables (again - checking for the same thing twice stops the function breaking?)
      form_c <- character()
      for (j in 1:vlen) {
        if (length(ncol(form_l[[j]])) != 0) { #if more than 1 significant variable:
          for (k in 1:ncol(form_l[[j]])) {
            #attach ~ to the beginning of each variable
            form_c <- c(form_c, paste0('~', paste0(form_l[[j]][,k], collapse = '+')))
          }
          #add all possible variable combinations to a list, 'models'
          #place first model in list
          models <- list(lm(paste0('case',form_c[1]), data = p_v[[i]])) 
          #add formula as a name to the list (this was a pain):
          if (length(models[[1]]$terms[[3]]) <= 1) {
            names(models)[1] <- paste0(as.character(models[[1]]$terms[[2]]), '~', as.character(models[[1]]$terms[[3]]))
          } else {
            x <- c()
            for (m in 2:length(models[[1]]$terms[[3]])) {x <- c(x,as.character(models[[1]]$terms[[3]][[m]]))}
            y <- (models)[1] <- paste0(as.character(models[[1]]$terms[[2]]), as.character(models[[1]]$terms[[1]]), paste0(x,collapse='+'))
            y <- gsub('++','+',y,fixed=T)
            y <- gsub('~+','~',y,fixed=T)
            y <- gsub(' ','',y)
            names(models)[l] <- y
          }
          for (l in 2:length(form_c)) { #then add all other models
            models[[length(models)+1]] <- lm(paste0('case',form_c[l]), data = p_v$p_40_ve)
            #naming:
            if (length(models[[l]]$terms[[3]]) <= 1) {
              names(models)[l] <- paste0(as.character(models[[l]]$terms[[2]]), '~', as.character(models[[l]]$terms[[3]]))
            } else {
              x <- c()
              for (n in 2:length(models[[l]]$terms[[3]])) {x <- c(x,as.character(models[[l]]$terms[[3]][[n]]))}
              y <- paste0(as.character(models[[l]]$terms[[2]]), '~', paste0(x,collapse='+'))
              y <- gsub('++','+',y,fixed=T)
              y <- gsub('~+','~',y,fixed=T)
              y <- gsub(' ','',y)
              names(models)[l] <- y
            }
          }
        } else { #else if only 1 significant variable:
          #add the one formula of case~variable to 'models' list
          form_c <- paste0('~',form_l)
          models <- list(lm(paste0('case',form_c[1]), data = p_v[[i]])) 
        }
      }
      #add models list to final list of all possible variable combinations for all cohorts:
      aic_irm[[i]] <- models
      #run AIC testing on all models
      print('running AIC')
      aic_models[[i]] <- aictab(cand.set = models, modnames = form_c)
    }
  } else { #if there are no significant variables:
    aic_irm[[i]] <- NA} #just add NA to the list
}

names(aic_irm) <- names(p_v)
names(aic_models) <- names(p_v)

#10 Conclusion and model ROCAUCs
#=================================
#Building model iteratively with ROCAUC suggests best risk model is ~sym_age+abdo_pain+grs+sex
#Get ROCAUC of this model:
rocauc('case~sym_age+abdo_pain+grs+sex', p_v$p_40_ve, TRUE)
#AIC suggests best model is ~sym_age+abdo_pain+grs+sex+rectal_bloodloss+change_bowel_habit
#Get ROCAUC:
rocauc('case~sym_age+abdo_pain+grs+sex+rectal_bloodloss+change_bowel_habit', p_v$p_40_ve, TRUE)              
                     
#And of GRS alone:
rocauc('case~grs', p_v$p_40_ve, TRUE)
                     
