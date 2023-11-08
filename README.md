# This repository is under construction. Files and pipleine will be uploaded here before submission to MedRxiv.
In the meantime, a draft version of the repository can be found at: https://github.com/ID690016874/HPDM042

**analysis.R** contains all the code used for the analysis. It mostly runs on R version 4.1.1.

It's split into the following sections. Some sections require files as input, and sometimes analysis decisions were made based on descriptive graphs of the data. I.e. the code isn't designed to run from beginning to end non-stop & will probably need adapting to different datasets.

Contents:
  1. Identify a list of CRC symptoms
  2. Find UKBB participants with symptoms and make a table of earliest symptom for each participant
  3. Find earliest occurence of CRC for participants (identify cases & controls)
  4. Add all lifestyle/symptom/health variables to participant data frame
  5. Check case/control numbers by ancestry. Analysis continued with only European cohort due to case numbers. Split cohort into subcohorts by age and sex.
  6. Generate the polygenic risk score for all participants and work out quintiles
  7. Logistic regression analysis
  8. Calculate ROCAUC of each variable and build integrated risk model iteratively based on ROCAUC values
  9. Compare all possible integrated risk models with AIC

The **find_read_codes** folder contains an R function which takes read codes as input and returns similar read codes.

**CRC_symptom_read_codes** contains the list of 235 read codes for CRC symptoms which were used to include participants in this study.

**CRC_read_codes** contains the list of 59 read codes used to identify cases of CRC in participants' GP records.

**tables_and_figures** contains code used to generate all tables and figures (including supplementary)



Abbreviations: AIC, Akaike information criterion, CRC = colorectal cancer, GP = general practice, ROCAUC = receiver operating characteristic area under the curve
