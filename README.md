# This repository contains files and pipeline for 'Colorectal cancer risk stratification using a polygenic risk score in symptomatic patients presenting to primary care – a UK Biobank retrospective cohort study'.
MedRXiv link: [insert]

**analysis.R** contains all the code used for the analysis. It mostly runs on R version 4.1.1.

It's split into the following sections. Some sections require files as input (often hosted securely on UKBB and can't be shared publicly - for reproducibility, I've tried to describe relevant details of these files e.g. column headers in code annotations). Sometimes analysis decisions were made based on descriptive graphs of the data. In summary, the code isn't designed to run from beginning to end non-stop & will need adapting if applied to different datasets.

Contents:
  1. Identify a list of CRC symptoms
  2. Find UKBB participants with symptoms and make a table of earliest symptom for each participant
  3. Find earliest occurence of CRC for participants (identify cases & controls) and remove participants with hereditary syndromes increasing risk of CRC.
  4. Add all lifestyle/symptom/health variables to participant data frame
  5. Check case/control numbers by ancestry. Analysis continued with only European cohort due to case numbers and unrelated individuals to avoid bias.
  6. Generate the polygenic risk score for all participants and work out quintiles
  7. Split cohort 80:20 into training and testing groups for validation. Stratify both testing & training cohorts by age and sex.
  8. In training cohort: Logistic regression analysis to find variables associated with case or control groups.
  9. In training cohort: Calculate ROCAUC of each variable and build integrated risk model iteratively based on ROCAUC values, with 5-fold cross validation.
  10. In training cohort: Compare all possible integrated risk models with AIC.
  11. Results of steps 9 and 10 concurred that a 6-variable integrated risk model performed best in the training cohort. Evaluate this model in the testing cohort.</p>

Abbreviations: AIC, Akaike information criterion, CRC = colorectal cancer, ROCAUC = receiver operating characteristic area under the curve</p>

The **find_read_codes** folder contains an R function which takes read codes as input and returns similar read codes.</p>

**CRC_read_codes** contains some of the 227 Read codes for CRC symptoms which were used to include participants in this study (others are available upon request - see folder readme file), and the list of 49 Read codes used to identify cases of CRC in participants' GP records.</p>
