This folder contains the Read codes for CRC symptoms and CRC diagnosis used in the analysis.</p>

**Symptoms**</p>
200 Read codes (filtered to 159) were provided by the Diagnosis of Symptomatic Cancer Optimally (DISCO) consortium, University of Exeter. These are available upon reasonable request to either the first author (Bethan Mallabar-Rimmer) or corresponding author (Dr. Harry Green) - [link to MedRXiv]. Thanks to Dr. Sarah Price for first identifying these 200 Read codes.</p>

19 additional Read codes were found by by Dr. Matthew Barclay, University College London.</p>

Using the aforementioned 178 (159+19) codes as input, 57 further Read codes were identified by Bethan Mallabar-Rimmer using the find_read_codes function: https://github.com/bethan-mallabar-rimmer/CRC_IRM/blob/main/find_read_codes/find_read_codes.R </p>

Codes found by MB and BMR can be found in CRC_symptoms_76_codes.csv</p>

**Diagnosis**</p>
Study participants with a CRC diagnosis were identified by looking for the ICD-10 codes C18, C19, C20 and C21 in Hospital Episode Statistic, Cancer Registry and death records, or by looking for the 59 Read codes in CRC_diagnosis_59_codes.csv in GP records. These 59 Read codes were found by cropping Read codes from Fairhurst et al.(1) to 3 characters, and using this as input to the find_read_codes function.</p>

(1) Fairhurst C, Watt I, Martin F, Bland M, Brackenbury WJ. Exposure to sodium channel-inhibiting drugs and cancer survival: protocol for a cohort study using the QResearch primary care database. BMJ Open. 2014. 4(11):e006604. Article link: https://bmjopen.bmj.com/content/4/11/e006604 For list of codes: https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/17/codelist/res17-colorectal-cancer/
