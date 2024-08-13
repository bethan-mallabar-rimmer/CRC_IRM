This folder contains the Read codes for CRC symptoms and CRC diagnosis used in the analysis.</p>

**Symptoms**</p>
200 Read codes (filtered to 151) were provided by the Diagnosis of Symptomatic Cancer Optimally (DISCO) consortium, University of Exeter. These are available upon reasonable request to either the first author (Bethan Mallabar-Rimmer) or corresponding author (Dr. Harry Green) - https://doi.org/10.1038/s41431-024-01654-3 . Thanks to Dr. Sarah Price for identifying these 200 Read codes.</p>

19 additional Read codes were found by by Dr. Matthew Barclay, University College London.</p>

Using the aforementioned 170 (151+19) codes as input, 57 further Read codes were identified by Bethan Mallabar-Rimmer using the find_read_codes function: https://github.com/bethan-mallabar-rimmer/CRC_IRM/blob/main/find_read_codes/find_read_codes.R </p>

Codes found by MB and BMR can be found in CRC_symptoms_76_codes.csv</p>

**Diagnosis**</p>
Study participants with a CRC diagnosis were identified by looking for the ICD-10 codes C18.0*, C18.2, C18.3, C18.4, C18.5, C18.6, C18.7, C18.8, C18.9, C19, C20 in Hospital Episode Statistic, Cancer Registry and death records, or by looking for the 49 Read codes in CRC_diagnosis_49_codes.csv in GP records. These 49 Read codes were found by cropping Read codes from Fairhurst et al.(1) to 3 characters, and using this as input to the find_read_codes function.</p>

*C18.1, cancer of the appendix, was excluded</p>

(1) Fairhurst C, Watt I, Martin F, Bland M, Brackenbury WJ. Exposure to sodium channel-inhibiting drugs and cancer survival: protocol for a cohort study using the QResearch primary care database. BMJ Open. 2014. 4(11):e006604. Article link: https://bmjopen.bmj.com/content/4/11/e006604 For list of codes: https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/17/codelist/res17-colorectal-cancer/
