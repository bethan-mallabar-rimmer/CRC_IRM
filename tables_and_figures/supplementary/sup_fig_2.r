
#required files: tab-separated table of risk-associated variants, with the following columns: chr, bp, other, effect, weight.
#Derived from:
#Fernandez-Rozadilla C, Timofeeva M, Chen Z, Law P, Thomas M, Schmit S, et al. Deciphering colorectal cancer genetics through
#multi-omic analysis of 100,204 cases and 154,587 controls of European and east Asian ancestries. Nat Genet. 2023;55:89–99

#generate GRS for all UKBB participants
source_url("https://raw.githubusercontent.com/hdg204/Rdna-nexus/main/install.R")
library(ggplot2)

grs <- generate_grs('00_GRS_snp_list.tsv')

#some of these SNPs have a bp location ending in 000000000 and I'm sure that's not accurate
#so try and replace the bp location based on the rsIDs:
bp <- grs$report$bp[grs$report$included == "MISSING"][-19]
rsids <- c('rs5028523','rs12137232','rs12078075',
           'rs2078095','rs497916','rs7299936',
           'rs1078563','rs4668039','rs2388976',
           'rs10006803','rs1426947','rs472959',
           'rs145997965','rs6911915','rs151127921',
           'rs10978941','rs7038489','rs11789898')
#The following commented code was run in terminal not from this script:
#grep 'rs5028523' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c1_b0_v3.mfi.txt
#grep 'rs12137232' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c1_b0_v3.mfi.txt
#grep 'rs12078075' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c1_b0_v3.mfi.txt
#grep 'rs2078095' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c1_b0_v3.mfi.txt
#grep 'rs497916' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c11_b0_v3.mfi.txt
#grep 'rs7299936' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c12_b0_v3.mfi.txt
#grep 'rs1078563' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c13_b0_v3.mfi.txt
#grep 'rs4668039' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c2_b0_v3.mfi.txt
#grep 'rs2388976' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c4_b0_v3.mfi.txt
#grep 'rs10006803' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c4_b0_v3.mfi.txt
#grep 'rs1426947' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c4_b0_v3.mfi.txt
#grep 'rs472959' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c5_b0_v3.mfi.txt
#grep 'rs145997965' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c6_b0_v3.mfi.txt
#grep 'rs6911915' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c6_b0_v3.mfi.txt
#grep 'rs151127921' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c6_b0_v3.mfi.txt
#grep 'rs10978941' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c9_b0_v3.mfi.txt
#grep 'rs7038489' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c9_b0_v3.mfi.txt
#grep 'rs11789898' ../../../mnt/project/Bulk/Imputation/UKB\ imputation\ from\ genotype/ukb22828_c9_b0_v3.mfi.txt
chr <- c('1','1','1','1','11','12','13','2','4','4','4','5','6','6','6','9','9','9')
new_bps <- c(172864224,201885446,205163798,240408346,118758089,115934000,110352851,169025379,115502406,151501208,
             175420523,172324558,106482613,117809031,133993925,110373819,136682468,136925663)
bps <- data.frame(bp = bp, new_bps = new_bps, rsids=rsids, chr=chr)
snps <- read.csv('00_GRS_snp_list.tsv',sep='')
snps2 <- left_join(snps,bps,by=c('bp','chr'))
snps2 <- snps2[-22,]
snps2 <- snps2[-146,]
snps3 <- snps2
snps3$bp[!is.na(snps3$new_bps)] <- snps3$new_bps[!is.na(snps3$new_bps)]
snps3 <- snps3[,1:5]

#now export amended sheet to csv (I then converted this to .tsv in Excel)
write.csv(snps3, file='snps_amended.csv', row.names = FALSE)


#generate a GRS, which outputs quality of imputation for each SNP:
grs <- generate_grs('snps_amended.tsv')


#plot quality of SNP imputations:
#one of the loci included in the GRS is duplicated, so exclude that one:
grs$report$bp[duplicated(grs$report$bp)]
which(grs$report$bp == '136682468') #94 95
grs_info <- as.data.frame(grs$report$info[-95])
grs_info$rsid <- grs$dosage$variants$rsid
colnames(grs_info)[1] <- 'infoscore'
grs_info$rsid[grs_info$infoscore >= 0.9] <- ''
grs_info$index <- 1:nrow(grs_info)

infoscore <- ggplot(data = grs_info, aes(y=infoscore, x=index)) + geom_point(size=0.6) + 
  geom_text(aes(label=rsid), hjust=-0.1, size=3) + HGtheme + geom_hline(yintercept=0.9, linetype='dashed') +
  xlab('Variant number') + ylab('Info score')  +
  theme(plot.title = element_text(size=9, family = 'Helvetica'),
        axis.title = element_text(family = 'Helvetica'),
        axis.text = element_text(size=9, family = 'Helvetica')) +
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200)) + 
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), limits=c(0.4,1.0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

pdf(file = "sup_fig_2.pdf",
    width = 7,
    height = 3.44488)
print(infoscore)
dev.off()


#-----------------
#install.packages('patchwork')

library(patchwork)

HGtheme=theme_bw()+ #a graph theme made by Harry Green @ Exeter:
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

#ROCAUC values from Supplmentary Table 7:
bv_4049 <- data.frame(x=c("All","Female","Male"), min=c(0.6,0.57,0.6), low=c(0.6,0.57,0.6), mid=c(0.68,0.68,0.72), top=c(0.76,0.79,0.82), max=c(0.76,0.79,0.82))

bv_5059 <- data.frame(x=c("All","Female","Male"), min=c(0.75,0.73,0.72), low=c(0.75,0.73,0.72), mid=c(0.78,0.78,0.76), top=c(0.81,0.83,0.81), max=c(0.81,0.83,0.81))

bv_6069 <- data.frame(x=c("All","Female","Male"), min=c(0.72,0.71,0.68), low=c(0.72,0.71,0.68), mid=c(0.75,0.76,0.72), top=c(0.77,0.8,0.75), max=c(0.77,0.8,0.75))

bv_7079 <- data.frame(x=c("All","Female","Male"), min=c(0.72,0.66,0.69), low=c(0.72,0.66,0.69), mid=c(0.77,0.76,0.75), top=c(0.82,0.86,0.80), max=c(0.82,0.86,0.80))

bv_4049_p <- ggplot(bv_4049, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity") + ylim(c(0.5, 1)) + ylab("ROCAUC") + xlab("Age 40-49") + HGtheme

bv_5059_p <- ggplot(bv_5059, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity") + ylim(c(0.5, 1)) + xlab("Age 50-59") + HGtheme + theme(axis.title.y=element_blank(),
                                                                                axis.text.y=element_blank(),
                                                                                axis.ticks.y=element_blank())

bv_6069_p <- ggplot(bv_6069, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity") + ylim(c(0.5, 1)) + xlab("Age 60-69") + HGtheme + theme(axis.title.y=element_blank(),
                                                                                axis.text.y=element_blank(),
                                                                                axis.ticks.y=element_blank())

bv_7079_p <- ggplot(bv_7079, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity") + ylim(c(0.5, 1)) + xlab("Age 70-79") + HGtheme + theme(axis.title.y=element_blank(),
                                                                                axis.text.y=element_blank(),
                                                                                axis.ticks.y=element_blank())

bv_4049_p + bv_5059_p + bv_6069_p + bv_7079_p + plot_layout(nrow = 1)
