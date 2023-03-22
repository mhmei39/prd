if (TRUE) {    # path, files, ...
  rm(list=ls())
  options(width=160, stringsAsFactors=F)
  
  #--- 
  na.str <- c(NA, "NA", NaN, "", " ", ".", "-")
  
} #end

datapath <- 'U:/minghua.mei/Otsuka/bioanalysis/cns'
outpath <- 'U:/minghua.mei/Otsuka/bioanalysis/cns/volcano_plot/output'
progpath <- 'U:/minghua.mei/Otsuka/bioanalysis/cns/cns_assays_visits_comparisons.R'

library(Hmisc)
library(rstatix)
library(tidyverse)
library(ggpubr)
library(stringr)
library(dplyr)
library(ggplot2)
library(OlinkAnalyze)
library(ggplotify)
library(pheatmap)
library(ggrepel)

data <- read.csv(file.path(datapath, "CTN_46_OLINK_PROTEOMIC_RESULTS_COPY.csv"))
table(data$Panel)

attach(data)
new_data1 <- data[order(SUBJID,VisitCode),]
detach(data)
View(new_data1)

drops <- c("Normalization","Assay_Warning","STUDYID","SCRNO","DOSE","FORMULA","VisitCode")
new_data<-new_data1[ , !(names(new_data1) %in% drops)]
###glimpse(new_data$Assay_Warning)

### select panel (Cardiometabolic) for comparisons of proteins
data_Cardiometabolic_1 <-new_data[new_data$Panel == "Cardiometabolic", ]
View(data_Cardiometabolic_1)
data_Cardiometabolic <-data_Cardiometabolic_1[data_Cardiometabolic_1$QC_Warning == "PASS",] 
table(data_Cardiometabolic$QC_Warning)

###sig_panel_cardiom <- subset(data_Cardiometabolic, select = c (Assay, VISIT, VisitCode, NPX))
###head(sig_panel_cardiom)
###dim(sig_panel_cardiom)
###table(sig_panel_cardiom$VISIT)

####excluding some missing values of some proteins with NPX =’NA’ for comparisons 
####data_subset <- data_Cardiometabolic[ , c("NPX")]
data_subset <- data_Cardiometabolic[ , c("NPX")]
data_new <-data_Cardiometabolic[ complete.cases(data_subset), ]
###describe(data_new$NPX)
###summary(data_new$NPX)

attach(data_new)
sig_panel_cardiom0 <- data.frame(data_new[order(VISIT),])
detach(data_new)
head(sig_panel_cardiom0)
table(sig_panel_cardiom0$VISIT)

###remove leading and trailing blanks for all character variables in the dataset
sig_panel_cardiom1 <- sig_panel_cardiom0 %>%
  mutate(across(where(is.character), function(x)trimws(x)))

###create Volcano plot for proteins between Day 1 1h and Day 1 6h in Cardiometaboloc panel
sig_panel_cardiom2 <-subset(sig_panel_cardiom1, VISIT %in% c("Day 1 1h", "Day 1 6h"))
table(sig_panel_cardiom2$VISIT)
table(sig_panel_cardiom2$Panel)
###new_subdata <-subset(sig_panel_cardiom2, Assay %in% c("IGFBP1", "SPON2","SERPINA12", "CCL15","CCN3","FAS","HYAL1","MFAP5","PCSK9","SFTPD"))

sig_panel_cardiom2 <- sig_panel_cardiom2 %>% 
  rename(Panel_Verison = Panel_Lot_Nr, SUBJECT = SUBJID)
view(sig_panel_cardiom2)
###sig_panel_cardiom2$SampleID <- seq_along(sig_panel_cardiom2$SampleID)

wilcox_results <- sig_panel_cardiom2 %>%
  group_by(Assay) %>%
  wilcox_test(NPX ~ VISIT, detailed = TRUE) %>%
  add_significance()
view(wilcox_results)
table(wilcox_results$p.signif)
describe(wilcox_results)

Adjusted_pval <- p.adjust(wilcox_results$p, method = "BH", n = length(wilcox_results$p))
wilcox_results_2 <- data.frame(Adjusted_pval)
view(wilcox_results_2)
wilcox_results_2$ID <- seq_along(wilcox_results_2$Adjusted_pval)
wilcox_results$ID <- seq_along(wilcox_results$p)

wilcox_results_new <- merge(wilcox_results, wilcox_results_2, by = "ID")
view(wilcox_results_new)

wilcox_results_new
write.csv(wilcox_results_new, "U:/minghua.mei/Otsuka/bioanalysis/cns/volcano_plot/output/sig_proteins_day1.1h_day1.6h_Cardiometabolic_newcode_test.csv")

wilcox_results_new <- wilcox_results_new %>% 
  rename(Estimate = estimate, p_value = p)
view(wilcox_results_new)
# add a column of NAs
wilcox_results_new$Threshold <- "Non-significant"
# if Adjusted_pval < 0.05, set as "Significant" 
wilcox_results_new$Threshold[wilcox_results_new$Adjusted_pval < 0.05] <- "Significant"
vcnplot = ggplot(data=wilcox_results_new, aes(x=Estimate, y=-log10(p_value), col=Threshold)) + geom_point() + theme_minimal() + 
  geom_hline(yintercept=-log10(0.05), linetype = 'dotted', col="black")+labs(
    title = "Volcano plot between Day 1 1h and Day 1 6h in Cardiometaboloc")
print(vcnplot)

###modify the location of the name of significant genes beside the points
###using geom_text_repel() to replace geom_text()
wilcox_results_new$delabel <- NA
wilcox_results_new$delabel[wilcox_results_new$Threshold != "Non-significant"] <- wilcox_results_new$Assay[wilcox_results_new$Threshold != "Non-significant"]
vcnplot = ggplot(data=wilcox_results_new, aes(x=Estimate, y=-log10(p_value), col=Threshold, label=delabel)) + geom_point() + theme_minimal() + geom_text_repel() +
  geom_hline(yintercept=-log10(0.05), linetype = 'dotted', col="black")+labs(
    title = "Volcano plot between Day 1 1h and Day 1 6h in Cardiometaboloc")
print(vcnplot)

## Change point color 
mcolors <- c("red", "#00BFC4")
names(mcolors) <- c("Significant", "Non-significant")
vcnplot <- vcnplot + scale_colour_manual(values = mcolors)
print(vcnplot)
ggsave(file = file.path(outpath, "Volcano plot for proteins between Day 1 1h and Day 1 6h in Cardiometaboloc panel_newcode_test.pdf"), 
       print(vcnplot), height=8, width=12)

