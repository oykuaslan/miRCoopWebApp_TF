library(readr)
library(sqldf)
library(dplyr)

ACC <- read_csv("TF_Type3Results/ACC_Type3Pval001.csv")
BRCA <- read_csv("TF_Type3Results/BRCA_Type3Pval001.csv")
CESC <- read_csv("TF_Type3Results/CESC_Type3Pval001.csv")
COAD <- read_csv("TF_Type3Results/COAD_Type3Pval001.csv")
DLBC<- read_csv("TF_Type3Results/DLBC_Type3Pval001.csv")
ESCA <- read_csv("TF_Type3Results/ESCA_Type3Pval001.csv")
KICH <- read_csv("TF_Type3Results/KICH_Type3Pval001.csv")
KIRC <- read_csv("TF_Type3Results/KIRC_Type3Pval001.csv")
KIRP <- read_csv("TF_Type3Results/KIRP_Type3Pval001.csv")
LUAD <- read_csv("TF_Type3Results/LUAD_Type3Pval001.csv")
LUSC <- read_csv("TF_Type3Results/LUSC_Type3Pval001.csv")
MESO <- read_csv("TF_Type3Results/MESO_Type3Pval001.csv")
OV <- read_csv("TF_Type3Results/OV_Type3Pval001.csv")
PAAD <- read_csv("TF_Type3Results/PAAD_Type3Pval001.csv")
PCPG <- read_csv("TF_Type3Results/PCPG_Type3Pval001.csv")
PRAD <- read_csv("TF_Type3Results/PRAD_Type3Pval001.csv")
READ <- read_csv("TF_Type3Results/READ_Type3Pval001.csv")
SKCM <- read_csv("TF_Type3Results/SKCM_Type3Pval001.csv")
THCA <- read_csv("TF_Type3Results/THCA_Type3Pval001.csv")
UCS <- read_csv("TF_Type3Results/UCS_Type3Pval001.csv")
UVM <- read_csv("TF_Type3Results/UVM_Type3Pval001.csv")




counts <- data.frame(Cancer = character(0), Count = integer(0), DistinctCount = integer(0))

counts <-rbind(counts, data.frame(Cancer = "ACC", Count = nrow(ACC), DistinctCount = nrow(ACC %>% distinct()) ))
counts <-rbind(counts, data.frame(Cancer = "BRCA", Count = nrow(BRCA), DistinctCount = nrow(BRCA %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "CESC", Count = nrow(CESC), DistinctCount = nrow(CESC %>% distinct()) ))
counts <-rbind(counts, data.frame(Cancer = "COAD", Count = nrow(COAD), DistinctCount = nrow(COAD %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "DLBC", Count = nrow(DLBC), DistinctCount = nrow(DLBC %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "ESCA", Count = nrow(ESCA), DistinctCount = nrow(ESCA %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "KICH", Count = nrow(KICH), DistinctCount = nrow(KICH %>% distinct()) ))
counts <-rbind(counts, data.frame(Cancer = "KIRC", Count = nrow(KIRC), DistinctCount = nrow(KIRC %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "KIRP", Count = nrow(KIRP), DistinctCount = nrow(KIRP %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "LUAD", Count = nrow(LUAD), DistinctCount = nrow(LUAD %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "LUSC", Count = nrow(LUSC), DistinctCount = nrow(LUSC %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "MESO", Count = nrow(MESO), DistinctCount = nrow(MESO %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "OV", Count = nrow(OV), DistinctCount = nrow(OV %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "PAAD", Count = nrow(PAAD), DistinctCount = nrow(PAAD %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "PCPG", Count = nrow(PCPG), DistinctCount = nrow(PCPG %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "PRAD", Count = nrow(PRAD), DistinctCount = nrow(PRAD %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "READ", Count = nrow(READ), DistinctCount = nrow(READ %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "SKCM", Count = nrow(SKCM), DistinctCount = nrow(SKCM %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "THCA", Count = nrow(THCA), DistinctCount = nrow(THCA %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "UCS", Count = nrow(UCS), DistinctCount = nrow(UCS %>% distinct())))
counts <-rbind(counts, data.frame(Cancer = "UVM", Count = nrow(UVM), DistinctCount = nrow(UVM %>% distinct())))






