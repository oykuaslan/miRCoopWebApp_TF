library(shiny)
library(DT)
library(dplyr)
library(shinythemes)
library(shinyWidgets)
library(igraph)
library(RColorBrewer)
library(networkD3)
library(htmlwidgets)
library(visNetwork)
library(shinyalert)
library(shinycssloaders)
library(shinycustomloader)
library(sqldf)
library(readr)
library(ggplot2)
library(formattable)
library(shinyBS)
library(purrr)
library(bslib)
library(spsComps)
library(bsplus)
library(reshape)
library(data.table)
library(readxl)
library(heatmaply)
library(plotly)



ACC_allData <- read_csv("finalDTData/ACC.csv",show_col_types = FALSE)
ACC_filtered <- ACC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]
ACCWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/ACCWBenjaminiHochbergCorrection.csv")
# ACC <- sqldf::sqldf("SELECT ACC_filtered.*, BH_rejected, BH_pvalues_adjusted from ACC_filtered LEFT JOIN ACCWBenjaminiHochbergCorrection ON 
#                           (ACC_filtered.entrezgene_id = ACCWBenjaminiHochbergCorrection.mrna AND ACC_filtered.mirna1 = ACCWBenjaminiHochbergCorrection.mirna1 AND ACC_filtered.mirna2 = ACCWBenjaminiHochbergCorrection.mirna2) 
#                           OR (ACC_filtered.entrezgene_id = ACCWBenjaminiHochbergCorrection.mrna AND ACC_filtered.mirna1 = ACCWBenjaminiHochbergCorrection.mirna2 AND ACC_filtered.mirna2 = ACCWBenjaminiHochbergCorrection.mirna1)")
ACC <- sqldf::sqldf("SELECT ACC_filtered.*, BH_pvalues_adjusted,BH_rejected from ACC_filtered LEFT JOIN ACCWBenjaminiHochbergCorrection ON 
                          (ACC_filtered.entrezgene_id = ACCWBenjaminiHochbergCorrection.mrna AND ACC_filtered.mirna1 = ACCWBenjaminiHochbergCorrection.mirna1 AND ACC_filtered.mirna2 = ACCWBenjaminiHochbergCorrection.mirna2) 
                          OR (ACC_filtered.entrezgene_id = ACCWBenjaminiHochbergCorrection.mrna AND ACC_filtered.mirna1 = ACCWBenjaminiHochbergCorrection.mirna2 AND ACC_filtered.mirna2 = ACCWBenjaminiHochbergCorrection.mirna1)")

ACC_BH_pvalues_adjusted_min = min(ACC$BH_pvalues_adjusted)
ACC_BH_pvalues_adjusted_max = max(ACC$BH_pvalues_adjusted)

BLCA_allData <- read_csv("finalDTData/BLCA.csv",show_col_types = FALSE)
BLCA_filtered <- BLCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
BLCAWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/BLCAWBenjaminiHochbergCorrection.csv")
BLCA <- sqldf::sqldf("SELECT BLCA_filtered.*, BH_pvalues_adjusted,BH_rejected  from BLCA_filtered LEFT JOIN BLCAWBenjaminiHochbergCorrection ON 
                          (BLCA_filtered.entrezgene_id = BLCAWBenjaminiHochbergCorrection.mrna AND BLCA_filtered.mirna1 = BLCAWBenjaminiHochbergCorrection.mirna1 AND BLCA_filtered.mirna2 = BLCAWBenjaminiHochbergCorrection.mirna2) 
                          OR (BLCA_filtered.entrezgene_id = BLCAWBenjaminiHochbergCorrection.mrna AND BLCA_filtered.mirna1 = BLCAWBenjaminiHochbergCorrection.mirna2 AND BLCA_filtered.mirna2 = BLCAWBenjaminiHochbergCorrection.mirna1)")

BLCA_BH_pvalues_adjusted_min = min(BLCA$BH_pvalues_adjusted)
BLCA_BH_pvalues_adjusted_max = max(BLCA$BH_pvalues_adjusted)

BRCA_allData <- read_csv("finalDTData/BRCA.csv",show_col_types = FALSE)
BRCA_filtered <- BRCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
BRCAWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/BRCAWBenjaminiHochbergCorrection.csv")
BRCA <- sqldf::sqldf("SELECT BRCA_filtered.*,  BH_pvalues_adjusted,BH_rejected  from BRCA_filtered LEFT JOIN BRCAWBenjaminiHochbergCorrection ON 
                          (BRCA_filtered.entrezgene_id = BRCAWBenjaminiHochbergCorrection.mrna AND BRCA_filtered.mirna1 = BRCAWBenjaminiHochbergCorrection.mirna1 AND BRCA_filtered.mirna2 = BRCAWBenjaminiHochbergCorrection.mirna2) 
                          OR (BRCA_filtered.entrezgene_id = BRCAWBenjaminiHochbergCorrection.mrna AND BRCA_filtered.mirna1 = BRCAWBenjaminiHochbergCorrection.mirna2 AND BRCA_filtered.mirna2 = BRCAWBenjaminiHochbergCorrection.mirna1)")
BRCA_BH_pvalues_adjusted_min = min(BRCA$BH_pvalues_adjusted)
BRCA_BH_pvalues_adjusted_max = max(BRCA$BH_pvalues_adjusted)

CESC_allData <- read_csv("finalDTData/CESC.csv",show_col_types = FALSE)
CESC_filtered <- CESC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
CESCWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/CESCWBenjaminiHochbergCorrection.csv")
CESC <- sqldf::sqldf("SELECT CESC_filtered.*,  BH_pvalues_adjusted,BH_rejected  from CESC_filtered LEFT JOIN CESCWBenjaminiHochbergCorrection ON 
                          (CESC_filtered.entrezgene_id = CESCWBenjaminiHochbergCorrection.mrna AND CESC_filtered.mirna1 = CESCWBenjaminiHochbergCorrection.mirna1 AND CESC_filtered.mirna2 = CESCWBenjaminiHochbergCorrection.mirna2) 
                          OR (CESC_filtered.entrezgene_id = CESCWBenjaminiHochbergCorrection.mrna AND CESC_filtered.mirna1 = CESCWBenjaminiHochbergCorrection.mirna2 AND CESC_filtered.mirna2 = CESCWBenjaminiHochbergCorrection.mirna1)")
CESC_BH_pvalues_adjusted_min = min(CESC$BH_pvalues_adjusted)
CESC_BH_pvalues_adjusted_max = max(CESC$BH_pvalues_adjusted)

CHOL_allData <- read_csv("finalDTData/CHOL.csv",show_col_types = FALSE)
CHOL_filtered <- CHOL_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
CHOLWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/CHOLWBenjaminiHochbergCorrection.csv")
CHOL <- sqldf::sqldf("SELECT CHOL_filtered.*,  BH_pvalues_adjusted,BH_rejected  from CHOL_filtered LEFT JOIN CHOLWBenjaminiHochbergCorrection ON 
                          (CHOL_filtered.entrezgene_id = CHOLWBenjaminiHochbergCorrection.mrna AND CHOL_filtered.mirna1 = CHOLWBenjaminiHochbergCorrection.mirna1 AND CHOL_filtered.mirna2 = CHOLWBenjaminiHochbergCorrection.mirna2) 
                          OR (CHOL_filtered.entrezgene_id = CHOLWBenjaminiHochbergCorrection.mrna AND CHOL_filtered.mirna1 = CHOLWBenjaminiHochbergCorrection.mirna2 AND CHOL_filtered.mirna2 = CHOLWBenjaminiHochbergCorrection.mirna1)")
CHOL_BH_pvalues_adjusted_min = min(CHOL$BH_pvalues_adjusted)
CHOL_BH_pvalues_adjusted_max = max(CHOL$BH_pvalues_adjusted)

COAD_allData <- read_csv("finalDTData/COAD.csv",show_col_types = FALSE)
COAD_filtered <- COAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC")]
COADWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/COADWBenjaminiHochbergCorrection.csv")
COAD <- sqldf::sqldf("SELECT COAD_filtered.*, BH_pvalues_adjusted,BH_rejected  from COAD_filtered LEFT JOIN COADWBenjaminiHochbergCorrection ON 
                          (COAD_filtered.entrezgene_id = COADWBenjaminiHochbergCorrection.mrna AND COAD_filtered.mirna1 = COADWBenjaminiHochbergCorrection.mirna1 AND COAD_filtered.mirna2 = COADWBenjaminiHochbergCorrection.mirna2) 
                          OR (COAD_filtered.entrezgene_id = COADWBenjaminiHochbergCorrection.mrna AND COAD_filtered.mirna1 = COADWBenjaminiHochbergCorrection.mirna2 AND COAD_filtered.mirna2 = COADWBenjaminiHochbergCorrection.mirna1)")
COAD_BH_pvalues_adjusted_min = min(COAD$BH_pvalues_adjusted)
COAD_BH_pvalues_adjusted_max = max(COAD$BH_pvalues_adjusted)

DLBC_allData <- read_csv("finalDTData/DLBC.csv",show_col_types = FALSE)
DLBC_filtered <- DLBC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]
DLBCWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/DLBCWBenjaminiHochbergCorrection.csv")
DLBC <- sqldf::sqldf("SELECT DLBC_filtered.*,  BH_pvalues_adjusted,BH_rejected  from DLBC_filtered LEFT JOIN DLBCWBenjaminiHochbergCorrection ON 
                          (DLBC_filtered.entrezgene_id = DLBCWBenjaminiHochbergCorrection.mrna AND DLBC_filtered.mirna1 = DLBCWBenjaminiHochbergCorrection.mirna1 AND DLBC_filtered.mirna2 = DLBCWBenjaminiHochbergCorrection.mirna2) 
                          OR (DLBC_filtered.entrezgene_id = DLBCWBenjaminiHochbergCorrection.mrna AND DLBC_filtered.mirna1 = DLBCWBenjaminiHochbergCorrection.mirna2 AND DLBC_filtered.mirna2 = DLBCWBenjaminiHochbergCorrection.mirna1)")
DLBC_BH_pvalues_adjusted_min = min(DLBC$BH_pvalues_adjusted)
DLBC_BH_pvalues_adjusted_max = max(DLBC$BH_pvalues_adjusted)

ESCA_allData <- read_csv("finalDTData/ESCA.csv",show_col_types = FALSE)
ESCA_filtered <- ESCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
ESCAWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/ESCAWBenjaminiHochbergCorrection.csv")
ESCA <- sqldf::sqldf("SELECT ESCA_filtered.*,  BH_pvalues_adjusted,BH_rejected  from ESCA_filtered LEFT JOIN ESCAWBenjaminiHochbergCorrection ON 
                          (ESCA_filtered.entrezgene_id = ESCAWBenjaminiHochbergCorrection.mrna AND ESCA_filtered.mirna1 = ESCAWBenjaminiHochbergCorrection.mirna1 AND ESCA_filtered.mirna2 = ESCAWBenjaminiHochbergCorrection.mirna2) 
                          OR (ESCA_filtered.entrezgene_id = ESCAWBenjaminiHochbergCorrection.mrna AND ESCA_filtered.mirna1 = ESCAWBenjaminiHochbergCorrection.mirna2 AND ESCA_filtered.mirna2 = ESCAWBenjaminiHochbergCorrection.mirna1)")
ESCA_BH_pvalues_adjusted_min = min(ESCA$BH_pvalues_adjusted)
ESCA_BH_pvalues_adjusted_max = max(ESCA$BH_pvalues_adjusted)

HNSC_allData <- read_csv("finalDTData/HNSC.csv",show_col_types = FALSE)
HNSC_filtered <- HNSC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
HNSCWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/HNSCWBenjaminiHochbergCorrection.csv")
HNSC <- sqldf::sqldf("SELECT HNSC_filtered.*,  BH_pvalues_adjusted,BH_rejected  from HNSC_filtered LEFT JOIN HNSCWBenjaminiHochbergCorrection ON 
                          (HNSC_filtered.entrezgene_id = HNSCWBenjaminiHochbergCorrection.mrna AND HNSC_filtered.mirna1 = HNSCWBenjaminiHochbergCorrection.mirna1 AND HNSC_filtered.mirna2 = HNSCWBenjaminiHochbergCorrection.mirna2) 
                          OR (HNSC_filtered.entrezgene_id = HNSCWBenjaminiHochbergCorrection.mrna AND HNSC_filtered.mirna1 = HNSCWBenjaminiHochbergCorrection.mirna2 AND HNSC_filtered.mirna2 = HNSCWBenjaminiHochbergCorrection.mirna1)")
HNSC_BH_pvalues_adjusted_min = min(HNSC$BH_pvalues_adjusted)
HNSC_BH_pvalues_adjusted_max = max(HNSC$BH_pvalues_adjusted)

KICH_allData <- read_csv("finalDTData/KICH.csv",show_col_types = FALSE)
KICH_filtered <- KICH_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
KICHWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/KICHWBenjaminiHochbergCorrection.csv")
KICH <- sqldf::sqldf("SELECT KICH_filtered.*,  BH_pvalues_adjusted,BH_rejected  from KICH_filtered LEFT JOIN KICHWBenjaminiHochbergCorrection ON 
                          (KICH_filtered.entrezgene_id = KICHWBenjaminiHochbergCorrection.mrna AND KICH_filtered.mirna1 = KICHWBenjaminiHochbergCorrection.mirna1 AND KICH_filtered.mirna2 = KICHWBenjaminiHochbergCorrection.mirna2) 
                          OR (KICH_filtered.entrezgene_id = KICHWBenjaminiHochbergCorrection.mrna AND KICH_filtered.mirna1 = KICHWBenjaminiHochbergCorrection.mirna2 AND KICH_filtered.mirna2 = KICHWBenjaminiHochbergCorrection.mirna1)")
KICH_BH_pvalues_adjusted_min = min(KICH$BH_pvalues_adjusted)
KICH_BH_pvalues_adjusted_max = max(KICH$BH_pvalues_adjusted)

KIRC_allData <- read_csv("finalDTData/KIRC.csv",show_col_types = FALSE)
KIRC_filtered <- KIRC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
KIRCWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/KIRCWBenjaminiHochbergCorrection.csv")
KIRC <- sqldf::sqldf("SELECT KIRC_filtered.*,  BH_pvalues_adjusted,BH_rejected  from KIRC_filtered LEFT JOIN KIRCWBenjaminiHochbergCorrection ON 
                          (KIRC_filtered.entrezgene_id = KIRCWBenjaminiHochbergCorrection.mrna AND KIRC_filtered.mirna1 = KIRCWBenjaminiHochbergCorrection.mirna1 AND KIRC_filtered.mirna2 = KIRCWBenjaminiHochbergCorrection.mirna2) 
                          OR (KIRC_filtered.entrezgene_id = KIRCWBenjaminiHochbergCorrection.mrna AND KIRC_filtered.mirna1 = KIRCWBenjaminiHochbergCorrection.mirna2 AND KIRC_filtered.mirna2 = KIRCWBenjaminiHochbergCorrection.mirna1)")
KIRC_BH_pvalues_adjusted_min = min(KIRC$BH_pvalues_adjusted)
KIRC_BH_pvalues_adjusted_max = max(KIRC$BH_pvalues_adjusted)

KIRP_allData <- read_csv("finalDTData/KIRP.csv",show_col_types = FALSE)
KIRP_filtered <- KIRP_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
KIRPWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/KIRPWBenjaminiHochbergCorrection.csv")
KIRP <- sqldf::sqldf("SELECT KIRP_filtered.*,  BH_pvalues_adjusted,BH_rejected  from KIRP_filtered LEFT JOIN KIRPWBenjaminiHochbergCorrection ON 
                          (KIRP_filtered.entrezgene_id = KIRPWBenjaminiHochbergCorrection.mrna AND KIRP_filtered.mirna1 = KIRPWBenjaminiHochbergCorrection.mirna1 AND KIRP_filtered.mirna2 = KIRPWBenjaminiHochbergCorrection.mirna2) 
                          OR (KIRP_filtered.entrezgene_id = KIRPWBenjaminiHochbergCorrection.mrna AND KIRP_filtered.mirna1 = KIRPWBenjaminiHochbergCorrection.mirna2 AND KIRP_filtered.mirna2 = KIRPWBenjaminiHochbergCorrection.mirna1)")
KIRP_BH_pvalues_adjusted_min = min(KIRP$BH_pvalues_adjusted)
KIRP_BH_pvalues_adjusted_max = max(KIRP$BH_pvalues_adjusted)

LGG_allData <- read_csv("finalDTData/LGG.csv",show_col_types = FALSE)
LGG_filtered <- LGG_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]
LGGWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/LGGWBenjaminiHochbergCorrection.csv")
LGG <- sqldf::sqldf("SELECT LGG_filtered.*, BH_pvalues_adjusted,BH_rejected  from LGG_filtered LEFT JOIN LGGWBenjaminiHochbergCorrection ON 
                          (LGG_filtered.entrezgene_id = LGGWBenjaminiHochbergCorrection.mrna AND LGG_filtered.mirna1 = LGGWBenjaminiHochbergCorrection.mirna1 AND LGG_filtered.mirna2 = LGGWBenjaminiHochbergCorrection.mirna2) 
                          OR (LGG_filtered.entrezgene_id = LGGWBenjaminiHochbergCorrection.mrna AND LGG_filtered.mirna1 = LGGWBenjaminiHochbergCorrection.mirna2 AND LGG_filtered.mirna2 = LGGWBenjaminiHochbergCorrection.mirna1)")
LGG_BH_pvalues_adjusted_min = min(LGG$BH_pvalues_adjusted)
LGG_BH_pvalues_adjusted_max = max(LGG$BH_pvalues_adjusted)

LIHC_allData <- read_csv("finalDTData/LIHC.csv",show_col_types = FALSE)
LIHC_filtered <- LIHC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
LIHCWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/LIHCWBenjaminiHochbergCorrection.csv")
LIHC <- sqldf::sqldf("SELECT LIHC_filtered.*, BH_pvalues_adjusted,BH_rejected  from LIHC_filtered LEFT JOIN LIHCWBenjaminiHochbergCorrection ON 
                          (LIHC_filtered.entrezgene_id = LIHCWBenjaminiHochbergCorrection.mrna AND LIHC_filtered.mirna1 = LIHCWBenjaminiHochbergCorrection.mirna1 AND LIHC_filtered.mirna2 = LIHCWBenjaminiHochbergCorrection.mirna2) 
                          OR (LIHC_filtered.entrezgene_id = LIHCWBenjaminiHochbergCorrection.mrna AND LIHC_filtered.mirna1 = LIHCWBenjaminiHochbergCorrection.mirna2 AND LIHC_filtered.mirna2 = LIHCWBenjaminiHochbergCorrection.mirna1)")
LIHC_BH_pvalues_adjusted_min = min(LIHC$BH_pvalues_adjusted)
LIHC_BH_pvalues_adjusted_max = max(LIHC$BH_pvalues_adjusted)

LUAD_allData <- read_csv("finalDTData/LUAD.csv",show_col_types = FALSE)
LUAD_filtered <- LUAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
LUADWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/LUADWBenjaminiHochbergCorrection.csv")
LUAD <- sqldf::sqldf("SELECT LUAD_filtered.*, BH_pvalues_adjusted,BH_rejected  from LUAD_filtered LEFT JOIN LUADWBenjaminiHochbergCorrection ON 
                          (LUAD_filtered.entrezgene_id = LUADWBenjaminiHochbergCorrection.mrna AND LUAD_filtered.mirna1 = LUADWBenjaminiHochbergCorrection.mirna1 AND LUAD_filtered.mirna2 = LUADWBenjaminiHochbergCorrection.mirna2) 
                          OR (LUAD_filtered.entrezgene_id = LUADWBenjaminiHochbergCorrection.mrna AND LUAD_filtered.mirna1 = LUADWBenjaminiHochbergCorrection.mirna2 AND LUAD_filtered.mirna2 = LUADWBenjaminiHochbergCorrection.mirna1)")
LUAD_BH_pvalues_adjusted_min = min(LUAD$BH_pvalues_adjusted)
LUAD_BH_pvalues_adjusted_max = max(LUAD$BH_pvalues_adjusted)

LUSC_allData <- read_csv("finalDTData/LUSC.csv",show_col_types = FALSE)
LUSC_filtered <- LUSC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
LUSCWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/LUSCWBenjaminiHochbergCorrection.csv")
LUSC <- sqldf::sqldf("SELECT LUSC_filtered.*, BH_pvalues_adjusted,BH_rejected  from LUSC_filtered LEFT JOIN LUSCWBenjaminiHochbergCorrection ON 
                          (LUSC_filtered.entrezgene_id = LUSCWBenjaminiHochbergCorrection.mrna AND LUSC_filtered.mirna1 = LUSCWBenjaminiHochbergCorrection.mirna1 AND LUSC_filtered.mirna2 = LUSCWBenjaminiHochbergCorrection.mirna2) 
                          OR (LUSC_filtered.entrezgene_id = LUSCWBenjaminiHochbergCorrection.mrna AND LUSC_filtered.mirna1 = LUSCWBenjaminiHochbergCorrection.mirna2 AND LUSC_filtered.mirna2 = LUSCWBenjaminiHochbergCorrection.mirna1)")
LUSC_BH_pvalues_adjusted_min = min(LUSC$BH_pvalues_adjusted)
LUSC_BH_pvalues_adjusted_max = max(LUSC$BH_pvalues_adjusted)

MESO_allData <- read_csv("finalDTData/MESO.csv",show_col_types = FALSE)
MESO_filtered <- MESO_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]
MESOWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/MESOWBenjaminiHochbergCorrection.csv")
MESO <- sqldf::sqldf("SELECT MESO_filtered.*, BH_pvalues_adjusted,BH_rejected  from MESO_filtered LEFT JOIN MESOWBenjaminiHochbergCorrection ON 
                          (MESO_filtered.entrezgene_id = MESOWBenjaminiHochbergCorrection.mrna AND MESO_filtered.mirna1 = MESOWBenjaminiHochbergCorrection.mirna1 AND MESO_filtered.mirna2 = MESOWBenjaminiHochbergCorrection.mirna2) 
                          OR (MESO_filtered.entrezgene_id = MESOWBenjaminiHochbergCorrection.mrna AND MESO_filtered.mirna1 = MESOWBenjaminiHochbergCorrection.mirna2 AND MESO_filtered.mirna2 = MESOWBenjaminiHochbergCorrection.mirna1)")
MESO_BH_pvalues_adjusted_min = min(MESO$BH_pvalues_adjusted)
MESO_BH_pvalues_adjusted_max = max(MESO$BH_pvalues_adjusted)

OV_allData <- read_csv("finalDTData/OV.csv",show_col_types = FALSE)
OV_filtered <- OV_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]
OVWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/OVWBenjaminiHochbergCorrection.csv")
OV <- sqldf::sqldf("SELECT OV_filtered.*, BH_pvalues_adjusted,BH_rejected  from OV_filtered LEFT JOIN OVWBenjaminiHochbergCorrection ON 
                          (OV_filtered.entrezgene_id = OVWBenjaminiHochbergCorrection.mrna AND OV_filtered.mirna1 = OVWBenjaminiHochbergCorrection.mirna1 AND OV_filtered.mirna2 = OVWBenjaminiHochbergCorrection.mirna2) 
                          OR (OV_filtered.entrezgene_id = OVWBenjaminiHochbergCorrection.mrna AND OV_filtered.mirna1 = OVWBenjaminiHochbergCorrection.mirna2 AND OV_filtered.mirna2 = OVWBenjaminiHochbergCorrection.mirna1)")
OV_BH_pvalues_adjusted_min = min(OV$BH_pvalues_adjusted)
OV_BH_pvalues_adjusted_max = max(OV$BH_pvalues_adjusted)

PAAD_allData <- read_csv("finalDTData/PAAD.csv",show_col_types = FALSE)
PAAD_filtered <- PAAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
PAADWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/PAADWBenjaminiHochbergCorrection.csv")
PAAD <- sqldf::sqldf("SELECT PAAD_filtered.*, BH_pvalues_adjusted,BH_rejected  from PAAD_filtered LEFT JOIN PAADWBenjaminiHochbergCorrection ON 
                          (PAAD_filtered.entrezgene_id = PAADWBenjaminiHochbergCorrection.mrna AND PAAD_filtered.mirna1 = PAADWBenjaminiHochbergCorrection.mirna1 AND PAAD_filtered.mirna2 = PAADWBenjaminiHochbergCorrection.mirna2) 
                          OR (PAAD_filtered.entrezgene_id = PAADWBenjaminiHochbergCorrection.mrna AND PAAD_filtered.mirna1 = PAADWBenjaminiHochbergCorrection.mirna2 AND PAAD_filtered.mirna2 = PAADWBenjaminiHochbergCorrection.mirna1)")
PAAD_BH_pvalues_adjusted_min = min(PAAD$BH_pvalues_adjusted)
PAAD_BH_pvalues_adjusted_max = max(PAAD$BH_pvalues_adjusted)

PCPG_allData <- read_csv("finalDTData/PCPG.csv",show_col_types = FALSE)
PCPG_filtered <- PCPG_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
PCPGWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/PCPGWBenjaminiHochbergCorrection.csv")
PCPG <- sqldf::sqldf("SELECT PCPG_filtered.*, BH_pvalues_adjusted,BH_rejected  from PCPG_filtered LEFT JOIN PCPGWBenjaminiHochbergCorrection ON 
                          (PCPG_filtered.entrezgene_id = PCPGWBenjaminiHochbergCorrection.mrna AND PCPG_filtered.mirna1 = PCPGWBenjaminiHochbergCorrection.mirna1 AND PCPG_filtered.mirna2 = PCPGWBenjaminiHochbergCorrection.mirna2) 
                          OR (PCPG_filtered.entrezgene_id = PCPGWBenjaminiHochbergCorrection.mrna AND PCPG_filtered.mirna1 = PCPGWBenjaminiHochbergCorrection.mirna2 AND PCPG_filtered.mirna2 = PCPGWBenjaminiHochbergCorrection.mirna1)")
PCPG_BH_pvalues_adjusted_min = min(PCPG$BH_pvalues_adjusted)
PCPG_BH_pvalues_adjusted_max = max(PCPG$BH_pvalues_adjusted)

PRAD_allData <- read_csv("finalDTData/PRAD.csv",show_col_types = FALSE)
PRAD_filtered <- PRAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
PRADWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/PRADWBenjaminiHochbergCorrection.csv")
PRAD <- sqldf::sqldf("SELECT PRAD_filtered.*, BH_pvalues_adjusted,BH_rejected  from PRAD_filtered LEFT JOIN PRADWBenjaminiHochbergCorrection ON 
                          (PRAD_filtered.entrezgene_id = PRADWBenjaminiHochbergCorrection.mrna AND PRAD_filtered.mirna1 = PRADWBenjaminiHochbergCorrection.mirna1 AND PRAD_filtered.mirna2 = PRADWBenjaminiHochbergCorrection.mirna2) 
                          OR (PRAD_filtered.entrezgene_id = PRADWBenjaminiHochbergCorrection.mrna AND PRAD_filtered.mirna1 = PRADWBenjaminiHochbergCorrection.mirna2 AND PRAD_filtered.mirna2 = PRADWBenjaminiHochbergCorrection.mirna1)")
PRAD_BH_pvalues_adjusted_min = min(PRAD$BH_pvalues_adjusted)
PRAD_BH_pvalues_adjusted_max = max(PRAD$BH_pvalues_adjusted)

READ_allData <- read_csv("finalDTData/READ.csv",show_col_types = FALSE)
READ_filtered <- READ_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC")]
READWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/READWBenjaminiHochbergCorrection.csv")
READ <- sqldf::sqldf("SELECT READ_filtered.*, BH_pvalues_adjusted,BH_rejected  from READ_filtered LEFT JOIN READWBenjaminiHochbergCorrection ON 
                          (READ_filtered.entrezgene_id = READWBenjaminiHochbergCorrection.mrna AND READ_filtered.mirna1 = READWBenjaminiHochbergCorrection.mirna1 AND READ_filtered.mirna2 = READWBenjaminiHochbergCorrection.mirna2) 
                          OR (READ_filtered.entrezgene_id = READWBenjaminiHochbergCorrection.mrna AND READ_filtered.mirna1 = READWBenjaminiHochbergCorrection.mirna2 AND READ_filtered.mirna2 = READWBenjaminiHochbergCorrection.mirna1)")
READ_BH_pvalues_adjusted_min = min(READ$BH_pvalues_adjusted)
READ_BH_pvalues_adjusted_max = max(READ$BH_pvalues_adjusted)

SARC_allData <- read_csv("finalDTData/SARC.csv",show_col_types = FALSE)
SARC_filtered <- SARC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC")]
SARCWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/SARCWBenjaminiHochbergCorrection.csv")
SARC <- sqldf::sqldf("SELECT SARC_filtered.*,  BH_pvalues_adjusted,BH_rejected  from SARC_filtered LEFT JOIN SARCWBenjaminiHochbergCorrection ON 
                          (SARC_filtered.entrezgene_id = SARCWBenjaminiHochbergCorrection.mrna AND SARC_filtered.mirna1 = SARCWBenjaminiHochbergCorrection.mirna1 AND SARC_filtered.mirna2 = SARCWBenjaminiHochbergCorrection.mirna2) 
                          OR (SARC_filtered.entrezgene_id = SARCWBenjaminiHochbergCorrection.mrna AND SARC_filtered.mirna1 = SARCWBenjaminiHochbergCorrection.mirna2 AND SARC_filtered.mirna2 = SARCWBenjaminiHochbergCorrection.mirna1)")
SARC_BH_pvalues_adjusted_min = min(SARC$BH_pvalues_adjusted)
SARC_BH_pvalues_adjusted_max = max(SARC$BH_pvalues_adjusted)

SKCM_allData <- read_csv("finalDTData/SKCM.csv",show_col_types = FALSE)
SKCM_filtered <- SKCM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
SKCMWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/SKCMWBenjaminiHochbergCorrection.csv")
SKCM <- sqldf::sqldf("SELECT SKCM_filtered.*, BH_pvalues_adjusted,BH_rejected  from SKCM_filtered LEFT JOIN SKCMWBenjaminiHochbergCorrection ON 
                          (SKCM_filtered.entrezgene_id = SKCMWBenjaminiHochbergCorrection.mrna AND SKCM_filtered.mirna1 = SKCMWBenjaminiHochbergCorrection.mirna1 AND SKCM_filtered.mirna2 = SKCMWBenjaminiHochbergCorrection.mirna2) 
                          OR (SKCM_filtered.entrezgene_id = SKCMWBenjaminiHochbergCorrection.mrna AND SKCM_filtered.mirna1 = SKCMWBenjaminiHochbergCorrection.mirna2 AND SKCM_filtered.mirna2 = SKCMWBenjaminiHochbergCorrection.mirna1)")
SKCM_BH_pvalues_adjusted_min = min(SKCM$BH_pvalues_adjusted)
SKCM_BH_pvalues_adjusted_max = max(SKCM$BH_pvalues_adjusted)

STAD_allData <- read_csv("finalDTData/STAD.csv",show_col_types = FALSE)
STAD_filtered <- STAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
STADWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/STADWBenjaminiHochbergCorrection.csv")
STAD <- sqldf::sqldf("SELECT STAD_filtered.*, BH_pvalues_adjusted,BH_rejected  from STAD_filtered LEFT JOIN STADWBenjaminiHochbergCorrection ON 
                          (STAD_filtered.entrezgene_id = STADWBenjaminiHochbergCorrection.mrna AND STAD_filtered.mirna1 = STADWBenjaminiHochbergCorrection.mirna1 AND STAD_filtered.mirna2 = STADWBenjaminiHochbergCorrection.mirna2) 
                          OR (STAD_filtered.entrezgene_id = STADWBenjaminiHochbergCorrection.mrna AND STAD_filtered.mirna1 = STADWBenjaminiHochbergCorrection.mirna2 AND STAD_filtered.mirna2 = STADWBenjaminiHochbergCorrection.mirna1)")
STAD_BH_pvalues_adjusted_min = min(STAD$BH_pvalues_adjusted)
STAD_BH_pvalues_adjusted_max = max(STAD$BH_pvalues_adjusted)

TGCT_allData <- read_csv("finalDTData/TGCT.csv",show_col_types = FALSE)
TGCT_filtered <- TGCT_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]
TGCTWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/TGCTWBenjaminiHochbergCorrection.csv")
TGCT <- sqldf::sqldf("SELECT TGCT_filtered.*, BH_pvalues_adjusted,BH_rejected  from TGCT_filtered LEFT JOIN TGCTWBenjaminiHochbergCorrection ON 
                          (TGCT_filtered.entrezgene_id = TGCTWBenjaminiHochbergCorrection.mrna AND TGCT_filtered.mirna1 = TGCTWBenjaminiHochbergCorrection.mirna1 AND TGCT_filtered.mirna2 = TGCTWBenjaminiHochbergCorrection.mirna2) 
                          OR (TGCT_filtered.entrezgene_id = TGCTWBenjaminiHochbergCorrection.mrna AND TGCT_filtered.mirna1 = TGCTWBenjaminiHochbergCorrection.mirna2 AND TGCT_filtered.mirna2 = TGCTWBenjaminiHochbergCorrection.mirna1)")
TGCT_BH_pvalues_adjusted_min = min(TGCT$BH_pvalues_adjusted)
TGCT_BH_pvalues_adjusted_max = max(TGCT$BH_pvalues_adjusted)

THCA_allData <- read_csv("finalDTData/THCA.csv",show_col_types = FALSE)
THCA_filtered <- THCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
THCAWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/THCAWBenjaminiHochbergCorrection.csv")
THCA <- sqldf::sqldf("SELECT THCA_filtered.*, BH_pvalues_adjusted,BH_rejected  from THCA_filtered LEFT JOIN THCAWBenjaminiHochbergCorrection ON 
                          (THCA_filtered.entrezgene_id = THCAWBenjaminiHochbergCorrection.mrna AND THCA_filtered.mirna1 = THCAWBenjaminiHochbergCorrection.mirna1 AND THCA_filtered.mirna2 = THCAWBenjaminiHochbergCorrection.mirna2) 
                          OR (THCA_filtered.entrezgene_id = THCAWBenjaminiHochbergCorrection.mrna AND THCA_filtered.mirna1 = THCAWBenjaminiHochbergCorrection.mirna2 AND THCA_filtered.mirna2 = THCAWBenjaminiHochbergCorrection.mirna1)")
THCA_BH_pvalues_adjusted_min = min(THCA$BH_pvalues_adjusted)
THCA_BH_pvalues_adjusted_max = max(THCA$BH_pvalues_adjusted)

THYM_allData <- read_csv("finalDTData/THYM.csv",show_col_types = FALSE)
THYM_filtered <- THYM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
THYMWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/THYMWBenjaminiHochbergCorrection.csv")
THYM <- sqldf::sqldf("SELECT THYM_filtered.*, BH_pvalues_adjusted,BH_rejected  from THYM_filtered LEFT JOIN THYMWBenjaminiHochbergCorrection ON 
                          (THYM_filtered.entrezgene_id = THYMWBenjaminiHochbergCorrection.mrna AND THYM_filtered.mirna1 = THYMWBenjaminiHochbergCorrection.mirna1 AND THYM_filtered.mirna2 = THYMWBenjaminiHochbergCorrection.mirna2) 
                          OR (THYM_filtered.entrezgene_id = THYMWBenjaminiHochbergCorrection.mrna AND THYM_filtered.mirna1 = THYMWBenjaminiHochbergCorrection.mirna2 AND THYM_filtered.mirna2 = THYMWBenjaminiHochbergCorrection.mirna1)")
THYM_BH_pvalues_adjusted_min = min(THYM$BH_pvalues_adjusted)
THYM_BH_pvalues_adjusted_max = max(THYM$BH_pvalues_adjusted)

UCEC_allData <- read_csv("finalDTData/UCEC.csv",show_col_types = FALSE)
UCEC_filtered <- UCEC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]
UCECWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/UCECWBenjaminiHochbergCorrection.csv")
UCEC <- sqldf::sqldf("SELECT UCEC_filtered.*, BH_pvalues_adjusted,BH_rejected  from UCEC_filtered LEFT JOIN UCECWBenjaminiHochbergCorrection ON 
                          (UCEC_filtered.entrezgene_id = UCECWBenjaminiHochbergCorrection.mrna AND UCEC_filtered.mirna1 = UCECWBenjaminiHochbergCorrection.mirna1 AND UCEC_filtered.mirna2 = UCECWBenjaminiHochbergCorrection.mirna2) 
                          OR (UCEC_filtered.entrezgene_id = UCECWBenjaminiHochbergCorrection.mrna AND UCEC_filtered.mirna1 = UCECWBenjaminiHochbergCorrection.mirna2 AND UCEC_filtered.mirna2 = UCECWBenjaminiHochbergCorrection.mirna1)")
UCEC_BH_pvalues_adjusted_min = min(UCEC$BH_pvalues_adjusted)
UCEC_BH_pvalues_adjusted_max = max(UCEC$BH_pvalues_adjusted)

UCS_allData <- read_csv("finalDTData/UCS.csv",show_col_types = FALSE)
UCS_filtered <- UCS_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]
UCSWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/UCSWBenjaminiHochbergCorrection.csv")
UCS <- sqldf::sqldf("SELECT UCS_filtered.*, BH_pvalues_adjusted,BH_rejected  from UCS_filtered LEFT JOIN UCSWBenjaminiHochbergCorrection ON 
                          (UCS_filtered.entrezgene_id = UCSWBenjaminiHochbergCorrection.mrna AND UCS_filtered.mirna1 = UCSWBenjaminiHochbergCorrection.mirna1 AND UCS_filtered.mirna2 = UCSWBenjaminiHochbergCorrection.mirna2) 
                          OR (UCS_filtered.entrezgene_id = UCSWBenjaminiHochbergCorrection.mrna AND UCS_filtered.mirna1 = UCSWBenjaminiHochbergCorrection.mirna2 AND UCS_filtered.mirna2 = UCSWBenjaminiHochbergCorrection.mirna1)")
UCS_BH_pvalues_adjusted_min = min(UCS$BH_pvalues_adjusted)
UCS_BH_pvalues_adjusted_max = max(UCS$BH_pvalues_adjusted)

UVM_allData <- read_csv("finalDTData/UVM.csv",show_col_types = FALSE)
UVM_filtered <- UVM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]
UVMWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/UVMWBenjaminiHochbergCorrection.csv")
UVM <- sqldf::sqldf("SELECT UVM_filtered.*, BH_pvalues_adjusted,BH_rejected  from UVM_filtered LEFT JOIN UVMWBenjaminiHochbergCorrection ON 
                          (UVM_filtered.entrezgene_id = UVMWBenjaminiHochbergCorrection.mrna AND UVM_filtered.mirna1 = UVMWBenjaminiHochbergCorrection.mirna1 AND UVM_filtered.mirna2 = UVMWBenjaminiHochbergCorrection.mirna2) 
                          OR (UVM_filtered.entrezgene_id = UVMWBenjaminiHochbergCorrection.mrna AND UVM_filtered.mirna1 = UVMWBenjaminiHochbergCorrection.mirna2 AND UVM_filtered.mirna2 = UVMWBenjaminiHochbergCorrection.mirna1)")
UVM_BH_pvalues_adjusted_min = min(UVM$BH_pvalues_adjusted)
UVM_BH_pvalues_adjusted_max = max(UVM$BH_pvalues_adjusted)


write_csv(ACC,"finalDTDataWBHCorrection/ACC.csv")
write_csv(BLCA,"finalDTDataWBHCorrection/BLCA.csv")
write_csv(BRCA,"finalDTDataWBHCorrection/BRCA.csv")
write_csv(CESC,"finalDTDataWBHCorrection/CESC.csv")
write_csv(CHOL,"finalDTDataWBHCorrection/CHOL.csv")
write_csv(COAD,"finalDTDataWBHCorrection/COAD.csv")
write_csv(DLBC,"finalDTDataWBHCorrection/DLBC.csv")
write_csv(ESCA,"finalDTDataWBHCorrection/ESCA.csv")
write_csv(HNSC,"finalDTDataWBHCorrection/HNSC.csv")
write_csv(KICH,"finalDTDataWBHCorrection/KICH.csv")
write_csv(KIRC,"finalDTDataWBHCorrection/KIRC.csv")
write_csv(KIRP,"finalDTDataWBHCorrection/KIRP.csv")
write_csv(LGG,"finalDTDataWBHCorrection/LGG.csv")
write_csv(LIHC,"finalDTDataWBHCorrection/LIHC.csv")
write_csv(LUAD,"finalDTDataWBHCorrection/LUAD.csv")
write_csv(LUSC,"finalDTDataWBHCorrection/LUSC.csv")
write_csv(MESO,"finalDTDataWBHCorrection/MESO.csv")
write_csv(OV,"finalDTDataWBHCorrection/OV.csv")
write_csv(PAAD,"finalDTDataWBHCorrection/PAAD.csv")
write_csv(PCPG,"finalDTDataWBHCorrection/PCPG.csv")
write_csv(PRAD,"finalDTDataWBHCorrection/PRAD.csv")
write_csv(READ,"finalDTDataWBHCorrection/READ.csv")
write_csv(SARC,"finalDTDataWBHCorrection/SARC.csv")
write_csv(SKCM,"finalDTDataWBHCorrection/SKCM.csv")
write_csv(STAD,"finalDTDataWBHCorrection/STAD.csv")
write_csv(TGCT,"finalDTDataWBHCorrection/TGCT.csv")
write_csv(THCA,"finalDTDataWBHCorrection/THCA.csv")
write_csv(THYM,"finalDTDataWBHCorrection/THYM.csv")
write_csv(UCEC,"finalDTDataWBHCorrection/UCEC.csv")
write_csv(UCS,"finalDTDataWBHCorrection/UCS.csv")
write_csv(UVM,"finalDTDataWBHCorrection/UVM.csv")

ACC_allData <- read_csv("finalDTDataWBHCorrection/ACC.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
ACC_filtered <- ACC_allData%>%dplyr::filter(BH_rejected == TRUE)
print(ACC_filtered$hgnc_symbol)

BRCA_allData <- read_csv("finalDTDataWBHCorrection/BRCA.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
BRCA <- BRCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted")]

UCEC_allData <- read_csv("finalDTDataWBHCorrection/UCEC.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
UCEC <- UCEC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted")]

UCEC_BH_pvalues_adjusted_min = min(UCEC$BH_pvalues_adjusted)
UCEC_BH_pvalues_adjusted_max = max(UCEC$BH_pvalues_adjusted)