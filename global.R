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
library(randomcoloR)
library(tidyr)


infoBtn <- function(id) {
  actionButton(id,
               label = "",
               icon = icon("question"),
               style="color: #fff; background-color: #074487; border-color: #074487; border-radius: 4px;
                 height:17px; width:17px;padding:0px 0.5px 0.5px 0.5px;
                 font-size:70%"
               #size = "small",
               
               
  )
}




# cancerNames <- c("ACC","BLCA","BRCA","CESC","CHOL","COAD","DLBC","ESCA","HNSC","KICH","KIRC","KIRP",
#                  "LGG","LIHC","LUAD","LUSC","MESO","OV","PAAD","PCPG","PRAD","READ","SARC","SKCM",
#                  "STAD","TGCT","THCA","THYM","UCEC","UCS","UVM")

cancerNames <- c("ACC","BRCA","CESC","COAD","DLBC","ESCA","KICH","KIRC","KIRP","LUAD","LUSC","MESO","OV","PAAD","PCPG","PRAD","READ","SKCM","THCA","UCS","UVM")
####################################################################################################
# ACC_allData <- read.csv("miRNAClusterandFamily/ACC.csv")
# ACC_Type3 <- ACCType3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation")] %>% distinct()
# 
# ACC_BH_pvalues_adjusted_min = min(ACC$BH_pvalues_adjusted)
# ACC_BH_pvalues_adjusted_max = max(ACC$BH_pvalues_adjusted)
# 
# BLCA_allData <- read_csv("miRNAClusterandFamily/BLCA.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# BLCA <- BLCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# BLCA_BH_pvalues_adjusted_min = min(BLCA$BH_pvalues_adjusted)
# BLCA_BH_pvalues_adjusted_max = max(BLCA$BH_pvalues_adjusted)
# 
# BRCA_allData <- read_csv("miRNAClusterandFamily/BRCA.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# BRCA <- BRCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# BRCA_BH_pvalues_adjusted_min = min(BRCA$BH_pvalues_adjusted)
# BRCA_BH_pvalues_adjusted_max = max(BRCA$BH_pvalues_adjusted)
# 
# CESC_allData <- read_csv("miRNAClusterandFamily/CESC.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# CESC <- CESC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# CESC_BH_pvalues_adjusted_min = min(CESC$BH_pvalues_adjusted)
# CESC_BH_pvalues_adjusted_max = max(CESC$BH_pvalues_adjusted)
# 
# CHOL_allData <- read_csv("miRNAClusterandFamily/CHOL.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# CHOL <- CHOL_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# CHOL_BH_pvalues_adjusted_min = min(CHOL$BH_pvalues_adjusted)
# CHOL_BH_pvalues_adjusted_max = max(CHOL$BH_pvalues_adjusted)
# 
# COAD_allData <- read_csv("miRNAClusterandFamily/COAD.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# COAD <- COAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# COAD_BH_pvalues_adjusted_min = min(COAD$BH_pvalues_adjusted)
# COAD_BH_pvalues_adjusted_max = max(COAD$BH_pvalues_adjusted)
# 
# DLBC_allData <- read_csv("miRNAClusterandFamily/DLBC.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# DLBC <- DLBC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# DLBC_BH_pvalues_adjusted_min = min(DLBC$BH_pvalues_adjusted)
# DLBC_BH_pvalues_adjusted_max = max(DLBC$BH_pvalues_adjusted)
# 
# ESCA_allData <- read_csv("miRNAClusterandFamily/ESCA.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# ESCA <- ESCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# ESCA_BH_pvalues_adjusted_min = min(ESCA$BH_pvalues_adjusted)
# ESCA_BH_pvalues_adjusted_max = max(ESCA$BH_pvalues_adjusted)
# 
# HNSC_allData <- read_csv("miRNAClusterandFamily/HNSC.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# HNSC <- HNSC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# HNSC_BH_pvalues_adjusted_min = min(HNSC$BH_pvalues_adjusted)
# HNSC_BH_pvalues_adjusted_max = max(HNSC$BH_pvalues_adjusted)
# 
# KICH_allData <- read_csv("miRNAClusterandFamily/KICH.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# KICH <- KICH_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# KICH_BH_pvalues_adjusted_min = min(KICH$BH_pvalues_adjusted)
# KICH_BH_pvalues_adjusted_max = max(KICH$BH_pvalues_adjusted)
# 
# KIRC_allData <- read_csv("miRNAClusterandFamily/KIRC.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# KIRC <- KIRC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# KIRC_BH_pvalues_adjusted_min = min(KIRC$BH_pvalues_adjusted)
# KIRC_BH_pvalues_adjusted_max = max(KIRC$BH_pvalues_adjusted)
# 
# KIRP_allData <- read_csv("miRNAClusterandFamily/KIRP.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# KIRP <- KIRP_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# KIRP_BH_pvalues_adjusted_min = min(KIRP$BH_pvalues_adjusted)
# KIRP_BH_pvalues_adjusted_max = max(KIRP$BH_pvalues_adjusted)
# 
# LGG_allData <- read_csv("miRNAClusterandFamily/LGG.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# LGG <- LGG_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# LGG_BH_pvalues_adjusted_min = min(LGG$BH_pvalues_adjusted)
# LGG_BH_pvalues_adjusted_max = max(LGG$BH_pvalues_adjusted)
# 
# LIHC_allData <- read_csv("miRNAClusterandFamily/LIHC.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# LIHC <- LIHC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# LIHC_BH_pvalues_adjusted_min = min(LIHC$BH_pvalues_adjusted)
# LIHC_BH_pvalues_adjusted_max = max(LIHC$BH_pvalues_adjusted)
# 
# LUAD_allData <- read_csv("miRNAClusterandFamily/LUAD.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# LUAD <- LUAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# LUAD_BH_pvalues_adjusted_min = min(LUAD$BH_pvalues_adjusted)
# LUAD_BH_pvalues_adjusted_max = max(LUAD$BH_pvalues_adjusted)
# 
# LUSC_allData <- read_csv("miRNAClusterandFamily/LUSC.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# LUSC <- LUSC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# LUSC_BH_pvalues_adjusted_min = min(LUSC$BH_pvalues_adjusted)
# LUSC_BH_pvalues_adjusted_max = max(LUSC$BH_pvalues_adjusted)
# 
# MESO_allData <- read_csv("miRNAClusterandFamily/MESO.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# MESO <- MESO_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# MESO_BH_pvalues_adjusted_min = min(MESO$BH_pvalues_adjusted)
# MESO_BH_pvalues_adjusted_max = max(MESO$BH_pvalues_adjusted)
# 
# OV_allData <- read_csv("miRNAClusterandFamily/OV.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# OV <- OV_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# OV_BH_pvalues_adjusted_min = min(OV$BH_pvalues_adjusted)
# OV_BH_pvalues_adjusted_max = max(OV$BH_pvalues_adjusted)
# 
# PAAD_allData <- read_csv("miRNAClusterandFamily/PAAD.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# PAAD <- PAAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# PAAD_BH_pvalues_adjusted_min = min(PAAD$BH_pvalues_adjusted)
# PAAD_BH_pvalues_adjusted_max = max(PAAD$BH_pvalues_adjusted)
# 
# PCPG_allData <- read_csv("miRNAClusterandFamily/PCPG.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# PCPG <- PCPG_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# PCPG_BH_pvalues_adjusted_min = min(PCPG$BH_pvalues_adjusted)
# PCPG_BH_pvalues_adjusted_max = max(PCPG$BH_pvalues_adjusted)
# 
# PRAD_allData <- read_csv("miRNAClusterandFamily/PRAD.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# PRAD <- PRAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# PRAD_BH_pvalues_adjusted_min = min(PRAD$BH_pvalues_adjusted)
# PRAD_BH_pvalues_adjusted_max = max(PRAD$BH_pvalues_adjusted)
# 
# READ_allData <- read_csv("miRNAClusterandFamily/READ.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# READ <- READ_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# READ_BH_pvalues_adjusted_min = min(READ$BH_pvalues_adjusted)
# READ_BH_pvalues_adjusted_max = max(READ$BH_pvalues_adjusted)
# 
# SARC_allData <- read_csv("miRNAClusterandFamily/SARC.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# SARC <- SARC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# SARC_BH_pvalues_adjusted_min = min(SARC$BH_pvalues_adjusted)
# SARC_BH_pvalues_adjusted_max = max(SARC$BH_pvalues_adjusted)
# 
# SKCM_allData <- read_csv("miRNAClusterandFamily/SKCM.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# SKCM <- SKCM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# SKCM_BH_pvalues_adjusted_min = min(SKCM$BH_pvalues_adjusted)
# SKCM_BH_pvalues_adjusted_max = max(SKCM$BH_pvalues_adjusted)
# 
# STAD_allData <- read_csv("miRNAClusterandFamily/STAD.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# STAD <- STAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# STAD_BH_pvalues_adjusted_min = min(STAD$BH_pvalues_adjusted)
# STAD_BH_pvalues_adjusted_max = max(STAD$BH_pvalues_adjusted)
# 
# TGCT_allData <- read_csv("miRNAClusterandFamily/TGCT.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# TGCT <- TGCT_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# TGCT_BH_pvalues_adjusted_min = min(TGCT$BH_pvalues_adjusted)
# TGCT_BH_pvalues_adjusted_max = max(TGCT$BH_pvalues_adjusted)
# 
# THCA_allData <- read_csv("miRNAClusterandFamily/THCA.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# THCA <- THCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# THCA_BH_pvalues_adjusted_min = min(THCA$BH_pvalues_adjusted)
# THCA_BH_pvalues_adjusted_max = max(THCA$BH_pvalues_adjusted)
# 
# THYM_allData <- read_csv("miRNAClusterandFamily/THYM.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# THYM <- THYM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# THYM_BH_pvalues_adjusted_min = min(THYM$BH_pvalues_adjusted)
# THYM_BH_pvalues_adjusted_max = max(THYM$BH_pvalues_adjusted)
# 
# UCEC_allData <- read_csv("miRNAClusterandFamily/UCEC.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# UCEC <- UCEC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# UCEC_BH_pvalues_adjusted_min = min(UCEC$BH_pvalues_adjusted)
# UCEC_BH_pvalues_adjusted_max = max(UCEC$BH_pvalues_adjusted)
# 
# UCS_allData <- read_csv("miRNAClusterandFamily/UCS.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# UCS <- UCS_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# UCS_BH_pvalues_adjusted_min = min(UCS$BH_pvalues_adjusted)
# UCS_BH_pvalues_adjusted_max = max(UCS$BH_pvalues_adjusted)
# 
# UVM_allData <- read_csv("miRNAClusterandFamily/UVM.csv",show_col_types = FALSE)%>%dplyr::filter(BH_rejected == TRUE)
# UVM <- UVM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","BH_pvalues_adjusted","miRNA1Family","miRNA2Family","miRNA1Cluster","miRNA2Cluster")]
# 
# UVM_BH_pvalues_adjusted_min = min(UVM$BH_pvalues_adjusted)
# UVM_BH_pvalues_adjusted_max = max(UVM$BH_pvalues_adjusted)

####################################################################################################
ACC_Type3_allData <- read_csv("TF_Type3Results/ACC_Type3Pval001.csv")
ACC_Type3 <- ACC_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

BRCA_Type3_allData <- read_csv("TF_Type3Results/BRCA_Type3Pval001.csv")
BRCA_Type3 <- BRCA_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

CESC_Type3_allData <- read_csv("TF_Type3Results/CESC_Type3Pval001.csv")
CESC_Type3 <- CESC_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

#CHOL YOK
# CHOL_Type3_allData <- read_csv("TF_Type3Results/CHOL_Type3Pval001.csv")
# CHOL_Type3 <- CHOL_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")]

COAD_Type3_allData <- read_csv("TF_Type3Results/COAD_Type3Pval001.csv")
COAD_Type3 <- COAD_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

DLBC_Type3_allData <- read_csv("TF_Type3Results/DLBC_Type3Pval001.csv")
DLBC_Type3 <- DLBC_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

ESCA_Type3_allData <- read_csv("TF_Type3Results/ESCA_Type3Pval001.csv")
ESCA_Type3 <- ESCA_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

KICH_Type3_allData <- read_csv("TF_Type3Results/KICH_Type3Pval001.csv")
KICH_Type3 <- KICH_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

KIRC_Type3_allData <- read_csv("TF_Type3Results/KIRC_Type3Pval001.csv")
KIRC_Type3 <- KIRC_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

KIRP_Type3_allData <- read_csv("TF_Type3Results/KIRP_Type3Pval001.csv")
KIRP_Type3 <- KIRP_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

LUAD_Type3_allData <- read_csv("TF_Type3Results/LUAD_Type3Pval001.csv")
LUAD_Type3 <- LUAD_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

LUSC_Type3_allData <- read_csv("TF_Type3Results/LUSC_Type3Pval001.csv")
LUSC_Type3 <- LUSC_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

MESO_Type3_allData <- read_csv("TF_Type3Results/MESO_Type3Pval001.csv")
MESO_Type3 <- MESO_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

OV_Type3_allData <- read_csv("TF_Type3Results/OV_Type3Pval001.csv")
OV_Type3 <- OV_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

PAAD_Type3_allData <- read_csv("TF_Type3Results/PAAD_Type3Pval001.csv")
PAAD_Type3 <- PAAD_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

PCPG_Type3_allData <- read_csv("TF_Type3Results/PCPG_Type3Pval001.csv")
PCPG_Type3 <- PCPG_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

PRAD_Type3_allData <- read_csv("TF_Type3Results/PRAD_Type3Pval001.csv")
PRAD_Type3 <- PRAD_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

READ_Type3_allData <- read_csv("TF_Type3Results/READ_Type3Pval001.csv")
READ_Type3 <- READ_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] 

SKCM_Type3_allData <- read_csv("TF_Type3Results/SKCM_Type3Pval001.csv")
SKCM_Type3 <- SKCM_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] %>% distinct()

THCA_Type3_allData <- read_csv("TF_Type3Results/THCA_Type3Pval001.csv")
THCA_Type3 <- THCA_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] %>% distinct()

UCS_Type3_allData <- read_csv("TF_Type3Results/UCS_Type3Pval001.csv")
UCS_Type3 <- UCS_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] %>% distinct()

UVM_Type3_allData <- read_csv("TF_Type3Results/UVM_Type3Pval001.csv")
UVM_Type3 <- UVM_Type3_allData[,c("miRNA1", "miRNA2", "TF", "Target", "ModeOfRegulation","mirna1mirna2_target")] %>% distinct()

####################################################################################################

concated <- data.frame(source=c(),target=c())

ACC_source_target <- read.table("networkData/ACC_source_target.csv", header = TRUE, sep = ";")
ACC_node_attr <- read.table("networkDataWClusterFamily/ACC.csv", header = TRUE, sep = ",")

BLCA_source_target <- read.table("networkData/BLCA_source_target_new.csv", header=TRUE, sep = ";" )
BLCA_node_attr <- read.table("networkDataWClusterFamily/BLCA.csv", header = T, sep = ",")

BRCA_source_target <- read.table("networkData/BRCA_source_target.csv", header=TRUE, sep = ";" )
BRCA_node_attr <- read.table("networkDataWClusterFamily/BRCA.csv", header = T, sep = ",")

CESC_source_target <- read.table("networkData/CESC_source_target_info.csv", header=TRUE, sep = ";" )
CESC_node_attr <- read.table("networkDataWClusterFamily/CESC.csv", header = T, sep = ",")

CHOL_source_target <- read.table("networkData/CHOL_source_target.csv", header=TRUE, sep = ";" )
CHOL_node_attr <- read.table("networkDataWClusterFamily/CHOL.csv", header = T, sep = ",")

COAD_source_target <- read.table("networkData/COAD_source_target_info.csv", header=TRUE, sep = ";" )
COAD_node_attr <- read.table("networkDataWClusterFamily/COAD.csv", header = T, sep = ",")

DLBC_source_target <- read.table("networkData/DLBC_source_target.csv", header=TRUE, sep = ";" )
DLBC_node_attr <- read.table("networkDataWClusterFamily/DLBC.csv", header = T, sep = ",")

ESCA_source_target <- read.table("networkData/ESCA_source_target.csv", header=TRUE, sep = ";" )
ESCA_node_attr <- read.table("networkDataWClusterFamily/ESCA.csv", header = T, sep = ",")

HNSC_source_target <- read.table("networkData/HNSC_source_target.csv", header=TRUE, sep = ";" )
HNSC_node_attr <- read.table("networkDataWClusterFamily/HNSC.csv", header = T, sep = ",")

KICH_source_target <- read.table("networkData/KICH_source_target.csv", header=TRUE, sep = ";" )
KICH_node_attr <- read.table("networkDataWClusterFamily/KICH.csv", header = T, sep = ",")

KIRC_source_target <- read.table("networkData/KIRC_source_target.csv", header=TRUE, sep = ";" )
KIRC_node_attr <- read.table("networkDataWClusterFamily/KIRC.csv", header = T, sep = ",")

KIRP_source_target <- read.table("networkData/KIRP_source_target.csv", header=TRUE, sep = ";" )
KIRP_node_attr <- read.table("networkDataWClusterFamily/KIRP.csv", header = T, sep = ",")

LIHC_source_target <- read.table("networkData/LIHC_source_target.csv", header=TRUE, sep = ";" )
LIHC_node_attr <- read.table("networkDataWClusterFamily/LIHC.csv", header = T, sep = ",")

LGG_source_target <- read.table("networkData/LGG_source_target_info.csv", header=TRUE, sep = ";" )
LGG_node_attr <- read.table("networkDataWClusterFamily/LGG.csv", header = T, sep = ",")

LUAD_source_target <- read.table("networkData/LUAD_source_target.csv", header=TRUE, sep = ";" )
LUAD_node_attr <- read.table("networkDataWClusterFamily/LUAD.csv", header = T, sep = ",")

LUSC_source_target <- read.table("networkData/LUSC_source_target.csv", header=TRUE, sep = ";" )
LUSC_node_attr <- read.table("networkDataWClusterFamily/LUSC.csv", header = T, sep = ",")

MESO_source_target <- read.table("networkData/MESO_source_target.csv", header=TRUE, sep = ";" )
MESO_node_attr <- read.table("networkDataWClusterFamily/MESO.csv", header = T, sep = ",")

OV_source_target <- read.table("networkData/OV_source_target.csv", header=TRUE, sep = ";" )
OV_node_attr <- read.table("networkDataWClusterFamily/OV.csv", header = T, sep = ",")

PAAD_source_target <- read.table("networkData/PAAD_source_target.csv", header=TRUE, sep = ";" )
PAAD_node_attr <- read.table("networkDataWClusterFamily/PAAD.csv", header = T, sep = ",")

PCPG_source_target <- read.table("networkData/PCPG_source_target.csv", header=TRUE, sep = ";" )
PCPG_node_attr <- read.table("networkDataWClusterFamily/PCPG.csv", header = T, sep = ",")

PRAD_source_target <- read.table("networkData/PRAD_source_target.csv", header = TRUE, sep = ";")
PRAD_node_attr <- read.table("networkDataWClusterFamily/PRAD.csv", header = TRUE, sep = ",")

READ_source_target <- read.table("networkData/READ_source_target.csv", header=TRUE, sep = ";" )
READ_node_attr <- read.table("networkDataWClusterFamily/READ.csv", header = T, sep = ",")

SARC_source_target <- read.table("networkData/SARC_source_target.csv", header=TRUE, sep = ";" )
SARC_node_attr <- read.table("networkDataWClusterFamily/SARC.csv", header = T, sep = ",")

SKCM_source_target <- read.table("networkData/SKCM_source_target.csv", header=TRUE, sep = ";" )
SKCM_node_attr <- read.table("networkDataWClusterFamily/SKCM.csv", header = T, sep = ",")

STAD_source_target <- read.table("networkData/STAD_source_target_info.csv", header=TRUE, sep = ";" )
STAD_node_attr <- read.table("networkDataWClusterFamily/STAD.csv", header = T, sep = ",")

TGCT_source_target <- read.table("networkData/TGCT_source_target.csv", header=TRUE, sep = ";" )
TGCT_node_attr <- read.table("networkDataWClusterFamily/TGCT.csv", header = T, sep = ",")

THCA_source_target <- read.table("networkData/THCA_source_target.csv", header=TRUE, sep = ";" )
THCA_node_attr <- read.table("networkDataWClusterFamily/THCA.csv", header = T, sep = ",")

THYM_source_target <- read.table("networkData/THYM_source_target.csv", header=TRUE, sep = ";" )
THYM_node_attr <- read.table("networkDataWClusterFamily/THYM.csv", header = T, sep = ",")

UCEC_source_target <- read.table("networkData/UCEC_source_target.csv", header=TRUE, sep = ";" )
UCEC_node_attr <- read.table("networkDataWClusterFamily/UCEC.csv", header = T, sep = ",")

UCS_source_target <- read.table("networkData/UCS_source_target.csv", header=TRUE, sep = ";" )
UCS_node_attr <- read.table("networkDataWClusterFamily/UCS.csv", header = TRUE, sep = ",")

UVM_source_target <- read.table("networkData/UVM_source_target.csv", header=TRUE, sep = ";" )
UVM_node_attr <- read.table("networkDataWClusterFamily/UVM.csv", header = T, sep = ",")


#commonMirnaPairs_node_attr <- read_csv("networkData/commonMirnaPairsAfterReRunNode.csv")
commonMirnaPairs_node_attr <- read_delim("networkData/common_mirna_pair_withclusterandfamily.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

commonTriplets_node_attr <- read_csv("networkData/commonTripletsAfterReRunNode.csv")

commonTriplet_source_target <- read_delim("networkData/commonTriplet_source_target.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

commonMirnaPair_source_target <- read_delim("networkData/commonMirnaPair_source_target.csv", 
                                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
####################################################################################################
ACCwMedian1 <- read_csv("dataset/medians/ACCwMedian1.csv",show_col_types = FALSE)
ACCwMedian2 <- read_csv("dataset/medians/ACCwMedian2.csv",show_col_types = FALSE)
BLCAwMedian1 <- read_csv("dataset/medians/BLCAwMedian1.csv",show_col_types = FALSE)
BLCAwMedian2 <- read_csv("dataset/medians/BLCAwMedian2.csv",show_col_types = FALSE)
BRCAwMedian1 <- read_csv("dataset/medians/BRCAwMedian1.csv",show_col_types = FALSE)
BRCAwMedian2 <- read_csv("dataset/medians/BRCAwMedian2.csv",show_col_types = FALSE)
CESCwMedian1 <- read_csv("dataset/medians/CESCwMedian1.csv",show_col_types = FALSE)
CESCwMedian2 <- read_csv("dataset/medians/CESCwMedian2.csv",show_col_types = FALSE)
CHOLwMedian1 <- read_csv("dataset/medians/CHOLwMedian1.csv",show_col_types = FALSE)
CHOLwMedian2 <- read_csv("dataset/medians/CHOLwMedian2.csv",show_col_types = FALSE)
COADwMedian1 <- read_csv("dataset/medians/COADwMedian1.csv",show_col_types = FALSE)
COADwMedian2 <- read_csv("dataset/medians/COADwMedian2.csv",show_col_types = FALSE)
DLBCwMedian1 <- read_csv("dataset/medians/DLBCwMedian1.csv",show_col_types = FALSE)
DLBCwMedian2 <- read_csv("dataset/medians/DLBCwMedian2.csv",show_col_types = FALSE)
ESCAwMedian1 <- read_csv("dataset/medians/ESCAwMedian1.csv",show_col_types = FALSE)
ESCAwMedian2 <- read_csv("dataset/medians/ESCAwMedian2.csv",show_col_types = FALSE)
HNSCwMedian1 <- read_csv("dataset/medians/HNSCwMedian1.csv",show_col_types = FALSE)
HNSCwMedian2 <- read_csv("dataset/medians/HNSCwMedian2.csv",show_col_types = FALSE)
KICHwMedian1 <- read_csv("dataset/medians/KICHwMedian1.csv",show_col_types = FALSE)
KICHwMedian2 <- read_csv("dataset/medians/KICHwMedian2.csv",show_col_types = FALSE)
KIRCwMedian1 <- read_csv("dataset/medians/KIRCwMedian1.csv",show_col_types = FALSE)
KIRCwMedian2 <- read_csv("dataset/medians/KIRCwMedian2.csv",show_col_types = FALSE)
KIRPwMedian1 <- read_csv("dataset/medians/KIRPwMedian1.csv",show_col_types = FALSE)
KIRPwMedian2 <- read_csv("dataset/medians/KIRPwMedian2.csv",show_col_types = FALSE)
LGGwMedian1 <- read_csv("dataset/medians/LGGwMedian1.csv",show_col_types = FALSE)
LGGwMedian2 <- read_csv("dataset/medians/LGGwMedian2.csv",show_col_types = FALSE)
LIHCwMedian1 <- read_csv("dataset/medians/LIHCwMedian1.csv",show_col_types = FALSE)
LIHCwMedian2 <- read_csv("dataset/medians/LIHCwMedian2.csv",show_col_types = FALSE)
LUADwMedian1 <- read_csv("dataset/medians/LUADwMedian1.csv",show_col_types = FALSE)
LUADwMedian2 <- read_csv("dataset/medians/LUADwMedian2.csv",show_col_types = FALSE)
LUSCwMedian1 <- read_csv("dataset/medians/LUSCwMedian1.csv",show_col_types = FALSE)
LUSCwMedian2 <- read_csv("dataset/medians/LUSCwMedian2.csv",show_col_types = FALSE)
MESOwMedian1 <- read_csv("dataset/medians/MESOwMedian1.csv",show_col_types = FALSE)
MESOwMedian2 <- read_csv("dataset/medians/MESOwMedian2.csv",show_col_types = FALSE)
OVwMedian1 <- read_csv("dataset/medians/OVwMedian1.csv",show_col_types = FALSE)
OVwMedian2 <- read_csv("dataset/medians/OVwMedian2.csv",show_col_types = FALSE)
PAADwMedian1 <- read_csv("dataset/medians/PAADwMedian1.csv",show_col_types = FALSE)
PAADwMedian2 <- read_csv("dataset/medians/PAADwMedian2.csv",show_col_types = FALSE)
PCPGwMedian1 <- read_csv("dataset/medians/PCPGwMedian1.csv",show_col_types = FALSE)
PCPGwMedian2 <- read_csv("dataset/medians/PCPGwMedian2.csv",show_col_types = FALSE)
PRADwMedian1 <- read_csv("dataset/medians/PRADwMedian1.csv",show_col_types = FALSE)
PRADwMedian2 <- read_csv("dataset/medians/PRADwMedian2.csv",show_col_types = FALSE)
READwMedian1 <- read_csv("dataset/medians/READwMedian1.csv",show_col_types = FALSE)
READwMedian2 <- read_csv("dataset/medians/READwMedian2.csv",show_col_types = FALSE)
SARCwMedian1 <- read_csv("dataset/medians/SARCwMedian1.csv",show_col_types = FALSE)
SARCwMedian2 <- read_csv("dataset/medians/SARCwMedian2.csv",show_col_types = FALSE)
SKCMwMedian1 <- read_csv("dataset/medians/SKCMwMedian1.csv",show_col_types = FALSE)
SKCMwMedian2 <- read_csv("dataset/medians/SKCMwMedian2.csv",show_col_types = FALSE)
STADwMedian1 <- read_csv("dataset/medians/STADwMedian1.csv",show_col_types = FALSE)
STADwMedian2 <- read_csv("dataset/medians/STADwMedian2.csv",show_col_types = FALSE)
TGCTwMedian1 <- read_csv("dataset/medians/TGCTwMedian1.csv",show_col_types = FALSE)
TGCTwMedian2 <- read_csv("dataset/medians/TGCTwMedian2.csv",show_col_types = FALSE)
THCAwMedian1 <- read_csv("dataset/medians/THCAwMedian1.csv",show_col_types = FALSE)
THCAwMedian2 <- read_csv("dataset/medians/THCAwMedian2.csv",show_col_types = FALSE)
THYMwMedian1 <- read_csv("dataset/medians/THYMwMedian1.csv",show_col_types = FALSE)
THYMwMedian2 <- read_csv("dataset/medians/THYMwMedian2.csv",show_col_types = FALSE)
UCECwMedian1 <- read_csv("dataset/medians/UCECwMedian1.csv",show_col_types = FALSE)
UCECwMedian2 <- read_csv("dataset/medians/UCECwMedian2.csv",show_col_types = FALSE)
UCSwMedian1 <- read_csv("dataset/medians/UCSwMedian1.csv",show_col_types = FALSE)
UCSwMedian2 <- read_csv("dataset/medians/UCSwMedian2.csv",show_col_types = FALSE)
UVMwMedian1 <- read_csv("dataset/medians/UVMwMedian1.csv",show_col_types = FALSE)
UVMwMedian2 <- read_csv("dataset/medians/UVMwMedian2.csv",show_col_types = FALSE)

####################################################################################################
TripletsInWhichCancerWCount<- read_delim("dataset/CommonTripletsAfterBHCorrection.csv",delim = ";",escape_double = FALSE, trim_ws = TRUE,show_col_types = FALSE)
TripletsInWhichCancerWCount <-filter(TripletsInWhichCancerWCount, Count> 1)
mrnaFilterCommonTriplets <- filter(commonTriplets_node_attr, info %in% "mrna")
mirnaFilterCommonTriplets <- filter(commonTriplets_node_attr, info %in% "mirna")

MirnaPairsInWhichCancerWCount <- read_delim("dataset/commonMirnaPairsAfterBHCorrection.csv",escape_double = FALSE, trim_ws = TRUE,show_col_types = FALSE)
MirnaPairsInWhichCancerWCount <-filter(MirnaPairsInWhichCancerWCount, Count> 1)
mirnaListCommonMirnaPairs <- unique(rbind(commonMirnaPair_source_target$source, commonMirnaPair_source_target$target))


TCGA_abbreviations <- read.table("dataset/TCGA_abbreviations.csv", header = T, sep = ";")

Glossary <- read.table("dataset/Glossary.csv", header = T, sep = ";")


miRCoopTotalCounts <- read_delim("dataset/stats/miRCoopTotalCountsAfterBHCorrection.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

commonMrnaAbove20 <- read_excel("dataset/stats/totalMrnaCountCancerMatrixWithTotalMrnaCountInDatabasesAfterBHCorrectionAbove15.xlsx")
commonMrnaAbove20 <- as.data.frame(commonMrnaAbove20)

commonMirnaAbove50 <- read_excel("dataset/stats/totalMirnaCountCancerMatrixWithTotalMirnaCountInDatabasesAfterBHCorrectionAbove30.xlsx")
commonMirnaAbove50 <- as.data.frame(commonMirnaAbove50)

mRNACountsScatter <- read_delim("dataset/stats/mRNACountsScatterAfterBHCorrection.csv", 
                                delim = ",", escape_double = FALSE, trim_ws = TRUE)

miRNACountsScatter <- read_delim("dataset/stats/miRNACountsScatterAfterBHCorrection.csv", 
                                 delim = ",", escape_double = FALSE, trim_ws = TRUE)

####################################################################################################

bslib_sabanci20_theme <- bs_theme(
  #version = 5,  
  # bg = "#FFFFFF",
  # fg = "#000000",
  # bootswatch = "united",
  #primary = "#074487",
  #secondary = "#179E93",
  #success = "#00A5DF",
  # info = "#00A5DF",
  # warning = "#FFF100",
  # danger = "#FF00E3",
  base_font = font_google("Roboto", local = TRUE) ,
  heading_font = font_google("Roboto", local = TRUE),
  code_font = font_google("Roboto", local = TRUE) ,
  #"input-border-color" = "#179E93",
  #"border-radius" =  ".70rem"
)

