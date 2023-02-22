
library(readr)

ACC_allData <- read_csv("finalDTData/ACC.csv",show_col_types = FALSE)
ACC <- ACC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

BLCA_allData <- read_csv("finalDTData/BLCA.csv",show_col_types = FALSE)
BLCA <- BLCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

# BRCA_allData <- read_csv("finalDTData/BRCA.csv")
# BRCA <- BRCA_allData[,c()]

CESC_allData <- read_csv("finalDTData/CESC.csv",show_col_types = FALSE)
CESC <- CESC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

CHOL_allData <- read_csv("finalDTData/CHOL.csv",show_col_types = FALSE)
CHOL <- CHOL_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

COAD_allData <- read_csv("finalDTData/COAD.csv",show_col_types = FALSE)
COAD <- COAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC")]

DLBC_allData <- read_csv("finalDTData/DLBC.csv",show_col_types = FALSE)
DLBC <- DLBC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

ESCA_allData <- read_csv("finalDTData/ESCA.csv",show_col_types = FALSE)
ESCA <- ESCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

HNSC_allData <- read_csv("finalDTData/HNSC.csv",show_col_types = FALSE)
HNSC <- HNSC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

KICH_allData <- read_csv("finalDTData/KICH.csv",show_col_types = FALSE)
KICH <- KICH_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

KIRC_allData <- read_csv("finalDTData/KIRC.csv",show_col_types = FALSE)
KIRC <- KIRC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

KIRP_allData <- read_csv("finalDTData/KIRP.csv",show_col_types = FALSE)
KIRP <- KIRP_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

LGG_allData <- read_csv("finalDTData/LGG.csv",show_col_types = FALSE)
LGG <- LGG_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

LIHC_allData <- read_csv("finalDTData/LIHC.csv",show_col_types = FALSE)
LIHC <- LIHC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

LUAD_allData <- read_csv("finalDTData/LUAD.csv",show_col_types = FALSE)
LUAD <- LUAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

LUSC_allData <- read_csv("finalDTData/LUSC.csv",show_col_types = FALSE)
LUSC <- LUSC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

MESO_allData <- read_csv("finalDTData/MESO.csv",show_col_types = FALSE)
MESO <- MESO_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

OV_allData <- read_csv("finalDTData/OV.csv",show_col_types = FALSE)
OV <- OV_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

PAAD_allData <- read_csv("finalDTData/PAAD.csv",show_col_types = FALSE)
PAAD <- PAAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

PCPG_allData <- read_csv("finalDTData/PCPG.csv",show_col_types = FALSE)
PCPG <- PCPG_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

PRAD_allData <- read_csv("finalDTData/PRAD.csv",show_col_types = FALSE)
PRAD <- PRAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

READ_allData <- read_csv("finalDTData/READ.csv",show_col_types = FALSE)
READ <- READ_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC")]

SARC_allData <- read_csv("finalDTData/SARC.csv",show_col_types = FALSE)
SARC <- SARC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC")]

SKCM_allData <- read_csv("finalDTData/SKCM.csv",show_col_types = FALSE)
SKCM <- SKCM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

STAD_allData <- read_csv("finalDTData/STAD.csv",show_col_types = FALSE)
STAD <- STAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

TGCT_allData <- read_csv("finalDTData/TGCT.csv",show_col_types = FALSE)
TGCT <- TGCT_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

THCA_allData <- read_csv("finalDTData/THCA.csv",show_col_types = FALSE)
THCA <- THCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

THYM_allData <- read_csv("finalDTData/THYM.csv",show_col_types = FALSE)
THYM <- THYM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

UCEC_allData <- read_csv("finalDTData/UCEC.csv",show_col_types = FALSE)
UCEC <- UCEC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

UCS_allData <- read_csv("finalDTData/UCS.csv",show_col_types = FALSE)
UCS <- UCS_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

UVM_allData <- read_csv("finalDTData/UVM.csv",show_col_types = FALSE)
UVM <- UVM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

####################################################################################################

concated <- data.frame(source=c(),target=c())

ACC_source_target <- read.table("networkData/ACC_source_target.csv", header = TRUE, sep = ";")
ACC_node_attr <- read.table("networkData/ACC_generalNetwork_default_node.csv", header = TRUE, sep = ",")

BLCA_source_target <- read.table("networkData/BLCA_source_target_new.csv", header=TRUE, sep = ";" )
BLCA_node_attr <- read.table("networkData/BLCA_generalNetwork_default_node.csv", header = T, sep = ",")

CESC_source_target <- read.table("networkData/CESC_source_target_info.csv", header=TRUE, sep = ";" )
CESC_node_attr <- read.table("networkData/CESCGeneralNetwork_default_node.csv", header = T, sep = ",")

CHOL_source_target <- read.table("networkData/CHOL_source_target.csv", header=TRUE, sep = ";" )
CHOL_node_attr <- read.table("networkData/CHOLGeneralNetwork_default_node.csv", header = T, sep = ",")

COAD_source_target <- read.table("networkData/COAD_source_target_info.csv", header=TRUE, sep = ";" )
COAD_node_attr <- read.table("networkData/COADNetwork_default_node.csv", header = T, sep = ",")

DLBC_source_target <- read.table("networkData/DLBC_source_target.csv", header=TRUE, sep = ";" )
DLBC_node_attr <- read.table("networkData/DLBC_generalNetwork_default_node.csv", header = T, sep = ",")

ESCA_source_target <- read.table("networkData/ESCA_source_target.csv", header=TRUE, sep = ";" )
ESCA_node_attr <- read.table("networkData/ESCA_generalNetwork_default_node.csv", header = T, sep = ",")

HNSC_source_target <- read.table("networkData/HNSC_source_target.csv", header=TRUE, sep = ";" )
HNSC_node_attr <- read.table("networkData/HNSC_generalNetwork_default_node.csv", header = T, sep = ",")

KICH_source_target <- read.table("networkData/KICH_source_target.csv", header=TRUE, sep = ";" )
KICH_node_attr <- read.table("networkData/KICH_generalNetwork_default_node.csv", header = T, sep = ",")

KIRC_source_target <- read.table("networkData/KIRC_source_target.csv", header=TRUE, sep = ";" )
KIRC_node_attr <- read.table("networkData/KIRCGeneralNetwork_default_node.csv", header = T, sep = ",")

KIRP_source_target <- read.table("networkData/KIRP_source_target.csv", header=TRUE, sep = ";" )
KIRP_node_attr <- read.table("networkData/KIRPGeneralNetwork_default_node.csv", header = T, sep = ",")

KICH_source_target <- read.table("networkData/KICH_source_target.csv", header=TRUE, sep = ";" )
KICH_node_attr <- read.table("networkData/KICH_generalNetwork_default_node.csv", header = T, sep = ",")

LGG_source_target <- read.table("networkData/LGG_source_target_info.csv", header=TRUE, sep = ";" )
LGG_node_attr <- read.table("networkData/LGG_GeneralNetwork_default_node.csv", header = T, sep = ",")

LIHC_source_target <- read.table("networkData/LIHC_source_target.csv", header=TRUE, sep = ";" )
LIHC_node_attr <- read.table("networkData/LIHC_generalNetwork_default_node.csv", header = T, sep = ",")

LUAD_source_target <- read.table("networkData/LUAD_source_target.csv", header=TRUE, sep = ";" )
LUAD_node_attr <- read.table("networkData/LUAD_generalNetwork_default_node.csv", header = T, sep = ",")

LUSC_source_target <- read.table("networkData/LUSC_source_target.csv", header=TRUE, sep = ";" )
LUSC_node_attr <- read.table("networkData/LUSC_generalNetwork_default_node.csv", header = T, sep = ",")

MESO_source_target <- read.table("networkData/MESO_source_target.csv", header=TRUE, sep = ";" )
MESO_node_attr <- read.table("networkData/MESO_generalNetwork_default_node.csv", header = T, sep = ",")

OV_source_target <- read.table("networkData/OV_source_target.csv", header=TRUE, sep = ";" )
OV_node_attr <- read.table("networkData/OV_generalNetwork_default_node.csv", header = T, sep = ",")

PAAD_source_target <- read.table("networkData/PAAD_source_target.csv", header=TRUE, sep = ";" )
PAAD_node_attr <- read.table("networkData/PAAD_generalNetwork_default_node.csv", header = T, sep = ",")

PCPG_source_target <- read.table("networkData/PCPG_source_target.csv", header=TRUE, sep = ";" )
PCPG_node_attr <- read.table("networkData/PCPG_generalNetwork_default_node.csv", header = T, sep = ",")

PRAD_source_target <- read.table("networkData/PRAD_source_target.csv", header = TRUE, sep = ";")
PRAD_node_attr <- read.table("networkData/PRADGeneralNetwork_default_node.csv", header = TRUE, sep = ",")

READ_source_target <- read.table("networkData/READ_source_target.csv", header=TRUE, sep = ";" )
READ_node_attr <- read.table("networkData/READ_generalNetwork_default_node.csv", header = T, sep = ",")

SARC_source_target <- read.table("networkData/SARC_source_target.csv", header=TRUE, sep = ";" )
SARC_node_attr <- read.table("networkData/SARC_generalNetwork_default_node.csv", header = T, sep = ",")

SKCM_source_target <- read.table("networkData/SKCM_source_target.csv", header=TRUE, sep = ";" )
SKCM_node_attr <- read.table("networkData/SKCM_generalNetwork_default_node.csv", header = T, sep = ",")

STAD_source_target <- read.table("networkData/STAD_source_target_info.csv", header=TRUE, sep = ";" )
STAD_node_attr <- read.table("networkData/STADNetwork_default_node.csv", header = T, sep = ",")

TGCT_source_target <- read.table("networkData/TGCT_source_target.csv", header=TRUE, sep = ";" )
TGCT_node_attr <- read.table("networkData/TGCT_generalNetwork_default_node.csv", header = T, sep = ",")

THCA_source_target <- read.table("networkData/THCA_source_target.csv", header=TRUE, sep = ";" )
THCA_node_attr <- read.table("networkData/THCA_generalNetwork_default_node.csv", header = T, sep = ",")

THYM_source_target <- read.table("networkData/THYM_source_target.csv", header=TRUE, sep = ";" )
THYM_node_attr <- read.table("networkData/THYM_generalNetwork_default_node.csv", header = T, sep = ",")

UCEC_source_target <- read.table("networkData/UCEC_source_target.csv", header=TRUE, sep = ";" )
UCEC_node_attr <- read.table("networkData/UCEC_generalNetwork_default_node.csv", header = T, sep = ",")

UCS_source_target <- read.table("networkData/UCS_source_target.csv", header=TRUE, sep = ";" )
UCS_node_attr <- read.table("networkData/UCS_generalNetwork_default_node.csv", header = TRUE, sep = ",")

UVM_source_target <- read.table("networkData/UVM_source_target.csv", header=TRUE, sep = ";" )
UVM_node_attr <- read.table("networkData/UVM_generalNetwork_default_node.csv", header = T, sep = ",")

####################################################################################################
BLCA_node_attr["significance"] <- ""
for (x in (1:nrow(BLCA_node_attr))){
  concatWhsa <- paste("hsa", BLCA_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(BLCA))){
    if(concatWhsa == BLCA$mirna1[y]){
      BLCA_node_attr$significance[x]=BLCA$miRNA1_pvalue[y]
    }
    else if(concatWhsa == BLCA$mirna2[y]){
      BLCA_node_attr$significance[x]=BLCA$miRNA2_pvalue[y]
    }
  }
}

CESC_node_attr["significance"] <- ""
for (x in (1:nrow(CESC_node_attr))){
  concatWhsa <- paste("hsa", CESC_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(CESC))){
    if(concatWhsa == CESC$mirna1[y]){
      CESC_node_attr$significance[x]=CESC$miRNA1_pvalue[y]
    }
    else if(concatWhsa == CESC$mirna2[y]){
      CESC_node_attr$significance[x]=CESC$miRNA2_pvalue[y]
    }
  }
}

CHOL_node_attr["significance"] <- ""
for (x in (1:nrow(CHOL_node_attr))){
  concatWhsa <- paste("hsa", CHOL_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(CHOL))){
    if(concatWhsa == CHOL$mirna1[y]){
      CHOL_node_attr$significance[x]=CHOL$miRNA1_pvalue[y]
    }
    else if(concatWhsa == CHOL$mirna2[y]){
      CHOL_node_attr$significance[x]=CHOL$miRNA2_pvalue[y]
    } 
  }
}


ESCA_node_attr["significance"] <- ""
for (x in (1:nrow(ESCA_node_attr))){
  concatWhsa <- paste("hsa", ESCA_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(ESCA))){
    if(concatWhsa == ESCA$mirna1[y]){
      ESCA_node_attr$significance[x]=ESCA$miRNA1_pvalue[y]
    }
    else if(concatWhsa == ESCA$mirna2[y]){
      ESCA_node_attr$significance[x]=ESCA$miRNA2_pvalue[y]
    } 
  }
}

HNSC_node_attr["significance"] <- ""
for (x in (1:nrow(HNSC_node_attr))){
  concatWhsa <- paste("hsa", HNSC_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(HNSC))){
    if(concatWhsa == HNSC$mirna1[y]){
      HNSC_node_attr$significance[x]=HNSC$miRNA1_pvalue[y]
    }
    else if(concatWhsa == HNSC$mirna2[y]){
      HNSC_node_attr$significance[x]=HNSC$miRNA2_pvalue[y]
    } 
  }
}

KICH_node_attr["significance"] <- ""
for (x in (1:nrow(KICH_node_attr))){
  concatWhsa <- paste("hsa", KICH_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(KICH))){
    if(concatWhsa == KICH$mirna1[y]){
      KICH_node_attr$significance[x]=KICH$miRNA1_pvalue[y]
    }
    else if(concatWhsa == KICH$mirna2[y]){
      KICH_node_attr$significance[x]=KICH$miRNA2_pvalue[y]
    } 
  }
}

KIRC_node_attr["significance"] <- ""
for (x in (1:nrow(KIRC_node_attr))){
  concatWhsa <- paste("hsa", KIRC_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(KIRC))){
    if(concatWhsa == KIRC$mirna1[y]){
      KIRC_node_attr$significance[x]=KIRC$miRNA1_pvalue[y]
    }
    else if(concatWhsa == KIRC$mirna2[y]){
      KIRC_node_attr$significance[x]=KIRC$miRNA2_pvalue[y]
    } 
  }
}

KIRP_node_attr["significance"] <- ""
for (x in (1:nrow(KIRP_node_attr))){
  concatWhsa <- paste("hsa", KIRP_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(KIRP))){
    if(concatWhsa == KIRP$mirna1[y]){
      KIRP_node_attr$significance[x]=KIRP$miRNA1_pvalue[y]
    }
    else if(concatWhsa == KIRP$mirna2[y]){
      KIRP_node_attr$significance[x]=KIRP$miRNA2_pvalue[y]
    } 
  }
}

LIHC_node_attr["significance"] <- ""
for (x in (1:nrow(LIHC_node_attr))){
  concatWhsa <- paste("hsa", LIHC_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(LIHC))){
    if(concatWhsa == LIHC$mirna1[y]){
      LIHC_node_attr$significance[x]=LIHC$miRNA1_pvalue[y]
    }
    else if(concatWhsa == LIHC$mirna2[y]){
      LIHC_node_attr$significance[x]=LIHC$miRNA2_pvalue[y]
    }
  }
}


LUAD_node_attr["significance"] <- ""
for (x in (1:nrow(LUAD_node_attr))){
  concatWhsa <- paste("hsa", LUAD_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(LUAD))){
    if(concatWhsa == LUAD$mirna1[y]){
      LUAD_node_attr$significance[x]=LUAD$miRNA1_pvalue[y]
    }
    else if(concatWhsa == LUAD$mirna2[y]){
      LUAD_node_attr$significance[x]=LUAD$miRNA2_pvalue[y]
    } 
  }
}


LUSC_node_attr["significance"] <- ""
for (x in (1:nrow(LUSC_node_attr))){
  concatWhsa <- paste("hsa", LUSC_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(LUSC))){
    if(concatWhsa == LUSC$mirna1[y]){
      LUSC_node_attr$significance[x]=LUSC$miRNA1_pvalue[y]
    }
    else if(concatWhsa == LUSC$mirna2[y]){
      LUSC_node_attr$significance[x]=LUSC$miRNA2_pvalue[y]
    } 
  }
}

PAAD_node_attr["significance"] <- ""
for (x in (1:nrow(PAAD_node_attr))){
  concatWhsa <- paste("hsa", PAAD_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(PAAD))){
    if(concatWhsa == PAAD$mirna1[y]){
      PAAD_node_attr$significance[x]=PAAD$miRNA1_pvalue[y]
    }
    else if(concatWhsa == PAAD$mirna2[y]){
      PAAD_node_attr$significance[x]=PAAD$miRNA2_pvalue[y]
    } 
  }
}

PCPG_node_attr["significance"] <- ""
for (x in (1:nrow(PCPG_node_attr))){
  concatWhsa <- paste("hsa", PCPG_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(PCPG))){
    if(concatWhsa == PCPG$mirna1[y]){
      PCPG_node_attr$significance[x]=PCPG$miRNA1_pvalue[y]
    }
    else if(concatWhsa == PCPG$mirna2[y]){
      PCPG_node_attr$significance[x]=PCPG$miRNA2_pvalue[y]
    } 
  }
}

PRAD_node_attr["significance"] <- ""
for (x in (1:nrow(PRAD_node_attr))){
  concatWhsa <- paste("hsa", PRAD_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(PRAD))){
    if(concatWhsa == PRAD$mirna1[y]){
      PRAD_node_attr$significance[x]=PRAD$miRNA1_pvalue[y]
    }
    else if(concatWhsa == PRAD$mirna2[y]){
      PRAD_node_attr$significance[x]=PRAD$miRNA2_pvalue[y]
    }
  }
}

SKCM_node_attr["significance"] <- ""
for (x in (1:nrow(SKCM_node_attr))){
  concatWhsa <- paste("hsa", SKCM_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(SKCM))){
    if(concatWhsa == SKCM$mirna1[y]){
      SKCM_node_attr$significance[x]=SKCM$miRNA1_pvalue[y]
    }
    else if(concatWhsa == SKCM$mirna2[y]){
      SKCM_node_attr$significance[x]=SKCM$miRNA2_pvalue[y]
    } 
  }
}

STAD_node_attr["significance"] <- ""
for (x in (1:nrow(STAD_node_attr))){
  concatWhsa <- paste("hsa", STAD_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(STAD))){
    if(concatWhsa == STAD$mirna1[y]){
      STAD_node_attr$significance[x]=STAD$miRNA1_pvalue[y]
    }
    else if(concatWhsa == STAD$mirna2[y]){
      STAD_node_attr$significance[x]=STAD$miRNA2_pvalue[y]
    } 
  }
}

THCA_node_attr["significance"] <- ""
for (x in (1:nrow(THCA_node_attr))){
  concatWhsa <- paste("hsa", THCA_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(THCA))){
    if(concatWhsa == THCA$mirna1[y]){
      THCA_node_attr$significance[x]=THCA$miRNA1_pvalue[y]
    }
    else if(concatWhsa == THCA$mirna2[y]){
      THCA_node_attr$significance[x]=THCA$miRNA2_pvalue[y]
    } 
  }
}

THYM_node_attr["significance"] <- ""
for (x in (1:nrow(THYM_node_attr))){
  concatWhsa <- paste("hsa", THYM_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(THYM))){
    if(concatWhsa == THYM$mirna1[y]){
      THYM_node_attr$significance[x]=THYM$miRNA1_pvalue[y]
    }
    else if(concatWhsa == THYM$mirna2[y]){
      THYM_node_attr$significance[x]=THYM$miRNA2_pvalue[y]
    } 
  }
}

UCEC_node_attr["significance"] <- ""
for (x in (1:nrow(UCEC_node_attr))){
  concatWhsa <- paste("hsa", UCEC_node_attr$shared.name[x],sep="-")
  for(y in (1:nrow(UCEC))){
    if(concatWhsa == UCEC$mirna1[y]){
      UCEC_node_attr$significance[x]=UCEC$miRNA1_pvalue[y]
    }
    else if(concatWhsa == UCEC$mirna2[y]){
      UCEC_node_attr$significance[x]=UCEC$miRNA2_pvalue[y]
    }
  }
}

####################################################################################################

