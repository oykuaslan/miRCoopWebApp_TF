library(readr)
library(sqldf)

mirnaCluster <- read_csv("~/mirCoopFinal/miRNAClusterandFamily/mirnaCluster.csv")
mirnaFamily <- read_delim("~/mirCoopFinal/miRNAClusterandFamily/miR_Family_Info.txt", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)

ACC <- read.table("networkData/ACC_generalNetwork_default_node.csv", header = TRUE, sep = ",")
BLCA <- read.table("networkData/BLCA_node_attr_significantmirnas.csv", header = T, sep = ",")
BRCA <- read.table("networkData/BRCA_general_network_default_node.csv", header = T, sep = ",")
CESC <- read.table("networkData/CESC_node_attr_significantmirnas.csv", header = T, sep = ",")
CHOL <- read.table("networkData/CHOL_node_attr_significantmirnas.csv", header = T, sep = ",")
COAD <- read.table("networkData/COADNetwork_default_node.csv", header = T, sep = ",")
DLBC <- read.table("networkData/DLBC_generalNetwork_default_node.csv", header = T, sep = ",")
ESCA <- read.table("networkData/ESCA_node_attr_significantmirnas.csv", header = T, sep = ",")
HNSC <- read.table("networkData/HNSC_node_attr_significantmirnas.csv", header = T, sep = ",")
KICH <- read.table("networkData/KICH_node_attr_significantmirnas.csv", header = T, sep = ",")
KIRC <- read.table("networkData/KIRC_node_attr_significantmirnas.csv", header = T, sep = ",")
KIRP <- read.table("networkData/KIRP_node_attr_significantmirnas.csv", header = T, sep = ",")
LIHC <- read.table("networkData/LIHC_node_attr_significantmirnas.csv", header = T, sep = ",")
LGG <- read.table("networkData/LGG_GeneralNetwork_default_node.csv", header = T, sep = ",")
LUAD <- read.table("networkData/LUAD_node_attr_significantmirnas.csv", header = T, sep = ",")
LUSC <- read.table("networkData/LUSC_node_attr_significantmirnas.csv", header = T, sep = ",")
MESO <- read.table("networkData/MESO_generalNetwork_default_node.csv", header = T, sep = ",")
OV <- read.table("networkData/OV_generalNetwork_default_node.csv", header = T, sep = ",")
PAAD <- read.table("networkData/PAAD_node_attr_significantmirnas.csv", header = T, sep = ",")
PCPG <- read.table("networkData/PCPG_node_attr_significantmirnas.csv", header = T, sep = ",")
PRAD <- read.table("networkData/PRAD_node_attr_significantmirnas.csv", header = TRUE, sep = ",")
READ <- read.table("networkData/READ_generalNetwork_default_node.csv", header = T, sep = ",")
SARC <- read.table("networkData/SARC_generalNetwork_default_node.csv", header = T, sep = ",")
SKCM <- read.table("networkData/SKCM_node_attr_significantmirnas.csv", header = T, sep = ",")
STAD <- read.table("networkData/STAD_node_attr_significantmirnas.csv", header = T, sep = ",")
TGCT <- read.table("networkData/TGCT_generalNetwork_default_node.csv", header = T, sep = ",")
THCA <- read.table("networkData/THCA_node_attr_significantmirnas.csv", header = T, sep = ",")
THYM <- read.table("networkData/THYM_node_attr_significantmirnas.csv", header = T, sep = ",")
UCEC<- read.table("networkData/UCEC_node_attr_significantmirnas.csv", header = T, sep = ",")
UCS <- read.table("networkData/UCS_generalNetwork_default_node.csv", header = TRUE, sep = ",")
UVM <- read.table("networkData/UVM_generalNetwork_default_node.csv", header = T, sep = ",")



ACCFamily <- sqldf::sqldf("SELECT ACC.*, mirnaFamily.miRFamily from ACC left join mirnaFamily on 'hsa-' || lower(ACC.name) = lower(mirnaFamily.MiRBaseID)")
ACCCluster <- sqldf::sqldf("SELECT ACCFamily.*, mirnaCluster.clusterString as mirnaCluster from ACCFamily left join mirnaCluster on 'hsa-' || lower(ACCFamily.name) = lower(mirnaCluster.mirna)")
write.csv(ACCCluster,"networkDataWClusterFamily/ACC.csv",row.names = FALSE)

BLCAFamily <- sqldf::sqldf("SELECT BLCA.*, mirnaFamily.miRFamily from BLCA left join mirnaFamily on 'hsa-' || lower(BLCA.name) = lower(mirnaFamily.MiRBaseID)")
BLCACluster <- sqldf::sqldf("SELECT BLCAFamily.*, mirnaCluster.clusterString as mirnaCluster from BLCAFamily left join mirnaCluster on 'hsa-' || lower(BLCAFamily.name) = lower(mirnaCluster.mirna)")
write.csv(BLCACluster,"networkDataWClusterFamily/BLCA.csv",row.names = FALSE)

BRCAFamily <- sqldf::sqldf("SELECT BRCA.*, mirnaFamily.miRFamily from BRCA left join mirnaFamily on 'hsa-' || lower(BRCA.name) = lower(mirnaFamily.MiRBaseID)")
BRCACluster <- sqldf::sqldf("SELECT BRCAFamily.*, mirnaCluster.clusterString as mirnaCluster from BRCAFamily left join mirnaCluster on 'hsa-' || lower(BRCAFamily.name) = lower(mirnaCluster.mirna)")
write.csv(BRCACluster,"networkDataWClusterFamily/BRCA.csv",row.names = FALSE)

CESCFamily <- sqldf::sqldf("SELECT CESC.*, mirnaFamily.miRFamily from CESC left join mirnaFamily on 'hsa-' || lower(CESC.name) = lower(mirnaFamily.MiRBaseID)")
CESCCluster <- sqldf::sqldf("SELECT CESCFamily.*, mirnaCluster.clusterString as mirnaCluster from CESCFamily left join mirnaCluster on 'hsa-' || lower(CESCFamily.name) = lower(mirnaCluster.mirna)")
write.csv(CESCCluster,"networkDataWClusterFamily/CESC.csv",row.names = FALSE)

CHOLFamily <- sqldf::sqldf("SELECT CHOL.*, mirnaFamily.miRFamily from CHOL left join mirnaFamily on 'hsa-' || lower(CHOL.name) = lower(mirnaFamily.MiRBaseID)")
CHOLCluster <- sqldf::sqldf("SELECT CHOLFamily.*, mirnaCluster.clusterString as mirnaCluster from CHOLFamily left join mirnaCluster on 'hsa-' || lower(CHOLFamily.name) = lower(mirnaCluster.mirna)")
write.csv(CHOLCluster,"networkDataWClusterFamily/CHOL.csv",row.names = FALSE)

COADFamily <- sqldf::sqldf("SELECT COAD.*, mirnaFamily.miRFamily from COAD left join mirnaFamily on 'hsa-' || lower(COAD.name) = lower(mirnaFamily.MiRBaseID)")
COADCluster <- sqldf::sqldf("SELECT COADFamily.*, mirnaCluster.clusterString as mirnaCluster from COADFamily left join mirnaCluster on 'hsa-' || lower(COADFamily.name) = lower(mirnaCluster.mirna)")
write.csv(COADCluster,"networkDataWClusterFamily/COAD.csv",row.names = FALSE)

DLBCFamily <- sqldf::sqldf("SELECT DLBC.*, mirnaFamily.miRFamily from DLBC left join mirnaFamily on 'hsa-' || lower(DLBC.name) = lower(mirnaFamily.MiRBaseID)")
DLBCCluster <- sqldf::sqldf("SELECT DLBCFamily.*, mirnaCluster.clusterString as mirnaCluster from DLBCFamily left join mirnaCluster on 'hsa-' || lower(DLBCFamily.name) = lower(mirnaCluster.mirna)")
write.csv(DLBCCluster,"networkDataWClusterFamily/DLBC.csv",row.names = FALSE)

ESCAFamily <- sqldf::sqldf("SELECT ESCA.*, mirnaFamily.miRFamily from ESCA left join mirnaFamily on 'hsa-' || lower(ESCA.name) = lower(mirnaFamily.MiRBaseID)")
ESCACluster <- sqldf::sqldf("SELECT ESCAFamily.*, mirnaCluster.clusterString as mirnaCluster from ESCAFamily left join mirnaCluster on 'hsa-' || lower(ESCAFamily.name) = lower(mirnaCluster.mirna)")
write.csv(ESCACluster,"networkDataWClusterFamily/ESCA.csv",row.names = FALSE)

HNSCFamily <- sqldf::sqldf("SELECT HNSC.*, mirnaFamily.miRFamily from HNSC left join mirnaFamily on 'hsa-' || lower(HNSC.name) = lower(mirnaFamily.MiRBaseID)")
HNSCCluster <- sqldf::sqldf("SELECT HNSCFamily.*, mirnaCluster.clusterString as mirnaCluster from HNSCFamily left join mirnaCluster on 'hsa-' || lower(HNSCFamily.name) = lower(mirnaCluster.mirna)")
write.csv(HNSCCluster,"networkDataWClusterFamily/HNSC.csv",row.names = FALSE)

KICHFamily <- sqldf::sqldf("SELECT KICH.*, mirnaFamily.miRFamily from KICH left join mirnaFamily on 'hsa-' || lower(KICH.name) = lower(mirnaFamily.MiRBaseID)")
KICHCluster <- sqldf::sqldf("SELECT KICHFamily.*, mirnaCluster.clusterString as mirnaCluster from KICHFamily left join mirnaCluster on 'hsa-' || lower(KICHFamily.name) = lower(mirnaCluster.mirna)")
write.csv(KICHCluster,"networkDataWClusterFamily/KICH.csv",row.names = FALSE)

KIRCFamily <- sqldf::sqldf("SELECT KIRC.*, mirnaFamily.miRFamily from KIRC left join mirnaFamily on 'hsa-' || lower(KIRC.name) = lower(mirnaFamily.MiRBaseID)")
KIRCCluster <- sqldf::sqldf("SELECT KIRCFamily.*, mirnaCluster.clusterString as mirnaCluster from KIRCFamily left join mirnaCluster on 'hsa-' || lower(KIRCFamily.name) = lower(mirnaCluster.mirna)")
write.csv(KIRCCluster,"networkDataWClusterFamily/KIRC.csv",row.names = FALSE)

KIRPFamily <- sqldf::sqldf("SELECT KIRP.*, mirnaFamily.miRFamily from KIRP left join mirnaFamily on 'hsa-' || lower(KIRP.name) = lower(mirnaFamily.MiRBaseID)")
KIRPCluster <- sqldf::sqldf("SELECT KIRPFamily.*, mirnaCluster.clusterString as mirnaCluster from KIRPFamily left join mirnaCluster on 'hsa-' || lower(KIRPFamily.name) = lower(mirnaCluster.mirna)")
write.csv(KIRPCluster,"networkDataWClusterFamily/KIRP.csv",row.names = FALSE)

LGGFamily <- sqldf::sqldf("SELECT LGG.*, mirnaFamily.miRFamily from LGG left join mirnaFamily on 'hsa-' || lower(LGG.name) = lower(mirnaFamily.MiRBaseID)")
LGGCluster <- sqldf::sqldf("SELECT LGGFamily.*, mirnaCluster.clusterString as mirnaCluster from LGGFamily left join mirnaCluster on 'hsa-' || lower(LGGFamily.name) = lower(mirnaCluster.mirna)")
write.csv(LGGCluster,"networkDataWClusterFamily/LGG.csv",row.names = FALSE)

LIHCFamily <- sqldf::sqldf("SELECT LIHC.*, mirnaFamily.miRFamily from LIHC left join mirnaFamily on 'hsa-' || lower(LIHC.name) = lower(mirnaFamily.MiRBaseID)")
LIHCCluster <- sqldf::sqldf("SELECT LIHCFamily.*, mirnaCluster.clusterString as mirnaCluster from LIHCFamily left join mirnaCluster on 'hsa-' || lower(LIHCFamily.name) = lower(mirnaCluster.mirna)")
write.csv(LIHCCluster,"networkDataWClusterFamily/LIHC.csv",row.names = FALSE)

LUADFamily <- sqldf::sqldf("SELECT LUAD.*, mirnaFamily.miRFamily from LUAD left join mirnaFamily on 'hsa-' || lower(LUAD.name) = lower(mirnaFamily.MiRBaseID)")
LUADCluster <- sqldf::sqldf("SELECT LUADFamily.*, mirnaCluster.clusterString as mirnaCluster from LUADFamily left join mirnaCluster on 'hsa-' || lower(LUADFamily.name) = lower(mirnaCluster.mirna)")
write.csv(LUADCluster,"networkDataWClusterFamily/LUAD.csv",row.names = FALSE)

LUSCFamily <- sqldf::sqldf("SELECT LUSC.*, mirnaFamily.miRFamily from LUSC left join mirnaFamily on 'hsa-' || lower(LUSC.name) = lower(mirnaFamily.MiRBaseID)")
LUSCCluster <- sqldf::sqldf("SELECT LUSCFamily.*, mirnaCluster.clusterString as mirnaCluster from LUSCFamily left join mirnaCluster on 'hsa-' || lower(LUSCFamily.name) = lower(mirnaCluster.mirna)")
write.csv(LUSCCluster,"networkDataWClusterFamily/LUSC.csv",row.names = FALSE)

MESOFamily <- sqldf::sqldf("SELECT MESO.*, mirnaFamily.miRFamily from MESO left join mirnaFamily on 'hsa-' || lower(MESO.name) = lower(mirnaFamily.MiRBaseID)")
MESOCluster <- sqldf::sqldf("SELECT MESOFamily.*, mirnaCluster.clusterString as mirnaCluster from MESOFamily left join mirnaCluster on 'hsa-' || lower(MESOFamily.name) = lower(mirnaCluster.mirna)")
write.csv(MESOCluster,"networkDataWClusterFamily/MESO.csv",row.names = FALSE)

OVFamily <- sqldf::sqldf("SELECT OV.*, mirnaFamily.miRFamily from OV left join mirnaFamily on 'hsa-' || lower(OV.name) = lower(mirnaFamily.MiRBaseID)")
OVCluster <- sqldf::sqldf("SELECT OVFamily.*, mirnaCluster.clusterString as mirnaCluster from OVFamily left join mirnaCluster on 'hsa-' || lower(OVFamily.name) = lower(mirnaCluster.mirna)")
write.csv(OVCluster,"networkDataWClusterFamily/OV.csv",row.names = FALSE)

PAADFamily <- sqldf::sqldf("SELECT PAAD.*, mirnaFamily.miRFamily from PAAD left join mirnaFamily on 'hsa-' || lower(PAAD.name) = lower(mirnaFamily.MiRBaseID)")
PAADCluster <- sqldf::sqldf("SELECT PAADFamily.*, mirnaCluster.clusterString as mirnaCluster from PAADFamily left join mirnaCluster on 'hsa-' || lower(PAADFamily.name) = lower(mirnaCluster.mirna)")
write.csv(PAADCluster,"networkDataWClusterFamily/PAAD.csv",row.names = FALSE)

PCPGFamily <- sqldf::sqldf("SELECT PCPG.*, mirnaFamily.miRFamily from PCPG left join mirnaFamily on 'hsa-' || lower(PCPG.name) = lower(mirnaFamily.MiRBaseID)")
PCPGCluster <- sqldf::sqldf("SELECT PCPGFamily.*, mirnaCluster.clusterString as mirnaCluster from PCPGFamily left join mirnaCluster on 'hsa-' || lower(PCPGFamily.name) = lower(mirnaCluster.mirna)")
write.csv(PCPGCluster,"networkDataWClusterFamily/PCPG.csv",row.names = FALSE)

PRADFamily <- sqldf::sqldf("SELECT PRAD.*, mirnaFamily.miRFamily from PRAD left join mirnaFamily on 'hsa-' || lower(PRAD.name) = lower(mirnaFamily.MiRBaseID)")
PRADCluster <- sqldf::sqldf("SELECT PRADFamily.*, mirnaCluster.clusterString as mirnaCluster from PRADFamily left join mirnaCluster on 'hsa-' || lower(PRADFamily.name) = lower(mirnaCluster.mirna)")
write.csv(PRADCluster,"networkDataWClusterFamily/PRAD.csv",row.names = FALSE)

READFamily <- sqldf::sqldf("SELECT READ.*, mirnaFamily.miRFamily from READ left join mirnaFamily on 'hsa-' || lower(READ.name) = lower(mirnaFamily.MiRBaseID)")
READCluster <- sqldf::sqldf("SELECT READFamily.*, mirnaCluster.clusterString as mirnaCluster from READFamily left join mirnaCluster on 'hsa-' || lower(READFamily.name) = lower(mirnaCluster.mirna)")
write.csv(READCluster,"networkDataWClusterFamily/READ.csv",row.names = FALSE)

SARCFamily <- sqldf::sqldf("SELECT SARC.*, mirnaFamily.miRFamily from SARC left join mirnaFamily on 'hsa-' || lower(SARC.name) = lower(mirnaFamily.MiRBaseID)")
SARCCluster <- sqldf::sqldf("SELECT SARCFamily.*, mirnaCluster.clusterString as mirnaCluster from SARCFamily left join mirnaCluster on 'hsa-' || lower(SARCFamily.name) = lower(mirnaCluster.mirna)")
write.csv(SARCCluster,"networkDataWClusterFamily/SARC.csv",row.names = FALSE)

SKCMFamily <- sqldf::sqldf("SELECT SKCM.*, mirnaFamily.miRFamily from SKCM left join mirnaFamily on 'hsa-' || lower(SKCM.name) = lower(mirnaFamily.MiRBaseID)")
SKCMCluster <- sqldf::sqldf("SELECT SKCMFamily.*, mirnaCluster.clusterString as mirnaCluster from SKCMFamily left join mirnaCluster on 'hsa-' || lower(SKCMFamily.name) = lower(mirnaCluster.mirna)")
write.csv(SKCMCluster,"networkDataWClusterFamily/SKCM.csv",row.names = FALSE)

STADFamily <- sqldf::sqldf("SELECT STAD.*, mirnaFamily.miRFamily from STAD left join mirnaFamily on 'hsa-' || lower(STAD.name) = lower(mirnaFamily.MiRBaseID)")
STADCluster <- sqldf::sqldf("SELECT STADFamily.*, mirnaCluster.clusterString as mirnaCluster from STADFamily left join mirnaCluster on 'hsa-' || lower(STADFamily.name) = lower(mirnaCluster.mirna)")
write.csv(STADCluster,"networkDataWClusterFamily/STAD.csv",row.names = FALSE)

TGCTFamily <- sqldf::sqldf("SELECT TGCT.*, mirnaFamily.miRFamily from TGCT left join mirnaFamily on 'hsa-' || lower(TGCT.name) = lower(mirnaFamily.MiRBaseID)")
TGCTCluster <- sqldf::sqldf("SELECT TGCTFamily.*, mirnaCluster.clusterString as mirnaCluster from TGCTFamily left join mirnaCluster on 'hsa-' || lower(TGCTFamily.name) = lower(mirnaCluster.mirna)")
write.csv(TGCTCluster,"networkDataWClusterFamily/TGCT.csv",row.names = FALSE)

THCAFamily <- sqldf::sqldf("SELECT THCA.*, mirnaFamily.miRFamily from THCA left join mirnaFamily on 'hsa-' || lower(THCA.name) = lower(mirnaFamily.MiRBaseID)")
THCACluster <- sqldf::sqldf("SELECT THCAFamily.*, mirnaCluster.clusterString as mirnaCluster from THCAFamily left join mirnaCluster on 'hsa-' || lower(THCAFamily.name) = lower(mirnaCluster.mirna)")
write.csv(THCACluster,"networkDataWClusterFamily/THCA.csv",row.names = FALSE)

THYMFamily <- sqldf::sqldf("SELECT THYM.*, mirnaFamily.miRFamily from THYM left join mirnaFamily on 'hsa-' || lower(THYM.name) = lower(mirnaFamily.MiRBaseID)")
THYMCluster <- sqldf::sqldf("SELECT THYMFamily.*, mirnaCluster.clusterString as mirnaCluster from THYMFamily left join mirnaCluster on 'hsa-' || lower(THYMFamily.name) = lower(mirnaCluster.mirna)")
write.csv(THYMCluster,"networkDataWClusterFamily/THYM.csv",row.names = FALSE)

UCECFamily <- sqldf::sqldf("SELECT UCEC.*, mirnaFamily.miRFamily from UCEC left join mirnaFamily on 'hsa-' || lower(UCEC.name) = lower(mirnaFamily.MiRBaseID)")
UCECCluster <- sqldf::sqldf("SELECT UCECFamily.*, mirnaCluster.clusterString as mirnaCluster from UCECFamily left join mirnaCluster on 'hsa-' || lower(UCECFamily.name) = lower(mirnaCluster.mirna)")
write.csv(UCECCluster,"networkDataWClusterFamily/UCEC.csv",row.names = FALSE)

UCSFamily <- sqldf::sqldf("SELECT UCS.*, mirnaFamily.miRFamily from UCS left join mirnaFamily on 'hsa-' || lower(UCS.name) = lower(mirnaFamily.MiRBaseID)")
UCSCluster <- sqldf::sqldf("SELECT UCSFamily.*, mirnaCluster.clusterString as mirnaCluster from UCSFamily left join mirnaCluster on 'hsa-' || lower(UCSFamily.name) = lower(mirnaCluster.mirna)")
write.csv(UCSCluster,"networkDataWClusterFamily/UCS.csv",row.names = FALSE)

UVMFamily <- sqldf::sqldf("SELECT UVM.*, mirnaFamily.miRFamily from UVM left join mirnaFamily on 'hsa-' || lower(UVM.name) = lower(mirnaFamily.MiRBaseID)")
UVMCluster <- sqldf::sqldf("SELECT UVMFamily.*, mirnaCluster.clusterString as mirnaCluster from UVMFamily left join mirnaCluster on 'hsa-' || lower(UVMFamily.name) = lower(mirnaCluster.mirna)")
write.csv(UVMCluster,"networkDataWClusterFamily/UVM.csv",row.names = FALSE)
