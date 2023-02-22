BiocManager::install("biomaRt")

library(biomaRt)
library(readr)

#Triplets <- read_delim(file.choose(), delim = ",", escape_double = FALSE, trim_ws = TRUE)
Triplets <-  read_csv("TF_Type3Results/READ_Type3Pval001_OrderedMirnaPair.csv")
tf <- Triplets$TF
target <- Triplets$Target
ensembl <- useEnsembl(biomart = "ensembl", dataset = "hsapiens_gene_ensembl")

#listEnsembl() #if you're working with other organism like mouse or snps etc
#listFilters(ensembl) #shows possible filters (input)
#listAttributes(ensembl) #shows Attributes #It will give these conversions (output)


geneIDconversion_TF <- getBM(attributes = c("entrezgene_id", 'hgnc_symbol'), filters = "entrezgene_id",
      mart = ensembl, values = tf) #sometimes biomaRT is busy and connects to Asia mirror.

geneIDconversion_TF = geneIDconversion_TF[!geneIDconversion_TF$hgnc_symbol == "",] #it gives empty symbols stimes
geneIDconversion_TF[which(duplicated(geneIDconversion_TF$entrezgene_id)),]#check if more than one symbol for the same Entrez ID
geneIDconversion_TF$hgnc_symbol_tf <- geneIDconversion_TF$hgnc_symbol


Triplets_withSymbols_TF = merge(Triplets, geneIDconversion_TF, by.x = "TF", by.y = "entrezgene_id")


############################

geneIDconversion_Target <- getBM(attributes = c("entrezgene_id", 'hgnc_symbol'), filters = "entrezgene_id",
                             mart = ensembl, values = target) #sometimes biomaRT is busy and connects to Asia mirror.


geneIDconversion_Target = geneIDconversion_Target[!geneIDconversion_Target$hgnc_symbol == "",] #it gives empty symbols stimes
geneIDconversion_Target[which(duplicated(geneIDconversion_Target$entrezgene_id)),]#check if more than one symbol for the same Entrez ID
geneIDconversion_Target$hgnc_symbol_target <- geneIDconversion_Target$hgnc_symbol

Triplets_withSymbols_Target = merge(Triplets_withSymbols_TF, geneIDconversion_Target, by.x = "Target", by.y = "entrezgene_id")


##############################################

final <- Triplets_withSymbols_Target[,c("miRNA1", "miRNA2", "TF","hgnc_symbol_tf" ,"Target","hgnc_symbol_target","ModeOfRegulation","TwoVar_mirna1_tf","TwoVar_mirna2_tf","mirna1mirna2_tf","TwoVar_mirna1_target","TwoVar_mirna2_target","mirna1mirna2_target")]

write.csv(final, "TF_Type3Results/READ_Type3Pval001_OderedMirnaPair_withNames.csv", row.names =FALSE)




#####################################################
# Another way for conversion (for Ensembl IDs) - shows pseudogenes, lncs.

BiocManager::install("EnsDb.Hsapiens.v86") #doesn't work with ensembl gene ID wersion
library(EnsDb.Hsapiens.v86)

genes2 <- gsub("\\..*","", genes) #removes the .X version info.

#Convert from ensembl.gene to gene.symbol
ensembl.genes <- genes2
ensembl.genes

geneIDs1 <- ensembldb::select(EnsDb.Hsapiens.v86, keys= ensembl.genes, keytype = "GENEID", columns = c("SYMBOL","GENEID"))




