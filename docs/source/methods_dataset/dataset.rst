Dataset and Data Processing
====================================

.. _miRNATargets:
miRNA Targets
----------------------------
The experimentally validated miRNA targets were obtained from three different databases: 

  * `miRTarBase v8.0 <https://mirtarbase.cuhk.edu.cn/>`_
  * `TarBase v7.0 <https://dianalab.e-ce.uth.gr/html/diana/web/index.php?r=tarbasev8/index/>`_
  * `miRecords v4.0 <http://c1.accurascience.com/miRecords/>`_

The experimentally validated miRNA targets were then intersected with `TargetScan <https://www.targetscan.org/vert_80/>`_ in order to obtain the binding regions.

Expression data and Pre-processing
----------------------------

For each TCGA cancer project, we retrieved mRNA expression levels, Fragments per Kilobase of transcript per Million mapped reads upper quartile normalisation (FPKM-UQ), from `GDC Data Portal <https://portal.gdc.cancer.gov/>`_ . The noncoding genes in the RNA-Seq data were extracted using `GENCODE v37 <https://www.gencodegenes.org/>`_ annotations and only protein-coding transcripts were included in the analysis. We filtered genes that are lowly expressed in many samples; for this, genes that have expression values lower than 0.05 in more than 20% of the samples were removed from the analysis. We also removed genes with low variance across samples; we filtered genes with median absolute deviation (MAD) lower than 0.5. Expression values were added with a constant of 0.25 to deal with the 0 gene expression values and log2 transformed. log2-transformed mature miRNA expression data, reads per million mapped reads (RPM), were retrieved from  `FireBrowse <http://firebrowse.org/>`_ . We filtered miRNAs with NA values of more than 20% of the samples and miRNAs that do not vary across samples. For this, we filtered miRNAs with MAD below absolute 1. We only used matched Primary Tumor samples whose both mRNA and miRNA data are available.

