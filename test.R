######### R upgrade
install.packages("installr")
require(installr)
updateR()

#################### Bioconductor
source("https://bioconductor.org/biocLite.R")
biocLite("TCGAbiolinks")
biocLite("limma")
biocLite("TCGAWorkflowData")
biocLite("DT")
biocLite("Rcpp")
biocLite()
library(TCGAbiolinks)
source("https://bioconductor.org/biocLite.R")
deps <- c("pathview","clusterProfiler","ELMER", "DO.db","GO.db", 
          "ComplexHeatmap","EDASeq", "TCGAbiolinks","AnnotationHub",
          "gaia","ChIPseeker","minet","BSgenome.Hsapiens.UCSC.hg19",
          "MotifDb","MotIV", "rGADEM", "motifStack","RTCGAToolbox")
for(pkg in deps)  if (!pkg %in% installed.packages()) biocLite(pkg, dependencies = TRUE)
deps <- c("devtools","DT","pbapply","readr","circlize")
for(pkg in deps)  if (!pkg %in% installed.packages())  install.packages(pkg,dependencies = TRUE)
devtools::install_github("BioinformaticsFMRP/TCGAWorkflowData")
devtools::install_github("BioinformaticsFMRP/TCGAWorkflow", dependencies = TRUE)
library(TCGAWorkflowData)
library(DT)