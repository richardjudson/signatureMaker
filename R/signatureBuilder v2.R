library(openxlsx)
#--------------------------------------------------------------------------------------
#' Create the files needed for the signature calculations before adding random genes
#'
#' @param min.ngene Signatures will only be saved if the number of genes is >= this value
#' @param max.ngene Signatures will only be saved if the number of genes is <= this value
#' @return No output.
#' @export
#--------------------------------------------------------------------------------------
signatureBuilder = function(min.ngene=10,max.ngene=100000){
  printCurrentFunction()
  load("../input/MsigDB/MsigDB_signatures.RData")
  load("../input/Ryan/Ryan_signatures.RData")
  load("../input/Bioplanet/Bioplanet_signatures.RData")
  load("../input/CMAP/CMAP_signatures.RData")
  load("../input/DisGeNET/DisGeNET_signatures.RData")
  load("../input/Corton/Corton_signatures.RData")
  load("../input/Stress/Stress_signatures.RData")
  load("../input/Cho/Cho_signatures.RData")
  load("../input/Dorothea/Dorothea_signatures.RData")

  name.list <- c("signature","parent","source","subsource","type","direction","ngene","description","gene.list")

  sigdb <- rbind(Dorothea_signatures,Corton_signatures,Cho_signatures,Stress_signatures,Bioplanet_signatures,MsigDB_signatures,Ryan_signatures,CMAP_signatures)

  temp <- DisGeNET_signatures
  names(temp)[2] <- "source"
  temp$subsource <- temp$source
  temp$type <- "nondirectional"
  temp$direction <- "nondirectional"
  temp$parent <- temp$signature
  temp <- temp[,name.list]

  sigdb <- sigdb[,name.list]

  x <- temp$signature
  y <- sigdb$signature
  z <- x[is.element(x,y)]
  temp <- temp[!is.element(temp$signature,z),]
  sigdb <- rbind(sigdb,temp)
  rownames(sigdb) <- sigdb$signature

  genelists = strsplit(sigdb$gene.list, "\\|")
  print(length(genelists))
  names(genelists) = sigdb$signature

  catalog <- sigdb[,1:8]
  catalog$target_class <- "-"
  catalog$super_target <- "-"
  catalog$include0 <- 1
  catalog$set1 <- 0
  catalog$set2 <- 0
  catalog$set3 <- 0
  catalog$set4 <- 0
  catalog$set5 <- 0

  file <- "../input/CMAP/CMAP refchemdb output.xlsx"
  refchem <- read.xlsx(file)
  rownames(refchem) <- refchem$description
  refchem <- refchem[!is.na(refchem$target),]
  for(i in 1:nrow(refchem)) {
    desc <- refchem[i,"description"]
    target <- refchem[i,"target"]
    catalog[is.element(catalog$description,desc),"target_class"] <- target
  }

  sigdb <- sigdb[sigdb$ngene>=min.ngene,]
  sigdb <- sigdb[sigdb$ngene<=max.ngene,]
  catalog <- catalog[catalog$ngene>=min.ngene,]
  catalog <- catalog[catalog$ngene<=max.ngene,]
  genelists <- genelists[sigdb$signature]

  file = "../input/signatureDB_no_rand.RData"
  save(sigdb,file=file)
  file <- paste0("../input/signatureDB_genelists_no_rand.RData")
  save(genelists,file=file)
  file <- paste0("../input/signatureDB_master_catalog_no_rand.xlsx")
  write.xlsx(catalog,file)
}
