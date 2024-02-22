library(openxlsx)
#--------------------------------------------------------------------------------------
#' Create the files needed for the signature calculations before adding random genes
#'
#' @param min.ngene Signatures will only be saved if the number of genes is >= this value
#' @param max.ngene Signatures will only be saved if the number of genes is <= this value
#' @return No output.
#' @export
#--------------------------------------------------------------------------------------
signatureBuilder.v2 = function(min.ngene=10,max.ngene=100000){
  printCurrentFunction()

  load("data/MsigDB/MsigDB_signatures.RData")
  load("data/Ryan/Ryan_signatures.RData")
  load("data/Bioplanet/Bioplanet_signatures.RData")
  load("data/CMAP/CMAP_signatures.RData")
  #load("data/DisGeNET/DisGeNET_signatures.RData")
  load("data/Corton/Corton_signatures.RData")
  load("data/Stress/Stress_signatures.RData")
  load("data/Cho/Cho_signatures.RData")
  load("data/Dorothea/Dorothea_signatures.RData")

  name.list <- c("signature","parent","source","subsource","type","direction","ngene","description","gene.list")

  sigdb <- rbind(Dorothea_signatures,Corton_signatures,Cho_signatures,Stress_signatures,Bioplanet_signatures,MsigDB_signatures,Ryan_signatures,CMAP_signatures)

  # temp <- DisGeNET_signatures
  # names(temp)[2] <- "source"
  # temp$subsource <- temp$source
  # temp$type <- "nondirectional"
  # temp$direction <- "nondirectional"
  # temp$parent <- temp$signature
  # temp <- temp[,name.list]

  sigdb <- sigdb[,name.list]

  # x <- temp$signature
  # y <- sigdb$signature
  # z <- x[is.element(x,y)]
  # temp <- temp[!is.element(temp$signature,z),]
  # sigdb <- rbind(sigdb,temp)
  rownames(sigdb) <- sigdb$signature

  genelists = strsplit(sigdb$gene.list, "\\|")
  print(length(genelists))
  names(genelists) = sigdb$signature

  catalog <- sigdb[,1:8]
  catalog$target_class <- "-"
  catalog$super_target <- "-"
  catalog$gene_target <- "-"
  catalog$effect_direction <- "-"
  catalog$super_target_level <- "-"

  catalog$include0 <- 1
  catalog$set1 <- 0
  catalog$set2 <- 0
  catalog$set3 <- 0
  catalog$set4 <- 0
  catalog$set5 <- 0

  file <- "data/signatureDB manual annotations.xlsx"
  annot <- read.xlsx(file)
  x = annot$parent
  y = x[duplicated(x)]
  if(length(y)>0) {
    cat("Duplicated entries in the manual annotation file need to be eliminated\n")
    print(y)
    browser()
  }
  rownames(annot) = annot$parent
  for(i in 1:nrow(annot)) {
    parent = annot[i,"parent"]
    catalog[catalog$parent==parent,"super_target"] = annot[i,"super_target"]
    catalog[catalog$parent==parent,"gene_target"] = annot[i,"gene_target"]
    catalog[catalog$parent==parent,"effect_direction"] = annot[i,"effect_direction"]
    catalog[catalog$parent==parent,"super_target_level"] = annot[i,"super_target_level"]
  }

  file <- "data/CMAP/CMAP refchemdb output.xlsx"
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

  file = "data/signatureDB_no_rand.RData"
  save(sigdb,file=file)
  file <- paste0("data/signatureDB_genelists_no_rand.RData")
  save(genelists,file=file)
  file <- paste0("data/signatureDB_master_catalog_no_rand.xlsx")
  write.xlsx(catalog,file)
}
