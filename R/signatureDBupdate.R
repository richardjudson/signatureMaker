library(openxlsx)
#--------------------------------------------------------------------------------------
#' Create the files needed for the signature calculations before adding random genes
#'
#' @param min.ngene Signatures will only be saved if the number of genes is >= this value
#' @param max.ngene Signatures will only be saved if the number of genes is <= this value
#' @return No output.
#' @export
#--------------------------------------------------------------------------------------
signatureDBupdate = function(do.save=F){
  printCurrentFunction()

  file = "../input/signatureDB.RData"
  load(file=file)
  sigdb.old = sigdb
  cat("old signatures (1):",nrow(sigdb.old),"\n")
  file = paste0("../input/saved_versions/signatureDB_",Sys.Date(),".RData")
  if(do.save) save(sigdb,file=file)

  file <- paste0("../input/signatureDB_genelists.RData")
  load(file=file)
  genelists.old = genelists
  cat("old signatures (2):",length(genelists.old),"\n")
  file = paste0("../input/saved_versions/signatureDB_genelists_",Sys.Date(),".RData")
  if(do.save) save(genelists,file=file)

  file = "../input/signatureDB_no_rand.RData"
  load(file=file)
  sigdb.new = sigdb
  cat("new signatures (1):",nrow(sigdb.new),"\n")

  file <- paste0("../input/signatureDB_genelists_no_rand.RData")
  load(file=file)
  genelists.new = genelists
  cat("new signatures (2):",length(genelists.new),"\n")

  delta = genelists.new[!is.element(genelists.new,genelists.old)]
  genelists = c(genelists.old,delta)
  cat("final signatures (1):",length(genelists),"\n")
  file = paste0("../input/signatureDB_genelists.RData")
  if(do.save) save(genelists,file=file)

  delta = sigdb.new[!is.element(sigdb.new$signature,sigdb.old$signature),]
  sigdb = rbind(sigdb.old,delta)
  cat("final signatures (2):",nrow(sigdb),"\n")
  file = paste0("../input/signatureDB.RData")
  if(do.save) save(sigdb,file=file)
  else browser()
}
