library(qusage)
#--------------------------------------------------------------------------------------
#' Build the standard input file for the stress signatures from Bryan Chambers
#'
#' @return No output.
#' @export
#--------------------------------------------------------------------------------------
signatureBuildStress <- function(){
  printCurrentFunction()

  file ="../input/Stress/complete.SRP.consigs-2021.02.17.gmt"
  temp = read.gmt(file)
  nsig = length(temp)
  name.list = c("signature","parent","source","type","direction","description","subsource","ngene","gene.list")
  mat = as.data.frame(matrix(nrow=nsig,ncol=length(name.list)))
  names(mat) = name.list
  for(i in 1:nsig) {
    sig = names(temp)[i]
    genes = temp[i][[1]]

    gene.list = paste(genes,collapse="|")
    mat[i,"signature"] = sig
    mat[i,"parent"] = sig
    mat[i,"source"] = "Bryant Stress Signatures"
    mat[i,"type"] = "nondirectional"
    mat[i,"direction"] ="nondirectional"
    mat[i,"description"] = "Bryant Stress Signatures"
    mat[i,"subsource"] = "Bryant Stress Signatures"
    mat[i,"ngene"] = length(genes)
    mat[i,"gene.list"] = gene.list
  }

  Stress_signatures = mat
  file = "../input/Stress/Stress_signatures.RData"
  save(Stress_signatures,file=file)
}
