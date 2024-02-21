library(qusage)
#--------------------------------------------------------------------------------------
#' Build the standard input file for the stress signatures from Bryan Chambers
#'
#' @return No output.
#' @export
#--------------------------------------------------------------------------------------
signatureBuildCho <- function(){
  printCurrentFunction()

  file ="../input/Cho/Cho DNA Damage Biomarker.xlsx"
  temp = read.xlsx(file)
  genes = temp[,1]
  nsig = length(temp)
  name.list = c("signature","parent","source","type","direction","description","subsource","ngene","gene.list")
  mat = as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(mat) = name.list
  gene.list = paste(genes,collapse="|")
  mat[1,"signature"] = "Cho DNA Damage Biomarker"
  mat[1,"parent"] = "Cho DNA Damage Biomarker"
  mat[1,"source"] = "Cho et al EMM 2018"
  mat[1,"type"] = "nondirectional"
  mat[1,"direction"] ="nondirectional"
  mat[1,"description"] = "Cho DNA Damage Biomarker"
  mat[1,"subsource"] = "Cho DNA Damage Biomarker"
  mat[1,"ngene"] = length(genes)
  mat[1,"gene.list"] = gene.list

  Cho_signatures = mat
  file = "../input/Cho/Cho_signatures.RData"
  save(Cho_signatures,file=file)
}
