#--------------------------------------------------------------------------------------
#' Build the standard input file for the Corton signatures
#'
#' @return No output.
#' @export
#--------------------------------------------------------------------------------------
signatureBuildCorton <- function(){
  printCurrentFunction()

  file ="data/Corton/ER_Biomarker_TempoSeq_Corton.xlsx"
  temp = read.xlsx(file)
  name.list = c("signature","parent","source","type","direction","description","subsource","ngene","gene.list")
  mat = as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(mat) = name.list
  genes = temp[,1]
  gene.list = paste(genes,collapse="|")
  mat[1,"signature"] <- "Corton Estrogen Receptor Signature"
  mat[1,"parent"] <- "Corton Estrogen Receptor Signature"
  mat[1,"source"] <- "Corton"
  mat[1,"type"] <- "nondirectional"
  mat[1,"direction"] <- "nondirectional"
  mat[1,"description"] <- "Bioset 1: ER_Biomarker_For_TempoSeq (Study: ER Biomarker for TempoSeq)"
  mat[1,"subsource"] <- "Corton"
  mat[1,"ngene"] <- length(genes)
  mat[1,"gene.list"] <- gene.list

  Corton_signatures = mat
  file = "data/Corton/Corton_signatures.RData"
  save(Corton_signatures,file=file)
}
