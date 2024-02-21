#--------------------------------------------------------------------------------------
#' Build the standard input file for the Dorothea signatures
#'
#' @return No output.
#' @export
#--------------------------------------------------------------------------------------
signatureBuildDorothea <- function(){
  printCurrentFunction()

  file ="data/Dorothea/Dorothea.RData"
  load(file=file)
  temp = DOROTHEA
  temp = temp[is.element(temp$confidence,c("A","B","C")),]
  tflist = sort(unique(temp$tf))

  name.list = c("signature","parent","source","type","direction","description","subsource","ngene","gene.list")
  mat_up = as.data.frame(matrix(nrow=length(tflist),ncol=length(name.list)))
  mat_dn = as.data.frame(matrix(nrow=length(tflist),ncol=length(name.list)))
  names(mat_up) = name.list
  names(mat_dn) = name.list
  for(i in 1:length(tflist)) {
    tf = tflist[i]
    t2 = temp[is.element(temp$tf,tf),]
    genes_up = t2[t2$mor==  1,"target"]
    genes_dn = t2[t2$mor== -1,"target"]
    gene.list_up = paste(genes_up,collapse="|")
    gene.list_dn = paste(genes_dn,collapse="|")

    mat_up[i,"signature"] <- paste0("dorothea_",tf,"_up")
    mat_up[i,"parent"] <- paste0("dorothea_",tf,"_up")
    mat_up[i,"source"] <- "dorothea"
    mat_up[i,"type"] <- "nondirectional"
    mat_up[i,"direction"] <- "nondirectional"
    mat_up[i,"description"] <- "Dorothea human regulons"
    mat_up[i,"subsource"] <- "dorothea"
    mat_up[i,"ngene"] <- length(genes_up)
    mat_up[i,"gene.list"] <- gene.list_up

    mat_dn[i,"signature"] <- paste0("dorothea_",tf,"_dn")
    mat_dn[i,"parent"] <- paste0("dorothea_",tf,"_dn")
    mat_dn[i,"source"] <- "Dorothea"
    mat_dn[i,"type"] <- "nondirectional"
    mat_dn[i,"direction"] <- "nondirectional"
    mat_dn[i,"description"] <- "Dorothea human regulons"
    mat_dn[i,"subsource"] <- "Dorothea"
    mat_dn[i,"ngene"] <- length(genes_dn)
    mat_dn[i,"gene.list"] <- gene.list_dn
  }
  mat = rbind(mat_up,mat_dn)
  mat = mat[order(mat$signature),]
  mat = mat[mat$ngene>=20,]
  Dorothea_signatures = mat
  browser()
  file = "data/Dorothea/Dorothea_signatures.RData"
  save(Dorothea_signatures,file=file)
}
