#' Produce a SummarizedExperiment instance with assay elements
#' (i,j) the simba score relating gene i to cell j
#' @note The simba algorithm produces probability-like scores for
#' measuring tendency of two entities to be linked.  This
#' function takes the embeddings produced by sce_to_embeddings
#' and produces edgewise softmax rescalings of C'G where
#' C is the embedding for cells and G the embedding for genes
#' @param embout output of `sce_to_embeddings`
#' @param colfeat character(1) element of colData of `sce` to index cells
#' @param rowfeat character(1) element of rowData of `sce` to index genes
#' @return a SummarizedExperiment
embs_to_scores = function(embout, sce, colfeat="Barcode", rowfeat="Symbol") {
  C = t(embout$cemb)
  FF = embout$gemb
  C %*% FF -> BIG
  EBIG = exp(BIG)
  CSUMS = apply(EBIG,1,sum)
  pmat = t(as.matrix(EBIG/CSUMS))
  rownames(pmat) = embout$gents
  colnames(pmat) = embout$cents
  rinds = make.names(rowData(sce)[[rowfeat]], unique=TRUE)
  pse = SummarizedExperiment(pmat[rinds, colData(sce)[[colfeat]]])
  mc = match.call()
  metadata(pse) = list(call = mc)
  pse
}

#ep = embs_to_probs(nn50, t3k)
#
#altExp(t3k, "sprobs", withDimnames=FALSE) = ep
#o = order(assay(altExp(t3k, "sprobs"))["NKG7",], decreasing=TRUE)
#plot(assay(altExp(t3k, "sprobs"))["NKG7",o], pch=19, col=factor(t3k[,o]$celltype))
#legend(2000,.0012, legend=unique(factor(t3k[,o]$celltype)),col = unique(factor(t3k[,o]$celltype)), pch=19)
#

#' update an SCE with simba edge scores
#' @param embs output of sce_to_embeddings
#' @param sce the associated sce
#' @return SCE updated with altExp giving the simba scores
#' @examples
#' data(nn50)
#' data(t3k)
#' t3k = add_simba_scores(nn50, t3k)
#' t3k
#' @export
add_simba_scores = function(embs, sce) {
  ep = embs_to_scores(embs, sce)
  altExp(sce, "sprobs", withDimnames=FALSE) = ep
  sce
}

#' set up a data.frame for barplot visualization for a gene
#' @param sce SingleCellExperiment with altExp element "sprobs"
#' @param genesym character(1)
#' @param colvars character() columns from colData(sce) to propagate
#' @return data.frame with elements rank, prob, and propagated columns
#' @examples
#' data(nn50)
#' data(t3k)
#' t3k = add_simba_scores(nn50, t3k)
#' gns = c("CST3", "MS4A1", "NKG7", "GAPDH")
#' alldf = lapply(gns, function(x) simba_barplot_df(t3k, x, 
#'   colvars = "celltype"))
#' alldf = do.call(rbind, alldf)
#' ggplot(alldf, aes(x=rank,xend=rank,y=0, yend=prob,colour=celltype)) + geom_point() +
#'   geom_segment() + facet_wrap(~symbol, ncol=2) + 
#'     theme(strip.text=element_text(size=24), 
#'       legend.text=element_text(size=24), 
#'       axis.text=element_text(size=24)) +
#'     guides(colour = guide_legend(override.aes = 
#'        list(linetype=1, lwd=2)))
#' @export
simba_barplot_df = function(sce, genesym, colvars="celltype") {
  stopifnot("sprobs" %in% altExpNames(sce))
  stopifnot(genesym %in% rownames(altExp(sce, "sprobs")))
  stopifnot(all(colvars %in% names(colData(sce))))
  resp = as.numeric(assay(altExp(sce, "sprobs"))[genesym,])
  o = order(resp, decreasing=TRUE)
  ans = data.frame(rank=seq_len(length(o)), prob = resp[o], tmp=colData(sce)[o,colvars])
  names(ans)[-c(1,2)] = colvars
  ans$symbol = genesym
  ans
}
# 
# library(ggplot2)
# mydf = simba_barplot_df(t3k, "CST3", colvars = "celltype")
# ggplot(mydf, aes(x=rank,y=prob,colour=celltype)) + geom_point()
#   
# 
# library(ggplot2)
# ggplot(mydf, aes(x=rank, xend=rank, y=0, yend=prob, colour=celltype)) +
#   geom_segment() + facet_wrap(~symbol, ncol=2) + 
#     theme(strip.text=element_text(size=24), 
#       legend.text=element_text(size=24), 
#       axis.text=element_text(size=24)) +
# guides(colour = guide_legend(override.aes = list(linetype=1, lwd=2)))
