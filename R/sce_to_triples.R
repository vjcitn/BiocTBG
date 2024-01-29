

#' filter a discretized SCE using getTopHVGs from scran, the produce triples (edges)
#' (cell - weight - gene)
#' @import scran
#' @param sce instance of SingleCellExperiment with assays logcounts and disc
#' @param outtsv character(1) file to hold triples in tsv format
#' @param ngenes numeric(1) number of genes to retain from getTopHVGs after
#' scran::modelGeneVar on logcounts
#' @examples
#' p3k = TENxPBMCData::TENxPBMCData("pbmc3k")
#' p3k = scater::logNormCounts(p3k)
#' tsv = tempfile()
#' mt = sce_to_triples(p3k, tsv, ngenes=2000)
#' readLines(tsv, 5)
#' @export
sce_to_triples = function (sce, outtsv, ngenes = 5000)
{
    if (!("logcounts" %in% assayNames(sce)))
        stop("no logcounts present in sce")
    stats = scran::modelGeneVar(assay(sce, "logcounts", withDimnames = FALSE))
    tt = scran::getTopHVGs(stats, n = ngenes)
    lim = sce[tt, ]
    assays(lim)$logcounts = as.matrix(assays(lim)$logcounts)
    assays(lim)$disc = disc_matrix(assays(lim)$logcounts)
    make_triples(lim, outtsv)
}

