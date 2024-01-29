
#' helper function
docol = function (sce, co, colname="Barcode", assayname="disc") 
{
    nc = names(colData(sce))
    stopifnot(colname %in% nc)
    an = assayNames(sce)
    stopifnot(assayname %in% an)
    data.frame(cell = colData(sce)[[colname]][co], rel = paste0("r", 
        assay(sce, assayname)[, co]), gene = rownames(sce))
}

#' make triples from a discretized SCE
#' @param sce SingleCellExperiment
#' @param fname character(1) destination file
#' @param colname character(1) colData variable to use for labeling cells
#' @param assayname character(1) assayNames element to use, defaults to "disc"
#' @examples
#' if (!requireNamespace("TENxPBMCData")) stop("install TENxPBMCData to use this example")
#' p3k = TENxPBMCData::TENxPBMCData("pbmc3k")
#' @export
make_triples = function(sce, fname, colname="Barcode", assayname="disc") {
  todo = seq_len(ncol(sce))
  t1 = docol(sce, 1, colname = colname, assayname = assayname)
  write.table(t1, fname, col.names=FALSE, row.names=FALSE, sep="\t", quote=FALSE)
  for (j in todo[-1]) {
    if (j %% 50 == 0) cat(".")
    t1 = docol(sce, j, colname = colname, assayname = assayname)
    write.table(t1, fname, col.names=FALSE, row.names=FALSE, sep="\t", append=TRUE, quote=FALSE)
    }
   NULL
}

