

#' produce torchbiggraph embeddings for discretized single-cell
#' RNA-seq measures in a SingleCellExperiment
#' @param sce SingleCellExperiment instance that includes `disc` among
#' its assays, intended to hold output of `disc_mat` applied to
#' normalized log counts
#' @param workdir character(1) folder for interim computations, must
#' exist prior to call
#' @param N_EPOCHS numeric(1)
#' @param N_GENES numeric(1)
#' @param N_GPUS numeric(1)
#' @param BATCH_SIZE integer
#' @param pbg reference to torchbiggraph
#' @param palib reference to pathlib
#' @return a list with elements `cemb` and `gemb` (matrix
#' representations of embeddings of cells and genes respectively),
#' `cents` and `gents` (vectors of names of cells and
#' genes as ordered in the returned matrices),
#' stats (a data.frame of training statistics),
#' call (the match.call) and config (a python reference,
#' not useful after the session in which object was
#' produced ends).
#' @examples
#' pbg = reticulate::import("torchbiggraph")
#' palib = reticulate::import("pathlib")
#' data(t3k)
#' testd = paste0(tempdir(), "/testse")
#' dir.create(testd)
#' nn2 = sce_to_embeddings(t3k, testd, N_EPOCHS=2L, BATCH_SIZE=1000L,
#'    N_GENES=500L, N_GPUS=0L, pbg=pbg, palib=palib)
#' dir(testd, full.names=TRUE)
#' @export
sce_to_embeddings = function(sce, workdir, N_EPOCHS, N_GENES, N_GPUS=1L, 
   BATCH_SIZE=100000L, pbg, palib) {

   stopifnot(dir.exists(workdir))

   N_EPOCHS = as.integer(N_EPOCHS)
   N_GENES = as.integer(N_GENES)
   BATCH_SIZE = as.integer(BATCH_SIZE)

   owd = getwd()
   setwd(workdir)
   on.exit(setwd(owd))

   sce_to_triples(sce, "basic.tsv", ngenes=N_GENES) # forced name
   
   
   entC = make_entity_schema(pbgref = pbg)
   entG = make_entity_schema(pbgref = pbg)
   ents = list(C = entC, G = entG)
   
   rels = paste0("r", 0:4) # FIXME should be parameter drawn from disc
   wts = 1.0+(0:4)
   rels = lapply(1:5, function(x) make_rel_schema(name = rels[x], lhs='C', rhs='G',
      weight = wts[x], operator='none', all_negs = FALSE, pbgref = pbg))

   epath = sprintf("./ents%d", N_EPOCHS) 
   
# FIXME should we be using workdir in the paths stated here
#
   cc = setup_config_schema(pbgref = pbg, entities = ents, relations = rels,
     entity_path = epath, batch_size=BATCH_SIZE,
     edge_paths = list('tr'="tr"), checkpoint_path = sprintf("cp%d", N_EPOCHS),
     dimension = 50L, dynamic_relations = FALSE, num_epochs=N_EPOCHS, num_gpus=N_GPUS )
   
   tt = triples_to_hdf5( cc, c(train="./basic.tsv"), pbgref = pbg, paref = palib )
   train_eval( list(config=cc), pbg ,evind=1) # FIXME set do_eval
   
   cembpath = sprintf("cp%d/embeddings_C_0.v%d.h5", N_EPOCHS, N_EPOCHS)
   gembpath = sprintf("cp%d/embeddings_G_0.v%d.h5", N_EPOCHS, N_EPOCHS)

   cemb = rhdf5::h5read(cembpath, "embeddings")
   gemb = rhdf5::h5read(gembpath, "embeddings")

   cents = jsonlite::fromJSON(paste0(epath, "/", "entity_names_C_0.json"))
   gents = jsonlite::fromJSON(paste0(epath, "/", "entity_names_G_0.json"))
   statstmp = readLines(sprintf("cp%d/training_stats.json", N_EPOCHS))
   statsa = lapply(statstmp, jsonlite::fromJSON)
   stats = do.call( rbind, lapply(statsa, data.frame))

  mc = match.call()
  list(cemb=cemb, gemb=gemb, cents=cents, gents=gents, 
        stats=stats, call=mc, config=cc)
}

