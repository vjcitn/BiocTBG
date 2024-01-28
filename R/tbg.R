get_my_pip3 = function() {
 ver = as.character(packageVersion("BiocTBG"))
 dir(file.path(basilisk.utils::getExternalDir(), "BiocTBG", ver, "bsklenv", "bin"), pattern="pip3", full=TRUE)
}

# here's what used to be available!
#> names(pbgref)
# [1] "config"         "converters"     "edgelist"       "entitylist"    
# [5] "graph_storages" "pkg_resources"  "plugin"         "schema"        
# [9] "tensorlist"     "types"          "util"          


#' basic interface, shows how to get selected references
#' @importFrom reticulate import
#' @return basiliskRun result with import from reticulate, typically a Module
#' @examples
#' tbgrefs <- tbgR()
#' tbgrefs
#' names(tbgconf$config)
#' names(tbgconf$train)
#' @export
tbgR <- function() {
  proc <- basilisk::basiliskStart(bsklenv)
  basilisk::basiliskRun(proc, function() {
  on.exit(basilisk::basiliskStop(proc))
  mypip = get_my_pip3()
  chk = try(reticulate::import("torchbiggraph"), silent=TRUE)
  if (inherits(chk, "try-error")) {
    system2(mypip, c("install", system.file("tbg_source", "pytorch-biggraph", package="BiocTBG")))
  }
    reticulate::import("torchbiggraph")
    cc = reticulate::py_run_string("from torchbiggraph import config")
    uu = reticulate::py_run_string("from torchbiggraph import util")
    tt = reticulate::py_run_string("from torchbiggraph import train")
    list(config=cc$config, util=uu$util, train=tt$train)
  })
}
