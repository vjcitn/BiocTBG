
# necessary for python module control
#' python declarations
#' @import basilisk
bsklenv <- basilisk::BasiliskEnvironment(
  envname = "bsklenv",
  pkgname = "BiocTBG",
  packages = c("python=3.11")
  )


