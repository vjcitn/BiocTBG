perm_rows = function(x) {
  o = sample(nrow(x),nrow(x),replace=FALSE)
  x[o,]
}

#' convert data.frame to train/test/validate subsamples
#' @param dfr a data.frame
#' @param p numeric(3) with proportions of samples in train, test, validate
#' @export
make_trvate = function(dfr, p=c(.7,.2,.1)) {
  px = perm_rows(dfr)
  n1 = floor(nrow(px)*p[1])
  n2 = floor(nrow(px)*p[2])
  list(tr=px[seq_len(n1),], te=px[seq(n1+n2+1, nrow(px)),],
      va=px[seq(n1+1, n1+n2),])
}
  
