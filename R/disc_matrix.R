#' discretize a dense matrix
#' @param x matrix
#' @param n_bins numeric(1)
#' @param q4pos logical(1) if TRUE, quantiles are formed for positive entries only to form the cutpoints
#' @note The lowest category includes all zero values.
#' @return matrix with values 0:(nbins-1) corresponding to quantile groups
#' @export
disc_matrix = function(x, n_bins=5, q4pos=TRUE) {
 xn = as.numeric(x)
 cuts = quantile(xn, probs = seq(0, 1, 1/n_bins))
 if (q4pos) cuts = quantile(xn[xn>0], probs = seq(0, 1, 1/n_bins))
 cuts[1] = min(xn)-.01
 cuts[length(cuts)] = max(xn)+.01
 dx = cut(xn,  cuts)
 x[] = as.numeric(dx)-1
 x
}
