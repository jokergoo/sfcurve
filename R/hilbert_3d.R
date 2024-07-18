
#' Three dimensional Hilbert curve
#' 
#' @param level The level of the curve.
#' 
#' @details
#' There are many forms of 3D Hilbert curve. Here we only implement one specific form of it.
#' 
#' @export
#' @import Rcpp
#' @useDynLib sfcurve, .registration = TRUE
#' @seealso Michael Bader. Space-Filling Curves: An Introduction with Applications in Scientific Computing, Springer Science & Business Media, 2012. \url{https://doi.org/10.1007/978-3-642-31046-1}.
#' @return A three-column matrix of the coordinates of the Hilbert curve segments.
#' @examples
#' pos = hilbert_3d(2)
#' if(require(rgl) && interactive()) {
#'     plot3d(pos, type = "l", lwd = 4, col = 2)
#' }
hilbert_3d = function(level = 2L) {
	if(level < 1) {
		stop_wrap("`level` should not smaller than 1.")
	}
	lt = hilbert_curve_3d_cpp(level)

	cbind(x = lt[[1]], y = lt[[2]], z = lt[[3]])
}
