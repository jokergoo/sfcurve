

#' Reduce a curve
#' @aliases sfc_reduce
#' @rdname sfc_reduce
#' @param p An `sfc_nxn` object.
#' @param to Which level to reduce to? Value should be between 1 and `sfc_level(p) - 1`.
#' @details
#' The reduction is applied on the coordinates of points.
#' @return
#' A two-column matrix of coordinates of the reduced curve.
#' @export
#' @examples
#' p = sfc_3x3_peano("I", level = 3)
#' draw_multiple_curves(
#'     p, 
#'     sfc_reduce(p, 2), 
#'     sfc_reduce(p, 1), 
#'     nrow = 1)
setMethod("sfc_reduce",
	signature = "sfc_nxn",
	definition = function(p, to = sfc_level(p) - 1) {

	if(!(to <= sfc_level(p) - 1 && to >= 1)) {
		stop_wrap("`to` should take values in [", 1, ", ", sfc_level(p) - 1, "]")
	}

	n = p@n
	level = sfc_level(p)

	n_block = (n^2)^to
	block_size = (n^2)^(level - to)

	loc = sfc_segments(p)
	loc2 = matrix(NA_real_, nrow = n_block, ncol = 2)

	for(i in seq_len(n_block)) {
		ind = seq( (i-1)*block_size + 1, i*block_size )
		loc2[i, ] = colMeans(loc[ind, , drop = FALSE])
	}

	loc2

})

#' @rdname sfc_reduce
#' @export
#' @examples
#' p = hilbert_curve(level = 4)
#' draw_multiple_curves(
#'     p, 
#'     sfc_reduce(p, 3), 
#'     sfc_reduce(p, 2), 
#'     nrow = 1)
setMethod("sfc_reduce",
	signature = "matrix",
	definition = function(p, to = level - 1) {

	N = nrow(p)
	if( abs(round(log(N)/2/log(2)) - log(N)/2/log(2)) < 1e-4) {
		n = 2
		level = round(log(N)/2/log(2))
	} else if( abs(round(log(N)/2/log(3)) - log(N)/2/log(3)) < 1e-4) {
		n = 3
		level = round(log(N)/2/log(3))
	}
	
	n_block = (n^2)^to
	block_size = (n^2)^(level - to)

	loc = p
	loc2 = matrix(NA_real_, nrow = n_block, ncol = 2)

	for(i in seq_len(n_block)) {
		ind = seq( (i-1)*block_size + 1, i*block_size )
		loc2[i, ] = colMeans(loc[ind, , drop = FALSE])
	}

	loc2

})


#' @rdname sfc_reduce
#' @param gb A `grob` object returned by [`sfc_grob()`] or a `sfc_nxn` object then [`sfc_grob()`] is internally applied.
#' @param level The level of the unit.
#' @export
#' @details
#' `add_base_structure()` adds a base structure on a certain level to the curve.
#' @examples
#' p = hilbert_curve(3)
#' draw_multiple_curves(
#'     add_base_structure(p, level = 1),
#'     add_base_structure(p, level = 2),
#'     nrow = 1
#' )
add_base_structure = function(gb, level = 1) {
	if(inherits(gb, "sfc_nxn")) {
		gb = sfc_grob(gb)
	} else if(inherits(gb, "matrix")) {
		gb = sfc_grob(gb)
	}
	x = c(gb$children[[1]]$x0[1], gb$children[[1]]$x1)
	y = c(gb$children[[1]]$y0[1], gb$children[[1]]$y1)

	loc = cbind(x, y)

	loc_reduced = sfc_reduce(loc, level)
	n = nrow(loc_reduced)

	gb$children[["base_structure"]] = segmentsGrob(x0 = unit(loc_reduced[1:(n-1), 1], "native"), y0 = unit(loc_reduced[1:(n-1), 2], "native"),
		                               x1 = unit(loc_reduced[2:n, 1], "native"), y1 = unit(loc_reduced[2:n, 2], "native"),
		                               gp = gpar(col = "black", lwd = 2, lty = 3))
	gb$childrenOrder = c(gb$childrenOrder, "base_structure")

	gb
}
