

#' Apply to every unit in the sfc_nxn curve
#' @aliases sfc_apply
#' @rdname sfc_apply
#' @param p An `sfc_nxn` object.
#' @param depth An integer between 0 and `level-1` of the curve.
#' @param fun A function of which the argument `x` is a sub-unit in the curve.
#' 
#' @details
#' The size of the square unit is determined by `depth`. A depth of 0 corresponds to the complete curve.
#' A depth of 1 corresponds to the nine first-level units, et al.
#' 
#' @export
#' @examples
#' p = sfc_peano("I", 111)
#' draw_multiple_curves(p, sfc_apply(p, 0, function(x) sfc_flip_unit(x)))
#' draw_multiple_curves(p, sfc_apply(p, 1, function(x) sfc_flip_unit(x)))
#' draw_multiple_curves(p, sfc_apply(p, 2, function(x) sfc_flip_unit(x)))
#' 
#' # only works on the lowest unit
#' peano_unit_orientation = function(u) {
#'     loc = sfc_segments(u)
#'     if(loc[1, 1] == loc[2, 1] && loc[2, 1] == loc[3, 1]) {
#'         "vertical"
#'     } else {
#'         "horizontal"
#'     }
#' }
#' p2 = sfc_apply(p, 2, function(x) {
#'     if(peano_unit_orientation(x) == "vertical") {
#'         sfc_flip_unit(x)
#'     } else {
#'         x
#'     }
#' })
#' draw_multiple_curves(p, p2, title = FALSE)
#' 
#' p3 = sfc_apply(p, 2, function(x) {
#'     if(peano_unit_orientation(x) == "horizontal") {
#'         sfc_flip_unit(x)
#'     } else {
#'         x
#'     }
#' })
#' draw_multiple_curves(p, p3, title = FALSE)
setMethod("sfc_apply",
	signature = "sfc_nxn",
	definition = function(p, depth = 1, fun = function(x) x) {
	if(depth >= p@level) {
		stop_wrap("`depth` should only be smaller than the level of the curve.")
	}
	if(depth < 0) {
		stop_wrap("`depth` should be a positive integer.")
	}

	n = p@n

	n_block = (n^2)^depth
	block_size = (n^2)^(p@level - depth)

	for(i in seq_len(n_block)) {
		ind = seq( (i-1)*block_size + 1, i*block_size )
		unit = p[ind, TRUE]

		p[ind] = fun(unit)
	}
	p
})
