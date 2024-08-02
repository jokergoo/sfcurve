

#' Apply to every unit in the sfc_nxn curve
#' @aliases sfc_apply
#' @rdname sfc_apply
#' @param p An `sfc_nxn` object.
#' @param depth An integer between 0 and `level-1` of the curve.
#' @param fun A function of which the argument `x` is a subunit in the curve. The subunit is an `sfc_nxn` object but only contains the current sub-sequence.
#'       The function should return an `sfc_seuqence` object with the same length as of `x`. The function can take an optional second argument which
#'       the index of the current subunit in the curve.
#' 
#' @details
#' This function is mainly used to flip subunits on various levels on the curve, thus mainly on the Peano curve and the Meander curve.
#' A depth of 0 corresponds to the complete curve. A depth of 1 corresponds to the nine first-level units, et al.
#' 
#' Currently, `sfc_apply()` only works on curves with a single base pattern as the seed.
#' 
#' @export
#' @examples
#' p = sfc_peano("I", level = 3)
#' # flip the global curve
#' draw_multiple_curves(
#'     p, 
#'     sfc_apply(p, 0, sfc_flip_unit),
#'     nrow = 1
#' )
#' 
#' # flip all the subunits on depth = 1
#' draw_multiple_curves(
#'     p, 
#'     sfc_apply(p, 1, sfc_flip_unit),
#'     nrow = 1
#' )
#' 
#' # flip all the subunits on depth = 2
#' draw_multiple_curves(
#'     p, 
#'     sfc_apply(p, 2, sfc_flip_unit),
#'     nrow = 1
#' )
#' 
#' # flip all level-1 patterns on the Peano curve to horizontal
#' # only works on the lowest subunit, 
#' p2 = sfc_apply(p, 2, function(x) {
#'     if(level1_unit_orientation(x) == "vertical") {
#'         sfc_flip_unit(x)
#'     } else {
#'         x
#'     }
#' })
#' # then on depth=1, only flip the unit with odd index
#' p3 = sfc_apply(p2, 1, function(x, i) {
#'     if(i %% 2 == 1) {
#'         sfc_flip_unit(x)
#'     } else {
#'         x
#'     }
#' })
#' draw_multiple_curves(p2, p3, nrow = 1)
#' 
#' # flip all level-1 patterns to vertical
#' p3 = sfc_apply(p, 2, function(x) {
#'     if(level1_unit_orientation(x) == "horizontal") {
#'         sfc_flip_unit(x)
#'     } else {
#'         x
#'     }
#' })
#' draw_multiple_curves(p, p3, nrow = 1)
setMethod("sfc_apply",
	signature = "sfc_nxn",
	definition = function(p, depth = 1, fun = function(x) x) {
	if(depth >= p@level) {
		stop_wrap("`depth` should only be smaller than the level of the curve.")
	}
	if(depth < 0) {
		stop_wrap("`depth` should be zero or a positive integer.")
	}

	n = p@n

	n_block = (n^2)^depth
	block_size = (n^2)^(p@level - depth)

	fm = formals(fun)
	narg = length(fm)

	for(i in seq_len(n_block)) {
		ind = seq( (i-1)*block_size + 1, i*block_size )
		unit = p[ind, TRUE]

		if(narg == 1) {
			p[ind] = fun(unit)
		} else if(narg == 2) {
			if(names(fm)[2] %in% c("i", "j", "k")) {
				p[ind] = fun(unit, i)
			} else {
				p[ind] = fun(unit)
			}
		}
	}
	p
})
