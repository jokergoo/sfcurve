

#' Coordinates of the points on the curve
#' @aliases sfc_segments
#' @rdname sfc_segments
#' @param p An `sfc_nxn` or `sfc_sequence` object.
#' @param bases A list of base patterns, consider to use [`BASE_LIST`].
#' @param start Coordinate of the start point.
#' @param ... Other argument.
#' 
#' @return
#' A two-column matrix of the coordinates of points on the curve.
#' @export
#' @examples
#' p = sfc_2x2("I", "11")
#' loc = sfc_segments(p)
#' plot(loc, type = "l", asp = 1)
setMethod("sfc_segments", 
	signature = "sfc_nxn", 
	definition = function(p, bases = p@rules@bases, start = c(0, 0), ...) {

	callNextMethod(p, bases, start, ...)
})


#' @rdname sfc_segments
#' @details
#' For the `sfc_segments()` on the `sfc_sequence` object, if `bases` is not set,
#' it uses [`BASE_LIST`] internally. Make sure the sequence only contains the pre-defined base patterns.
#' @param by Which implementation? Only for the testing purpose.
#' @export
setMethod("sfc_segments", 
	signature = "sfc_sequence", 
	definition = function(p, bases = NULL, start = c(0, 0), by = "Cpp") {

	if(is.null(bases)) {
		bases = BASE_LIST[sfc_universe(p)]
	}

	if(any(sapply(bases, is.null))) {
		stop_wrap("`bases` does not cover all base patterns in `p`.")
	}

	bases = bases[ sfc_universe(p) ]
	seq = as.integer(p@seq)
	rot = p@rot
	
	if(by == "Cpp") {
		pos = sfc_segments_cpp(seq, rot, bases, start)
	} else {
		n = length(seq)
		pos = matrix(NA, nrow = n, ncol = 2)

		pos[1, ] = start
		for(i in seq_len(n)[-1]) {
			pos[i, ] = sfc_next_point(bases[[ seq[i-1] ]], pos[i-1, ], rot[i-1])
		}
	}

	pos2 = as.integer(round(pos))
	dim(pos2) = dim(pos)

	pos2
})
