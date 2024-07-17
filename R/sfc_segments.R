

#' Coordinates of the points on the curve
#' @aliases sfc_segments
#' @rdname sfc_segments
#' @param p An `sfc_nxn` or `sfc_sequence` object.
#' @param bases A list of base patterns, consider to use `BASE_LIST`.
#' @param start Coordinate of the start point.
#' 
#' @return
#' A two-column matrix.
#' @export
#' @examples
#' p = sfc_hilbert("I", "11")
#' loc = sfc_segments(p)
#' plot(loc, type = "l")
setMethod("sfc_segments", 
	signature = "sfc_nxn", 
	definition = function(p, bases = p@rules@bases, start = c(0, 0)) {

	callNextMethod(p, bases, start)
})


#' @rdname sfc_segments
#' @export
setMethod("sfc_segments", 
	signature = "sfc_sequence", 
	definition = function(p, bases, start = c(0, 0)) {

	seq = as.character(p@seq)
	rot = p@rot
	n = length(seq)

	n = length(seq)
	pos = matrix(NA_integer_, nrow = n, ncol = 2)

	pos[1, ] = start
	for(i in seq_len(n)[-1]) {
		pos[i, ] = sfc_next_point(bases[[ seq[i-1] ]], pos[i-1, ], rot[i-1])
	}

	pos
})
