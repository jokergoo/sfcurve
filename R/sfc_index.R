
#' @rdname sfc_index
#' @param x An `sfc_nxn` object.
#' @param i Numeric index or a character index representing the hierarchy of the subunit in the curve.
#' @param j A value of `TRUE` or `FALSE` that controls whether to keep the `sfc_nxn` class or degenerate to the `sfc_sequence` class.
#' @param ... Ignore.
#' @param drop Ignore.
#' @export
#' @examples
#' p = sfc_hilbert("I", "11111")
#' p["321"]
#' p["321", TRUE]
`[.sfc_nxn` = function(x, i, j, ..., drop = TRUE) {

	if(missing(i)) {
		return(x)
	}
	if(length(i) == 0) {
		return(x)
	}

	if(missing(j)) {
		`[.sfc_sequence`(x, sfc_index(x, i))
	} else {
		if(!is.logical(j)) {
			stop_wrap("The second index should be TURE or FALSE.")
		}
		if(j) {
			x2 = x
			ind = sfc_index(x, i)
			x2@seq = x@seq[ind]
			x2@rot = x@rot[ind]
			x2
		} else {
			`[.sfc_sequence`(x, sfc_index(x, i))
		}
	}
}

get_index_from_nxn = function(index, level, n) {
	index = as.integer(strsplit(as.character(index), "")[[1]])

	ind = seq_len( (n^2)^level )
	for(i in index) {
		if(is.na(i)) {
			stop_wrap("It should only contain digits.")
		}
		if(i > n^2) {
			stop_wrap( paste0("Each digit in `index` should be smaller than ", n^2, ".") )
		}
		if(i < 1) {
			stop_wrap("Each digit in `index` should be larger than zero.")
		}
		portion = length(ind)/(n^2)

		if(portion < 1) {
			stop_wrap( paste0("Depth in `index` should be not larger than ", level, ".") )
		}
		ind = ind[seq( (i-1)*portion+1, i*portion )]
	}
	ind
}

#' Subunit in the curve
#' @aliases sfc_index
#' @rdname sfc_index
#' @param p An `sfc_nxn` object.
#' @param index A string of digits representing the path on the hierarchy of the curve. The left side
#'       corresponds to the lower level and the right side corresponds to the high level in the curve. For the
#'       Hilbert curve, the digits can only be 1-4, and for the Peano and Meander curves, the digites can be 1-9.
#' @export
#' @details
#' `sfc_index()` only works on square curves (i.e. a curve with a single base letter as seed.)
#' @examples
#' # only for testing
#' p = sfc_hilbert("I", "11111")
#' om = par(no.readonly = TRUE)
#' par(mfrow = c(2, 2))
#' sfcurve:::test_sfc_index(p, "3")
#' sfcurve:::test_sfc_index(p, "32")
#' sfcurve:::test_sfc_index(p, "321")
#' sfcurve:::test_sfc_index(p, "3211")
#' par(om)
#' 
#' p = sfc_meander("I", "11111")
#' om = par(no.readonly = TRUE)
#' par(mfrow = c(2, 2))
#' sfcurve:::test_sfc_index(p, "7")
#' sfcurve:::test_sfc_index(p, "75")
#' sfcurve:::test_sfc_index(p, "759")
#' sfcurve:::test_sfc_index(p, "7592")
#' par(om)
setMethod("sfc_index",
	signature = "sfc_nxn",
	definition = function(p, index = "") {

	if(is.numeric(index)) {
		return(index)
	} else if(length(index) == 0) {
		return(seq_len(length(p)))
	} else if(identical(index, "")) {
		return(seq_len(length(p)))
	} else {
		get_index_from_nxn(as.character(index), p@level, p@n)
	}
})

#' @import graphics
test_sfc_index = function(p, index) {
	loc = sfc_segments(p)

	om = par("mar")
	on.exit(par(mar = om))
	par(mar = c(1, 1, 4, 1))
	plot(loc, type = "l", col = "grey", ann = FALSE, axes = FALSE, asp = 1)

	index = sfc_index(p, index)

	if(length(index) == 1) {
		points(loc[index, 1], loc[index, 2], pch = 16, col = "black")
	} else {
		lines(loc[index, 1], loc[index, 2], lwd = 2, col = "black")
	}
	title(paste0(class(p), ": level = ", p@level, ", ", p@n, "x", p@n, ", index = ", index))
}



