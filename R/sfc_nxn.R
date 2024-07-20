
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

#' Units in the curve
#' @aliases sfc_index
#' @rdname sfc_index
#' @param p An `sfc_nxn` object.
#' @param index A string of digits representing the path on the hierarchy of the curve. The left side
#'       corresponds to the lower level and the right side corresponds to the high level in the curve. For the
#'       Hilbert curve, the digits can only be 1-4, and for the Peano and Meander curves, the digites can be 1-9.
#' @export
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
		lines(loc[index, 1], loc[index, 2], lwd = 4, col = "black")
	}
	title(paste0(class(p), ": level = ", p@level, ", ", p@n, "x", p@n))
}



reduce_loc_to_level_1 = function(p, n, step = 1) {

	loc = sfc_segments(p)
	while(step && nrow(loc) > n*n) {
		loc2 = matrix(nrow = nrow(loc)/n/n, ncol = 2)
		for(i in seq_len(nrow(loc2))) {
			loc2[i, ] = colMeans(loc[seq((i-1)*n*n + 1, i*n*n), ])
		}
		loc = loc2

		step = step - 1
	}

	loc
}

#' @rdname sfc_index
#' @export
unit_orientation = function(p, index = "") {
	if(inherits(p, "sfc_unit")) {
		loc = sfc_segments(p)
	} else if(length(index) == 0 || identical(index, "")) {
		unit = p[index]

		loc = reduce_loc_to_level_1(unit, p@n, Inf)
	} else {
		if(!is.character(index)) {
			stop_wrap("`index` should be a character scalar.")
		}
		if(nchar(index) < ceiling(p@level/2)) {
			if(inherits(p, "sfc_hilbert")) {
				fun = sfc_hilbert
			} else if(inherits(p, "sfc_peano")) {
				fun = sfc_peano
			} else if(inherits(p, "sfc_meander")) {
				fun = sfc_meander
			}

			pp = fun(p@seed, code = p@code[seq_len(length(index) + 1)])
			loc = reduce_loc_to_level_1(pp, pp@n, Inf)
		} else {
			unit = p[index]

			loc = reduce_loc_to_level_1(unit, p@n, Inf)
		}
	}

	n = nrow(loc)
	min_x = min(loc[, 1])
	max_x = max(loc[, 1])
	min_y = min(loc[, 2])
	max_y = max(loc[, 2])

	if(equal_to(loc[1, 1], loc[n, 1])) {
		return("vertical")
	} else if(equal_to(loc[1, 2], loc[n, 2])) {
		return("horizontal")
	} else if( (loc[1,1] < loc[n, 1] && loc[1, 2] < loc[n, 2]) || (loc[1,1] > loc[n, 1] && loc[1, 2] > loc[n, 2]) ) {
		return("diagonal_1")
	} else if( (loc[1,1] > loc[n, 1] && loc[1, 2] < loc[n, 2]) || (loc[1,1] < loc[n, 1] && loc[1, 2] > loc[n, 2]) ) {
		return("diagonal_-1")
	} else {
		return("unknown")
	}
}

#' @rdname sfc_index
#' @aliases sfc_flip_unit
#' @details
#' An unit in the curve is represented as a square block (`2^k x 2^k` for the Hilbert curve and `3^k x 3^k` for the Peano and Meander curves).
#' In the Hilbert curve, if an unit can be flipped, it is symmetric, thus flipping in the Hilbert curve does not change its form.
#' The flipping is mainly applied in the Peano curve and the Meander curves. Peano curve only allows flippings by the diagonals and the Meander
#' curve only allows flipping horizontally or vertically. The type of flipping is choosen automatically in the function.
#' @export
#' @examples
#' p = sfc_meander("I", 11)
#' draw_multiple_curves(p, 
#'     sfc_flip_unit(p, "1"),
#'     sfc_flip_unit(p, "2"),
#'     sfc_flip_unit(p, "3"),
#'     title = FALSE, nrow = 1)
setMethod("sfc_flip_unit",
	signature = "sfc_nxn",
	definition = function(p, index = "") {

	if(length(p) < (p@n^2)^p@level) {
		if(!length(p) %in% (p@n^2)^(seq_len(p@level))) {
			stop_wrap("Since `p` is only a fragment of the curve, it should be represented as a square with length of ", paste((p@n^2)^(seq_len(p@level)), collapse = ", "), ".")
		}
	}

	unit = p[index]
	orientation = unit_orientation(p, index)

	if(orientation == "vertical") {
		unit = sfc_vflip(unit, fix_ends = TRUE, bases = p@rules@bases)
	} else if(orientation == "horizontal") {
		unit = sfc_hflip(unit, fix_ends = TRUE, bases = p@rules@bases)
	} else if(orientation == "diagonal_1") {
		unit = sfc_dflip(unit, slop = 1, fix_ends = TRUE, bases = p@rules@bases)
	} else if(orientation == "diagonal_-1") {
		unit = sfc_dflip(unit, slop = -1, fix_ends = TRUE, bases = p@rules@bases)
	} else {
		stop_wrap("Cannot identify the orientation of the unit")
	}

	p[sfc_index(p, index)] = unit
	p

})

#' @rdname sfc_index
#' @param bases Normally use [`BASE_LIST`].
#' @export
setMethod("sfc_flip_unit",
	signature = "sfc_unit",
	definition = function(p, bases) {

	unit = p
	orientation = unit_orientation(unit, "")

	if(orientation == "vertical") {
		unit = sfc_vflip(unit, fix_ends = TRUE, bases = bases)
	} else if(orientation == "horizontal") {
		unit = sfc_hflip(unit, fix_ends = TRUE, bases = bases)
	} else if(orientation == "diagonal_1") {
		unit = sfc_dflip(unit, slop = 1, fix_ends = TRUE, bases = bases)
	} else if(orientation == "diagonal_-1") {
		unit = sfc_dflip(unit, slop = -1, fix_ends = TRUE, bases = bases)
	} else {
		stop_wrap("Cannot identify the orientation of the unit")
	}

	unit
})


# just assume using R, L and I
# the side of the square is determined by the first L/R
guess_unit_corner = function(seq, which = "first") {
	n = length(seq)
	if(which == "first") {
		for(i in 2:n) {
			if(seq@seq[i] %in% c("R", "L")) {
				break
			}
		}
		fragment = seq[1:(i+2)]
		loc = sfc_segments(fragment)
		min_x = min(loc[, 1])
		max_x = max(loc[, 1])
		min_y = min(loc[, 2])
		max_y = max(loc[, 2])
		if(equal_to(loc[1,1], min_x) && equal_to(loc[1,2], min_y)) {
			"bottomleft"
		} else if(equal_to(loc[1,1], min_x) && equal_to(loc[1,2], max_y)) {
			"topleft"
		} else if(equal_to(loc[1,1], max_x) && equal_to(loc[1,2], min_y)) {
			"bottomright"
		} else if(equal_to(loc[1,1], max_x) && equal_to(loc[1,2], max_y)) {
			"topright"
		} else {
			""
		}
	} else {
		for(i in (n-1):1) {
			if(seq@seq[i] %in% c("R", "L")) {
				break
			}
		}

		fragment = seq[(i-2):n]
		loc = sfc_segments(fragment)
		k = nrow(loc)
		min_x = min(loc[, 1])
		max_x = max(loc[, 1])
		min_y = min(loc[, 2])
		max_y = max(loc[, 2])
		if(equal_to(loc[k,1], min_x) && equal_to(loc[k,2], min_y)) {
			"bottomleft"
		} else if(equal_to(loc[k,1], min_x) && equal_to(loc[k,2], max_y)) {
			"topleft"
		} else if(equal_to(loc[k,1], max_x) && equal_to(loc[k,2], min_y)) {
			"bottomright"
		} else if(equal_to(loc[k,1], max_x) && equal_to(loc[k,2], max_y)) {
			"topright"
		} else {
			""
		}
	}
}

