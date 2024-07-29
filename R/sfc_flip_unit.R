
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


#' @rdname sfc_flip_unit
#' @details
#' For `unit_orientation()`, it first reduce the unit to level-1, then it checks the orientation
#' of the line connected by the in-corner and out-corner.
#' @return
#' `unit_orientation()` returns a string one of "vertical", "horizontal", "diagonal_1" and "diagonal_-1".
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
		
		unit = p[index]
		loc = reduce_loc_to_level_1(unit, p@n, Inf)
		
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

#' Flip units
#' @rdname sfc_flip_unit
#' @aliases sfc_flip_unit
#' @param p The corresponding object.
#' @param index A string of digits representing the path on the hierarchy of the curve. The left 
#'         side corresponds to the lower level and the right side corresponds to the high level 
#'         in the curve. For the Hilbert curve, the digits can only be 1-4, and for the Peano and 
#'         Meander curves, the digites can be 1-9. See examples in [`sfc_index()`]. The value can also
#'         be a vector where each flipping is applied in sequence.
#' @param to The orientation to flip to. If the specified unit already has such orientation, the function returns
#'       the original curve.
#' @details
#' An unit in the curve is represented as a square block (`2^k x 2^k` for the Hilbert curve and `3^k x 3^k` for the Peano and Meander curves, `k` between 1 and the level of the curve).
#' In the Hilbert curve, if an unit can be flipped, it is symmetric, thus flipping in the Hilbert curve does not change its form.
#' The flipping is mainly applied on the Peano curve and the Meander curves. Peano curve only allows flippings by the diagonals and the Meander
#' curve only allows flipping horizontally or vertically. The type of flipping is choosen automatically in the function.
#' @export
#' @return
#' `sfc_flip_unit` returns an `sfc_nxn` object.
#' @examples
#' p = sfc_meander("I", 11)
#' draw_multiple_curves(
#'     p, 
#'     sfc_flip_unit(p, "1"), # bottom left
#'     sfc_flip_unit(p, "2"), # bottom middle
#'     sfc_flip_unit(p, "3"), # bottom right
#'     title = FALSE, nrow = 2)
#' 
#' p = sfc_peano("I", level = 3)
#' draw_multiple_curves(
#'     p, 
#'     sfc_flip_unit(p, ""),
#'     sfc_flip_unit(p, "2"),
#'     sfc_flip_unit(p, "21"),
#'     title = FALSE, nrow = 2)
#' 
#' p = sfc_peano("I", level = 2)
#' draw_multiple_curves(p, 
#'     sfc_flip_unit(p, c("4", "7")),
#'     sfc_flip_unit(p, c("1", "2", "3", "5", "6", "8", "9")),
#'     title = FALSE, nrow = 1)
setMethod("sfc_flip_unit",
	signature = "sfc_nxn",
	definition = function(p, index = "", to = NULL) {

	if(length(p) < (p@n^2)^p@level) {
		if(!length(p) %in% (p@n^2)^(seq_len(p@level))) {
			stop_wrap("Since `p` is only a fragment of the curve, it should be represented as a square with length of ", paste((p@n^2)^(seq_len(p@level)), collapse = ", "), ".")
		}
	}

	if(!is.character(index)) {
		stop_wrap("`index` should be in character.")
	}

	if(length(index) > 1) {
		for(i in index) {
			p = sfc_flip_unit(p, i)
		}
		return(p)
	}

	unit = p[index]
	orientation = unit_orientation(p, index)

	if(identical(orientation, to)) {
		return(p)
	}

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

#' @rdname sfc_flip_unit
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
