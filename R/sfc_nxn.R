

`[.sfc_nxn` = function(x, i) {
	if(length(i) == 0) {
		return(x)
	}
	`[.sfc_sequence`(x, sfc_index(x, i))
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

#' Get the indicies of points in the curve
#' @aliases sfc_index
#' @rdname sfc_index
#' @param p An `sfc_nxn` object.
#' @param index A string representing the path of transverse the hierarchy of the curve. The left side
#'       corresponds to the lower level and the right side corresponds to the high level in the curve. For the
#'       Hilbert curve, the digits can only be 1-4, and for the Peano and Meander curves, the digites can be 1-9.
#' @export
#' @examples
#' # only for testing
#' p = sfc_hilbert("I", "11111")
#' om = par(no.readonly = TRUE)
#' par(mfrow = c(2, 2))
#' test_sfc_index(p, "3")
#' test_sfc_index(p, "32")
#' test_sfc_index(p, "321")
#' test_sfc_index(p, "3211")
#' par(om)
#' 
#' p = sfc_meander("I", "11111")
#' om = par(no.readonly = TRUE)
#' par(mfrow = c(2, 2))
#' test_sfc_index(p, "7")
#' test_sfc_index(p, "75")
#' test_sfc_index(p, "759")
#' test_sfc_index(p, "7592")
#' par(om)
setMethod("sfc_index",
	signature = "sfc_nxn",
	definition = function(p, index) {

	if(is.numeric(index)) {
		return(index)
	} else if(length(index) == 0) {
		return(index)
	} else {
		get_index_from_nxn(as.character(index), p@level, p@n)
	}
})


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

unit_orientation = function(p, index) {
	if(!is.character(index)) {
		stop_wrap("`index` should be a character scalar.")
	}
	if(nchar(index) < 5) {
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

	n = nrow(loc)
	# min_x = min(loc[, 1])
	# max_x = max(loc[, 1])
	# min_y = min(loc[, 2])
	# max_y = max(loc[, 2])

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

flip_unit = function(p, index) {
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

}
