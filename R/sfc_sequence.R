

#' Constructor of the sfc_sequence class
#' 
#' @param seq A sequence of base patterns. The value can be a vector of letters or a string.
#' @param rot The corresponding rotations of base patterns. If it has length one and the sequence contains R/L/I (right/left/forward),
#'          `rot` controls the rotation of the first base pattern and the rotations for remaining base patterns in the sequence are automatically
#'          identified.
#' @param universe The universe of base patterns. A vector of letters.
#' 
#' @details
#' This funtion is very low-level. Normally, users do not need to directly use this constructor.
#' 
#' @export
#' @examples
#' sfc_sequence("ABCD", rot = c(0, 90, 180, 270), universe = c("A", "B", "C", "D"))
sfc_sequence = function(seq, rot = 0L, universe = NULL) {

	if(is.factor(seq)) {
		universe = levels(seq)
	} else {
		if(length(seq) == 1) {
			seq = strsplit(seq, "")[[1]]
		}
		if(is.null(universe)) {
			seq = factor(seq)
			universe = levels(seq)
		} else {
			seq = factor(seq, levels = universe)
		}
	}

	n = length(seq)

	rot = as.integer(rot)
	if(length(rot) == 1) {
		rot2 = integer(n)
		rot2[1] = rot[1]
		if(n > 1) {
			for(i in 2:n) {
				rot2[i] = next_rotation(seq[i-1], rot2[i-1])
			}
		}
		rot = rot2
	}

	diff = setdiff(levels(seq), universe)
	if(length(diff)) {
		stop_wrap("all letters should be included in universe.")
	}
	
	p = new("sfc_sequence")
	p@seq = seq
	p@rot = rot %% 360L
	p@universe = universe


	p
}

#' @rdname sfc_sequence
#' @details
#' `sfc_seed` class is the same as the `sfc_sequence` class. It is used specifically as the "seed sequence"
#' when generating the curves. 
#' @export
sfc_seed = function(seq, rot = 0L, universe = NULL) {
	p = sfc_sequence(seq, rot, universe)
	p2 = new("sfc_seed")
	p2@seq = p@seq
	p2@rot = p@rot
	p2@universe = p@universe
	p2
}



#' @rdname sfc_sequence
#' @details
#' `sfc_unit` class is also the same as the `sfc_sequence` class. It is used specifically when defining the expansion rules. 
#' @export
sfc_unit = function(seq, rot = 0L, universe = NULL) {
	p = sfc_sequence(seq, rot, universe)

	p2 = new("sfc_unit")
	p2@seq = p@seq
	p2@rot = p@rot
	p2@universe = p@universe

	p2
}



# rotation of the direction of approaching
next_rotation = function(letter, rot) {
	if(letter == "R") {
		return(rot - 90L)
	} else if(letter == "L") {
		return(rot + 90L)
	} else if(letter == "U") {
		return(rot + 180L)
	} else {
		return(rot)
	}
}


# subset allows an incomplete curve

#' Utility functions
#' @rdname utility
#' @param x An `sfc_sequence` object.
#' @param i Index.
#' @param value An `sfc_sequence` object.
#' @param ... A list of `sfc_sequence` objects or other arguments.
#' 
#' @export
`[.sfc_sequence` = function(x, i) {
	x2 = new("sfc_sequence")
	x2@seq = x@seq[i]
	x2@rot = x@rot[i]
	x2@universe = x@universe
	x2
}


#' @rdname utility
#' @export
`[<-.sfc_sequence` = function(x, i, value) {
	x@seq[i] = value@seq
	x@rot[i] = value@rot
	x
}

#' @rdname utility
#' @export
length.sfc_sequence = function(x) {
	length(x@seq)
}

#' @rdname utility
#' @details
#' For efficiency, `c.sfc_sequence()` does not check whether the input `sfc_sequence` objects are compatible.
#' @export
c.sfc_sequence = function(...) {
	lt = list(...)
	seq = do.call("c", lapply(lt, function(x) x@seq))
	rot = do.call("c", lapply(lt, function(x) x@rot))
	
	p = new("sfc_sequence")
	p@seq = seq
	p@rot = rot
	p@universe = lt[[1]]@universe

	p
}


#' @param object The corresponding object.
#' @rdname show
#' @export
setMethod("show",
	signature = "sfc_sequence",
	definition = function(object) {

	n = length(object)
	nr = ceiling(n/8)

	seq = object@seq
	rot = object@rot

	rot_str = function(x) {
		# ifelse(x == 0, "  0", ifelse(x == 90, " 90", x))
		x
	}

	cat("A sequence of ", n, " base pattern", ifelse(n == 1, ".", "s."), "\n", sep = "")
	if(nr > 4) {
		flag = 0
		for(i in 1:nr) {
			if(i >= 3 && i <= nr-2) {
				if(!flag) cat("  .... other ", nr-4, " line", ifelse(nr-4 > 1, "s", ""), " ....\n", sep = "")
				flag = TRUE
				next
			}
			ind = seq( (i-1)*8+1, min(i*8, n) )
			cat("  ")
			for(k in seq_along(ind)) {
				cat(paste0(seq[ ind[k] ], "(", rot_str(rot[ ind[k] ]), ")"))
				if(k == 4) {
					cat("  ")
				}
			}
			cat("\n")
		}
	} else {
		for(i in 1:nr) {
			ind = seq( (i-1)*8+1, min(i*8, n) )
			cat("  ")
			for(k in seq_along(ind)) {
				cat(paste0(seq[ ind[k] ], "(", rot_str(rot[ ind[k] ]), ")"))
				if(k == 4) {
					cat("  ")
				}
			}
			cat("\n")
		}
	}
})

#' @rdname sfc_universe
#' @export
setMethod("sfc_universe",
	signature = "sfc_sequence",
	definition = function(p) {
	p@universe
})

#' Whether two sfc_sequence objects are compatible
#' @aliases sfc_is_compatible
#' @rdname sfc_is_compatible
#' @param p An `sfc_sequence` object.
#' @param p2 An `sfc_sequence` object.
#' 
#' @details
#' The function compares whether the two universe base pattern sets are identical.
#' Note the order of universe base patterns should also be identical.
#' @export
#' @examples
#' p1 = sfc_hilbert("I")
#' p2 = sfc_hilbert("R")
#' sfc_is_compatible(p1, p2)
#' 
#' p1 = sfc_sequence("ABC")
#' p2 = sfc_sequence("DEF")
#' sfc_is_compatible(p1, p2)
setMethod("sfc_is_compatible",
	signature = "sfc_sequence",
	definition = function(p, p2) {
	identical(sfc_universe(p), sfc_universe(p2))
})


