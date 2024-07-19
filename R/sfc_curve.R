
#' Create space-filling curves
#' 
#' @param seed The seed sequence. In most cases, the seed sequence is a single base pattern, which can be specified as a single letter, then `rot` controls
#'      the initial rotation of the base pattern. It also supports a sequence with more than one base patterns as the seed sequence. In this case,
#'      it can be specified as a string of more than one base letters, then `rot` can be set to a single rotation scalar which controls the rotation of the
#'      first letter, or a vector with the same length as the number of base letters.
#' @param code A vector of the transverse code. The left side corresponds to the lower levels of the curve and the right side corresponds to the higher level of the curve.
#'      The value can be set as a vector e.g. `c(1, 2, 1)`, or as a string e.g. `"121"`, or as a number e.g. `121`.
#' @param rot Rotation of the seed sequence, measured in the polar coordinate system, in degrees.
#' 
#' @details
#' `sfc_hilbert()` generates the Hilbert curve from the seed sequence.
#' `sfc_peano()` generates the Peano curve from the seed sequence.
#' `sfc_meander()` generates the Meander curve from the seed sequence.
#' 
#' @rdname spacefilling
#' @return
#' `sfc_hilbert()` returns an `sfc_hilbert` object.
#' `sfc_peano()` returns an `sfc_peano` object.
#' `sfc_meander()` returns an `sfc_meander` object.
#' @export
#' @import methods
#' @examples
#' sfc_hilbert("I", "111") |> plot()
#' sfc_hilbert("I", "111", rot = 90) |> plot()
#' sfc_hilbert("IR", "111", rot = 90) |> plot()
sfc_hilbert = function(seed, code = integer(0), rot = 0L) {

	code = .parse_code(code, 1:2)

	if(inherits(seed, "character")) {
		seed = sfc_seed(seed, rot = rot, universe = sfc_universe(SFC_RULES_HILBERT))
	} else if(inherits(seed, "sfc_seed")) {
		seed@seq = factor(as.vector(seed@seq), levels = sfc_universe(SFC_RULES_HILBERT))
	} else {
		seed = sfc_seed(seq = seed@seq, rot = seed@rot)
	}

	p = as(seed, "sfc_hilbert")
	p@seed = seed
	p@level = 0L
	p@n = 2L
	p@flip = c(FALSE, FALSE, FALSE, FALSE)

	for(i in seq_along(code)) {
		p = sfc_expand(p, code[i])
	}

	p@universe = sfc_universe(SFC_RULES_HILBERT)
	p
	
}

.parse_code = function(code, full) {
	if(length(code) == 1) {
		if(is.numeric(code)) {
			if(code > 2) {
				code = as.character(code)
			}
		}
	}
	if(is.character(code)) {
		code = as.integer(strsplit(code[1], "")[[1]])
	}

	if(any(!code %in% 1:2)) {
		stop_wrap(paste0("`code` should only contain ", paste(code, collapse = ", "), "."))
	}

	code
}

#' @rdname spacefilling
#' @param flip Whether to usethe "flipped" rules? For the Peano curve and the Meander curve, there is also a "fliiped" version 
#'      of curve expansion rules. On each level expansion in the Peano curve and the Meander curve, a point expands to nine points in 
#'      3x3 grids. Thus the value of `flip` can be set as a logical vector of length of nine that controls whether to use the flipped expansion
#'      for the corresponding unit. Besides such "1-to-9" mode, `flip` can also be set as a function which acccepts the number of current points in the curve and return
#'      a logical vector with the same length, i.e. the "all-to-all*9" mode.
#' @export
#' @examples
#' sfc_peano("I", "111") |> plot()
#' sfc_peano("I", "111", 
#'     flip = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)) |> plot()
#' sfc_peano("IJ", "111") |> plot()
#'
#' # set `flip` to a function
#' sfc_peano("I", 1111, flip = function(n) {
#'     if(n == 1) {
#'         return(FALSE)
#'     }
#'     l = rep(FALSE, n)
#'     portion = 1
#'     while(portion*9 <= n) {
#'         ind = ((1:(n/3/portion))*3*portion)[rep(c(TRUE, TRUE, FALSE), n/9/portion)]
#'         l[ind + 1] = TRUE
#'         portion = portion*9
#'     }
#'     l
#' }) |> plot()
#' 
#' sfc_peano("I", 1111, rot = 90, flip = function(n) {
#'     if(n == 1) {
#'         return(FALSE)
#'     }
#'     l = rep(FALSE, n)
#'     portion = 1
#'     while(portion*9 <= n) {
#'         ind = ((1:(n/3/portion))*3*portion)[rep(c(TRUE, TRUE, FALSE), n/9/portion)]
#'         l[ind + 1] = TRUE
#'         portion = portion*9
#'     }
#'     l
#' }) |> plot()
sfc_peano = function(seed, code = integer(0), rot = 0L, flip = FALSE) {

	code = .parse_code(code, 1L)

	if(inherits(seed, "character")) {
		seed = sfc_seed(seed, rot = rot, universe = sfc_universe(SFC_RULES_PEANO))
	} else if(inherits(seed, "sfc_seed")) {
		levels(seed@seq) = sfc_universe(SFC_RULES_PEANO)
	} else {
		seed = sfc_seed(seq = seed@seq, rot = seed@rot)
	}

	p = as(seed, "sfc_peano")
	p@seed = seed
	p@level = 0L
	p@n = 3L

	if(is.logical(flip)) {
		if(length(flip) == 1) {
			flip = rep(flip, p@n^2)
		}
		if(length(flip) != p@n^2) {
			stop_wrap("Length of `flip` should be a logical vector of length 1 or 9.")
		}
	} else {
		if(!is.function(flip)) {
			stop_wrap("`flip` can only be a logical vector or a function.")
		}
	}
	p@flip = flip

	for(i in seq_along(code)) {
		p = sfc_expand(p, code[i])
	}

	p@universe = sfc_universe(seed)
	p
	
}

#' @rdname spacefilling
#' @export
#' @examples
#' sfc_meander("I", "111") |> plot()
#' sfc_meander("I", "111", 
#'     flip = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)) |> plot()
#' sfc_meander("IR", "111") |> plot()
sfc_meander = function(seed, code = integer(0), rot = 0L, flip = FALSE) {

	code = .parse_code(code, 1:2)

	if(inherits(seed, "character")) {
		seed = sfc_seed(seed, rot = rot, universe = sfc_universe(SFC_RULES_MEANDER))
	} else if(inherits(seed, "sfc_seed")) {
		levels(seed@seq) = sfc_universe(SFC_RULES_MEANDER)
	} else {
		seed = sfc_seed(seq = seed@seq, rot = seed@rot)
	}

	p = as(seed, "sfc_meander")
	p@seed = seed
	p@level = 0L
	p@n = 3L

	if(length(flip) == 1) {
		flip = rep(flip, p@n^2)
	}
	if(length(flip) != p@n^2) {
		stop_wrap("Length of `flip` should be a logical vector of length 1 or 9.")
	}
	p@flip = flip

	for(i in seq_along(code)) {
		p = sfc_expand(p, code[i])
	}

	p@universe = sfc_universe(seed)
	p
	
}

#' The level of the curve
#' @aliases sfc_level
#' @rdname sfc_level
#' @param p An `sfc_nxn` object.
#'
#' @return An integer.
#' @export
#' @examples
#' p = sfc_hilbert("I", "11")
#' sfc_level(p)
#' 
#' p = sfc_hilbert("I", "1111")
#' sfc_level(p)
setMethod("sfc_level",
	signature = "sfc_nxn", 
	definition = function(p) {
	p@level
})


setAs("sfc_seed", "sfc_hilbert", function(from) {
	p = new("sfc_hilbert")
	p@seq = from@seq
	p@rot = from@rot
	p@universe = from@universe
	p@rules = SFC_RULES_HILBERT
	p@level = 0L
	p@n = 2L
	p@flip = rep(FALSE, 4)

	if(length(setdiff(p@seq, sfc_universe(SFC_RULES_HILBERT)))) {
		stop("Letters should all be in `SFC_RULES_HILBERT`.")
	}

	p
})


setAs("sfc_seed", "sfc_peano", function(from) {
	p = new("sfc_peano")
	p@seq = from@seq
	p@rot = from@rot
	p@universe = from@universe
	p@rules = SFC_RULES_PEANO
	p@level = 0L
	p@n = 3L
	p@flip = rep(FALSE, 9)

	p
})

setAs("sfc_seed", "sfc_meander", function(from) {
	p = new("sfc_meander")
	p@seq = from@seq
	p@rot = from@rot
	p@universe = from@universe
	p@rules = SFC_RULES_MEANDER
	p@level = 0L
	p@n = 3L
	p@flip = rep(FALSE, 9)

	p
})

setAs("sfc_seed", "sfc_nxn", function(from) {
	p = new("sfc_nxn")
	p@seq = from@seq
	p@rot = from@rot
	p@universe = from@universe
	p@rules = SFC_RULES_MEANDER
	p@level = 0L

	p
})


#' @param object The corresponding object.
#' @rdname show
#' @export
setMethod("show",
	signature = "sfc_nxn",
	definition = function(object) {

	cat("An", class(object)[1], "object.\n")
	cat("  Increase mode: ", object@n, " x ", object@n, "\n", sep = "")
	cat("  Level: ", object@level, "\n", sep = "")
	cat("  Expansion rule:", object@rules@name, "\n")
	if(is.logical(object@flip)) {
		cat("  Unit flip:", paste(ifelse(object@flip, "T", "F"), collapse = ""), "\n")
	} else {
		cat("  Unit flip: a self-defined function\n")
	}
	if(length(object) < (object@n^2)^object@level) {
		cat("  A fragment from the original curve.\n")
	}
	cat("\n")

	callNextMethod(object)
	
})
