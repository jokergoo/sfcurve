
#' Create space-filling curves
#' 
#' @param seed The seed sequence. In most cases, the seed sequence is a single base pattern, which can be specified as a single letter, then `rot` controls
#'      the initial rotation of the base pattern. It also supports a sequence with more than one base patterns as the seed sequence. In this case,
#'      it can be specified as a string of more than one base letters, then `rot` can be set to a single rotation scale which controls the rotation of the
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
#' 
#' sfc_peano("I", "111") |> plot()
#' sfc_peano("IJ", "111") |> plot()
#' 
#' sfc_meander("I", "111") |> plot()
#' sfc_meander("IR", "111") |> plot()
sfc_hilbert = function(seed, code = integer(0), rot = 0L) {

	code = .parse_code(code, 1:2)

	if(inherits(seed, "character")) {
		seed = sfc_seed(seed, rot = rot, universe = sfc_universe(SFC_RULES_HILBERT))
	} else if(inherits(seed, "sfc_seed")) {
		levels(seed@seq) = sfc_universe(SFC_RULES_HILBERT)
	} else {
		seed = sfc_seed(seq = seed@seq, rot = seed@rot)
	}

	p = as(seed, "sfc_hilbert")
	p@seed = seed
	p@level = 0L
	p@n = 2L

	for(i in seq_along(code)) {
		p = sfc_expand(p, code[i])
	}

	p@universe = sfc_universe(seed)
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
#' @param flip_rules Whether to usethe "flipped" rules? For the Peano curve and the Meander curve, there is also a "fliiped" version 
#'      of curve expansion rules. See the vignettes for details.
#' @export
sfc_peano = function(seed, code = integer(0), rot = 0L, flip_rules = FALSE) {

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

	if(flip_rules) {
		p@rules = SFC_RULES_PEANO_FLIP
	}

	for(i in seq_along(code)) {
		p = sfc_expand(p, code[i])
	}

	p@universe = sfc_universe(seed)
	p
	
}

#' @rdname spacefilling
#' @export
sfc_meander = function(seed, code = integer(0), rot = 0L, flip_rules = FALSE) {

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

	if(flip_rules) {
		p@rules = SFC_RULES_MEANDER_FLIP
	}

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

	p
})


#' Print the object
#' 
#' @param object The corresponding object.
#' @rdname show
#' @export
setMethod("show",
	signature = "sfc_nxn",
	definition = function(object) {

	cat("An", class(object)[1], "object.\n")
	cat("  Increase mode: ", object@n, " x ", object@n, "\n", sep = "")
	cat("  Expansion rule:", object@rules@name, "\n")
	cat("\n")

	callNextMethod(object)
	
})
