


#' Validate the sequence
#' @aliases sfc_validate
#' @rdname sfc_validate
#' @param p An `sfc_sequence` object or a character string. If it is a character string, rotations are all taken as zeros.
#' @param by One of `sfc_2x2`, `sfc_3x3_peano` and `sfc_3x3_meander`.
#' 
#' @details
#' It is mainly used to validate a seed sequence whether they follow the forward-left-right rule.
#' 
#' @export
#' @examples
#' try(sfc_validate("LLLLL"))
#' try(sfc_validate(sfc_sequence("IIIII", rot = c(0, 90, 180, 270, 0), 
#'         universe = sfc_universe(SFC_RULES_2x2))))
setMethod("sfc_validate",
	signature = "sfc_sequence",
	definition = function(p, by = "sfc_2x2") {

	n = length(p@seq)

	if(by %in% c("sfc_2x2", "sfc_3x3_meander")) {

		if(by == "sfc_2x2") {
			if(!sfc_is_compatible(p, SFC_RULES_2x2)) {
				stop_wrap("The universe of `p` is not compatible with `SFC_RULES_2x2`")
			}
		} else if(by == "sfc_3x3_meander") {
			if(!sfc_is_compatible(p, SFC_RULES_3x3_MEANDER)) {
				stop_wrap("The universe of `p` is not compatible with `SFC_RULES_3x3_MEANDER`")
			}
		}

		ind = which(p@seq == "U")
		if(length(ind)) {
			if( !(n == 1 || (n == 2 && length(ind) == 2)) ) {
				stop_wrap("If U is in a sequence, the sequence should only be U (singlet) or UU (doublet).")
			}
		}

		ind = which(p@seq == "C")
		if(length(ind)) {
			if(n > 1) {
				stop_wrap("If C is in a sequence, the sequence should only be C (singlet).")
			}
		}

		ind = which(p@seq %in% c("B", "D"))
		if(length(ind)) {
			if(!identical(ind, 1L)) {
				stop_wrap("If B/D is in a sequence, B/D should only be the start of the sequence.")
			}
		}

		ind = which(p@seq %in% c("P", "Q"))
		if(length(ind)) {
			if(!identical(ind, n)) {
				stop_wrap("If P/Q is in a sequence, P/Q should only be the end of the sequence.")
			}
		}
	} else if(by %in% "sfc_3x3_peano") {
		if(!sfc_is_compatible(p, SFC_RULES_3x3_PEANO)) {
			stop_wrap("The universe of `p` is not compatible with `SFC_RULES_3x3_PEANO`")
		}
	}

	pos = sfc_segments(p)
	n = nrow(pos)

	if(n > 1) {
		if(any(duplicated(pos))) {
			stop_wrap("Crossing is not allowed in the sequence.")
		} 

		# # two points should be in the same row or the same column
		# l = pos[1:(n-1), 1] == pos[2:n, 1] | pos[1:(n-1), 2] == pos[2:n, 2]
		# if(any(!l)) {
		# 	stop_wrap("Found gaps in the sequences.")
		# }
	}

	return(TRUE)
})


#' @rdname sfc_validate
#' @export
setMethod("sfc_validate",
	signature = "character",
	definition = function(p, by = "sfc_2x2") {

	if(by == "sfc_2x2") {
		p = sfc_sequence(p, universe = sfc_universe(SFC_RULES_2x2))
	} else if(by == "sfc_3x3_peano") {
		p = sfc_sequence(p, universe = sfc_universe(SFC_RULES_3x3_PEANO))
	} else if(by == "sfc_3x3_meander") {
		p = sfc_sequence(p, universe = sfc_universe(SFC_RULES_3x3_MEANDER))
	}

	sfc_validate(p, by = by)
})


