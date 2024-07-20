


#' Validate the sequence
#' @aliases sfc_validate
#' @rdname sfc_validate
#' @param p An `sfc_sequence` object.
#' @param by One of `sfc_hilbert`, `sfc_peano` and `sfc_meander`.
#' 
#' @details
#' It is mainly used to validate a seed sequence whether they follow the forward-left-right rule.
#' 
#' @export
setMethod("sfc_validate",
	signature = "sfc_sequence",
	definition = function(p, by = "sfc_hilbert") {

	n = length(p@seq)

	if(by %in% c("sfc_hilbert", "sfc_meander")) {

		if(by == "sfc_hilbert") {
			if(!sfc_is_compatible(p, SFC_RULES_HILBERT)) {
				stop_wrap("The universe of `p` is not compatible with `SFC_RULES_HILBERT`")
			}
		} else if(by == "sfc_meander") {
			if(!sfc_is_compatible(p, SFC_RULES_MEANDER)) {
				stop_wrap("The universe of `p` is not compatible with `SFC_RULES_MEANDER`")
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
	} else if(by %in% "sfc_peano") {
		if(!sfc_is_compatible(p, SFC_RULES_PEANO)) {
			stop_wrap("The universe of `p` is not compatible with `SFC_RULES_PEANO`")
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

