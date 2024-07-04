

# given a sequence, validate the following
# - the position of "closed" patterns
# - interrupt
# - crossing
# in theory this is a universal function which can be applied to all levels
# but mostly it is used to validate a small sequence.
#
# if a long sequence is expanded from a short initial sequence, the growth rule
# ensures the curve is valid if the initial sequence is valid.
sfc_validate = function(p) {

	n = length(p$seq)

	if(length(setdiff(p$seq, BASE_PATTERN_LEVELS))) {
		stop("Base patterns should be from I/R/L/U/C/B/D/P/Q.")
	}

	if(length(setdiff(p$rot, c(0, 90, 180, 270)))) {
		stop("rotation can only be multiple of 90.")
	}

	ind = which(p$seq == "U")
	if(length(ind)) {
		if( !(n == 1 || (n == 2 && length(ind) == 2)) ) {
			stop_wrap("If U is in a sequence, the sequence should only be U (singlet) or UU (doublet).")
		}
	}

	ind = which(p$seq == "C")
	if(length(ind)) {
		if(n > 1) {
			stop_wrap("If C is in a sequence, the sequence should only be C (singlet).")
		}
	}

	ind = which(p$seq %in% c("B", "D"))
	if(length(ind)) {
		if(!identical(ind, 1L)) {
			stop("If B/D is in a sequence, B/D should only be the start of the sequence.")
		}
	}

	ind = which(p$seq %in% c("P", "Q"))
	if(length(ind)) {
		if(!identical(ind, n)) {
			stop("If P/Q is in a sequence, P/Q should only be the end of the sequence.")
		}
	}

	pos = sfc_transverse(p, c(0, 0))
	n = nrow(pos)

	if(n > 1) {
		if(any(duplicated(pos))) {
			stop("Crossing is not allowed in the sequence.")
		} 

		# two points should be in the same row or the same column
		l = pos[1:(n-1), 1] == pos[2:n, 1] | pos[1:(n-1), 2] == pos[2:n, 2]
		if(any(!l)) {
			stop("Found gaps in the sequences.")
		}
	}

	return(TRUE)
}
