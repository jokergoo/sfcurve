

# initial: a vector of letters, a character strong, an sfc_initial object
# type: 0/1/a character scalar
sfc = function(initial, type = "11", increase = 2, rot = 0) {

	if(increase != 2) {
		stop("`increase` currently only be 2 (2x2 expansion)")
	}

	if(length(type) == 1) {
		if(is.numeric(type)) {
			if(type > 2) {
				type = as.character(type)
			}
		}
	}
	if(is.character(type)) {
		type = as.integer(strsplit(type[1], "")[[1]])
	}

	if(inherits(initial, "character")) {
		initial = sfc_initial(initial, rot)
	}

	if(inherits(initial, "sfc_expand")) {
		initial = p[c("seq", "rot")]
		class(initial) = c("sfc_initial", "sfc_sequence")
	}

	p = initial

	# most time-consuming part
	for(i in seq_along(type)) {
		p = sfc_expand(p, type[i], increase = increase)
	}

	p$id = sum(2^(which(rev(type) == 2)-1))
	p$initial = initial

	class(p) = union("sfc_expand", class(p))

	p
	
}

print.sfc_expand = function(x, ...) {
	print.sfc_sequence(x, ...)
	cat("\n")
	print.sfc_initial(x$initial, ...)
	cat("With expanding type code:\n")
	cat("  (", paste(x$type_code, collapse = ", "), ")\n", sep = "")
}


sfc_level = function(p) {
	if(inherits(p, "sfc_initial")) {
		return(0)
	}

	if(inherits(p, "sfc_expand")) {
		return(length(p$type_code))
	}
}
