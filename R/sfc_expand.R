
#' Expand the curve to the next level
#' @aliases sfc_expand
#' @rdname sfc_expand
#' @param p An `sfc_2x2` object or other related objects.
#' @param code Expansion code, a single integer.
#' 
#' @details
#' For the Hilbert curve and Meander curve, as long as the expansion code of the first base pattern in the sequence is determinted, 
#' the expansion codes for other base patterns in the sequence are all determined. For the Peano curve, since
#' there is only one traverse path on any level, `code` is ignored.
#' 
#' These functions are mainly used internally.
#' 
#' @return An object in the same class as the input.
#' @export
#' @examples
#' p = sfc_2x2("I", 11)
#' sfc_expand(p, 2) # I|211
setMethod("sfc_expand",
	signature = "sfc_2x2",
	definition = function(p, code, flip = FALSE) {
    # argument flip is not used
	seq = p@seq
	rot = p@rot
	n = length(p@seq)

	rules = p@rules

	tl = integer(n)
	tl[1] = code

	if(n > 1) {
		for(i in 2:n) {
			# here defines how a single pattern is expanded into four or more patterns
			tl[i] = traverse_type_2x2(tl[i-1], rot[i-1], rot[i])
		}
	}

	sfc_expand_by_rules(rules, p, code = tl, flip = FALSE)

})


# t: previous type code
# r: prevous rotation
# r_next: next rotation
traverse_type_2x2 = function(t, r, r_next) {
	if((r_next - r) %% 180L == 0L) {
		t
	} else {
		c(2, 1)[t]
	}
}


#' @rdname sfc_expand
#' @param flip Whethe to flip level-1 units? The value should be a logical vector of length one or the same as the length of `p`.
#' @export
#' @examples
#' p = sfc_3x3_peano("I", 11)
#' sfc_expand(p, 2) # I|211
setMethod("sfc_expand",
	signature = "sfc_3x3_peano",
	definition = function(p, code = 1, flip = FALSE) {

	seq = p@seq
	rot = p@rot
	n = length(p@seq)

	rules = p@rules

	code = 1

	tl = integer(n)
	tl[1] = code

	if(n > 1) {
		for(i in 2:n) {
			tl[i] = code
		}
	}

	sfc_expand_by_rules(rules, p, code = tl, flip = flip)

})


#' @rdname sfc_expand
#' @export
#' @examples
#' p = sfc_3x3_meander("I", 11)
#' sfc_expand(p, 2) # I|211
setMethod("sfc_expand",
	signature = "sfc_3x3_meander",
	definition = function(p, code, flip = FALSE) {

	seq = p@seq
	rot = p@rot
	n = length(p@seq)

	rules = p@rules

	tl = integer(n)
	tl[1] = code

	if(n > 1) {
		for(i in 2:n) {
			tl[i] = traverse_type_2x2(tl[i-1], rot[i-1], rot[i])
		}
	}
	
	sfc_expand_by_rules(rules, p, code = tl, flip = flip)

})
