
#' Expand the curve to the next level
#' @aliases sfc_expand
#' @rdname sfc_expand
#' @param p An `sfc_hilbert` object.
#' @param code Transverse code, a single integer.
#' 
#' @details
#' For the Hilbert curve and Meander curve, as long as the transverse code of the first base pattern in the sequence is determinted, 
#' the transverse codes for other base patterns in the sequence are all determined. For the Peano curve, `code` is always 1.
#' 
#' @return An `sfc_hilbert` object.
#' @export
#' @examples
#' p = sfc_hilbert("I", 11)
#' sfc_expand(p, 1)
setMethod("sfc_expand",
	signature = "sfc_hilbert",
	definition = function(p, code) {

	seq = p@seq
	rot = p@rot
	n = length(p@seq)

	rules = p@rules

	tl = integer(n)
	tl[1] = code

	if(n > 1) {
		for(i in 2:n) {
			# here defines how a single pattern is expanded into four or more patterns
			tl[i] = transverse_type_2x2(tl[i-1], rot[i-1], rot[i])
		}
	}

	p2 = sfc_rotate(sfc_expand(rules, seq, tl), rep(rot, each = 4))
	
	p3 = new("sfc_hilbert")
	p3@seq = p2@seq
	p3@rot = p2@rot
	p3@universe = p2@universe
	p3@seed = p@seed
	p3@rules = p@rules
	p3@level = p@level + 1L
	p3@n = p@n
	p3@expansion = c(p@expansion, as.integer(code))
	p3

})


# t: previous type code
# r: prevous rotation
# r_next: next rotation
transverse_type_2x2 = function(t, r, r_next) {
	if((r_next - r) %% 180L == 0L) {
		t
	} else {
		c(2, 1)[t]
	}
}


#' @rdname sfc_expand
#' @export
#' @examples
#' p = sfc_peano("I", 11)
#' sfc_expand(p, 1)
setMethod("sfc_expand",
	signature = "sfc_peano",
	definition = function(p, code = 1) {

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
	# p2 is in the sfc_sequence class
	p2 = sfc_rotate(sfc_expand(rules, seq, tl), rep(rot, each = 9))
	
	p3 = new("sfc_peano")
	p3@seq = p2@seq
	p3@rot = p2@rot
	p3@universe = p2@universe
	p3@seed = p@seed
	p3@rules = p@rules
	p3@level = p@level + 1L
	p3@n = p@n
	p3@expansion = c(p@expansion, as.integer(code))
	p3

})


#' @rdname sfc_expand
#' @export
#' @examples
#' p = sfc_meander("I", 11)
#' sfc_expand(p, 1)
setMethod("sfc_expand",
	signature = "sfc_meander",
	definition = function(p, code) {

	seq = p@seq
	rot = p@rot
	n = length(p@seq)

	rules = p@rules

	tl = integer(n)
	tl[1] = code

	if(n > 1) {
		for(i in 2:n) {
			tl[i] = transverse_type_2x2(tl[i-1], rot[i-1], rot[i])
		}
	}
	p2 = sfc_rotate(sfc_expand(rules, seq, tl), rep(rot, each = 9))
	
	p3 = new("sfc_meander")
	p3@seq = p2@seq
	p3@rot = p2@rot
	p3@universe = p2@universe
	p3@seed = p@seed
	p3@rules = p@rules
	p3@level = p@level + 1L
	p3@n = p@n
	p3@expansion = c(p@expansion, as.integer(code))
	p3

})
