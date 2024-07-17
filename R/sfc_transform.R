

#' Transformations of a sequence
#' 
#' @param p An `sfc_sequence` object.
#' @param rot Rotation measured in the polar coordinate system, in degree.
#' 
#' @rdname sfc_transformation
#' @aliases sfc_rotate
#' 
#' @details
#' - `sfc_rotate()` rotates each base pattern.
#' - `sfc_hflip()` flips a sequence horizontally.
#' - `sfc_vflip()` flips a sequence vertically.
#' - `sfc_dflio()` flips a sequence against a diagonal line.
#' @export
setMethod("sfc_rotate",
	signature = "sfc_sequence",
	definition = function(p, rot) {

	p@rot = (p@rot + as.integer(rot)) %% 360L
	p
})


#' @export
`^.sfc_sequence` = function(e1, e2) {
	sfc_rotate(e1, e2)
}


#' @rdname sfc_transformation
#' @aliases sfc_hflip
#' @param fix_ends Whether to keep the rotations of the two end base patterns?
#' @export
setMethod("sfc_hflip",
	signature = "sfc_sequence",
	definition = function(p, fix_ends = FALSE) {

	p2 = p
	p2@seq[p@seq == "L"] = "R"
	p2@seq[p@seq == "R"] = "L"

	p2@rot[p@rot == 90] = 270L
	p2@rot[p@rot == 270] = 90L

	p2@rot = p2@rot %% 360L

	if(fix_ends) {
		n = length(p)
		p2@rot[c(1, n)] = p@rot[c(1, n)]
	}
	p2
})


#' @rdname sfc_transformation
#' @aliases sfc_vflip
#' @export
setMethod("sfc_vflip",
	signature = "sfc_sequence",
	definition = function(p, fix_ends = FALSE) {
	p2 = sfc_hflip(sfc_rotate(p, 180L))

	if(fix_ends) {
		n = length(p)
		p2@rot[c(1, n)] = p@rot[c(1, n)]
	}
	p2
})

#' @rdname sfc_transformation
#' @aliases sfc_dflip
#' 
#' @param slop Slop of the diagonal.
#' 
#' @export
setMethod("sfc_dflip",
	signature = "sfc_sequence",
	definition = function(p, slop = 1, fix_ends = FALSE) {
	if(slop == 1) {
		p2 = sfc_rotate(sfc_hflip(p), -90L)
	} else {
		p2 = sfc_rotate(sfc_hflip(p), 90L)
	}

	if(fix_ends) {
		n = length(p)
		p2@rot[c(1, n)] = p@rot[c(1, n)]
	}
	p2
})


#' @rdname sfc_transformation
#' @aliases sfc_reverse
#' 
#' @export
setMethod("sfc_reverse",
	signature = "sfc_sequence", 
	definition = function(p) {
	p2 = p
	p2@seq = rev(p2@seq)
	p2@rot = rev(p2@rot)

	l1 = p2@seq == "I"
	if(any(l1)) {
		p2@rot[l1] = p2@rot[l1] + 180L
	}

	l2 = p2@seq == "R"
	l3 = p2@seq == "L"

	if(any(l2)) {
		p2@seq[l2] = "L"
		p2@rot[l2] = p2@rot[l2] + 90L
	}
	if(any(l3)) {
		p2@seq[l3] = "R"
		p2@rot[l3] = p2@rot[l3] - 90L
	}

	p2@rot = p2@rot %% 360L
	p2
})

#' @export
rev.sfc_pattern = function(x) {
	sfc_reverse(x)
}
