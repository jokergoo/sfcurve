

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
#' - `sfc_dflip()` flips a sequence against a diagonal line (with slop 1 or -1).
#' @export
#' @return An `sfc_sequence` object.
setMethod("sfc_rotate",
	signature = "sfc_sequence",
	definition = function(p, rot) {

	p@rot = (p@rot + as.integer(rot)) %% 360L
	p
})


#' @rdname sfc_transformation
#' @param e1 An `sfc_sequence` object.
#' @param e2 The rotation.
#' @export
`^.sfc_sequence` = function(e1, e2) {
	sfc_rotate(e1, e2)
}


#' @rdname sfc_transformation
#' @aliases sfc_hflip
#' @param fix_ends Whether to keep the orientation and rotations of the two end base patterns (start and end)?
#' @param bases A list of base patterns, consider to use [`BASE_LIST`]. It is only used when `fix_ends = TRUE`.
#' @export
#' @examples
#' p = sfc_meander("R", 2, rot = -90)
#' draw_multiple_curves(p, sfc_hflip(p), sfc_hflip(p, fix_ends = TRUE), title = FALSE)
setMethod("sfc_hflip",
	signature = "sfc_sequence",
	definition = function(p, fix_ends = FALSE, bases = NULL) {

	if(fix_ends) {
		if(is.null(bases)) {
			if(!inherits(p, "sfc_nxn")) {
				stop_wrap("`fix_ends = TURE` only works on the sfc_nxn objects.")
			}
			bases = p@rules@bases
		}

		p2 = sfc_hflip(sfc_reverse(p))

		lt = guess_base_pattern(bases, base_in_direction(bases[[ as.character(p@seq[1]) ]], p@rot[1]),
			                           base_out_direction(bases[[ as.character(p2@seq[1]) ]], p2@rot[1]))
		p2@seq[1] = lt[[1]]
		p2@rot[1] = as.integer(round(lt[[2]]))

		n = length(p)
		lt = guess_base_pattern(bases, base_in_direction(bases[[ as.character(p2@seq[n]) ]], p2@rot[n]),
			                           base_out_direction(bases[[ as.character(p@seq[n]) ]], p@rot[n]))
		p2@seq[n] = lt[[1]]
		p2@rot[n] = as.integer(round(lt[[2]]))

		p2
	} else {
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
	}
})


#' @rdname sfc_transformation
#' @aliases sfc_vflip
#' @export
#' @examples
#' p = sfc_meander("L", 2, rot = -90)
#' draw_multiple_curves(p, sfc_vflip(p), sfc_vflip(p, fix_ends = TRUE), title = FALSE)
setMethod("sfc_vflip",
	signature = "sfc_sequence",
	definition = function(p, fix_ends = FALSE, bases = NULL) {

	if(fix_ends) {
		if(is.null(bases)) {
			if(!inherits(p, "sfc_nxn")) {
				stop_wrap("`fix_ends = TURE` only works on the sfc_nxn objects.")
			}
			bases = p@rules@bases
		}

		p2 = sfc_vflip(sfc_reverse(p))

		lt = guess_base_pattern(bases, base_in_direction(bases[[ as.character(p@seq[1]) ]], p@rot[1]),
			                           base_out_direction(bases[[ as.character(p2@seq[1]) ]], p2@rot[1]))
		p2@seq[1] = lt[[1]]
		p2@rot[1] = as.integer(round(lt[[2]]))

		n = length(p)
		lt = guess_base_pattern(bases, base_in_direction(bases[[ as.character(p2@seq[n]) ]], p2@rot[n]),
			                           base_out_direction(bases[[ as.character(p@seq[n]) ]], p@rot[n]))
		p2@seq[n] = lt[[1]]
		p2@rot[n] = as.integer(round(lt[[2]]))

		p2
	} else {
		p2 = sfc_hflip(sfc_rotate(p, 180L))

		if(fix_ends) {
			n = length(p)
			p2@rot[c(1, n)] = p@rot[c(1, n)]
		}
		p2
	}
})

#' @rdname sfc_transformation
#' @aliases sfc_dflip
#' 
#' @param slop Slop of the diagonal.
#' 
#' @export
#' p = sfc_peano("I", 2)
#' draw_multiple_curves(p, sfc_dflip(p, 1), sfc_dflip(p, 1, fix_ends = TRUE), title = FALSE)
setMethod("sfc_dflip",
	signature = "sfc_sequence",
	definition = function(p, slop = 1, fix_ends = FALSE, bases = NULL) {

	if(fix_ends) {
		if(is.null(bases)) {
			if(!inherits(p, "sfc_nxn")) {
				stop_wrap("`fix_ends = TURE` only works on the sfc_nxn objects.")
			}
			bases = p@rules@bases
		}

		p2 = sfc_dflip(p, slop = slop)

		lt = guess_base_pattern(bases, base_in_direction(bases[[ as.character(p@seq[1]) ]], p@rot[1]),
			                           base_out_direction(bases[[ as.character(p2@seq[1]) ]], p2@rot[1]))
		p2@seq[1] = lt[[1]]
		p2@rot[1] = as.integer(round(lt[[2]]))

		n = length(p)
		lt = guess_base_pattern(bases, base_in_direction(bases[[ as.character(p2@seq[n]) ]], p2@rot[n]),
			                           base_out_direction(bases[[ as.character(p@seq[n]) ]], p@rot[n]))
		p2@seq[n] = lt[[1]]
		p2@rot[n] = as.integer(round(lt[[2]]))

		p2
	} else {
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
	}
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

	l1 = p2@seq == "I" | p2@seq == "J"
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

#' @rdname sfc_transformation
#' @param x An `sfc_sequence` object.
#' @export
rev.sfc_sequence = function(x) {
	sfc_reverse(x)
}

