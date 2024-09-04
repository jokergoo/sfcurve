

#' Transformations of a sequence
#' 
#' @param p,e1 An `sfc_sequence` object.
#' @param rot,e2 Rotation measured in the polar coordinate system, in degrees.
#' 
#' @rdname sfc_transformation
#' @aliases sfc_rotate
#' 
#' @details
#' - `sfc_rotate()` and `^()` rotate each base pattern.
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
#' @export
`^.sfc_sequence` = function(e1, e2) {
	sfc_rotate(e1, e2)
}

validate_in_directions = function(p2, in_direction) {
	 
	corner = guess_unit_corner(p2, "first")
	if(corner  == "bottomleft") {
		if(in_direction == 180) {
			in_direction = in_direction + 90
		} else if(in_direction == 270) {
			in_direction = in_direction - 90
		}
	} else if(corner == "bottomright") {
		if(in_direction == 0) {
			in_direction = in_direction - 90
		} else if(in_direction == 270) {
			in_direction = in_direction + 90
		}
	} else if(corner == "topleft") {
		if(in_direction == 90) {
			in_direction = in_direction + 90
		} else if(in_direction == 180) {
			in_direction = in_direction - 90
		}
	} else if(corner == "topright") {
		if(in_direction == 0) {
			in_direction = in_direction + 90
		} else if(in_direction == 90) {
			in_direction = in_direction - 90
		}
	}
	in_direction %% 360
}

validate_out_directions = function(p2, out_direction) {
	 
	corner = guess_unit_corner(p2, "last")
	if(corner  == "bottomleft") {
		if(out_direction == 0) {
			out_direction = out_direction + 90
		} else if(out_direction == 90) {
			out_direction = out_direction - 90
		}
	} else if(corner == "bottomright") {
		if(out_direction == 180) {
			out_direction = out_direction - 90
		} else if(out_direction == 90) {
			out_direction = out_direction + 90
		}
	} else if(corner == "topleft") {
		if(out_direction == 270) {
			out_direction = out_direction + 90
		} else if(out_direction == 0) {
			out_direction = out_direction - 90
		}
	} else if(corner == "topright") {
		if(out_direction == 180) {
			out_direction = out_direction + 90
		} else if(out_direction == 270) {
			out_direction = out_direction - 90
		}
	}
	out_direction %% 360
}


#' @rdname sfc_transformation
#' @aliases sfc_hflip
#' @param fix_ends By default, the curve is flipped as a complete whole, which means, the associated entry and exit directions of the curve
#'       is also adjusted accordingly. When flipping subunits in a curve, e.g. level-1 subunits in a Peano curve, we want the 
#'       entry and exit direction of the subunit not changed so that the subunits are still connected in the curve after the flipping. In this 
#'       case, `fix_ends` can be set to `TRUE`, then only the subunits are flipped while the connections to neighbouring subunits are not affected. 
#'       See the **Examples** section.
#' @param bases A list of base patterns, consider to use [`BASE_LIST`]. It is only used when `fix_ends = TRUE`.
#' @export
#' @examples
#' p = sfc_3x3_meander("R", 2, rot = -90)
#' draw_multiple_curves(
#'     p, 
#'     sfc_hflip(p), 
#'     sfc_hflip(p, fix_ends = TRUE), 
#'     nrow = 1)
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

		in_direction = base_in_direction(bases[[ as.character(p@seq[1]) ]], p@rot[1])
		out_direction = base_out_direction(bases[[ as.character(p2@seq[1]) ]], p2@rot[1])
		if(abs(in_direction - out_direction) == 180) {
			in_direction = validate_in_directions(p2, in_direction)
		}
		lt = guess_base_pattern(bases, in_direction, out_direction)
		p2@seq[1] = lt[[1]]
		p2@rot[1] = as.integer(round(lt[[2]]))
		
		n = length(p)
		in_direction = base_in_direction(bases[[ as.character(p2@seq[n]) ]], p2@rot[n])
		out_direction = base_out_direction(bases[[ as.character(p@seq[n]) ]], p@rot[n])
		if(abs(in_direction - out_direction) == 180) {
			out_direction = validate_out_directions(p2, out_direction)
		}
		lt = guess_base_pattern(bases, in_direction, out_direction)
		p2@seq[n] = lt[[1]]
		p2@rot[n] = as.integer(round(lt[[2]]))

		if("J" %in% sfc_universe(p)) {
			if(p2@seq[2] == "I" && p2@seq[1] == "I") {
				p2@seq[1] = "J"
			} else if(p2@seq[2] == "J" && p2@seq[1] == "J") {
				p2@seq[1] = "I"
			}

			if(p2@seq[n-1] == "I" && p2@seq[n] == "I") {
				p2@seq[n] = "J"
			} else if(p2@seq[n-1] == "J" && p2@seq[n] == "J") {
				p2@seq[n] = "I"
			}
		}

		p2
	} else {
		p2 = p
		p2@seq[p@seq == "L"] = "R"
		p2@seq[p@seq == "R"] = "L"

		if("J" %in% sfc_universe(p)) {
			p2@seq[p@seq == "I"] = "J"
			p2@seq[p@seq == "J"] = "I"
		}

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
#' p = sfc_3x3_meander("L", 2, rot = -90)
#' draw_multiple_curves(
#'     p, 
#'     sfc_vflip(p), 
#'     sfc_vflip(p, fix_ends = TRUE), 
#'     nrow = 1)
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

		in_direction = base_in_direction(bases[[ as.character(p@seq[1]) ]], p@rot[1])
		out_direction = base_out_direction(bases[[ as.character(p2@seq[1]) ]], p2@rot[1])
		if(abs(in_direction - out_direction) == 180) {
			in_direction = validate_in_directions(p2, in_direction)
		}
		lt = guess_base_pattern(bases, in_direction, out_direction)
		p2@seq[1] = lt[[1]]
		p2@rot[1] = as.integer(round(lt[[2]]))

		n = length(p)
		in_direction = base_in_direction(bases[[ as.character(p2@seq[n]) ]], p2@rot[n])
		out_direction = base_out_direction(bases[[ as.character(p@seq[n]) ]], p@rot[n])
		if(abs(in_direction - out_direction) == 180) {
			out_direction = validate_out_directions(p2, out_direction)
		}
		lt = guess_base_pattern(bases, in_direction, out_direction)
		p2@seq[n] = lt[[1]]
		p2@rot[n] = as.integer(round(lt[[2]]))

		if("J" %in% sfc_universe(p)) {
			if(p2@seq[2] == "I" && p2@seq[1] == "I") {
				p2@seq[1] = "J"
			} else if(p2@seq[2] == "J" && p2@seq[1] == "J") {
				p2@seq[1] = "I"
			}

			if(p2@seq[n-1] == "I" && p2@seq[n] == "I") {
				p2@seq[n] = "J"
			} else if(p2@seq[n-1] == "J" && p2@seq[n] == "J") {
				p2@seq[n] = "I"
			}
		}

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
#' @param slop Slop of the diagonal. Value can only be 1 or -1.
#' @export
#' @examples
#' p = sfc_3x3_peano("I", 2)
#' draw_multiple_curves(
#'     p, 
#'     sfc_dflip(p, 1), 
#'     sfc_dflip(p, 1, fix_ends = TRUE), 
#'     nrow = 1)
setMethod("sfc_dflip",
	signature = "sfc_sequence",
	definition = function(p, slop = 1L, fix_ends = FALSE, bases = NULL) {

	slop = as.integer(slop)
	if(fix_ends) {
		if(is.null(bases)) {
			if(!inherits(p, "sfc_nxn")) {
				stop_wrap("`fix_ends = TURE` only works on the sfc_nxn objects.")
			}
			bases = p@rules@bases
		}

		p2 = sfc_dflip(p, slop = slop)

		in_direction = base_in_direction(bases[[ as.character(p@seq[1]) ]], p@rot[1])
		out_direction = base_out_direction(bases[[ as.character(p2@seq[1]) ]], p2@rot[1])
		lt = guess_base_pattern(bases, in_direction, out_direction)
		p2@seq[1] = lt[[1]]
		p2@rot[1] = as.integer(round(lt[[2]]))

		n = length(p)
		in_direction = base_in_direction(bases[[ as.character(p2@seq[n]) ]], p2@rot[n])
		out_direction = base_out_direction(bases[[ as.character(p@seq[n]) ]], p@rot[n])
		lt = guess_base_pattern(bases, in_direction, out_direction)
		p2@seq[n] = lt[[1]]
		p2@rot[n] = as.integer(round(lt[[2]]))

		if("J" %in% sfc_universe(p)) {
			if(p2@seq[2] == "I" && p2@seq[1] == "I") {
				p2@seq[1] = "J"
			} else if(p2@seq[2] == "J" && p2@seq[1] == "J") {
				p2@seq[1] = "I"
			}

			if(p2@seq[n-1] == "I" && p2@seq[n] == "I") {
				p2@seq[n] = "J"
			} else if(p2@seq[n-1] == "J" && p2@seq[n] == "J") {
				p2@seq[n] = "I"
			}
		}

		p2
	} else {
		if(slop == 1L) {
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

