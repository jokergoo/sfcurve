
setClass("sfc_4x4_meander",
	slots = c("type" = "integer"),
	contains = "sfc_nxn",
	prototype = list(n = 4L))

setClass("sfc_4x4_meander_1",
	contains = "sfc_4x4_meander",
	prototype = list(type = 1L))

setClass("sfc_4x4_meander_2",
	contains = "sfc_4x4_meander",
	prototype = list(type = 2L))


#' 4x4 space-filling curves in meander type
#' 
#' @rdname sfc_4x4_meander
#' @param seed The seed sequence. In most cases, the seed sequence is a single base pattern, which can be specified as a single letter, then `rot` controls
#'      the initial rotation of the base pattern. It also supports a sequence with more than one base patterns as the seed sequence. In this case,
#'      it can be specified as a string of more than one base letters, then `rot` can be set to a single rotation scalar which controls the rotation of the
#'      first letter, or a vector with the same length as the number of base letters.
#' @param code A vector of the expansion code. The left side corresponds to the higher levels (more to the top-level) of the curve and the right side corresponds to the lower level (more to the bottom-level) of the curve.
#'      The value can be set as a vector e.g. `c(1, 2, 1)`, or as a string e.g. `"121"`, or as a number e.g. `121`.
#' @param rot Rotation of the seed sequence, measured in the polar coordinate system, in degrees.
#' @param flip The same setting as in [`sfc_3x3_peano()`] or [`sfc_3x3_meander()`].
#' @param type Which type of rules to use? 1 for [`SFC_RULES_4x4_MEANDER_1`] and 2 for [`SFC_RULES_4x4_MEANDER_2`].
#' 
#' @details
#' It is an extension of the 3x3 Meander curves to mode 4. For simplicity, it only supports `I/R/L` base patterns.
#' @export
#' @examples
#' draw_multiple_curves(
#'     sfc_4x4_meander("I", "11", type = 1),
#'     sfc_4x4_meander("I", "12", type = 1),
#'     sfc_4x4_meander("I", "11", type = 2),
#'     sfc_4x4_meander("I", "12", type = 2),
#'     nrow = 2
#' )
#' seed = paste(rep(paste0("R", sapply(0:10, function(i) strrep("I", i))), each = 2), collapse="")
#' sfc_4x4_meander(seed, 1) |> plot()
sfc_4x4_meander = function(seed, code = integer(0), rot = 0L, flip = FALSE, type = 1L) {

	code = .parse_code(code, 1:2)

	if(inherits(seed, "character")) {
		seed = sfc_seed(seed, rot = rot, universe = sfc_universe(SFC_RULES_4x4_MEANDER_1))
	} else if(inherits(seed, "sfc_seed")) {
		seed@seq = factor(as.vector(seed@seq), levels = sfc_universe(SFC_RULES_4x4_MEANDER_1))
	} else {
		seed = sfc_seed(seq = seed@seq, rot = seed@rot)
	}

	if(type == 1) {
		p = as(seed, "sfc_4x4_meander_1")
	} else {
		p = as(seed, "sfc_4x4_meander_2")
	}
	p@seed = seed
	p@level = 0L
	p@n = 4L

	if(is.logical(flip)) {
		if(!(length(flip) == length(seed) || length(flip) == 16 || length(flip) == 1)) {
			stop_wrap("If `flip` is a logical vector, it should have a length the same as `seed` or 16\n")
		}
	}

	if(is.function(flip)) {
		for(i in seq_along(code)) {
			p = sfc_expand(p, code[i], flip = flip(p))
		}
	} else {
		for(i in seq_along(code)) {
		
			if(i == 1) {
				if(length(flip) == length(seed)) {
					p = sfc_expand(p, code[i], flip = flip)
				} else {
					p = sfc_expand(p, code[i], flip = flip[1])
				}
			} else if (i == 2) {
				if(length(flip) == 16) {

				} else {
					flip = rep(flip, each = 16)
				}
				
				p = sfc_expand(p, code[i], flip = flip)
			} else {
				flip = rep(flip, each = 16)
				p = sfc_expand(p, code[i], flip = flip)
			}
		}
	}

	p@expansion = as.integer(code)
	p@universe = sfc_universe(SFC_RULES_4x4_MEANDER_1)
	p
	
}

setAs("sfc_sequence", "sfc_4x4_meander_1", function(from) {
	p = new("sfc_4x4_meander_1")
	p@seq = from@seq
	levels(p@seq) = sfc_universe(SFC_RULES_4x4_MEANDER_1)
	if(any(is.na(p@seq))) {
		stop_wrap("Base letters should all be in `sfc_universe(SFC_RULES_4x4_MEANDER_1)`.")
	}
	p@rot = from@rot
	p@universe = sfc_universe(SFC_RULES_4x4_MEANDER_1)
	p@level = 0L
	p@n = 4L
	p@rules = SFC_RULES_4x4_MEANDER_1

	p
})

setAs("sfc_sequence", "sfc_4x4_meander_2", function(from) {
	p = new("sfc_4x4_meander_2")
	p@seq = from@seq
	levels(p@seq) = sfc_universe(SFC_RULES_4x4_MEANDER_2)
	if(any(is.na(p@seq))) {
		stop_wrap("Base letters should all be in `sfc_universe(SFC_RULES_4x4_MEANDER_2)`.")
	}
	p@rot = from@rot
	p@universe = sfc_universe(SFC_RULES_4x4_MEANDER_2)
	p@level = 0L
	p@n = 4L
	p@rules = SFC_RULES_4x4_MEANDER_2

	p
})

#' @rdname sfc_4x4_meander
#' @param p An `sfc_4x4_meander` object.
#' @export
setMethod("sfc_expand",
	signature = "sfc_4x4_meander",
	definition = function(p, code, flip = FALSE) {

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

	sfc_expand_by_rules(rules, p, code = tl, flip = flip)

})

#' @rdname sfc_4x4_meander
#' @export
#' @examples
#' draw_rules_4x4_meander(type = 1)
#' draw_rules_4x4_meander(type = 2)
draw_rules_4x4_meander = function(type = 1, flip = FALSE) {

    p = sfc_4x4_meander("I", type = type, flip = flip)

    grid.newpage()

    if(flip) {
        rules = p@rules@flip
    } else {
        rules = p@rules@rules
    }
    equation_max_width = max(do.call("unit.c", lapply(names(rules), function(nm) {
        do.call("unit.c", lapply(seq_along(rules[[nm]]), function(i) {
            convertWidth(grobWidth(grob_math(tex_pattern(nm, i,  rules[[nm]][[i]]), x = 0, y = 0)), "mm")
        }))
    })))

    gb1 = grob_single_base_rule(p, "I", equation_max_width = equation_max_width, flip = flip, x = size, y = unit(1, "npc") - size, just = c("left", "top"))
    nc = length(gb1$children)
    gb1$children[[nc]]$width = gb1$children[[nc]]$width 
    grid.draw(gb1)

    gb2 = grob_single_base_rule(p, "R", equation_max_width = equation_max_width, flip = flip, x = size, y = unit(1, "npc") - size - gb1$vp$height, just = c("left", "top"))
    nc = length(gb2$children)
    gb2$children[[nc]]$width = gb2$children[[nc]]$width 
    grid.draw(gb2)

    gb3 = grob_single_base_rule(p, "L", equation_max_width = equation_max_width, flip = flip, x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height, just = c("left", "top"))
    nc = length(gb3$children)
    gb3$children[[nc]]$width = gb3$children[[nc]]$width
    grid.draw(gb3)

}

