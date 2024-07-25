
setClass("sfc_3x3_combined",
	contains = "sfc_nxn")


#' General 3x3 space-filling curves 
#' 
#' @rdname sfc_3x3_combined
#' @param seed The seed sequence. In most cases, the seed sequence is a single base pattern, which can be specified as a single letter, then `rot` controls
#'      the initial rotation of the base pattern. It also supports a sequence with more than one base patterns as the seed sequence. In this case,
#'      it can be specified as a string of more than one base letters, then `rot` can be set to a single rotation scalar which controls the rotation of the
#'      first letter, or a vector with the same length as the number of base letters.
#' @param rot Rotation of the seed sequence, measured in the polar coordinate system, in degrees.
#' @param level Level of the curve. Currently it is restricted to an integer less than 6.
#' @param flip The same setting as in [`sfc_peano()`] or [`sfc_meander()`].
#' 
#' @details
#' This type of 3x3 curve uses the combintation of base patterns from both the Peano curve and the Meander curve.
#' On each level, the transverse path is randomly selected.
#' @export
#' @examples
#' draw_multiple_curves(
#'     sfc_3x3_combined("I", level = 3),
#'     sfc_3x3_combined("I", level = 3),
#'     sfc_3x3_combined("I", level = 3),
#'     nrow = 1, title = FALSE
#' )
sfc_3x3_combined = function(seed, level = 0, rot = 0L, flip = FALSE) {

	if(level > 5) {
		stop_wrap("`level` should be not larger than 5.")
	}

	if(inherits(seed, "character")) {
		seed = sfc_seed(seed, rot = rot, universe = sfc_universe(SFC_RULES_3x3_COMBINED))
	} else if(inherits(seed, "sfc_seed")) {
		levels(seed@seq) = sfc_universe(SFC_RULES_3x3_COMBINED)
	} else {
		seed = sfc_seed(seq = seed@seq, rot = seed@rot)
	}

	p = as(seed, "sfc_3x3_combined")
	p@seed = seed
	p@level = 0L
	p@n = 3L

	if(is.logical(flip)) {
		if(length(flip) == 1) {
			flip = rep(flip, p@n^2)
		}
		if(length(flip) != p@n^2) {
			stop_wrap("Length of `flip` should be a logical vector of length 1 or 9.")
		}
	} else {
		if(!is.function(flip)) {
			stop_wrap("`flip` can only be a logical vector or a function.")
		}

	}
	p@flip = flip

	if(level == 1) { # only expand to level 1
		p@flip = flip[1]
	}

	for(i in seq_len(level)) {
		p = sfc_expand(p)
	}

	p@universe = sfc_universe(seed)
	p
	
}

setAs("sfc_seed", "sfc_3x3_combined", function(from) {
	p = new("sfc_3x3_combined")
	p@seq = from@seq
	p@rot = from@rot
	p@universe = from@universe
	p@rules = SFC_RULES_3x3_COMBINED
	p@level = 0L
	p@n = 3L
	p@flip = rep(FALSE, 9)

	p
})

#' @rdname sfc_3x3_combined
#' @param p An `sfc_3x3_combined` object.
#' @param code Ignore. The transverse code is selected randomly.
#' @export
setMethod("sfc_expand",
	signature = "sfc_3x3_combined",
	definition = function(p, code = NULL) {

	seq = p@seq
	rot = p@rot
	n = length(p@seq)

	rules = p@rules

	tl = get_one_transverse_path(rules, p)
	if(!is.null(code)) {
		tl = as.integer(code)
	}

	k = 0
	while(length(tl) == 0 && k < 10) {
		tl = get_one_transverse_path(rules, p)
		k = k + 1
	}

	sfc_expand_by_rules(rules, p, code = tl, flip = p@flip)

})

#' @rdname sfc_3x3_combined
#' @export
#' @examples
#' draw_rules_3x3_combined()
draw_rules_3x3_combined = function(flip = FALSE) {

    p = sfc_3x3_combined("I", flip = flip)

    grid.newpage()

    gb1 = grob_single_base_rule(p, "I", x = size, y = unit(1, "npc") - size, just = c("left", "top"))
    nc = length(gb1$children)
    gb1$children[[nc]]$width = gb1$children[[nc]]$width + unit(25, "mm")
    grid.draw(gb1)

    gb2 = grob_single_base_rule(p, "R", x = size, y = unit(1, "npc") - size - gb1$vp$height, just = c("left", "top"))
    nc = length(gb2$children)
    gb2$children[[nc]]$width = gb2$children[[nc]]$width + unit(25, "mm")
    grid.draw(gb2)

    gb3 = grob_single_base_rule(p, "L", x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height, just = c("left", "top"))
    nc = length(gb3$children)
    gb3$children[[nc]]$width = gb3$children[[nc]]$width + unit(25, "mm")
    grid.draw(gb3)

    gb4 = grob_single_base_rule(p, "U", x = size + gb1$vp$width + size + unit(25, "mm"), y = unit(1, "npc") - size, just = c("left", "top"))
    nc = length(gb4$children)
    gb4$children[[nc]]$width = gb4$children[[nc]]$width + unit(35, "mm")
    grid.draw(gb4)

	gb5 = grob_single_base_rule(p, "B", x = size + gb1$vp$width + size + unit(25, "mm"), y = unit(1, "npc") - size - gb4$vp$height, just = c("left", "top"))
    nc = length(gb5$children)
    gb5$children[[nc]]$width = gb5$children[[nc]]$width + unit(35, "mm")
    grid.draw(gb5)

    gb6 = grob_single_base_rule(p, "D", x = size + gb1$vp$width + size + unit(25, "mm"), y = unit(1, "npc") - size - gb4$vp$height - gb5$vp$height, just = c("left", "top"))
    nc = length(gb6$children)
    gb6$children[[nc]]$width = gb6$children[[nc]]$width + unit(35, "mm")
    grid.draw(gb6)

    gb7 = grob_single_base_rule(p, "P", x = size + gb1$vp$width + size + unit(25, "mm"), y = unit(1, "npc") - size - gb4$vp$height - gb5$vp$height - gb6$vp$height, just = c("left", "top"))
    nc = length(gb7$children)
    gb7$children[[nc]]$width = gb7$children[[nc]]$width + unit(35, "mm")
    grid.draw(gb7)

	gb8 = grob_single_base_rule(p, "Q", x = size + gb1$vp$width + size + unit(25, "mm"), y = unit(1, "npc") - size - gb4$vp$height - gb5$vp$height - gb6$vp$height - gb7$vp$height, just = c("left", "top"))
    nc = length(gb8$children)
    gb8$children[[nc]]$width = gb8$children[[nc]]$width + unit(35, "mm")
    grid.draw(gb8)

    gb9 = grob_single_base_rule(p, "C", x = size + gb1$vp$width + size + unit(25, "mm"), y = unit(1, "npc") - size - gb4$vp$height - gb5$vp$height - gb6$vp$height - gb7$vp$height - gb8$vp$height, just = c("left", "top"))
    nc = length(gb9$children)
    gb9$children[[nc]]$width = gb9$children[[nc]]$width + unit(35, "mm")
    grid.draw(gb9)

}

