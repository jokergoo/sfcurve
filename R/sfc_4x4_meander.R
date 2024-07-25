
setClass("sfc_4x4_meander",
	slots = c("type" = "integer"),
	contains = "sfc_nxn")

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
#' @param code A vector of the transverse code. The left side corresponds to the lower levels of the curve and the right side corresponds to the higher level of the curve.
#'      The value can be set as a vector e.g. `c(1, 2, 1)`, or as a string e.g. `"121"`, or as a number e.g. `121`.
#' @param rot Rotation of the seed sequence, measured in the polar coordinate system, in degrees.
#' @param flip The same setting as in [`sfc_peano()`] or [`sfc_meander()`].
#' @param type Which type of rules to use? 1 for [`SFC_RULES_MEANDER_4x4_1`] and 2 for [`SFC_RULES_MEANDER_4x4_2`].
#' 
#' @details
#' It is an extension of the 3x3 Meander curves. For simplicity, it only supports `I/R/L` base patterns.
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
		seed = sfc_seed(seed, rot = rot, universe = sfc_universe(SFC_RULES_MEANDER_4x4_1))
	} else if(inherits(seed, "sfc_seed")) {
		seed@seq = factor(as.vector(seed@seq), levels = sfc_universe(SFC_RULES_MEANDER_4x4_1))
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

	if(length(code) == 1) { # only expand to level 1
		p@flip = p@flip[1]
	}

	for(i in seq_along(code)) {
		p = sfc_expand(p, code[i])
	}

	p@universe = sfc_universe(SFC_RULES_MEANDER_4x4_1)
	p
	
}

setAs("sfc_seed", "sfc_4x4_meander_1", function(from) {
	p = new("sfc_4x4_meander_1")
	p@seq = from@seq
	p@rot = from@rot
	p@universe = from@universe
	p@rules = SFC_RULES_MEANDER_4x4_1
	p@level = 0L
	p@n = 4L
	p@flip = rep(FALSE, 16)

	p
})

setAs("sfc_seed", "sfc_4x4_meander_2", function(from) {
	p = new("sfc_4x4_meander_2")
	p@seq = from@seq
	p@rot = from@rot
	p@universe = from@universe
	p@rules = SFC_RULES_MEANDER_4x4_2
	p@level = 0L
	p@n = 4L
	p@flip = rep(FALSE, 16)

	p
})

#' @rdname sfc_4x4_meander
#' @param p An `sfc_4x4_meander` object.
#' @param code Transverse code, a single integer.
#' @export
setMethod("sfc_expand",
	signature = "sfc_4x4_meander",
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

	sfc_expand_by_rules(rules, p, code = tl, flip = p@flip)

})

#' @rdname sfc_4x4_meander
#' @export
#' @examples
#' draw_rules_4x4_meander(type = 1)
#' draw_rules_4x4_meander(type = 2)
draw_rules_4x4_meander = function(type = 1) {

    p = sfc_4x4_meander("I", type = type)

    grid.newpage()

    gb1 = grob_single_base_rule(p, "I", x = size, y = unit(1, "npc") - size, just = c("left", "top"))
    nc = length(gb1$children)
    gb1$children[[nc]]$width = gb1$children[[nc]]$width + unit(70, "mm")
    grid.draw(gb1)

    gb2 = grob_single_base_rule(p, "R", x = size, y = unit(1, "npc") - size - gb1$vp$height, just = c("left", "top"))
    nc = length(gb2$children)
    gb2$children[[nc]]$width = gb2$children[[nc]]$width + unit(70, "mm")
    grid.draw(gb2)

    gb3 = grob_single_base_rule(p, "L", x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height, just = c("left", "top"))
    nc = length(gb3$children)
    gb3$children[[nc]]$width = gb3$children[[nc]]$width + unit(70, "mm")
    grid.draw(gb3)

}

