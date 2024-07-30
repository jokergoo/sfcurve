

#' Generate a nxn curve based on expansion rules
#' 
#' @param rules An `sfc_rules` object.
#' @param name Name of the curve.
#' @param envir Environment where the functions are exported to.
#' @param verbose Whether to print messages?
#' 
#' @details
#' Two functions will be exported:
#' 
#' - `sfc_{name}()`
#' - `draw_rules_{name}()`
#' 
#' For the simplicity, flippng is not supported yet.
#' 
#' @export
#' @examples
#' UNIVERSE_4x4_PEANO = c("I", "R", "L")
#'
#' RULES_4x4_PEANO = list()
#' RULES_4x4_PEANO[["I"]][[1]] = sfc_unit("RIILLIIRRIILLIIR", rot = 0, universe = UNIVERSE_4x4_PEANO)
#' RULES_4x4_PEANO[["I"]][[2]] = sfc_hflip(RULES_4x4_PEANO[["I"]][[1]])
#' RULES_4x4_PEANO[["R"]][[1]] = sfc_unit("IIIRRIILLIIRRIIL", rot = 0, universe = UNIVERSE_4x4_PEANO)      
#' RULES_4x4_PEANO[["R"]][[2]] = sfc_rotate(sfc_unit("LIIRRIILLIIRRIII", 
#'                                 rot = 270, universe = UNIVERSE_4x4_PEANO), 90)
#' RULES_4x4_PEANO[["L"]][[1]] = sfc_hflip(RULES_4x4_PEANO[["R"]][[2]])
#' RULES_4x4_PEANO[["L"]][[2]] = sfc_hflip(RULES_4x4_PEANO[["R"]][[1]])
#'
#' SFC_RULES_4x4_PEANO = sfc_rules(rules = RULES_4x4_PEANO,
#'         name = "Peano 4x4",
#'         bases = BASE_LIST[UNIVERSE_4x4_PEANO])
#' sfc_generator(SFC_RULES_4x4_PEANO, "4x4_peano")
#' draw_rules_4x4_peano()
#' sfc_4x4_peano("I", 111) |> plot()
sfc_generator = function(rules, name, envir = topenv(parent.frame()), verbose = TRUE) {

	RULES = rules
	NAME = name

	if(!(identical(sort(names(RULES@rules)), c("I", "L", "R")) || identical(sort(names(RULES@rules)), c("I", "J", "L", "R")))) {
		stop_wrap("`rules` should only contain base patterns I/J/R/L.")
	}

	if(any(sapply(RULES@rules, length) > 2)) {
		stop_wrap("`rules` should only contain one or two transverse codes for a single base pattern.")
	}

	for(nm in names(RULES@rules)) {
		for(i in seq_along(RULES@rules[[nm]])) {
			pa = get_one_transverse_path(RULES, RULES@rules[[nm]][[i]])
			if(length(pa) == 0) {
				stop_wrap("Cannot find a complete transverse path for rule ", nm, "_", i)
			}
		}
	}

	code = as.vector(unique(t(do.call(cbind, lapply(RULES@rules, function(x) sapply(x, function(y) sort(y@corner)))))))
	if(!(identical(code, c(1L, 2L)) || identical(code, c(1L, 2L, 1L, 2L)) || identical(code, c(2L, 1L, 2L, 1L)))) {
		stop_wrap("`rules` can only contain 11|22 or 12|21 level-1 units.")
	}

	if(identical(code, c(1L, 2L))) {
		code_num = 2
	} else {
		code_num = 1
	}

	if(code_num == 2) {
		for(nm in names(RULES@rules)) {
			if(!identical(RULES@rules[[nm]][[1]]@corner, c(1L, 2L))) {
				stop_wrap("The first pattern should have corner value of [1, 2].")
			}
		}
	}

	cl = paste0("sfc_", name)

	setClass(cl,
		contains = "sfc_nxn",
		prototype = list(n = sfc_mode(RULES)), where = envir)

	sfc_fun = function(seed, code = integer(0), rot = 0L) {

		code = .parse_code(code, 1:2)

		if(inherits(seed, "character")) {
			seed = sfc_seed(seed, rot = rot, universe = sfc_universe(RULES))
		} else if(inherits(seed, "sfc_seed")) {
			seed@seq = factor(as.vector(seed@seq), levels = sfc_universe(RULES))
		} else {
			seed = sfc_seed(seq = seed@seq, rot = seed@rot)
		}

		p = as(seed, cl)
		p@seed = seed
		p@level = 0L
		p@n = sfc_mode(RULES)

		for(i in seq_along(code)) {
			p = sfc_expand(p, code[i])
		}
		
		p@expansion = as.integer(code)
		p@universe = sfc_universe(RULES)
		p
		
	}

	setAs("sfc_sequence", cl, function(from) {
		p = new(cl)
		p@seq = from@seq
		levels(p@seq) = sfc_universe(RULES)
		p@rot = from@rot
		p@universe = sfc_universe(RULES)
		p@level = 0L
		p@n = sfc_mode(RULES)
		p@rules = RULES

		p
	}, where = envir)

	setMethod("sfc_expand",
		signature = cl,
		definition = function(p, code) {

		seq = p@seq
		rot = p@rot
		n = length(p@seq)

		rules = p@rules

		tl = integer(n)

		if(code_num == 2) {
			tl[1] = code
			if(n > 1) {
				for(i in 2:n) {
					tl[i] = transverse_type_2x2(tl[i-1], rot[i-1], rot[i])
				}
			}
		} else {
			tl = rep(1L, n)
		}

		sfc_expand_by_rules(rules, p, code = tl)

	}, where = envir)

	draw_rules = function() {

	    p = sfc_fun("I")

	    grid.newpage()

	    gb1 = grob_single_base_rule(p, flip = FALSE, "I", x = size, y = unit(1, "npc") - size, just = c("left", "top"))
	    nc = length(gb1$children)
	    gb1$children[[nc]]$width = gb1$children[[nc]]$width + unit(sfc_mode(p)^2 * 4.5, "mm")
	    grid.draw(gb1)

	    if("J" %in% sfc_universe(p)) {
	    	gb2 = grob_single_base_rule(p, flip = FALSE, "J", x = size, y = unit(1, "npc") - size - gb1$vp$height, just = c("left", "top"))
		    nc = length(gb2$children)
		    gb2$children[[nc]]$width = gb2$children[[nc]]$width + unit(sfc_mode(p)^2 * 4.5, "mm")
		    grid.draw(gb2)

		    gb3 = grob_single_base_rule(p, flip = FALSE, "R", x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height, just = c("left", "top"))
		    nc = length(gb3$children)
		    gb3$children[[nc]]$width = gb3$children[[nc]]$width + unit(sfc_mode(p)^2 * 4.5, "mm")
		    grid.draw(gb3)

		    gb4 = grob_single_base_rule(p, flip = FALSE, "L", x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height - gb3$vp$height, just = c("left", "top"))
		    nc = length(gb4$children)
		    gb4$children[[nc]]$width = gb4$children[[nc]]$width + unit(sfc_mode(p)^2 * 4.5, "mm")
		    grid.draw(gb4)
	    } else {

		    gb2 = grob_single_base_rule(p, flip = FALSE, "R", x = size, y = unit(1, "npc") - size - gb1$vp$height, just = c("left", "top"))
		    nc = length(gb2$children)
		    gb2$children[[nc]]$width = gb2$children[[nc]]$width + unit(sfc_mode(p)^2 * 4.5, "mm")
		    grid.draw(gb2)

		    gb3 = grob_single_base_rule(p, flip = FALSE, "L", x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height, just = c("left", "top"))
		    nc = length(gb3$children)
		    gb3$children[[nc]]$width = gb3$children[[nc]]$width + unit(sfc_mode(p)^2 * 4.5, "mm")
		    grid.draw(gb3)
		}

	}

	assign(paste0("sfc_", NAME), sfc_fun, envir = envir)
	assign(paste0("draw_rules_", NAME), draw_rules, envir = envir)
	if(verbose) {
		cat("The following two functions are exported to the current top environment:\n", sep = "")
		cat("  - ", paste0("sfc_", NAME), "()\n", sep = "")
		cat("  - ", paste0("draw_rules_", NAME), "()\n", sep = "")
	}

}
