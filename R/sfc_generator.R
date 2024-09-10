

#' Generate a nxn curve based on expansion rules
#' 
#' @param rules An `sfc_rules` object.
#' @param name Name of the curve. The name will be used for the functions that will be generated.
#' @param envir Environment where the functions are exported.
#' @param flippable Whether `rules` can have flipped version? If it is `TRUE`, the generated function also accepts the `flip` argument.
#' @param verbose Whether to print messages?
#' 
#' @details
#' Two functions will be exported:
#' 
#' - `sfc_{name}()`
#' - `draw_rules_{name}()`
#' 
#' For the simplicity, flipping is not supported yet.
#' 
#' @export
#' @return No value is returned.
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
#' 
#' sfc_generator(SFC_RULES_4x4_PEANO, "4x4_peano")
#' draw_rules_4x4_peano()
#' sfc_4x4_peano("I", 111) |> plot()
sfc_generator = function(rules, name, envir = topenv(parent.frame()), 
	flippable = FALSE, verbose = TRUE) {

	RULES = rules
	NAME = name

	if(!(identical(sort(names(RULES@rules)), c("I", "L", "R")) || identical(sort(names(RULES@rules)), c("I", "J", "L", "R")))) {
		stop_wrap("`rules` should only contain base patterns I/J/R/L.")
	}

	if(any(sapply(RULES@rules, length) > 2)) {
		stop_wrap("`rules` should only contain one or two traverse codes for a single base pattern.")
	}

	for(nm in names(RULES@rules)) {
		for(i in seq_along(RULES@rules[[nm]])) {
			pa = get_one_traverse_path(RULES, RULES@rules[[nm]][[i]])
			if(length(pa) == 0) {
				stop_wrap("Cannot find a complete traverse path for rule ", nm, "_", i)
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

	if(flippable) {
		flip = lapply(RULES@rules, function(x) {
            lapply(x, function(u) sfc_flip_unit(u, RULES@bases))
        })

        if(identical(RULES@rules[[1]][[1]], flip[[1]][[1]])) {
        	if(verbose) {
        		message("The flipped rules are identical to the original rules. Flipping will not be supported.")
        	}
        } else {
        	RULES@flip = flip
        }
	}

	cl = paste0("sfc_", name)

	setClass(cl,
		contains = "sfc_nxn",
		prototype = list(mode = sfc_mode(RULES)), where = envir)

	sfc_fun = function(seed, code = integer(0), rot = 0L, flip = FALSE) {

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
		p@mode = sfc_mode(RULES)

		if(length(p@rules@flip) == 0) {
			for(i in seq_along(code)) {
				p = sfc_expand(p, code[i])
			}
		} else {
			if(is.logical(flip)) {
				if(!(length(flip) == length(seed) || length(flip) == p@mode^2 || length(flip) == 1)) {
					stop_wrap(paste0("If `flip` is a logical vector, it should have a length the same as `seed` or ", p@mode^2, "\n"))
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
						if(length(flip) == p@mode^2) {

						} else {
							flip = rep(flip, each = p@mode^2)
						}
						
						p = sfc_expand(p, code[i], flip = flip)
					} else {
						flip = rep(flip, each = p@mode^2)
						p = sfc_expand(p, code[i], flip = flip)
					}
				}
			}
		}
		
		
		p@expansion = as.integer(code)
		p@universe = sfc_universe(RULES)
		p
		
	}

	setAs("sfc_sequence", cl, function(from) {
		p = new(cl)
		p@seq = from@seq
		levels(p@seq) = sfc_universe(RULES)
		if(any(is.na(p@seq))) {
			stop_wrap("Base letters should all be in `sfc_universe(RULES)`.")
		}
		p@rot = from@rot
		p@universe = sfc_universe(RULES)
		p@level = 0L
		p@mode = sfc_mode(RULES)
		p@rules = RULES

		p
	}, where = envir)

	setMethod("sfc_expand",
		signature = cl,
		definition = function(p, code, flip = FALSE) {

		seq = p@seq
		rot = p@rot
		n = length(p@seq)

		rules = p@rules
		if(length(rules@flip) == 0) {
			flip = FALSE
		}

		tl = integer(n)

		if(code_num == 2) {
			tl[1] = code
			if(n > 1) {
				for(i in 2:n) {
					tl[i] = traverse_type_2x2(tl[i-1], rot[i-1], rot[i])
				}
			}
		} else {
			tl = rep(1L, n)
		}

		sfc_expand_by_rules(rules, p, code = tl, flip = flip)

	}, where = envir)

	draw_rules = function(flip = FALSE) {

	    p = sfc_fun("I", flip = flip)

	    if(length(p@rules@flip) == 0) {
	    	flip = FALSE
	    }

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

	    if("J" %in% sfc_universe(p)) {
	    	gb2 = grob_single_base_rule(p, "J", equation_max_width = equation_max_width, flip = flip, x = size, y = unit(1, "npc") - size - gb1$vp$height, just = c("left", "top"))
		    nc = length(gb2$children)
		    gb2$children[[nc]]$width = gb2$children[[nc]]$width
		    grid.draw(gb2)

		    gb3 = grob_single_base_rule(p, "R", equation_max_width = equation_max_width, flip = flip, x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height, just = c("left", "top"))
		    nc = length(gb3$children)
		    gb3$children[[nc]]$width = gb3$children[[nc]]$width
		    grid.draw(gb3)

		    gb4 = grob_single_base_rule(p, "L", equation_max_width = equation_max_width, flip = flip, x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height - gb3$vp$height, just = c("left", "top"))
		    nc = length(gb4$children)
		    gb4$children[[nc]]$width = gb4$children[[nc]]$width
		    grid.draw(gb4)
	    } else {

		    gb2 = grob_single_base_rule(p, "R", equation_max_width = equation_max_width, flip = flip, x = size, y = unit(1, "npc") - size - gb1$vp$height, just = c("left", "top"))
		    nc = length(gb2$children)
		    gb2$children[[nc]]$width = gb2$children[[nc]]$width
		    grid.draw(gb2)

		    gb3 = grob_single_base_rule(p, "L", equation_max_width = equation_max_width, flip = flip, x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height, just = c("left", "top"))
		    nc = length(gb3$children)
		    gb3$children[[nc]]$width = gb3$children[[nc]]$width
		    grid.draw(gb3)
		}

	}

	assign(paste0("sfc_", NAME), sfc_fun, envir = envir)
	assign(paste0("draw_rules_", NAME), draw_rules, envir = envir)
	if(verbose) {
		cat("The following two functions are exported to the current top environment:\n", sep = "")
		cat("  - ", paste0("sfc_", NAME), "()\n", sep = "")
		cat("  - ", paste0("draw_rules_", NAME), "()\n", sep = "")

		if(length(RULES@flip) > 0) {
			cat("")
			cat("flipping is supported.\n")
		}
	}

}
