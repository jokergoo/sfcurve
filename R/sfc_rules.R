

#' Constructor of the sfc_rules class
#' 
#' @param rules A list of rules.
#' @param bases A list of base patterns.
#' @param flip A list of rules. They are "flipped" version of `rules`. The value can also simply be `TRUE`, then
#'           the flipped version is automatically generated from `rules`.
#' @param name A self-defined string.
#' 
#' @details
#' It is mainly used internally.
#' 
#' `rules` is a two-level list. It is in a format of `rules[[ base ]][[ expansion_code ]] = sfc_unit()`.
#' In the following example where we define the expansion rules for the 2x2 curve:
#' 
#' ```
#' UNIVERSE_2x2 = c("I", "R", "L", "U", "B", "D", "P", "Q", "C")
#' RULES_2x2 = list()
#' RULES_2x2[["I"]][[1]] = sfc_unit(c("R", "L", "L", "R"), rot = 0, universe = UNIVERSE_2x2)
#' ```
#' 
#' `I` is the level-0 base pattern, `[[1]]` corresponds to the first form of expansion to level-1, and the value
#' assigned is a [`sfc_unit()`] object which is basically a list of base patterns.
#' 
#' Then we also need to provide the base patterns which define how to extend the curve. The list of base patterns
#' is assigned to the `bases` argument. In the same example, we set `bases` as:
#' 
#' ```
#' list("I" = BASE_I, "R" = BASE_R, "L" = BASE_L, "U" = BASE_U, ...)
#' ```
#' 
#' where e.g. [`BASE_I`] is a pre-defined base pattern in the [`sfc_base`] class.
#' 
#' There are the following pre-defined rules:
#' 
#' - [`SFC_RULES_2x2`]
#' - [`SFC_RULES_3x3_PEANO`]
#' - [`SFC_RULES_3x3_MEANDER`]
#' - [`SFC_RULES_3x3_COMBINED`]
#' - [`SFC_RULES_4x4_MEANDER_1`]
#' - [`SFC_RULES_4x4_MEANDER_2`]
#' 
#' Check \url{https://github.com/jokergoo/sfcurve/blob/master/R/zz_global.R} to see how these pre-defined rules are constructed.
#' 
#' @export
#' @return
#' An `sfc_rules` object.
sfc_rules = function(rules, bases, flip = list(), name = "sfc_rules") {
    r = new("sfc_rules")

    for(nm in names(rules)) {
        if(!is.list(rules[[nm]])) {
            rules[[nm]] = list(rules[[nm]])
        }
    }

    universe = sfc_universe(rules[[1]][[1]])
    if( !identical(sort(names(rules)), sort(universe)) ) {
        stop_wrap("`names(rules)` should be identical to the universe of its level-1 patterns.")
    }
    rules = rules[universe]
    r@bases = bases
    r@universe = universe
    r@name = name

    if(identical(flip, TRUE)) {
        flip = lapply(rules, function(x) {
            lapply(x, function(u) sfc_flip_unit(u, rules@base))
        })
    }

    if(length(flip) > 0) {
        for(nm in names(flip)) {
            if(!is.list(flip[[nm]])) {
                flip[[nm]] = list(flip[[nm]])
            }
        }

        universe2 = sfc_universe(flip[[1]][[1]])
        if( !identical(names(flip), universe2) ) {
            stop_wrap("`names(flip)` should be identical to the universe of its level-1 patterns.")
        }
        if( !identical(names(flip), r@universe) ) {
            stop_wrap("`names(flip)` should be identical to `names(rules)`.")
        }

        flip = flip[names(rules)]

        for(nm in names(rules)) {
            if(length(rules[[nm]]) != length(flip[[nm]])) {
                stop_wrap("Length of `rules[['", nm, "']]` should be the same as `flip[['", nm, "']]`.")
            }
        }

        # validate whether rules and flip are compatible, i.e. flip[[i]][[j]] is the flipped version of rules[[i]][[j]]
        for(nm in names(rules)) {
            for(j in seq_along(rules[[nm]])) {
                u1 = flip[[nm]][[j]]
                u2 = sfc_flip_unit(rules[[nm]][[j]], bases)
                if(!identical(u1, u2)) {
                    stop_wrap("`flip[['", nm, "']][[", j, "]]` is not a flip of `rules[[", nm, "]][[", j, "]]`.")
                }
            }
        }
    }

    for(nm in names(rules)) {
        for(i in seq_along(rules[[nm]])) {
            validate_rule(rules[[nm]][[i]], nm, i, bases)
        }
    }

    if(length(flip) > 0) {
        for(nm in names(flip)) {
            for(i in seq_along(flip[[nm]])) {
                validate_rule(flip[[nm]][[i]], nm, i, bases)
            }
        }
    }

    # add corner value for each rule
    .add_corners = function(rules) {
        for(i in seq_along(rules)) {
            for(j in seq_along(rules[[i]])) {
                p = rules[[i]][[j]]

                if(!sfc_is_compatible(rules[[1]][[1]], p)) {
                    stop_wrap(paste0("The universe of rules[[", names(rules)[1], "]][[1]] is not compatible with rules[[", names(rules)[i], "]][[", j, "]]."))
                }

                first_pos = c(0, 0)
                min_x = first_pos[1]
                max_x = first_pos[1]
                min_y = first_pos[2]
                max_y = first_pos[2]
                current_pos = first_pos
                seq = p@seq
                rot = p@rot
                for(k in seq_along(seq)[-1]) {
                    current_pos = sfc_next_point(bases[[ as.character(seq[k-1]) ]], current_pos, rot[k-1])

                    min_x = min(min_x, current_pos[1])
                    max_x = max(max_x, current_pos[1])
                    min_y = min(min_y, current_pos[2])
                    max_y = max(max_y, current_pos[2])
                }
                last_pos = current_pos

                corner = c(0L, 0L)
                if( (equal_to(first_pos[1], min_x) && equal_to(first_pos[2], min_y)) || 
                    (equal_to(first_pos[1], max_x) && equal_to(first_pos[2], max_y)) ) {
                    corner[1] = 1L
                }
                
                if( (equal_to(first_pos[1], min_x) && equal_to(first_pos[2], max_y)) ||
                    (equal_to(first_pos[1], max_x) && equal_to(first_pos[2], min_y)) ) {
                    corner[1] = 2L
                }

                if( (equal_to(last_pos[1], min_x) && equal_to(last_pos[2], min_y)) || 
                    (equal_to(last_pos[1], max_x) && equal_to(last_pos[2], max_y)) ) {
                    corner[2] = 1L
                }

                if( (equal_to(last_pos[1], min_x) && equal_to(last_pos[2], max_y)) ||
                    (equal_to(last_pos[1], max_x) && equal_to(last_pos[2], min_y)) ) {
                    corner[2] = 2L
                }

                rules[[i]][[j]]@corner = corner

            }
        }
        rules
    }

    r@rules = .add_corners(rules)
    if(length(flip)) {
        r@flip = .add_corners(flip)
    }
    
    r
}


# the in- and out-direction of the unit should be the same as the base
validate_rule = function(rule, bp, i, bases) {
    if( !equal_to(sqrt(length(rule)), round(sqrt(length(rule)))) ) {
        stop_wrap("The length of the unit should be k^2 where k = 2, 3, ...")
    }

    if(!is.na(bases[[ bp ]]@in_direction)) {
        prev_p1 = sfc_previous_point(bases[[ as.character(rule@seq[1]) ]], c(0, 0), rule@rot[1])
        prev_p2 = sfc_previous_point(bases[[ bp ]], c(0, 0), 0)

        if(!equal_to(prev_p1, prev_p2)) {
            prev_p1 = sfc_previous_point(bases[[ as.character(rule@seq[1]) ]], c(0, 0), 0)
            if(equal_to(prev_p1, prev_p2)) {
                stop_wrap("The in-direction is different from the base pattern ", bp, " to the level-1 unit. Maybe you need to rotate the unit by ", -rule@rot[1] %% 360, " degrees.")
            } else {
                stop_wrap("The in-direction is different from the base pattern ", bp, " to the level-1 unit.")
            }
        }
    }

    if(!is.na(bases[[ bp ]]@out_direction)) {
        n = length(rule)
        next_p1 = sfc_next_point(bases[[ as.character(rule@seq[n]) ]], c(0, 0), rule@rot[n])
        next_p2 = sfc_next_point(bases[[ bp ]], c(0, 0), 0)

        if(!equal_to(next_p1, next_p2)) {
            stop_wrap("The out-direction is different from the base pattern ", bp, " to the level-1 unit.")
        }
    }
}


#' @export
`[.sfc_rules` = function(x, i) {
    if(is.character(i)) {
        i = intersect(names(x@rules), i)
    } else if(is.numeric(i)) {
        i = intersect(seq_along(x@rules), i)
    }
    x@rules = x@rules[i]
    if(length(x@flip) > 0) {
        x@flip = x@flip[i]
    }
    x
}

#' @rdname show
#' @export
setMethod("show",
    signature = "sfc_rules",
    definition = function(object) {

    rot_str = function(x) {
        # ifelse(x == 0, "  0", ifelse(x == 90, " 90", x))
        x
    }

    .get_output = function(rules) {
        lt = rules
        output = list(character(0), character(0), character(0), character(0))
        for(nm in names(lt)) {
            if(is.null(names(lt[[nm]]))) {
                ind = seq_along(lt[[nm]])
            } else {
                ind = names(lt[[nm]])
            }

            k = 1
            for(i in ind) {
                if(k > 1) {
                    output[[1]] = c(output[[1]], "   ")
                } else {
                    output[[1]] = c(output[[1]], paste0(nm, " |"))
                }
                k = k + 1
                output[[2]] = c(output[[2]], paste0(nm, "_", i))

                seq = lt[[nm]][[i]]@seq
                rot = lt[[nm]][[i]]@rot
                corner = lt[[nm]][[i]]@corner
                output[[3]] = c(output[[3]], paste(seq, "(", rot_str(rot), ")", sep = "", collapse = ""))
                
                output[[4]] = c(output[[4]], paste0(" corner = (", corner[1], ", ", corner[2], ")"))
            }
        }

        max_nchar = sapply(output, function(x) max(nchar(x)))
        for(i in seq_along(output[[1]])) {
            cat(output[[1]][i]); cat(strrep(" ", max_nchar[1] - nchar(output[[1]][i]))); cat(" ")
            cat(output[[2]][i]); cat(strrep(" ", max_nchar[2] - nchar(output[[2]][i]))); cat(" = ")
            cat(output[[3]][i]); cat(strrep(" ", max_nchar[3] - nchar(output[[3]][i]))); cat(" ")
            cat(output[[4]][i]); cat(strrep(" ", max_nchar[4] - nchar(output[[4]][i]))); cat(" ")
            cat("\n")
        }
    }

    cat("Name: ", object@name, "\n", sep = "")
    .get_output(object@rules)
    
    if(length(object@flip)) {
        cat("\n")
        cat("Flipped:\n")
        .get_output(object@flip)
    }
})

#' The universe base pattern set
#' 
#' @param p The corresponding object.
#' @aliases sfc_universe
#' @rdname sfc_universe
#' @return
#' A vector of base patterns.
#' @export
#' @examples
#' sfc_universe(SFC_RULES_2x2)
#' sfc_universe(SFC_RULES_3x3_PEANO)
setMethod("sfc_universe",
    signature = "sfc_rules",
    definition = function(p) {

    p@universe
})


#' The mode of the curve
#' @aliases sfc_mode
#' @rdname sfc_mode
#' @param p The corresponding object.
#' @export
#' @examples
#' sfc_mode(SFC_RULES_2x2)
#' sfc_mode(SFC_RULES_3x3_PEANO)
setMethod("sfc_mode",
    signature = "sfc_rules",
    definition = function(p) {

    as.integer(sqrt(length(p@rules[[1]][[1]])))
})

#' Expand a sequence
#' @rdname sfc_expand_by_rules
#' @aliases sfc_expand_by_rules
#' @param p An `sfc_rules` object.
#' @param seq An `sfc_nxn` object or other objects.
#' @param code The expansion code.
#' @param flip For the Peano curve and the Meander curves, each unit can be flipped without affecting other parts in the curve. This argument
#'        controls whether to flip the unit. Since currently it only works on the Peano curve and the Meander curve, `flip` should be a logical
#'        vector of length one or with the same length as `seq`. Whether it flips horizontally, vertically or against the diagonal line is automatically choosen.
#'        The value of `flip` can also be a function which takes the current curve as the only argument.
#' @param by Which implementation? Only for the testing purpose.
#' 
#' @return
#' If `seq` is an `sfc_nxn` object, the function also returns an "expanded" `sfc_nxn` object. Or else it returns an `sfc_sequence` object.
#' @export
#' @examples
#' sfc_expand_by_rules(SFC_RULES_2x2, sfc_2x2("I"))
setMethod("sfc_expand_by_rules", 
    signature = c("sfc_rules", "sfc_nxn"),
    definition = function(p, seq, code = 1L, flip = FALSE, by = "Cpp") {

    if(!identical(sfc_universe(seq), sfc_universe(p))) {
        stop_wrap("The universe of `p` and `seq` should be identical.")
    }
   
    n = length(seq)
    letters = seq@seq

    do_flipping = TRUE
    if(length(flip) == 0) {
        flip = rep(FALSE, n)
        do_flipping = FALSE
    } else {
        if(is.logical(flip)) {
            if(length(flip) == 1) {
                flip = rep(flip, n)
            } else if(length(flip) == n) {

            } else {
                flip = rep(flip, times = n/sfc_mode(p)^(2*sfc_level(seq)))
            }
            if(length(flip) != n) {
                stop_wrap("The logical vector `flip` should have a length of one or with the same length as `seq`.")
            }
        } else {

            flip = flip(seq)
            if(length(flip) != n) {
                stop_wrap("The self-deifned function `flip` should return a logical vector with current curve as its argument.")
            }
        }
    }

    if(by == "R") {
        if(length(code) == 1) {
            pl = lapply(seq_along(letters), function(i) {
                if(flip[i]) {
                    p@flip[[ letters[i] ]][[ code ]]
                } else {
                    p@rules[[ letters[i] ]][[ code ]]
                }
            }) # x is as an integer
        } else if(length(letters) == length(code)) {
            pl = lapply(seq_along(letters), function(i) {
                if(flip[i]) {
                    p@flip[[ letters[i] ]][[ code[i] ]]
                } else {
                    p@rules[[ letters[i] ]][[ code[i] ]]
                }
            })
        }
        p2 = do.call("c", pl)
    } else {
        lt_rules = lapply(p@rules, function(r) {
            lapply(r, function(x) {
                list(seq = as.integer(x@seq), rot = x@rot)
            })
        })

        if(!do_flipping) {
            lt = expand_by_rules_cpp(lt_rules, as.integer(letters), code)
        } else {
            lt_flip = lapply(p@flip, function(r) {
                lapply(r, function(x) {
                    list(seq = as.integer(x@seq), rot = x@rot)
                })
            })

            lt = expand_by_rules_2_cpp(lt_rules, lt_flip, as.integer(letters), code, flip)
        }
        attributes(lt[[1]]) = list(levels = sfc_universe(p), class = "factor")
        p2 = sfc_sequence(seq = lt[[1]], rot = lt[[2]])
    }

    p2 = sfc_rotate(p2, rep(seq@rot, each = length(p@rules[[1]][[1]])))

    if(inherits(seq, "sfc_nxn")) {
        p3 = new(class(seq))
        p3@seq = p2@seq
        p3@rot = p2@rot
        p3@universe = p2@universe
        p3@level = seq@level + 1L
        p3@mode = seq@mode
        p3@seed = seq@seed
        p3@rules = seq@rules
        p3
    } else {
        p2
    }
    
})

#' @rdname sfc_expand_by_rules
#' @export
setMethod("sfc_expand_by_rules", 
    signature = c("sfc_rules", "factor"),
    definition = function(p, seq, code = 1L, flip = FALSE, by = "Cpp") {

    seq = sfc_sequence(seq, universe = sfc_universe(p))
    sfc_expand_by_rules(p, seq, code = code, flip = flip, by = by)

})

#' @rdname sfc_expand_by_rules
#' @export
setMethod("sfc_expand_by_rules", 
    signature = c("sfc_rules", "character"),
    definition = function(p, seq, code = 1L, flip = FALSE, by = "Cpp") {

    seq = sfc_sequence(seq, universe = sfc_universe(p))
    sfc_expand_by_rules(p, seq, code = code, flip = flip, by = by)
})


grob_math = function(label, x, y, gp = gpar(), ...) {
    expr = parse(text = label)
    gp$fontfamily = "Times"
    textGrob(expr, x, y, ..., gp = gp)
}

tex_pattern = function(x, which, p) {
    seq = p@seq
    rot = p@rot
    l = rot > 0
    rot[l] = paste0("^", rot[l], "")
    rot[!l] = ""
    
    seq = paste0("italic(", seq, ")")
    expr = paste(seq, rot, sep = "", collapse = ",")
    paste0("italic(", x, ")[", which, "] == paste(", expr, ")")
}

grob_single_base_rule = function(p, bp, equation_max_width, flip = FALSE, ...) {
    rules = p@rules
    bases = rules@bases

    if(inherits(p, "sfc_2x2")) {
        level0 = sfc_2x2(bp)
    } else if(inherits(p, "sfc_3x3_peano")) {
        level0 = sfc_3x3_peano(bp, flip = flip)
    } else if(inherits(p, "sfc_3x3_meander")) {
        level0 = sfc_3x3_meander(bp, flip = flip)
    } else if(inherits(p, "sfc_3x3_combined")) {
        level0 = sfc_3x3_combined(bp, level = 0, flip = flip)
    } else if(inherits(p, "sfc_4x4_meander")) {
        level0 = sfc_4x4_meander(bp, type = p@type, flip = flip)
    } else {
        cl = getClass(class(p))
        fun = get(class(p), envir = as.environment(cl@package))
        level0 = fun(bp, flip = flip)
    }

    pl = rules@rules[[bp]]

    for(i in seq_along(pl)) {
        pl[[i]] = sfc_expand(level0, i, flip = flip)
    }

    n = length(pl)
    nr = ceiling(n/2)
    nc = ceiling(n/nr)

    k = as.integer(sqrt(length(pl[[1]]@seq)))

    vp_xscale = c(0, 4 + (nc*k + nc) +7)
    vp_yscale = c(-(nr-1)*(k+2)-1, k+1)

    vp_width = (4 + (nc*k + nc))*size + equation_max_width + unit(8, "mm")
    vp_height = diff(vp_yscale)*size

    vp_xscale[2] = convertWidth(vp_width, "mm", valueOnly = TRUE)/as.numeric(size)
    vp = viewport(xscale = vp_xscale, yscale = vp_yscale, width = vp_width, height = vp_height, ...)

    gbl = list()

    gbl[[1]] = grob_math(paste0("italic(", bp, ")"), 1, k/2, default.units = "native")

    gbl[[2]] = sfc_grob(bases[[bp]])
    gbl[[2]]$vp$x = unit(2.5, "native")
    gbl[[2]]$vp$y = unit(k/2, "native")

    ii = 2;
    for(i in 1:nr) {
        ii = ii + 1
        gbl[[ii]] = sfc_grob(pl[[i*2-1]], x = 4 + k/2, y = k/2 - (i-1)*(k+2), default.units = "native", extend = TRUE)
        gbl[[ii]]$children[[1]]$gp = gpar(col = "grey", lwd = 1)
        gbl[[ii]]$children[[2]]$gp = gpar(col = "grey", lwd = 1)
        gbl[[ii]]$children[[3]]$gp = gpar(col = "black", lwd = 1)
        class(gbl[[ii]]) = setdiff(class(gbl[[ii]]), "grob_sfc_sequence")
        gbl[[ii]]$vp$width = (k+2)*size
        gbl[[ii]]$vp$height = (k+2)*size

        if(i*2 <= n) {
            ii = ii + 1
            gbl[[ii]] = sfc_grob(pl[[i*2]], x = 4 + k+1 + k/2, y = k/2 - (i-1)*(k+2), default.units = "native", extend = TRUE)
            gbl[[ii]]$children[[1]]$gp = gpar(col = "grey", lwd = 1)
            gbl[[ii]]$children[[2]]$gp = gpar(col = "grey", lwd = 1)
            gbl[[ii]]$children[[3]]$gp = gpar(col = "black", lwd = 1)
            class(gbl[[ii]]) = setdiff(class(gbl[[ii]]), "grob_sfc_sequence")
            gbl[[ii]]$vp$width = (k+2)*size
            gbl[[ii]]$vp$height = (k+2)*size
        }
    }

    ii = length(gbl)
    for(i in 1:n) {
        ii  = ii + 1
        gbl[[ii]] = grob_math(tex_pattern(bp, i, pl[[i]]), x = 6 + ifelse(n == 1, 1, 2)*k, k - 0.5 - (i-1)*1.2, just = "left", default.units = "native")
    }

    gbl[[ii+1]] = rectGrob(gp = gpar(fill = NA, col = "black"), x = 0, just = "left")

    arg = gbl
    arg$vp = vp
    
    do.call(grobTree, arg)
}

#' Draw the expansion rules
#' 
#' @rdname draw_rules
#' @details
#' The expansion rules define how the curve is expanded from level-0 to level-1.
#' @export
#' @return No value is returned.
#' @examples
#' draw_rules_2x2()
draw_rules_2x2 = function() {

    p = sfc_2x2("I")
    grid.newpage()

    rules = p@rules@rules
    equation_max_width = max(do.call("unit.c", lapply(names(rules), function(nm) {
        do.call("unit.c", lapply(seq_along(rules[[nm]]), function(i) {
            convertWidth(grobWidth(grob_math(tex_pattern(nm, i,  rules[[nm]][[i]]), x = 0, y = 0)), "mm")
        }))
    })))

    gb1 = grob_single_base_rule(p, "I", equation_max_width = equation_max_width, x = size, y = unit(1, "npc") - size, just = c("left", "top"))
    grid.draw(gb1)

    gb2 = grob_single_base_rule(p, "R", equation_max_width = equation_max_width, x = size, y = unit(1, "npc") - size - gb1$vp$height, just = c("left", "top"))
    grid.draw(gb2)

    gb3 = grob_single_base_rule(p, "L", equation_max_width = equation_max_width, x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height, just = c("left", "top"))
    grid.draw(gb3)

    gb4 = grob_single_base_rule(p, "U", equation_max_width = equation_max_width, x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height - gb3$vp$height, just = c("left", "top"))
    grid.draw(gb4)

    gb5 = grob_single_base_rule(p, "B", equation_max_width = equation_max_width, x = size + gb1$vp$width + size, y = unit(1, "npc") - size, just = c("left", "top"))
    grid.draw(gb5)

    gb6 = grob_single_base_rule(p, "D", equation_max_width = equation_max_width, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height, just = c("left", "top"))
    grid.draw(gb6)

    gb7 = grob_single_base_rule(p, "P", equation_max_width = equation_max_width, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height, just = c("left", "top"))
    grid.draw(gb7)

    gb8 = grob_single_base_rule(p, "Q", equation_max_width = equation_max_width, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height - gb7$vp$height, just = c("left", "top"))
    grid.draw(gb8)

    gb9 = grob_single_base_rule(p, "C", equation_max_width = equation_max_width, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height - gb7$vp$height - gb8$vp$height, just = c("left", "top"))
    grid.draw(gb9)
}

#' @rdname draw_rules
#' @param flip Whether to use the "flipped" rules? For the Peano curve and the Meander curve, there is also a "fliiped" version 
#'      of the curve expansion rules. See the vignettes for details.
#' @export
#' @examples
#' # the units in the main rules of the Peano curve are vertical
#' draw_rules_3x3_peano()
#' # the units in the flipped rules of the Peano curve are horizontal
#' draw_rules_3x3_peano(flip = TRUE)
draw_rules_3x3_peano = function(flip = FALSE) {

    p = sfc_3x3_peano("I", flip = flip)

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
}

#' @rdname draw_rules
#' @export
#' @examples
#' # the units in the main rules of the Meander curve are "forward"
#' # i.e. the direction of the "wave" is the same as the direction of the curve
#' draw_rules_3x3_meander()
#' # the units in the flipped rules of the Meander curve are "backward"
#' draw_rules_3x3_meander(flip = TRUE)
draw_rules_3x3_meander = function(flip = FALSE) {

    p = sfc_3x3_meander("I", flip = flip)

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

    gb4 = grob_single_base_rule(p, "U", equation_max_width = equation_max_width, flip = flip, x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height - gb3$vp$height, just = c("left", "top"))
    nc = length(gb4$children)
    gb4$children[[nc]]$width = gb4$children[[nc]]$width
    grid.draw(gb4)

    gb5 = grob_single_base_rule(p, "B", equation_max_width = equation_max_width, flip = flip, x = size + gb1$vp$width + size, y = unit(1, "npc") - size, just = c("left", "top"))
    nc = length(gb5$children)
    gb5$children[[nc]]$width = gb5$children[[nc]]$width
    grid.draw(gb5)

    gb6 = grob_single_base_rule(p, "D", equation_max_width = equation_max_width, flip = flip, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height, just = c("left", "top"))
    nc = length(gb6$children)
    gb6$children[[nc]]$width = gb6$children[[nc]]$width
    grid.draw(gb6)

    gb7 = grob_single_base_rule(p, "P", equation_max_width = equation_max_width, flip = flip, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height, just = c("left", "top"))
    nc = length(gb7$children)
    gb7$children[[nc]]$width = gb7$children[[nc]]$width
    grid.draw(gb7)

    gb8 = grob_single_base_rule(p, "Q", equation_max_width = equation_max_width, flip = flip, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height - gb7$vp$height, just = c("left", "top"))
    nc = length(gb8$children)
    gb8$children[[nc]]$width = gb8$children[[nc]]$width
    grid.draw(gb8)

    gb9 = grob_single_base_rule(p, "C", equation_max_width = equation_max_width, flip = flip, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height - gb7$vp$height - gb8$vp$height, just = c("left", "top"))
    nc = length(gb9$children)
    gb9$children[[nc]]$width = gb9$children[[nc]]$width
    grid.draw(gb9)

}


