

#' Constructur of the sfc_rules class
#' 
#' @param rules A list of rules.
#' @param bases A list of base patterns.
#' @param flip A list of rules. They are "flipped" version of `rules`.
#' @param name A self-defined string.
#' 
#' @details
#' It is used mainly internally.
#' 
#' `rules` is a two-level list. It is in a format of `rules[[ base ]][[ transverse_code]] = sfc_unit()`.
#' In the following example where we define the expansion rules for th Hilbert curve:
#' 
#' ```
#' UNIVERSE_HILBERT = c("I", "R", "L", "U", "B", "D", "P", "Q", "C")
#' RULES_HILBERT = list()
#' RULES_HILBERT[["I"]][[1]] = sfc_unit(c("R", "L", "L", "R"), rot = 0, universe = UNIVERSE_HILBERT)
#' ```
#' 
#' where `I` is the level-0 base pattern, `[[1]]` corresponds to the first form of expansion to level-1, the value
#' assigned is a [`sfc_unit()`] object which is basically a list of base patterns.
#' 
#' Then we also need to provide the base patterns which define how to extend the curve. The list of base patterns
#' is assigned to the `bases` argument. In the same example, we set `bases` as:
#' 
#' ```
#' list("I" = BASE_I, "R" = BASE_R, "L" = BASE_L, "U" = BASE_U, ...)
#' ```
#' 
#' where e.g. `BASE_I` is a pre-defined base pattern in [`sfc_base`] class.
#' 
#' @export
sfc_rules = function(rules, bases, flip = list(), name = "sfc_rules") {
    r = new("sfc_rules")

    for(nm in names(rules)) {
        if(!is.list(rules[[nm]])) {
            rules[[nm]] = list(rules[[nm]])
        }
    }

    universe = sfc_universe(rules[[1]][[1]])
    if( !identical(names(rules), universe) ) {
        stop_wrap("`names(rules)` should be identical to the universe of its level-1 patterns.")
    }
    r@bases = bases
    r@universe = universe
    r@name = name

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
#' sfc_universe(SFC_RULES_HILBERT)
#' sfc_universe(SFC_RULES_PEANO)
setMethod("sfc_universe",
    signature = "sfc_rules",
    definition = function(p) {

    p@universe
})

#' @rdname sfc_expand
#' @param p An `sfc_rules` object.
#' @param letters A list patterns, must be a factor.
#' @param code The transverse code.
#' @param flip For the Peano curve and the Meander curves, each unit can flip without affecting other parts in the curve. This argument
#'        controls whether to flip the unit. Since currently it only works on the Peano curve and the Meander curve, `flip` should be a logical
#'        vector of length one or length of 9. Whether it flips horizontally, vertically or against the diagonal line is automatically choosen.
#' @param by Which implemnetation, only for the testing purpose.
#' 
#' @export
#' @examples
#' sfc_expand(SFC_RULES_HILBERT, 
#'     factor(c("I", "R", "L"), levels = sfc_universe(SFC_RULES_HILBERT)))
setMethod("sfc_expand", 
    signature = "sfc_rules",
    definition = function(p, letters, code = 1L, flip = FALSE, by = "Cpp") {

    if(!is.factor(letters)) {
        stop_wrap("`letters` should be a factor.")
    }

    if(!identical(levels(letters), sfc_universe(p))) {
        stop_wrap("Levels of `letters` should be identical to the universe of `p`.")
    }

    n = length(letters)

    do_flipping = TRUE
    if(length(p@flip) == 0) {
        flip = rep(FALSE, n)
        do_flipping = FALSE
    } else {
        if(is.logical(flip)) {
            flip = rep(flip, times = n)
        } else {

            flip = flip(n)
            if(length(flip) != n) {
                stop_wrap("The self-deifned function `flip` should return a logical vector with length n with the same value as its argument.")
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
        do.call("c", pl)
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
        sfc_sequence(seq = lt[[1]], rot = lt[[2]])
    }
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

grob_single_base_rule = function(p, bp, ...) {
    rules = p@rules
    bases = rules@bases

    if(inherits(p, "sfc_hilbert")) {
        level0 = sfc_hilbert(bp)
    } else if(inherits(p, "sfc_peano")) {
        level0 = sfc_peano(bp, flip = p@flip)
    } else if(inherits(p, "sfc_meander")) {
        level0 = sfc_meander(bp, flip = p@flip)
    }

    pl = rules@rules[[bp]]

    for(i in seq_along(pl)) {
        pl[[i]] = sfc_expand(level0, i)
    }

    n = length(pl)
    nr = ceiling(n/2)

    if(length(pl[[1]]@seq) %% 2 == 0) {
        k = 2
    } else if(length(pl[[1]]@seq) %% 3 == 0) {
        k = 3
    }

    vp_xscale = c(0, 13+2*k)
    vp_yscale = c(-(nr-1)*(k+1)-1, k+1)

    vp_width = diff(vp_xscale)*size
    vp_height = diff(vp_yscale)*size

    vp = viewport(xscale = vp_xscale, yscale = vp_yscale, width = vp_width, height = vp_height, ...)

    gbl = list()

    gbl[[1]] = grob_math(paste0("italic(", bp, ")"), 1, k/2, default.units = "native")

    gbl[[2]] = sfc_grob(bases[[bp]])
    gbl[[2]]$vp$x = unit(2.5, "native")
    gbl[[2]]$vp$y = unit(k/2, "native")

    ii = 2;
    for(i in 1:nr) {
        ii = ii + 1
        gbl[[ii]] = sfc_grob(pl[[i*2-1]], x = 4 + k/2, y = k - (i-1)*(k+1) - k/2, default.units = "native", extend = TRUE)
        gbl[[ii]]$children[[1]]$gp = gpar(col = "grey", lwd = 1)
        gbl[[ii]]$children[[2]]$gp = gpar(col = "grey", lwd = 1)
        gbl[[ii]]$children[[3]]$gp = gpar(col = "black", lwd = 1)

        if(i*2 <= n) {
            ii = ii + 1
            gbl[[ii]] = sfc_grob(pl[[i*2]], x = 5 + k + k/2, y = k - (i-1)*(k+1) - k/2, default.units = "native", extend = TRUE)
            gbl[[ii]]$children[[1]]$gp = gpar(col = "grey", lwd = 1)
            gbl[[ii]]$children[[2]]$gp = gpar(col = "grey", lwd = 1)
            gbl[[ii]]$children[[3]]$gp = gpar(col = "black", lwd = 1)
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
#' @export
#' @examples
#' draw_rules_hilbert()
draw_rules_hilbert = function() {

    p = sfc_hilbert("I")
    grid.newpage()

    gb1 = grob_single_base_rule(p, "I", x = size, y = unit(1, "npc") - size, just = c("left", "top"))
    grid.draw(gb1)

    gb2 = grob_single_base_rule(p, "R", x = size, y = unit(1, "npc") - size - gb1$vp$height, just = c("left", "top"))
    grid.draw(gb2)

    gb3 = grob_single_base_rule(p, "L", x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height, just = c("left", "top"))
    grid.draw(gb3)

    gb4 = grob_single_base_rule(p, "U", x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height - gb3$vp$height, just = c("left", "top"))
    grid.draw(gb4)

    gb5 = grob_single_base_rule(p, "B", x = size + gb1$vp$width + size, y = unit(1, "npc") - size, just = c("left", "top"))
    grid.draw(gb5)

    gb6 = grob_single_base_rule(p, "D", x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height, just = c("left", "top"))
    grid.draw(gb6)

    gb7 = grob_single_base_rule(p, "P", x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height, just = c("left", "top"))
    grid.draw(gb7)

    gb8 = grob_single_base_rule(p, "Q", x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height - gb7$vp$height, just = c("left", "top"))
    grid.draw(gb8)

    gb9 = grob_single_base_rule(p, "C", x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height - gb7$vp$height - gb8$vp$height, just = c("left", "top"))
    grid.draw(gb9)
}

#' @rdname draw_rules
#' @param flip Whether to use the "flipped" rules? For the Peano curve and the Meander curve, there is also a "fliiped" version 
#'      of curve expansion rules. See the vignettes for details.
#' @export
#' @examples
#' draw_rules_peano()
#' draw_rules_peano(flip = TRUE)
draw_rules_peano = function(flip = FALSE) {

    p = sfc_peano("I", flip = flip)

    grid.newpage()

    gb1 = grob_single_base_rule(p, "I", x = size, y = unit(1, "npc") - size, just = c("left", "top"))
    nc = length(gb1$children)
    gb1$children[[nc]]$width = gb1$children[[nc]]$width + unit(5, "mm")
    grid.draw(gb1)

    gb2 = grob_single_base_rule(p, "J", x = size, y = unit(1, "npc") - size - gb1$vp$height, just = c("left", "top"))
    nc = length(gb2$children)
    gb2$children[[nc]]$width = gb2$children[[nc]]$width + unit(5, "mm")
    grid.draw(gb2)

    gb3 = grob_single_base_rule(p, "R", x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height, just = c("left", "top"))
    nc = length(gb3$children)
    gb3$children[[nc]]$width = gb3$children[[nc]]$width + unit(5, "mm")
    grid.draw(gb3)

    gb4 = grob_single_base_rule(p, "L", x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height - gb3$vp$height, just = c("left", "top"))
    nc = length(gb4$children)
    gb4$children[[nc]]$width = gb4$children[[nc]]$width + unit(5, "mm")
    grid.draw(gb4)
}

#' @rdname draw_rules
#' @export
#' @examples
#' draw_rules_meander()
#' draw_rules_meander(flip = TRUE)
draw_rules_meander = function(flip = FALSE) {

    p = sfc_meander("I", flip = flip)

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

    gb4 = grob_single_base_rule(p, "U", x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height - gb3$vp$height, just = c("left", "top"))
    nc = length(gb4$children)
    gb4$children[[nc]]$width = gb4$children[[nc]]$width + unit(25, "mm")
    grid.draw(gb4)

    gb5 = grob_single_base_rule(p, "B", x = size + gb1$vp$width + unit(25, "mm") + size, y = unit(1, "npc") - size, just = c("left", "top"))
    nc = length(gb5$children)
    gb5$children[[nc]]$width = gb5$children[[nc]]$width + unit(30, "mm")
    grid.draw(gb5)

    gb6 = grob_single_base_rule(p, "D", x = size + gb1$vp$width + unit(25, "mm")+ size, y = unit(1, "npc") - size - gb5$vp$height, just = c("left", "top"))
    nc = length(gb6$children)
    gb6$children[[nc]]$width = gb6$children[[nc]]$width + unit(30, "mm")
    grid.draw(gb6)

    gb7 = grob_single_base_rule(p, "P", x = size + gb1$vp$width + unit(25, "mm")+ size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height, just = c("left", "top"))
    nc = length(gb7$children)
    gb7$children[[nc]]$width = gb7$children[[nc]]$width + unit(30, "mm")
    grid.draw(gb7)

    gb8 = grob_single_base_rule(p, "Q", x = size + gb1$vp$width + unit(25, "mm")+ size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height - gb7$vp$height, just = c("left", "top"))
    nc = length(gb8$children)
    gb8$children[[nc]]$width = gb8$children[[nc]]$width + unit(30, "mm")
    grid.draw(gb8)

    gb9 = grob_single_base_rule(p, "C", x = size + gb1$vp$width + unit(25, "mm")+ size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height - gb7$vp$height - gb8$vp$height, just = c("left", "top"))
    nc = length(gb9$children)
    gb9$children[[nc]]$width = gb9$children[[nc]]$width + unit(30, "mm")
    grid.draw(gb9)

}
