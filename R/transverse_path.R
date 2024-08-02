
######

#' All transverse paths of a sequence
#' @rdname transverse_path
#' @param rules An `sfc_rules` object.
#' @param p An `sfc_sequence` sequence. `p` and `rules` should have the same universe base set.
#'        Please provide `p` as a small sequence because the total number of all transverse paths might be very huge.
#' @export
#' @details
#' Given an input sequence with rotations, `all_transverse_paths()` lists all combinations of expansion 
#' codes from the first letter to the last letter in `p` (i.e. all possible transverse paths).
all_transverse_paths = function(rules, p) {

    if(!identical(sfc_universe(rules), sfc_universe(p))) {
        stop_wrap("Universe set of `rules` and `p` should be identical.")
    }

    rules = rules@rules
    seq = p@seq
    rot = p@rot
    n = length(seq)

    pl = list()
    prev_code = seq_along(rules[[ as.character(seq[1]) ]])
    if(n == 1) {
        return(lapply(prev_code, function(x) x))
    }

    for(i in seq_along(seq)[-1]) {

        # pick current code accroding to prev_code
        link = list(prev = integer(0), curr = integer(0))
        prev_code2 = NULL
        for(pc in prev_code) {
            prev_corner = rules[[ as.character(seq[i-1]) ]][[pc]]@corner
            prev_corner = adjust_corner(prev_corner, rot[i-1])
            for(j in seq_along(rules[[ as.character(seq[i]) ]])) {
                curr_corner = rules[[ as.character(seq[i]) ]][[j]]@corner
                curr_corner = adjust_corner(curr_corner, rot[i])
                
                if(prev_corner[2] != curr_corner[[1]]) {
                    link$prev = c(link$prev, paste0(i-1, "-", pc))
                    link$curr = c(link$curr, paste0(i, "-", j))
                    prev_code2 = c(prev_code2, j)
                }
                
            }
        }
        prev_code2 = unique(prev_code2)
        if(length(prev_code2) == 0) {
            stop_wrap("Sequence cannot expand completely at `p[", i, "]` (letter = \"", seq[i], "\", rot = ", rot[i], ").")
        }
        prev_code = prev_code2
        link = cbind(link$prev, link$curr)
        pl[[i-1]] = link
    }
    m = do.call(rbind, pl)
    g = igraph::graph_from_edgelist(m)

    e = which(igraph::degree(g, mode = "out") == 0 & grepl(paste0("^", n, "-"), igraph::V(g)$name))
    s = which(grepl(paste0("^1-"), igraph::V(g)$name))

    do.call("c", lapply(s, function(x) {
        lapply(igraph::all_shortest_paths(g, x, to = e)$vpath, function(pp) {
            as.integer(gsub("^\\d+-", "", names(pp)))
        })
    }))
}

adjust_corner = function(corner, rot) {
    if(rot == 90 || rot == 270) {
        rev_corner(corner)
    } else {
        corner
    }
}

rev_corner = function(corner) {
    ifelse(corner == 1L, 2L, 1L)
}

#' @rdname transverse_path
#' @export
#' @details
#' `get_one_transverse_path()` returns one random transverse path.
#' @examples
#' # expansion rules for the general 3x3 curves
#' p = SFC_RULES_3x3_COMBINED@rules$I[[3]]
#' get_one_transverse_path(SFC_RULES_3x3_COMBINED, p)
#' get_one_transverse_path(SFC_RULES_3x3_COMBINED, p)
#' get_one_transverse_path(SFC_RULES_3x3_COMBINED, p)
#' get_one_transverse_path(SFC_RULES_3x3_COMBINED, p)
get_one_transverse_path = function(rules, p) {

    if(!identical(sfc_universe(rules), sfc_universe(p))) {
        stop_wrap("Universe set of `rules` and `p` should be identical.")
    }

    rules = rules@rules
    seq = p@seq
    rot = p@rot
    n = length(seq)

    pl = list()
    prev_code = sample(length(rules[[ as.character(seq[1]) ]]), 1)
    if(n == 1) {
        return(prev_code)
    }

    for(i in seq_along(seq)[-1]) {

        # pick current code accroding to prev_code
        link = list(prev = integer(0), curr = integer(0))
        prev_code2 = NULL
        
        pc = prev_code
        prev_corner = rules[[ as.character(seq[i-1]) ]][[pc]]@corner
        prev_corner = adjust_corner(prev_corner, rot[i-1])
        for(j in seq_along(rules[[ as.character(seq[i]) ]])) {
            curr_corner = rules[[ as.character(seq[i]) ]][[j]]@corner
            curr_corner = adjust_corner(curr_corner, rot[i])
            
            if(prev_corner[2] != curr_corner[[1]]) {
                link$prev = c(link$prev, paste0(i-1, "-", pc))
                link$curr = c(link$curr, paste0(i, "-", j))
                prev_code2 = c(prev_code2, j)
            }
            
        }
        
        if(length(prev_code2) == 0) {
            return(integer(0))
        }

        kk = sample(length(prev_code2), 1)

        prev_code = prev_code2[kk]
        link = cbind(link$prev[kk], link$curr[kk])
        pl[[i-1]] = link
    }
    pp = c(pl[[1]][[1]], sapply(pl, function(x) x[2]))
    as.integer(gsub("^\\d+-", "", pp))
}

#' @rdname transverse_path
#' @param type If the value is `"11|22"`, it highlights the paths only via 1-1/2-2 corners. If the value is `"12|21"`, it highlights the paths
#'       only via 1-2/2-1 corners.
#' @export
#' @examples
#' # 
#' p = SFC_RULES_3x3_COMBINED@rules$I[[3]]
#' plot_transverse_paths(SFC_RULES_3x3_COMBINED, p)
#' plot_transverse_paths(SFC_RULES_3x3_COMBINED, p, type = "11|22")
#' plot_transverse_paths(SFC_RULES_3x3_COMBINED, p, type = "12|21")
#' 
#' # Hilbert curve
#' p = sfc_hilbert("I", 11)
#' plot_transverse_paths(SFC_RULES_HILBERT, p)
#' 
#' # Peano curve
#' p = sfc_peano("I", 1)
#' plot_transverse_paths(SFC_RULES_PEANO, p)
#' 
#' # Meander curve
#' p = sfc_meander("I", 1)
#' plot_transverse_paths(SFC_RULES_MEANDER, p)
plot_transverse_paths = function(rules, p, type = c("all", "11|22", "12|21")) {

    if(!identical(sfc_universe(rules), sfc_universe(p))) {
        stop_wrap("Universe set of `rules` and `p` should be identical.")
    }

    paths = all_transverse_paths(rules, p)

    rules = rules@rules
    seq = p@seq
    rot = p@rot
    n = length(seq)

    maxr = max(sapply(rules, length))

    grid.newpage()
    pushViewport(viewport(xscale = c(1, max(2, n)), yscale = c(1, max(2, maxr)), x = unit(25, "mm"), width = unit(1, "npc") - unit(40, "mm"), y = unit(25, "mm"), height = unit(1, "npc") - unit(35, "mm"), just = c("left", "bottom")))
    grid.text(1:maxr, unit(1, "native") - unit(10, "mm"), seq(1, maxr), default.units = "native", gp = gpar(fontsize = 10))
    grid.text("Expansion code", unit(1, "native") - unit(18, "mm"), (1+maxr)/2, default.units = "native", rot = 90)
    grid.text(paste0(seq, "(", rot, ")"), 1:n, unit(1, "native") - unit(14, "mm"), default.units = "native", gp = gpar(fontsize = 10))
    
    type = match.arg(type)
    l2 = lapply(paths, function(path) {
        l = rep(FALSE, length(path))
        for(i in seq_along(path)) {
            corner = adjust_corner( rules[[ seq[i] ]][[ path[i] ]]@corner, rot[i])
            if(type == "11|22") {
                if(corner[1] == corner[2]) {
                    l[i] = TRUE
                }
            }else if(type == "12|21") {
                if(corner[1] != corner[2]) {
                    l[i] = TRUE
                }
            } else {
                l[i] = TRUE
            }
        }
        l
    })
    l2 = sapply(l2, all)

    for(i in order(l2)) {
        path = paths[[i]]
        np = length(path)
        pos = cbind(1:(np-1), path[-np], 2:np, path[-1])
        theta = atan( (pos[, 4] - pos[, 2])/(pos[, 3] - pos[, 1]) )
        len_x = 2/convertHeight(unit(1, "npc"), "mm", valueOnly = TRUE)*(maxr-1)
        len_y = len_x
        pos[, 1] = pos[, 1] + len_x*cos(theta)
        pos[, 2] = pos[, 2] + len_y*sin(theta)
        pos[, 3] = pos[, 3] - len_x*cos(theta)
        pos[, 4] = pos[, 4] - len_y*sin(theta)
        col = ifelse(l2[i], "black", "#DDDDDD")
        grid.segments(pos[, 1], pos[, 2], pos[, 3], pos[, 4], default.units = "native", gp = gpar(col = col, fill = col), arrow = arrow(length = unit(6, "pt"), angle = 15, type = "closed"))
    }

    for(i in seq_len(n)) {
        k = length(rules[[ seq[i] ]])
        for(j in 1:k) {
            corner = adjust_corner( rules[[ seq[i] ]][[j]]@corner, rot[i])
            col = "#DDDDDD"
            pch = 1
            if(type == "11|22") {
                if(corner[1] == corner[2]) {
                    col = "black"
                    pch = 16
                }
            } else if(type == "12|21") {
                if(corner[1] != corner[2]) {
                    col = "black"
                    pch = 16
                }
            } else {
                col = "black"
                pch = 16
            }
            grid.points(i, j, default.units = "native", pch = pch, size = unit(8, "pt"), gp = gpar(col = col))
            grid.text(paste0("(", corner[1], ",", corner[2], ")"), i, unit(j, "native")-unit(4, "mm"), default.units = "native", gp = gpar(fontsize = 8, col = col))
        }
    }
    popViewport()
}
