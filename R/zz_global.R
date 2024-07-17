

r = 0.3
s = 0.6
arrow_length = grid::unit(0.2, "native")
size =  grid::unit(5, "mm")

I_base = function(...) {
    gbl = list()

    gbl[[1]] = linesGrob(c(0, 0), c(-s, s), default.units = "native", gp = gpar(col = "grey"), arrow = arrow(angle = 15, length = arrow_length))
    gbl[[2]] = pointsGrob(0, 0, default.units = "native", pch = 16, size = unit(4, "pt"))

    grobTree(gbl[[1]], gbl[[2]], vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1), width = 2*size, height = 2*size, ...))
}

R_base = function(...) {
    gbl = list()

    gbl[[1]] = linesGrob(c(0, 0, s), c(-s, 0, 0), default.units = "native", gp = gpar(col = "grey"), arrow = arrow(angle = 15, length = arrow_length))
    gbl[[2]] = pointsGrob(0, 0, default.units = "native", pch = 16, size = unit(4, "pt"))

    grobTree(gbl[[1]], gbl[[2]], vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1), width = 2*size, height = 2*size, ...))
}

L_base = function(...) {
    gbl = list()

    gbl[[1]] = linesGrob(c(0, 0, -s), c(-s, 0, 0), default.units = "native", gp = gpar(col = "grey"), arrow = arrow(angle = 15, length = arrow_length))
    gbl[[2]] = pointsGrob(0, 0, default.units = "native", pch = 16, size = unit(4, "pt"))

    grobTree(gbl[[1]], gbl[[2]], vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1), width = 2*size, height = 2*size, ...))
}


U_base = function(...) {
    l = convertWidth(arrow_length, "native", valueOnly = TRUE)*1.2
    gbl = list()

    gbl[[1]] = linesGrob(c(0-0.05, 0-0.05, 0-0.05-l*sin(30/2/180*pi)), c(0, -s, l*cos(30/2/180*pi)-s), default.units = "native", gp = gpar(col = "grey"))
    gbl[[2]] = linesGrob(c(0+0.05, 0+0.05, 0+0.05+l*sin(30/2/180*pi)), c(-s, 0,  - l*cos(30/2/180*pi)), default.units = "native", gp = gpar(col = "grey"))
    gbl[[3]] = pointsGrob(0, 0, default.units = "native", pch = 16, size = unit(4, "pt"))

    grobTree(gbl[[1]], gbl[[2]], gbl[[3]], vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1), width = 2*size, height = 2*size, ...))
}

C_base = function(...) {
    theta = seq(0, 360)/180*pi
    x = cos(theta)*r
    y = sin(theta)*r
    n = length(x)

    gbl = list()
    gbl[[1]] = linesGrob(x, y, default.units = "native", gp = gpar(col = "grey"))
    gbl[[2]] = linesGrob(c(x[n-1]-0.002, x[n]), c(y[n-1], y[n]), default.units = "native", gp = gpar(col = "grey"), arrow = arrow(angle = 30, length = arrow_length))
    gbl[[3]] = pointsGrob(0, 0, default.units = "native", pch = 16, size = unit(4, "pt"))

    grobTree(gbl[[1]], gbl[[2]], gbl[[3]], vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1), width = 2*size, height = 2*size, ...))
}


B_base = function(...) {
    gbl = list()

    gbl[[1]] = linesGrob(c(0, 0), c(0, s), default.units = "native", gp = gpar(col = "grey"), arrow = arrow(angle = 15, length = arrow_length))
    theta = seq(0, 360)/180*pi
    x = cos(theta)*r
    y = sin(theta)*r
    n = length(x)
    gbl[[2]] = linesGrob(x+r, y, default.units = "native", gp = gpar(col = "grey"))
    gbl[[3]] = pointsGrob(0, 0, default.units = "native", pch = 16, size = unit(4, "pt"))

    grobTree(gbl[[1]], gbl[[2]], gbl[[3]], vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1), width = 2*size, height = 2*size, ...))
}

D_base = function(...) {
    gbl = list()

    gbl[[1]] = linesGrob(c(0, 0), c(0, s), default.units = "native", gp = gpar(col = "grey"), arrow = arrow(angle = 15, length = arrow_length))
    theta = seq(0, 360)/180*pi
    x = cos(theta)*r
    y = sin(theta)*r
    n = length(x)
    gbl[[2]] = linesGrob(x-r, y, default.units = "native", gp = gpar(col = "grey"))
    gbl[[3]] = pointsGrob(0, 0, default.units = "native", pch = 16, size = unit(4, "pt"))

    grobTree(gbl[[1]], gbl[[2]], gbl[[3]], vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1), width = 2*size, height = 2*size, ...))
}


P_base = function(...) {
    gbl = list()

    gbl[[1]] = linesGrob(c(0, 0), c(-s, 0), default.units = "native", gp = gpar(col = "grey"))
    gbl[[2]] = linesGrob(c(0, 0), c(-s, -s/2), default.units = "native", gp = gpar(col = "grey"), arrow = arrow(angle = 15, length = arrow_length))
    theta = seq(0, 360)/180*pi
    x = cos(theta)*r
    y = sin(theta)*r
    n = length(x)
    gbl[[3]] = linesGrob(x+r, y, default.units = "native", gp = gpar(col = "grey"))
    gbl[[4]] = pointsGrob(0, 0, default.units = "native", pch = 16, size = unit(4, "pt"))

    grobTree(gbl[[1]], gbl[[2]], gbl[[3]], gbl[[4]], vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1), width = 2*size, height = 2*size, ...))
}


Q_base = function(...) {
    gbl = list()

    gbl[[1]] = linesGrob(c(0, 0), c(-s, 0), default.units = "native", gp = gpar(col = "grey"))
    gbl[[2]] = linesGrob(c(0, 0), c(-s, -s/2), default.units = "native", gp = gpar(col = "grey"), arrow = arrow(angle = 15, length = arrow_length))
    theta = seq(0, 360)/180*pi
    x = cos(theta)*r
    y = sin(theta)*r
    n = length(x)
    gbl[[3]] = linesGrob(x-r, y, default.units = "native", gp = gpar(col = "grey"))
    gbl[[4]] = pointsGrob(0, 0, default.units = "native", pch = 16, size = unit(4, "pt"))

    grobTree(gbl[[1]], gbl[[2]], gbl[[3]], gbl[[4]], vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1), width = 2*size, height = 2*size, ...))
}


pdf(NULL)
oe = try({
    BASE_I = sfc_base("I", in_direction = 90, out_direction = 90, primary = TRUE, open = TRUE, grob = I_base())
    BASE_J = BASE_I
    BASE_R = sfc_base("R", in_direction = 90, out_direction = 0, primary = TRUE, open = TRUE, grob = R_base())
    BASE_L = sfc_base("L", in_direction = 90, out_direction = 180, primary = TRUE, open = TRUE, grob = L_base())
    BASE_U = sfc_base("U", in_direction = -90, out_direction = 90, primary = FALSE, open = TRUE, grob = U_base())
    BASE_B = sfc_base("B", in_direction = NA, out_direction = 90, primary = FALSE, open = FALSE, grob = B_base())
    BASE_D = sfc_base("D", in_direction = NA, out_direction = 90, primary = FALSE, open = FALSE, grob = D_base())
    BASE_P = sfc_base("P", in_direction = 90, out_direction = NA, primary = FALSE, open = FALSE, grob = P_base())
    BASE_Q = sfc_base("Q", in_direction = 90, out_direction = NA, primary = FALSE, open = FALSE, grob = Q_base())
    BASE_C = sfc_base("C", in_direction = NA, out_direction = NA, primary = FALSE, open = FALSE, grob = C_base())
}, silent = TRUE)
dev.off()

if(inherits(oe, "try-error")) {
    stop(oe)
}

BASE_LIST = list(
    "I" = BASE_I,
    "J" = BASE_J,
    "R" = BASE_R,
    "L" = BASE_L,
    "U" = BASE_U,
    "B" = BASE_B,
    "D" = BASE_D,
    "P" = BASE_P,
    "Q" = BASE_Q,
    "C" = BASE_C
)

UNIVERSE_GLOBAL = c("I", "J", "R", "L", "U", "B", "D", "P", "Q", "C")


UNIVERSE_HILBERT = c("I", "R", "L", "U", "B", "D", "P", "Q", "C")
RULES_HILBERT = list()
RULES_HILBERT[["I"]][[1]] = sfc_unit(c("R", "L", "L", "R"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["I"]][[2]] = sfc_unit(c("L", "R", "R", "L"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["R"]][[1]] = sfc_unit(c("I", "R", "R", "L"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["R"]][[2]] = sfc_unit(c("L", "R", "R", "I"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["L"]][[1]] = sfc_unit(c("R", "L", "L", "I"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["L"]][[2]] = sfc_unit(c("I", "L", "L", "R"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["U"]][[1]] = sfc_unit(c("I", "R", "R", "I"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["U"]][[2]] = sfc_unit(c("I", "L", "L", "I"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["B"]][[1]] = sfc_unit(c("R", "R", "R", "I"), rot = 270, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["B"]][[2]] = sfc_unit(c("L", "L", "L", "R"), rot = 180, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["D"]][[1]] = sfc_unit(c("L", "L", "L", "I"), rot = 90, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["D"]][[2]] = sfc_unit(c("R", "R", "R", "L"), rot = 180, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["P"]][[1]] = sfc_unit(c("I", "R", "R", "R"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["P"]][[2]] = sfc_unit(c("R", "L", "L", "L"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["Q"]][[1]] = sfc_unit(c("I", "L", "L", "L"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["Q"]][[2]] = sfc_unit(c("L", "R", "R", "R"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["C"]][[1]] = sfc_unit(c("L", "L", "L", "L"), rot = 0, universe = UNIVERSE_HILBERT)
RULES_HILBERT[["C"]][[2]] = sfc_unit(c("R", "R", "R", "R"), rot = 0, universe = UNIVERSE_HILBERT)


SFC_RULES_HILBERT = sfc_rules(rules = RULES_HILBERT, name = "Hilbert",
	bases = BASE_LIST[UNIVERSE_HILBERT])

###=========================================

UNIVERSE_PEANO = c("I", "J", "R", "L")

RULES_SET_PEANO = list()
RULES_SET_PEANO[["I"]][[1]] = sfc_unit("IJRRILLJI", rot = 0, universe = UNIVERSE_PEANO)  
RULES_SET_PEANO[["J"]][[1]] = sfc_unit("JILLJRRIJ", rot = 0, universe = UNIVERSE_PEANO)
RULES_SET_PEANO[["R"]][[1]] = sfc_unit("IJRRILLJR", rot = 0, universe = UNIVERSE_PEANO)      
RULES_SET_PEANO[["L"]][[1]] = sfc_unit("JILLJRRIL", rot = 0, universe = UNIVERSE_PEANO) 


SFC_RULES_PEANO = sfc_rules(rules = RULES_SET_PEANO, name = "Peano",
	bases = BASE_LIST[UNIVERSE_PEANO])

RULES_SET_PEANO_FLIP = list()
RULES_SET_PEANO_FLIP[["I"]][[1]] = sfc_unit("RILLJRRIL", rot = 0, universe = UNIVERSE_PEANO)  
RULES_SET_PEANO_FLIP[["J"]][[1]] = sfc_unit("LJRRILLJR", rot = 0, universe = UNIVERSE_PEANO)
RULES_SET_PEANO_FLIP[["R"]][[1]] = sfc_unit("RILLJRRIJ", rot = 0, universe = UNIVERSE_PEANO)      
RULES_SET_PEANO_FLIP[["L"]][[1]] = sfc_unit("LJRRILLJI", rot = 0, universe = UNIVERSE_PEANO) 

SFC_RULES_PEANO_FLIP = sfc_rules(rules = RULES_SET_PEANO_FLIP, name = "Peano_flip",
    bases = BASE_LIST[UNIVERSE_PEANO])

#===================================

UNIVERSE_MEANDER = c("I", "R", "L", "U", "B", "D", "P", "Q", "C")

RULES_MEANDER = list()
RULES_MEANDER[["I"]][[1]] = sfc_unit("RILILLRRI", rot = 0, universe = UNIVERSE_MEANDER)  
RULES_MEANDER[["I"]][[2]] = sfc_unit("LIRIRRLLI", rot = 0, universe = UNIVERSE_MEANDER)
RULES_MEANDER[["R"]][[1]] = sfc_unit("IIRIRRLLI", rot = 0, universe = UNIVERSE_MEANDER)      
RULES_MEANDER[["R"]][[2]] = sfc_unit("LIRIRRLLR", rot = 0, universe = UNIVERSE_MEANDER)
RULES_MEANDER[["L"]][[1]] = sfc_unit("RILILLRRL", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDER[["L"]][[2]] = sfc_unit("IILILLRRI", rot = 0, universe = UNIVERSE_MEANDER) 
RULES_MEANDER[["U"]][[1]] = sfc_unit("IIRIRRLLR", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDER[["U"]][[2]] = sfc_unit("IILILLRRL", rot = 0, universe = UNIVERSE_MEANDER) 
RULES_MEANDER[["B"]][[1]] = sfc_unit("LILILLRRI", rot = 180, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDER[["B"]][[2]] = sfc_unit("RIRIRRLLR", rot = 270, universe = UNIVERSE_MEANDER) 
RULES_MEANDER[["D"]][[1]] = sfc_unit("LILILLRRL", rot = 90, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDER[["D"]][[2]] = sfc_unit("RIRIRRLLI", rot = 180, universe = UNIVERSE_MEANDER) 
RULES_MEANDER[["P"]][[1]] = sfc_unit("IIRIRRLLL", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDER[["P"]][[2]] = sfc_unit("RILILLRRR", rot = 0, universe = UNIVERSE_MEANDER) 
RULES_MEANDER[["Q"]][[1]] = sfc_unit("IILILLRRR", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDER[["Q"]][[2]] = sfc_unit("LIRIRRLLL", rot = 0, universe = UNIVERSE_MEANDER) 
RULES_MEANDER[["C"]][[1]] = sfc_unit("LILILLRRR", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDER[["C"]][[2]] = sfc_unit("RIRIRRLLL", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2


SFC_RULES_MEANDER = sfc_rules(rules = RULES_MEANDER, name = "Meander",
    bases = BASE_LIST[UNIVERSE_MEANDER])

RULES_MEANDERL_FLIP = list()
RULES_MEANDERL_FLIP[["I"]][[1]] = sfc_unit("IRRLLILIR", rot = 0, universe = UNIVERSE_MEANDER)  
RULES_MEANDERL_FLIP[["I"]][[2]] = sfc_unit("ILLRRIRIL", rot = 0, universe = UNIVERSE_MEANDER)
RULES_MEANDERL_FLIP[["R"]][[1]] = sfc_unit("RLLRRIRIL", rot = 0, universe = UNIVERSE_MEANDER)      
RULES_MEANDERL_FLIP[["R"]][[2]] = sfc_unit("ILLRRIRII", rot = 0, universe = UNIVERSE_MEANDER)
RULES_MEANDERL_FLIP[["L"]][[1]] = sfc_unit("IRRLLILII", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDERL_FLIP[["L"]][[2]] = sfc_unit("LRRLLILIR", rot = 0, universe = UNIVERSE_MEANDER) 
RULES_MEANDERL_FLIP[["U"]][[1]] = sfc_unit("RLLRRIRII", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDERL_FLIP[["U"]][[2]] = sfc_unit("LRRLLILII", rot = 0, universe = UNIVERSE_MEANDER) 
RULES_MEANDERL_FLIP[["B"]][[1]] = sfc_unit("RRRLLILIR", rot = 180, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDERL_FLIP[["B"]][[2]] = sfc_unit("LLLRRIRII", rot = 270, universe = UNIVERSE_MEANDER) 
RULES_MEANDERL_FLIP[["D"]][[1]] = sfc_unit("RRRLLILII", rot = 90, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDERL_FLIP[["D"]][[2]] = sfc_unit("LLLRRIRIL", rot = 180, universe = UNIVERSE_MEANDER) 
RULES_MEANDERL_FLIP[["P"]][[1]] = sfc_unit("RLLRRIRIR", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDERL_FLIP[["P"]][[2]] = sfc_unit("IRRLLILIL", rot = 0, universe = UNIVERSE_MEANDER) 
RULES_MEANDERL_FLIP[["Q"]][[1]] = sfc_unit("LRRLLILIL", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDERL_FLIP[["Q"]][[2]] = sfc_unit("ILLRRIRIR", rot = 0, universe = UNIVERSE_MEANDER) 
RULES_MEANDERL_FLIP[["C"]][[1]] = sfc_unit("LLLRRIRIR", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2
RULES_MEANDERL_FLIP[["C"]][[2]] = sfc_unit("RRRLLILIL", rot = 0, universe = UNIVERSE_MEANDER)                    # 1, 2


SFC_RULES_MEANDER_FLIP = sfc_rules(rules = RULES_MEANDERL_FLIP, name = "Meander_flip",
    bases = BASE_LIST[UNIVERSE_MEANDER])


####



#' Base patterns
#' @rdname base_patterns
#' @export
"BASE_I"

#' @rdname base_patterns
#' @export
"BASE_J"

#' @rdname base_patterns
#' @export
"BASE_R"

#' @rdname base_patterns
#' @export
"BASE_L"

#' @rdname base_patterns
#' @export
"BASE_U"

#' @rdname base_patterns
#' @export
"BASE_B"

#' @rdname base_patterns
#' @export
"BASE_D"

#' @rdname base_patterns
#' @export
"BASE_P"

#' @rdname base_patterns
#' @export
"BASE_Q"

#' @rdname base_patterns
#' @export
"BASE_LIST"

#' @rdname base_patterns
#' @export
"BASE_C"


#' Rules
#' @rdname pre_defined_rules
#' @export
"SFC_RULES_HILBERT"

#' @rdname pre_defined_rules
#' @export
"SFC_RULES_PEANO"

#' @rdname pre_defined_rules
#' @export
"SFC_RULES_PEANO_FLIP"

#' @rdname pre_defined_rules
#' @export
"SFC_RULES_MEANDER"

#' @rdname pre_defined_rules
#' @export
"SFC_RULES_MEANDER_FLIP"

