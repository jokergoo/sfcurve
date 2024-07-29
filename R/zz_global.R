

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
    BASE_J = sfc_base("J", in_direction = 90, out_direction = 90, primary = TRUE, open = TRUE, grob = I_base())
    BASE_R = sfc_base("R", in_direction = 90, out_direction = 0, primary = TRUE, open = TRUE, grob = R_base())
    BASE_L = sfc_base("L", in_direction = 90, out_direction = 180, primary = TRUE, open = TRUE, grob = L_base())
    BASE_U = sfc_base("U", in_direction = 90, out_direction = -90, primary = FALSE, open = TRUE, grob = U_base())
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



###=========================================

UNIVERSE_PEANO = c("I", "J", "R", "L")

RULES_PEANO = list()
RULES_PEANO[["I"]][[1]] = sfc_unit("IJRRILLJI", rot = 0, universe = UNIVERSE_PEANO)  
RULES_PEANO[["J"]][[1]] = sfc_unit("JILLJRRIJ", rot = 0, universe = UNIVERSE_PEANO)
RULES_PEANO[["R"]][[1]] = sfc_unit("IJRRILLJR", rot = 0, universe = UNIVERSE_PEANO)      
RULES_PEANO[["L"]][[1]] = sfc_unit("JILLJRRIL", rot = 0, universe = UNIVERSE_PEANO) 


RULES_PEANO_FLIP = list()
RULES_PEANO_FLIP[["I"]][[1]] = sfc_unit("RILLJRRIL", rot = 0, universe = UNIVERSE_PEANO)  
RULES_PEANO_FLIP[["J"]][[1]] = sfc_unit("LJRRILLJR", rot = 0, universe = UNIVERSE_PEANO)
RULES_PEANO_FLIP[["R"]][[1]] = sfc_unit("RILLJRRIJ", rot = 0, universe = UNIVERSE_PEANO)      
RULES_PEANO_FLIP[["L"]][[1]] = sfc_unit("LJRRILLJI", rot = 0, universe = UNIVERSE_PEANO) 





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


####

UNIVERSE_3x3_COMBINED = c("I", "R", "L", "U", "B", "D", "P", "Q", "C")
RULES_3x3_COMBINED = list()

RULES_3x3_COMBINED[["I"]][[1]] = sfc_unit("RILILLRRI", rot = 0, universe = UNIVERSE_3x3_COMBINED)  
RULES_3x3_COMBINED[["I"]][[2]] = sfc_unit("LIRIRRLLI", rot = 0, universe = UNIVERSE_3x3_COMBINED)
RULES_3x3_COMBINED[["R"]][[1]] = sfc_unit("IIRIRRLLI", rot = 0, universe = UNIVERSE_3x3_COMBINED)      
RULES_3x3_COMBINED[["R"]][[2]] = sfc_unit("LIRIRRLLR", rot = 0, universe = UNIVERSE_3x3_COMBINED)
RULES_3x3_COMBINED[["L"]][[1]] = sfc_unit("RILILLRRL", rot = 0, universe = UNIVERSE_3x3_COMBINED)                    # 1, 2
RULES_3x3_COMBINED[["L"]][[2]] = sfc_unit("IILILLRRI", rot = 0, universe = UNIVERSE_3x3_COMBINED) 

RULES_3x3_COMBINED[["I"]][[3]] = sfc_unit("IIRRILLII", rot = 0, universe = UNIVERSE_3x3_COMBINED)  
RULES_3x3_COMBINED[["I"]][[4]] = sfc_unit("IILLIRRII", rot = 0, universe = UNIVERSE_3x3_COMBINED)
RULES_3x3_COMBINED[["R"]][[3]] = sfc_unit("IIRRILLIR", rot = 0, universe = UNIVERSE_3x3_COMBINED)      
RULES_3x3_COMBINED[["L"]][[3]] = sfc_unit("IILLIRRIL", rot = 0, universe = UNIVERSE_3x3_COMBINED) 

RULES_3x3_COMBINED[["U"]][[1]] = sfc_unit("IIRIRRLLR", rot = 0, universe = UNIVERSE_3x3_COMBINED)                    # 1, 2
RULES_3x3_COMBINED[["U"]][[2]] = sfc_unit("IILILLRRL", rot = 0, universe = UNIVERSE_3x3_COMBINED) 
RULES_3x3_COMBINED[["B"]][[1]] = sfc_unit("LILILLRRI", rot = 180, universe = UNIVERSE_3x3_COMBINED)  
RULES_3x3_COMBINED[["B"]][[2]] = sfc_unit("RIRIRRLLR", rot = 270, universe = UNIVERSE_3x3_COMBINED)                    # 1, 2
RULES_3x3_COMBINED[["B"]][[3]] = sfc_unit("LILLIRRII", rot = 270, universe = UNIVERSE_3x3_COMBINED)                    # 1, 2

RULES_3x3_COMBINED[["D"]][[1]] = sfc_unit("LILILLRRL", rot = 90, universe = UNIVERSE_3x3_COMBINED)  
RULES_3x3_COMBINED[["D"]][[2]] = sfc_unit("RIRIRRLLI", rot = 180, universe = UNIVERSE_3x3_COMBINED)                   # 1, 2
RULES_3x3_COMBINED[["D"]][[3]] = sfc_unit("RIRRILLII", rot = 90, universe = UNIVERSE_3x3_COMBINED)                    # 1, 2
RULES_3x3_COMBINED[["P"]][[1]] = sfc_unit("IIRIRRLLL", rot = 0, universe = UNIVERSE_3x3_COMBINED)                    # 1, 2
RULES_3x3_COMBINED[["P"]][[2]] = sfc_unit("RILILLRRR", rot = 0, universe = UNIVERSE_3x3_COMBINED)
RULES_3x3_COMBINED[["P"]][[3]] = sfc_unit("IIRRILLIL", rot = 0, universe = UNIVERSE_3x3_COMBINED) 
RULES_3x3_COMBINED[["Q"]][[1]] = sfc_unit("IILILLRRR", rot = 0, universe = UNIVERSE_3x3_COMBINED)                    # 1, 2
RULES_3x3_COMBINED[["Q"]][[2]] = sfc_unit("LIRIRRLLL", rot = 0, universe = UNIVERSE_3x3_COMBINED) 
RULES_3x3_COMBINED[["Q"]][[3]] = sfc_unit("IILLIRRIR", rot = 0, universe = UNIVERSE_3x3_COMBINED) 
RULES_3x3_COMBINED[["C"]][[1]] = sfc_unit("LILILLRRR", rot = 0, universe = UNIVERSE_3x3_COMBINED)                  # 1, 2
RULES_3x3_COMBINED[["C"]][[2]] = sfc_unit("RIRIRRLLL", rot = 0, universe = UNIVERSE_3x3_COMBINED)                    # 1, 2
RULES_3x3_COMBINED[["C"]][[3]] = sfc_unit("RIRRILLIL", rot = 90, universe = UNIVERSE_3x3_COMBINED)                    # 1, 2
RULES_3x3_COMBINED[["C"]][[4]] = sfc_unit("LILLIRRIR", rot = 270, universe = UNIVERSE_3x3_COMBINED)                   # 1, 2


###

.onLoad = function(libname, pkgname) {

    parent = parent.env(environment())
    SFC_RULES_HILBERT = sfc_rules(rules = RULES_HILBERT, name = "Hilbert",
        bases = BASE_LIST[UNIVERSE_HILBERT])

    SFC_RULES_PEANO = sfc_rules(rules = RULES_PEANO, flip = RULES_PEANO_FLIP,
        name = "Peano", bases = BASE_LIST[UNIVERSE_PEANO])

    RULES_MEANDER_FLIP = lapply(RULES_MEANDER, function(x) {
        lapply(x, function(u) sfc_flip_unit(u, BASE_LIST))
    })
    SFC_RULES_MEANDER = sfc_rules(rules = RULES_MEANDER, flip = RULES_MEANDER_FLIP,
        name = "Meander",
        bases = BASE_LIST[UNIVERSE_MEANDER])

    RULES_3x3_COMBINED_FLIP = lapply(RULES_3x3_COMBINED, function(x) {
        lapply(x, function(u) sfc_flip_unit(u, BASE_LIST))
    })

    SFC_RULES_3x3_COMBINED = sfc_rules(rules = RULES_3x3_COMBINED, flip = RULES_3x3_COMBINED_FLIP,
        name = "3x3_combined",
        bases = BASE_LIST[UNIVERSE_3x3_COMBINED])


    RULES_4x4_MEANDER_1_FLIP = lapply(RULES_4x4_MEANDER_1, function(x) {
        lapply(x, function(u) sfc_flip_unit(u, BASE_LIST))
    })

    SFC_RULES_4x4_MEANDER_1 = sfc_rules(rules = RULES_4x4_MEANDER_1, flip = RULES_4x4_MEANDER_1_FLIP,
        name = "Meander 4x4 type 1",
        bases = BASE_LIST[UNIVERSE_4x4_MEANDER])

    RULES_4x4_MEANDER_2_FLIP = lapply(RULES_4x4_MEANDER_2, function(x) {
        lapply(x, function(u) sfc_flip_unit(u, BASE_LIST))
    })
    SFC_RULES_4x4_MEANDER_2 = sfc_rules(rules = RULES_4x4_MEANDER_2, flip = RULES_4x4_MEANDER_2_FLIP,
        name = "Meander 4x4 type 2",
        bases = BASE_LIST[UNIVERSE_4x4_MEANDER])

    assign("SFC_RULES_HILBERT", SFC_RULES_HILBERT, envir = parent)
    assign("SFC_RULES_PEANO", SFC_RULES_PEANO, envir = parent)
    assign("SFC_RULES_MEANDER", SFC_RULES_MEANDER, envir = parent)
    assign("SFC_RULES_3x3_COMBINED", SFC_RULES_3x3_COMBINED, envir = parent)
    assign("SFC_RULES_4x4_MEANDER_1", SFC_RULES_4x4_MEANDER_1, envir = parent)
    assign("SFC_RULES_4x4_MEANDER_2", SFC_RULES_4x4_MEANDER_2, envir = parent)
}

#' Base patterns
#' 
#' A list of pre-defined base patterns. See the **Examples** section.
#' 
#' `BASE_I` and `BASE_J` are identical. They are only used to distinguish the two
#' "going forward" patterns in the 3x3 mode for the Peano curves, i.e. bottom-left to top-right, and bottom-right
#' to top-left.
#' @rdname base_patterns
#' @export
#' @examples
#' BASE_I
#' BASE_J
#' BASE_R
#' BASE_L
#' BASE_U
#' BASE_B
#' BASE_D
#' BASE_P
#' BASE_Q
#' BASE_C
#' draw_multiple_curves(
#'     BASE_I, BASE_J, BASE_R, BASE_L, BASE_U,
#'     BASE_B, BASE_D, BASE_P, BASE_Q, BASE_C,
#'     nrow = 2
#' )
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
"BASE_C"

#' @rdname base_patterns
#' @export
"BASE_LIST"


#' Rules
#' 
#' A list of pre-defined expansion rules for different curves.
#' 
#' @details
#' `SFC_RULES_PEANO`, `SFC_RULES_MEANDER` and `SFC_RULES_3x3_COMBINED`, `SFC_RULES_MEANDER_4x4_1`, `SFC_RULES_MEANDER_4x4_2` also contain the "flipped" expansion rules.
#' @rdname pre_defined_rules
#' @export
#' @examples
#' SFC_RULES_HILBERT
#' SFC_RULES_PEANO
#' SFC_RULES_MEANDER
#' SFC_RULES_3x3_COMBINED
#' SFC_RULES_4x4_MEANDER_1
#' SFC_RULES_4x4_MEANDER_2
"SFC_RULES_HILBERT"

#' @rdname pre_defined_rules
#' @export
"SFC_RULES_PEANO"

#' @rdname pre_defined_rules
#' @export
"SFC_RULES_MEANDER"

#' @rdname pre_defined_rules
#' @details
#' `SFC_RULES_3x3_COMBINED` is a combination of `SFC_RULES_PEANO` and `SFC_RULES_PEANO` where
#' in `SFC_RULES_PEANO`, `J` is replaced by its original pattern `I`.
#' @export
"SFC_RULES_3x3_COMBINED"

#' @rdname pre_defined_rules
#' @export
"SFC_RULES_4x4_MEANDER_1"

#' @rdname pre_defined_rules
#' @details
#' `SFC_RULES_4x4_MEANDER_1` and `SFC_RULES_4x4_MEANDER_2` are extension rules of `SFC_RULES_MEANDER`
#' to the 4x4 curve. It is only for the demonstration purpose, thus only `I/R/L` are supported.
#' @export
"SFC_RULES_4x4_MEANDER_2"

UNIVERSE_4x4_MEANDER = c("I", "R", "L")

RULES_4x4_MEANDER_1 = list()
RULES_4x4_MEANDER_1[["I"]][[1]] = sfc_unit("IIRIRRLLILIILIIR", rot = 0, universe = UNIVERSE_4x4_MEANDER)  
RULES_4x4_MEANDER_1[["I"]][[2]] = sfc_unit("IILILLRRIRIIRIIL", rot = 0, universe = UNIVERSE_4x4_MEANDER)
RULES_4x4_MEANDER_1[["R"]][[1]] = sfc_unit("RILILLRRIRIIRIIL", rot = 0, universe = UNIVERSE_4x4_MEANDER)      
RULES_4x4_MEANDER_1[["R"]][[2]] = sfc_unit("IILILLRRIRIIRIII", rot = 0, universe = UNIVERSE_4x4_MEANDER)
RULES_4x4_MEANDER_1[["L"]][[1]] = sfc_unit("IIRIRRLLILIILIII", rot = 0, universe = UNIVERSE_4x4_MEANDER)
RULES_4x4_MEANDER_1[["L"]][[2]] = sfc_unit("LIRIRRLLILIILIIR", rot = 0, universe = UNIVERSE_4x4_MEANDER) 

RULES_4x4_MEANDER_2 = list()
RULES_4x4_MEANDER_2[["I"]][[1]] = sfc_unit("RIILIILLIRIRRLLR", rot = 0, universe = UNIVERSE_4x4_MEANDER)  
RULES_4x4_MEANDER_2[["I"]][[2]] = sfc_unit("LIIRIIRRILILLRRL", rot = 0, universe = UNIVERSE_4x4_MEANDER)
RULES_4x4_MEANDER_2[["R"]][[1]] = sfc_unit("IIIRIIRRILILLRRL", rot = 0, universe = UNIVERSE_4x4_MEANDER)      
RULES_4x4_MEANDER_2[["R"]][[2]] = sfc_unit("LIIRIIRRILILLRRI", rot = 0, universe = UNIVERSE_4x4_MEANDER)
RULES_4x4_MEANDER_2[["L"]][[1]] = sfc_unit("RIILIILLIRIRRLLI", rot = 0, universe = UNIVERSE_4x4_MEANDER)
RULES_4x4_MEANDER_2[["L"]][[2]] = sfc_unit("IIILIILLIRIRRLLR", rot = 0, universe = UNIVERSE_4x4_MEANDER) 

