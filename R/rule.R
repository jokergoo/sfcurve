
r = 0.3
s = 0.6
arrow_length = unit(0.2, "native")
size = unit(5, "mm")

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

grob_math = function(label, x, y, gp = gpar(), ...) {
    tex = TeX(paste0("$", label, "$"), italic = TRUE)
    gp$fontfamily = "Times"
    textGrob(tex, x, y, ..., gp = gp)
}

tex_pattern = function(x, which, p) {
    seq = p$seq
    rot = p$rot
    l = rot > 0
    rot[l] = paste0("^{", rot[l], "}")
    rot[!l] = ""

    tex = paste(seq, rot, sep = "", collapse = "")
    paste0(x, "_", which, "=", tex)
}

grob_single_base_rule = function(bp, increase = 2, ...) {

    base = sfc_initial(bp)

    base_pattern_set = get_base_pattern_set(increase)
    pl = base_pattern_set[[bp]]

    n = length(pl)
    nr = ceiling(n/2)

    k = increase

    vp_xscale = c(0, 13+2*k)
    vp_yscale = c(-(nr-1)*(k+1)-1, k+1)

    vp_width = diff(vp_xscale)*size
    vp_height = diff(vp_yscale)*size

    vp = viewport(xscale = vp_xscale, yscale = vp_yscale, width = vp_width, height = vp_height, ...)

    gbl = list()

    gbl[[1]] = grob_math(bp, 0.5, k/2, default.units = "native")

    if(bp == "U") {
        gbl[[2]] = U_base(x = 2, y = k/2, default.units = "native")
    } else if(bp == "C") {
        gbl[[2]] = C_base(x = 2, y = k/2, default.units = "native")
    } else if(bp == "B") {
        gbl[[2]] = B_base(x = 2, y = k/2, default.units = "native")
    } else if(bp == "D") {
        gbl[[2]] = D_base(x = 2, y = k/2, default.units = "native")
    } else if(bp == "P") {
        gbl[[2]] = P_base(x = 2, y = k/2, default.units = "native")
    } else if(bp == "Q") {
        gbl[[2]] = Q_base(x = 2, y = k/2, default.units = "native")
    } else {
        gbl[[2]] = grob_sfc_sequence(base, x = 2, y = k/2, default.units = "native")
        gbl[[2]]$children[[1]]$gp = gpar(col = "grey", lwd = 1)
        gbl[[2]]$children[[2]]$gp = gpar(col = "grey", lwd = 1)
        gbl[[2]]$children[[3]]$gp = gpar(col = "black")
    }

    .transverse_code = function(p) {
        loc = sfc_transverse(p)
        xrange = range(loc[, 1])
        yrange = range(loc[, 2])
        nr = nrow(loc)
        k = sqrt(nr) - 1
        if( (loc[1, 1] == xrange[1] && loc[1, 2] == yrange[1]) ||  (loc[1, 1] == xrange[2] && loc[1, 2] == yrange[2])) {
            code1 = 0
        } else {
            code1 = 1
        }
        if( (loc[nr, 1] == xrange[1] && loc[nr, 2] == yrange[1]) ||  (loc[nr, 1] == xrange[2] && loc[nr, 2] == yrange[2])) {
            code2 = 0
        } else {
            code2 = 1
        }
        c(code1, code2)
    }

    ii = 2;
    for(i in 1:nr) {
        ii = ii + 1
        gbl[[ii]] = grob_sfc_sequence(pl[[i*2-1]], x = 4 + k/2, y = k - (i-1)*(k+1) - k/2, default.units = "native")
        gbl[[ii]]$children[[1]]$gp = gpar(col = "grey", lwd = 1)
        gbl[[ii]]$children[[2]]$gp = gpar(col = "grey", lwd = 1)
        gbl[[ii]]$children[[3]]$gp = gpar(col = "black", lwd = 1)
        
        ii = ii + 1
        code = .transverse_code(pl[[i*2-1]])
        gbl[[ii]] = grob_math(paste0("(", code[1], ",", code[2], ")"), x = 4 + k/2, y = k - (i-1)*(k+1) - k - 1/2, default.units = "native", gp = gpar(fontsize = 7))

        ii = ii + 1
        gbl[[ii]] = grob_sfc_sequence(pl[[i*2]], x = 5 + k + k/2, y = k - (i-1)*(k+1) - k/2, default.units = "native")
        gbl[[ii]]$children[[1]]$gp = gpar(col = "grey", lwd = 1)
        gbl[[ii]]$children[[2]]$gp = gpar(col = "grey", lwd = 1)
        gbl[[ii]]$children[[3]]$gp = gpar(col = "black", lwd = 1)

        ii = ii + 1
        code = .transverse_code(pl[[i*2]])
        gbl[[ii]] = grob_math(paste0("(", code[1], ",", code[2], ")"), x = 5 + k + k/2, y = k - (i-1)*(k+1) - k - 1/2, default.units = "native", gp = gpar(fontsize = 7))
        
    }

    ii = length(gbl)
    for(i in 1:n) {
        ii  = ii + 1
        gbl[[ii]] = grob_math(tex_pattern(bp, i, pl[[i]]), x = 6 + 2*k, k - 0.5 - (i-1)*1.2, just = "left", default.units = "native")
    }

    gbl[[ii+1]] = rectGrob(gp = gpar(fill = NA, col = "black"))

    arg = gbl
    arg$vp = vp
    
    do.call(grobTree, arg)
}

draw_rule = function(increase = 2) {
    grid.newpage()

    gb1 = grob_single_base_rule("I", increase = increase, x = size, y = unit(1, "npc") - size, just = c("left", "top"))
    grid.draw(gb1)

    gb2 = grob_single_base_rule("R", increase = increase, x = size, y = unit(1, "npc") - size - gb1$vp$height, just = c("left", "top"))
    grid.draw(gb2)

    gb3 = grob_single_base_rule("L", increase = increase, x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height, just = c("left", "top"))
    grid.draw(gb3)

    gb4 = grob_single_base_rule("U", increase = increase, x = size, y = unit(1, "npc") - size - gb1$vp$height - gb2$vp$height - gb3$vp$height, just = c("left", "top"))
    grid.draw(gb4)

    gb5 = grob_single_base_rule("C", increase = increase, x = size + gb1$vp$width + size, y = unit(1, "npc") - size, just = c("left", "top"))
    grid.draw(gb5)

    gb6 = grob_single_base_rule("B", increase = increase, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height, just = c("left", "top"))
    grid.draw(gb6)

    gb7 = grob_single_base_rule("D", increase = increase, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height, just = c("left", "top"))
    grid.draw(gb7)

    gb8 = grob_single_base_rule("P", increase = increase, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height - gb7$vp$height, just = c("left", "top"))
    grid.draw(gb8)

    gb9 = grob_single_base_rule("Q", increase = increase, x = size + gb1$vp$width + size, y = unit(1, "npc") - size - gb5$vp$height - gb6$vp$height - gb7$vp$height - gb8$vp$height, just = c("left", "top"))
    grid.draw(gb9)
}

