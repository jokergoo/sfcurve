
#' @aliases sfc_grob
#' @rdname sfc_grob
#' @param bases A list of base patterns, consider to use [`BASE_LIST`].
#' @param extend Whether to add the entering and leaving segments?
#' @param title Whether to add title on the top of the plot? The title is constructed in the form of `initial_seed|expansion_code`, e.g. `I|111`.
#'      Or the value is a string.
#' @param closed Whether the curve is closed? 
#' @param lwd Line width.
#' @param col Color for segments. If the value is `NULL`, it uses the "Spectral" color palettes.
#' @param ... Other arguments passed to [`grid::viewport()`] or `sfc_grob()`.
#' 
#' @details
#' If `p` is an `sfc_sequence` and if `p` contains base patterns defined in `"I/J/R/L/U/B/D/P/Q/C"`,
#' the default [`BASE_LIST`] is automatically used for `bases`. If `p` is an `sfc_nxn` object, `bases`
#' is already stored in `p` and it is passed to this function automatically.
#' 
#' @export
#' @import colorRamp2
setMethod("sfc_grob",
	signature = "sfc_sequence",
	definition = function(p, bases = NULL, extend = FALSE, title = FALSE, closed = FALSE, lwd = 4, col = NULL, ...) {

	if(is.null(bases)) {
		bases = BASE_LIST[sfc_universe(p)]
	}

	loc = sfc_segments(p, bases)
	if(closed) {
		loc = rbind(loc, loc[1, ])
	}

	n = nrow(loc)

	rgx = range(loc[, 1])
	rgx[1] = rgx[1] - 1; rgx[2] = rgx[2] + 1
	rgy = range(loc[, 2])
	rgy[1] = rgy[1] - 1; rgy[2] = rgy[2] + 1
	
	r = (diff(rgx) + 1)/(diff(rgy) + 1)

	vp = viewport(name = "vp_sfc_sequence", xscale = rgx, yscale = rgy, width = unit(r, "snpc"), height = unit(1, "snpc"), ...)

	gbl = list()

	if(n > 1) {
		if(is.null(col)) {
			col_fun = colorRamp2(seq(1, n, length = 11), c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2"))
			gbl[[1]] = segmentsGrob(loc[1:(n-1), 1], loc[1:(n-1), 2], loc[2:n, 1], loc[2:n, 2], default.units = "native", gp = gpar(col = col_fun(1:(n-1)), lwd = lwd))
		} else {
			gbl[[1]] = segmentsGrob(loc[1:(n-1), 1], loc[1:(n-1), 2], loc[2:n, 1], loc[2:n, 2], default.units = "native", gp = gpar(col = col, lwd = lwd))
		}
	
	} else {
		gbl[[1]] = pointsGrob(loc[, 1], loc[, 2], pch = 16, size = unit(4, "pt"), gp = gpar(col = "#9E0142"))
	}

	if(extend) {
		seq = as.character(p@seq)
		rot = p@rot

		b = bases[[ seq[1] ]]

		if(is.na(b@in_direction)) {
			gbl[[2]] = pointsGrob(loc[1, 1], loc[1, 2], default.units = "native", pch = 4, size = unit(4, "pt"), gp = gpar(col = "grey"))
		} else {
			prev = sfc_previous_point(b, loc[1, ], rot[1], length = 0.6)
			gbl[[2]] = segmentsGrob(prev[1], prev[2], 0, 0, default.units = "native", gp = gpar(col = "grey", lwd = 2))
		}

		b = bases[[ seq[n] ]]

		if(is.na(b@out_direction)) {
			gbl[[3]] = pointsGrob(loc[n, 1], loc[n, 2], default.units = "native", pch = 4, size = unit(4, "pt"), gp = gpar(col = "grey"))
		} else {
			last = sfc_next_point(b, loc[n, ], rot[n], length = 0.6)
			gbl[[3]] = segmentsGrob(loc[n, 1], loc[n, 2], last[1], last[2], default.units = "native", gp = gpar(col = "grey", lwd = 2), arrow = arrow(length = unit(0.2, "native"), angle = 15))
		}

		gbl = gbl[c(2, 3, 1)]
	}

	if(is.logical(title)) {
		if(title) {
			if(inherits(p, "sfc_nxn")) {
				seed = p@seed
				pt = paste0("paste(", paste("italic(", seed@seq[1], ")^", seed@rot[1], sep = "", collapse = ","), ")")
				pt = paste0("paste(", pt, ", symbol('|'), ", paste(p@expansion, collapse = ""), ")")
				gbl[[ length(gbl) + 1 ]] = textGrob(parse(text = pt), x = unit(mean(rgx), "native"), y = unit(rgy[2], "native") - unit(1, "native") + unit(4, "pt"), just = "bottom", gp = gpar(fontsize = 10, fontfamily = "Times"))
			}
		}
	} else {
		gbl[[ length(gbl) + 1 ]] = textGrob(title, x = unit(mean(rgx), "native"), y = unit(rgy[2], "native") - unit(1, "native") + unit(4, "pt"), just = "bottom", gp = gpar(fontsize = 10))
	}

	args = gbl
	args$vp = vp
	args$cl = "grob_sfc_sequence"

	do.call(grobTree, args)
})

#' @param x The corresponding object.
#' @param grid Whether to add grid lines on the plot?
#' @export
#' @rdname sfc_grob
#' @examples
#' plot(sfc_hilbert("I", "11"))
#' plot(sfc_hilbert("I", "11"), extend = TRUE, title = TRUE, grid = TRUE)
#' plot(sfc_sequence("IIIRRR"))
plot.sfc_sequence = function(x, bases = NULL, grid = FALSE, 
	extend = FALSE, title = FALSE, closed = FALSE, ...) {

	if(is.null(bases)) {
		bases = BASE_LIST[sfc_universe(x)]
	}

	gb = sfc_grob(x, bases, extend = extend, title = title, closed = closed, ...)
	grid.newpage()
	grid.draw(gb)

	if(grid) {
		add_grid_lines()
	}
}

#' @rdname sfc_grob
#' @export
setMethod("sfc_grob",
	signature = "sfc_nxn",
	definition = function(p, bases = p@rules@bases, extend = FALSE, title = FALSE, closed = FALSE, ...) {

	callNextMethod(p, bases, extend = extend, title = title, closed = closed, ...)
})

#' @export
makeContext.grob_sfc_sequence = function(x) {
    vp_width = convertWidth(unit(1, "npc"), "in", valueOnly = TRUE)
    vp_height = convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)

    r = as.numeric(x$vp$width)

    if(length(r)) {
	    if(vp_width > r*vp_height) {
	        x$vp$width = unit(r*vp_height, "in")
	        x$vp$height = unit(vp_height, "in")
	    } else {
	        x$vp$width = unit(vp_width, "in")
	        x$vp$height = unit(vp_width/r, "in")
	    }
	   }
    x
}


#' @export
#' @rdname sfc_grob
plot.sfc_nxn = function(x, grid = FALSE, extend = FALSE, title = FALSE, closed = FALSE, ...) {
	gb = sfc_grob(x, extend = extend, title = title, closed = closed, ...)
	grid.newpage()
	grid.draw(gb)

	if(grid) {
		add_grid_lines()
	}
}


add_grid_lines = function() {
	vp = current.viewport()
	nm = vp$name
	downViewport("vp_sfc_sequence")
	vp = current.viewport()
	xscale = vp$xscale
	yscale = vp$yscale

	nx = xscale[2] - xscale[1] - 1
	ny = yscale[2] - yscale[1] - 1

	grid.segments(seq(xscale[1] + 0.5, xscale[2] - 0.5, by = 1), rep(yscale[1] + 0.5, ny), 
		          seq(xscale[1] + 0.5, xscale[2] - 0.5, by = 1), rep(yscale[2] - 0.5, ny), 
		          default.units = "native", gp = gpar(col = "#CCCCCC", lty = 2))
	grid.segments(rep(xscale[1] + 0.5, ny), seq(yscale[1] + 0.5, yscale[2] - 0.5, by = 1),
		          rep(xscale[2] - 0.5, ny), seq(yscale[1] + 0.5, yscale[2] - 0.5, by = 1), 
		          default.units = "native", gp = gpar(col = "#CCCCCC", lty = 2))
}

#' @rdname sfc_grob
#' @export
setMethod("sfc_grob",
	signature = "matrix",
	definition = function(p, title = NULL, closed = FALSE, lwd = 4, col = NULL, ...) {

	loc = p
	if(closed) {
		loc = rbind(loc, loc[1, ])
	}

	n = nrow(loc)

	rgx = range(loc[, 1])
	rgx[1] = rgx[1] - 1; rgx[2] = rgx[2] + 1
	rgy = range(loc[, 2])
	rgy[1] = rgy[1] - 1; rgy[2] = rgy[2] + 1
	
	r = (diff(rgx) + 1)/(diff(rgy) + 1)

	vp = viewport(xscale = rgx, yscale = rgy, width = unit(r, "snpc"), height = unit(1, "snpc"), ...)

	gbl = list()

	if(n > 1) {
		if(is.null(col)) {
			col_fun = colorRamp2(seq(1, n, length = 11), c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2"))
			gbl[[1]] = segmentsGrob(loc[1:(n-1), 1], loc[1:(n-1), 2], loc[2:n, 1], loc[2:n, 2], default.units = "native", gp = gpar(col = col_fun(1:(n-1)), lwd = lwd))
		} else {
			gbl[[1]] = segmentsGrob(loc[1:(n-1), 1], loc[1:(n-1), 2], loc[2:n, 1], loc[2:n, 2], default.units = "native", gp = gpar(col = col, lwd = lwd))
		}
	} else {
		gbl[[1]] = pointsGrob(loc[, 1], loc[, 2], pch = 16, size = unit(4, "pt"), gp = gpar(col = "#9E0142"))
	}

	if(is.character(title)) {
		gbl[[ length(gbl) + 1 ]] = textGrob(title, x = unit(mean(rgx), "native"), y = unit(rgy[2], "native") - unit(1, "native") + unit(4, "pt"), just = "bottom", gp = gpar(fontsize = 10))
	}

	args = gbl
	args$vp = vp
	args$cl = "grob_sfc_sequence"

	do.call(grobTree, args)
})
