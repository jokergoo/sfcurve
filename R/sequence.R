

sfc_sequence = function(seq, rot = 0L) {
	if(length(seq) == 1) {
		seq = strsplit(seq, "")[[1]]
	}
	n = length(seq)

	rot = as.integer(rot)
	if(length(rot) == 1) {
		rot2 = integer(n)
		rot2[1] = rot[1]
		if(n > 1) {
			for(i in 2:n) {
				rot2[i] = next_rotation(seq[i-1], rot2[i-1])
			}
		}
		rot = rot2
	}
	seq = factor(seq, levels = BASE_PATTERN_LEVELS)
	lt = list(seq = seq, rot = rot %% 360L)
	class(lt) = "sfc_sequence"

	lt
}


next_rotation = function(direction, rot) {
	if(direction %in% c("I", "B", "D")) {
		return(rot)
	} else if(direction == "R") {
		return(rot + 90L)
	} else if(direction == "L") {
		return(rot - 90L)
	} else if(direction == "U") {
		return(rot + 180L)
	} else {
		stop("Only infer next rotations for I/R/L/U/B/D.")
	}
}


# subset allows an incomplete curve
`[.sfc_sequence` = function(x, i) {
	sfc_sequence(x$seq[i], x$rot[i])
}

length.sfc_sequence = function(x) {
	length(x$seq)
}

c.sfc_sequence = function(...) {
	lt = list(...)

	p = list()
	p$seq = do.call("c", lapply(lt, function(x) x$seq))
	p$rot = do.call("c", lapply(lt, function(x) x$rot))
	
	class(p) = "sfc_sequence"
	p
}

print.sfc_sequence = function(x, ...) {
	n = length(x)
	nr = ceiling(n/8)

	rot_str = function(x) {
		# ifelse(x == 0, "  0", ifelse(x == 90, " 90", x))
		x
	}

	cat("A sequence of ", n, " base pattern", ifelse(n == 1, ".", "s."), "\n", sep = "")
	if(nr > 4) {
		flag = 0
		for(i in 1:nr) {
			if(i >= 3 && i <= nr-2) {
				if(!flag) cat("  .... other ", nr-4, " line", ifelse(nr-4 > 1, "s", ""), " ....\n", sep = "")
				flag = TRUE
				next
			}
			ind = seq( (i-1)*8+1, min(i*8, n) )
			cat("  ")
			for(k in seq_along(ind)) {
				cat(paste0(x$seq[ ind[k] ], "(", rot_str(x$rot[ ind[k] ]), ")"))
				if(k == 4) {
					cat("  ")
				}
			}
			cat("\n")
		}
	} else {
		for(i in 1:nr) {
			ind = seq( (i-1)*8+1, min(i*8, n) )
			cat("  ")
			for(k in seq_along(ind)) {
				cat(paste0(x$seq[ ind[k] ], "(", rot_str(x$rot[ ind[k] ]), ")"))
				if(k == 4) {
					cat("  ")
				}
			}
			cat("\n")
		}
	}
}

grob_sfc_sequence = function(p, extend = TRUE, ...) {
	loc = sfc_transverse(p)
	n = nrow(loc)

	rgx = range(loc[, 1])
	rgx[1] = rgx[1] - 1; rgx[2] = rgx[2] + 1
	rgy = range(loc[, 2])
	rgy[1] = rgy[1] - 1; rgy[2] = rgy[2] + 1
	
	r = (diff(rgx) + 1)/(diff(rgy) + 1)

	vp = viewport(xscale = rgx, yscale = rgy, width = unit(r, "snpc"), height = unit(1, "snpc"), ...)

	gbl = list()

	if(n > 1) {
		col_fun = circlize::colorRamp2(seq(1, n, length = 11), RColorBrewer::brewer.pal(11, "Spectral"))
		gbl[[1]] = segmentsGrob(loc[1:(n-1), 1], loc[1:(n-1), 2], loc[2:n, 1], loc[2:n, 2], default.units = "native", gp = gpar(col = col_fun(1:(n-1)), lwd = 4))
	} else {
		gbl[[1]] = pointsGrob(loc[, 1], loc[, 2], pch = 16, size = unit(4, "pt"), gp = gpar(col = RColorBrewer::brewer.pal(11, "Spectral")[1]))
	}
		
	if(extend) {
		if(p$seq[1] %in% c("B", "D")) {
			gbl[[2]] = pointsGrob(0, 0, default.units = "native", pch = 4, size = unit(4, "pt"), gp = gpar(col = "grey"))
		} else {
			prev = sequence_prev_point(c(0, 0), p$rot[1], 0.5)
			gbl[[2]] = segmentsGrob(prev[1], prev[2], 0, 0, default.units = "native", gp = gpar(col = "grey", lwd = 2))
		}

		if(p$seq[n] %in% c("P", "Q")) {
			gbl[[3]] = pointsGrob(loc[n, 1], loc[n, 2], default.units = "native", pch = 4, size = unit(4, "pt"), gp = gpar(col = "grey"))
		} else {
			if(n > 1) {
				last = next_point(loc[n-1, ], loc[n, ], p$seq[n], 0.5)
			} else {
				last = next_point(prev, loc[n, ], p$seq[n], 0.5)
			}
			gbl[[3]] = segmentsGrob(loc[n, 1], loc[n, 2], last[1], last[2], default.units = "native", gp = gpar(col = "grey", lwd = 2), arrow = arrow(length = unit(0.2, "native"), angle = 15))
		}
		grobTree(gbl[[2]], gbl[[3]], gbl[[1]], vp = vp, cl = "grob_sfc_sequence")
	} else {
		grobTree(gbl[[1]], vp = vp, cl = "grob_sfc_sequence")
	}
}

makeContext.grob_sfc_sequence = function(x) {
    vp_width = convertWidth(unit(1, "npc"), "in", valueOnly = TRUE)
    vp_height = convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)

    r = as.numeric(x$vp$width)

    if(vp_width > r*vp_height) {
        x$vp$width = unit(r*vp_height, "in")
        x$vp$height = unit(vp_height, "in")
    } else {
        x$vp$width = unit(vp_width, "in")
        x$vp$height = unit(vp_width/r, "in")
    }
    x
}

plot.sfc_sequence = function(x, grid = FALSE, ...) {
	gb = grob_sfc_sequence(x, ...)
	grid.newpage()
	grid.draw(gb)

	if(grid) {
		vp = current.vpTree()$children[[1]]
		downViewport(vp$name)
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
		upViewport()
	}
}


### two child classes: sfc_base and sfc_initial
sfc_initial = function(seq, rot = 0L) {
	p = sfc_sequence(seq, rot)

	sfc_validate(p)

	if(length(p) == 1) {
		class(p) = c("sfc_base", "sfc_initial", class(p))
	} else {
		class(p) = c("sfc_initial", class(p))
	}

	p
}


print.sfc_initial = function(x, ...) {
	cat("An initial seed sequence:\n")
	print.sfc_sequence(x, ...)
}
