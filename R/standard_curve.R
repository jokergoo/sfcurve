
#' Various curves in their standard forms
#' 
#' @rdname standard_curve
#' @param level Level of the curve.
#' @param by Which implementation? Only for the testing purpose.
#' 
#' @details
#' These are just special forms of [`sfc_hilbert()`], [`sfc_peano()`], [`sfc_meander()`] and [`sfc_h()`].
#' @return
#' A two-column matrix of the coordinates of points on the curve.
#' @export
#' @examples
#' hilbert_curve(2)
#' draw_multiple_curves(
#'     hilbert_curve(3),
#'     hilbert_curve(4),
#'     nrow = 1
#' )
hilbert_curve = function(level = 2L, by = "Cpp") {
	if(by == "Cpp") {
		lt = hilbert_curve_cpp(level)
		cbind(lt[[1]], lt[[2]])
	} else {
		p = sfc_hilbert("R", code = rep(1, level))
		sfc_segments(p)
	}
}

#' @rdname standard_curve
#' @export
#' @examples
#' draw_multiple_curves(
#'     moore_curve(3),
#'     moore_curve(4),
#'     nrow = 1
#' )
moore_curve = function(level = 2L) {
	
	code = rep(1, level)
	code[1] = 2
	p = sfc_hilbert("C", code = code, rot = 90)
	sfc_segments(p)
	
}

#' @rdname standard_curve
#' @export
#' @examples
#' draw_multiple_curves(
#'     beta_omega_curve(3),
#'     beta_omega_curve(4),
#'     nrow = 1
#' )
beta_omega_curve = function(level = 2L) {
	
	if(level %% 2 == 1) {
		level = level + 1
		code = rep(c(2, 1), times = level/2L)
		code = code[-1]
	} else {
		code = rep(c(2, 1), times = level/2L)
	}

	p = sfc_hilbert("C", code = code, rot = -90)
	sfc_segments(p)
	
}

#' @rdname standard_curve
#' @param level_2 The orientation of units on level-2, i.e. the orientation of the 9 3x3 units. The 
#'        value should be a string with 9 letters of "v"/"h" (vertical or horizontal).
#' @export
#' @examples
#' draw_multiple_curves(
#'     peano_curve(2),
#'     peano_curve(3),
#'     nrow = 1
#' )
#' draw_multiple_curves(
#'     peano_curve(3, level_2 = "vhvhvhvhv"),
#'     peano_curve(3, level_2 = "vvvhhhvvv"),
#'     nrow = 1
#' )
peano_curve = function(level = 2L, level_2 = "vvvvvvvvv", by = "Cpp") {
	if(by == "Cpp" && (level_2 == "vvvvvvvvv" || level_2 == "v")) {
		lt = peano_curve_cpp(level)
		cbind(lt[[1]], lt[[2]])
	} else {
		if(length(level_2) != 1) {
			stop_wrap("Length of `level_2` can only be 1.")
		}
		level_2 = strsplit(level_2, "")[[1]]
		if(length(level_2) == 1) {
			level_2 = rep(level_2, 9)
		}
		if(length(level_2) != 9) {
			stop_wrap("`level_2` should contain 9 letters of v/h")
		}
		if(!all(level_2 %in% c("h", "v"))) {
			stop_wrap("`level_2` should contain 9 letters of v/h")
		}

		if(level_2[1] == "v") {
			bp = "I"
			d_base = c("v", "v", "v", "h", "v", "v", "h", "v", "v")
			l2 = level_2 != d_base
			rot = 0
		} else {
			bp = "J"
			d_base = c("h", "h", "h", "v", "h", "h", "v", "h", "h")
			l2 = level_2 != d_base
			rot = -90
		}

		p = sfc_peano(bp, code = rep(1, level), rot = rot, flip = function(n) {
		    if(n == 1) {
		        return(l2[1])
		    }
		    l = rep(FALSE, n)
		    portion = 1
		    while(portion*9 <= n) {
		        ind = ((0:(n/portion-1))*portion)[rep(l2, n/9/portion)]
		        l[ind + 1] = TRUE
		        portion = portion*9
		    }
		    l
		})
		sfc_segments(p)
	}
}

#' @rdname standard_curve
#' @export
#' @examples
#' draw_multiple_curves(
#'     meander_curve(2),
#'     meander_curve(3),
#'     nrow = 1
#' )
#' draw_multiple_curves(
#'     meander_curve(3, level_2 = "fbfbfbfbf"),
#'     meander_curve(3, level_2 = "bbbbbffff"),
#'     nrow = 1
#' )
meander_curve = function(level = 2L, level_2 = "fffffffff") {
	
	if(length(level_2) != 1) {
		stop_wrap("Length of `level_2` can only be 1.")
	}
	level_2 = strsplit(level_2, "")[[1]]
	if(length(level_2) == 1) {
		level_2 = rep(level_2, 9)
	}
	if(length(level_2) != 9) {
		stop_wrap("`level_2` should contain 9 letters of f/b")
	}
	if(!all(level_2 %in% c("f", "b"))) {
		stop_wrap("`level_2` should contain 9 letters of f/b")
	}

	bp = "R"
	d_base = c("f", "f", "f", "f", "f", "f", "f", "f", "f") # transverse code 1
	l2 = level_2 != d_base
	rot = 0

	p = sfc_meander(bp, code = rep(1, level), rot = rot, flip = function(n) {
	    if(n == 1) {
	        return(l2[1])
	    }
	    l = rep(FALSE, n)
	    portion = 1
	    while(portion*9 <= n) {
	        ind = ((0:(n/portion-1))*portion)[rep(l2, n/9/portion)]
	        l[ind + 1] = TRUE
	        portion = portion*9
	    }
	    l
	})
	sfc_segments(p)
	
}


#' @rdname standard_curve
#' @param iteration Number of iterations.
#' @export
#' @examples
#' draw_multiple_curves(
#'     h_curve(1),
#'     h_curve(2),
#'     nrow = 1
#' )
h_curve = function(iteration = 2L) {
	
	sfc_h(H1, iteration = iteration, connect = "h", random = FALSE)
	
}


#' Plot segments
#' @param x A two-column matrix of coordinates of points.
#' @param grid Whether to add grid lines on the plot?
#' @param title The value should be `FALSE` or a string.
#' @param closed Whether the curve is closed?
#' @param ... Other arguments passed to [`sfc_grob()`].
#' @export
#' @details
#' This function is only for a quick demonstration of curves represented as two-column coordinate matrices.
plot_segments = function(x, grid = FALSE, title = FALSE, closed = FALSE, ...) {
	gb = sfc_grob(x, title = title, closed = closed, ...)
	grid.newpage()
	grid.draw(gb)

	if(grid) {
		add_grid_lines()
	}
}
