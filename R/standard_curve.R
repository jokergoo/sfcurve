
#' Various curves in their standard forms
#' 
#' @rdname standard_curve
#' @param level Level of the curve.
#' @param by Which implemnetation? Only for the testing purpose.
#' 
#' @details
#' These are just special forms of [`sfc_hilbert()`], [`sfc_peano()`] and [`sfc_meander()`]
#' @return
#' A two-column matrix of the coordinates of points on the curve.
#' @export
#' @examples
#' hilbert_curve(2)
#' draw_multiple_curves(
#'     hilbert_curve(3),
#'     hilbert_curve(4)
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
#'     moore_curve(4)
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
#'     beta_omega_curve(4)
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
#' @export
#' @examples
#' draw_multiple_curves(
#'     peano_curve(2),
#'     peano_curve(3)
#' )
peano_curve = function(level = 2L, by = "Cpp") {
	if(by == "Cpp") {
		lt = peano_curve_cpp(level)
		cbind(lt[[1]], lt[[2]])
	} else {
		p = sfc_peano("I", code = rep(1, level), flip = function(n) {
		    if(n == 1) {
		        return(FALSE)
		    }
		    l = rep(FALSE, n)
		    portion = 1
		    while(portion*9 <= n) {
		        ind = ((1:(n/3/portion))*3*portion)[rep(c(TRUE, TRUE, FALSE), n/9/portion)]
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
#'     meander_curve(3)
#' )
meander_curve = function(level = 2L) {
	
	p = sfc_meander("R", code = rep(1, level))
	sfc_segments(p)
	
}

#' @rdname standard_curve
#' @param iteration Number of iterations.
#' @export
#' @examples
#' draw_multiple_curves(
#'     h_curve(1),
#'     h_curve(2)
#' )
h_curve = function(iteration = 2L) {
	
	sfc_h(H1, iteration = iteration, connect = "h", random = FALSE)
	
}


#' Plot segments
#' @param x A two-column matrix of coordinates of points.
#' @param grid Whether to add grid lines on the plot?
#' @param ... Other arguments passed to `[sfc_grob()]`.
#' @export
#' @details
#' This function is only for a quick demonstration of curves represented as two-column coordinate matrices.
plot_segments = function(x, grid = FALSE, ...) {
	gb = sfc_grob(x, ...)
	grid.newpage()
	grid.draw(gb)

	if(grid) {
		add_grid_lines()
	}
}
