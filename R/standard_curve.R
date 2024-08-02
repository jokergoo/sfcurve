
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
#' @param pattern The orientation of units on level-2, i.e. the orientation of the 9 3x3 units. The 
#'        value should be a string with 9 letters of "v"/"h" (vertical or horizontal) for the Peano curve,
#'        and "f"/"b" (forward or backward) for the Meander curve. The length of the string should be maximal 9.
#'        If the length is smaller than 9, the stringis automatically recycled.
#' @export
#' @examples
#' draw_multiple_curves(
#'     peano_curve(2),
#'     peano_curve(3),
#'     nrow = 1
#' )
#' draw_multiple_curves(
#'     peano_curve(3, pattern = "vh"),
#'     peano_curve(3, pattern = "vvvhhhvvv"),
#'     nrow = 1
#' )
peano_curve = function(level = 2L, pattern = "vvvvvvvvv", by = "Cpp") {

	if(by == "Cpp" && (pattern == "vvvvvvvvv" || pattern == "v")) {
		lt = peano_curve_cpp(level)
		cbind(lt[[1]], lt[[2]])
	} else {

		if(length(pattern) != 1) {
			stop_wrap("Length of `pattern` can only be 1.")
		}
		pattern = strsplit(pattern, "")[[1]]
		if(length(pattern) == 1) {
			pattern = rep(pattern, 9)
		}
		if(length(pattern) > 9) {
			stop_wrap("`pattern` can only contain maximal 9 letters.")
		}

		if(!all(pattern %in% c("h", "v"))) {
			stop_wrap("`pattern` should contain v/h")
		}

		bp = "I"
		l_v = pattern == "v"
		l_h = pattern == "h"
		p = sfc_peano(bp, code = rep(1, level), rot = 0, flip = function(p) {
			if(sfc_level(p) > 0) {
				n = length(p)
				l = rep(FALSE, n)
				l1 = rep(l_v, times = ceiling(9^(sfc_level(p))/length(pattern)))
				l1 = l1[1:n]
				l[l1] = p@rot[l1] %in% c(90, 270)
				
				l2 = rep(l_h, times = ceiling(9^(sfc_level(p))/length(pattern)))
				l2 = l2[1:n]
				l[l2] = p@rot[l2] %in% c(0, 180)
				l
			} else {
				FALSE
			}
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
#'     meander_curve(3, pattern = "fbfbfbfbf"),
#'     meander_curve(3, pattern = "bbbbbffff"),
#'     nrow = 1
#' )
meander_curve = function(level = 2L, pattern = "fffffffff") {
	
	if(length(pattern) != 1) {
		stop_wrap("Length of `pattern` can only be 1.")
	}
	pattern = strsplit(pattern, "")[[1]]
	if(length(pattern) == 1) {
		pattern = rep(pattern, 9)
	}
	if(length(pattern) > 9) {
		stop_wrap("`pattern` can only contain maximal 9 letters.")
	}

	if(!all(pattern %in% c("f", "b"))) {
		stop_wrap("`pattern` should contain f/b")
	}

	bp = "R"
	rot = 0

	l_f = pattern == "f"
	l_b = pattern == "b"
	p = sfc_meander(bp, code = rep(1, level), rot = 0, flip = function(p) {
		if(sfc_level(p) > 0) {
			n = length(p)
			l = rep(FALSE, n)

			l2 = rep(l_b, times = ceiling(9^(sfc_level(p))/length(pattern)))
			l2 = l2[1:n]
			l[l2] = TRUE
			l
		} else {
			FALSE
		}
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
#'     nrow = 1, closed = TRUE
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
