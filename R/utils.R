
#' Draw multiple curves
#' 
#' @param ... A list of `sfc_sequence` objects or objects in its child classes, a list of [`grid::grob`] objects 
#'          or a list of two-column coordinate matrices. I.e., all the forms that can represent curves in this package.
#' @param nrow Number of rows in the layout.
#' @param ncol Number of columns in the layout.
#' @param extend Whether to draw the entering and leaving segments? It is only used when input is a list of `sfc_sequence` objects.
#' @param title Whether to add titles on each panel? The title is constructed in the form of `initial_seed|transverse_code`, e.g. `I|111`.
#'   The value can be a vector of user-defined strings.
#' @param closed Whether the curves are closed? The value should be a logical vector. If it is `TRUE`, the last point
#'           is connected to the first point in the curve to form a closed curve.
#' @param padding Space around each curve. The value should be a [`grid::grob`] object.
#' @param lwd Line width.
#' @param col Color for segments. If the value is `NULL`, it uses the "Spectral" color palettes from the **RColorBrweer** package.
#' 
#' @details
#' This function is used for quickly comparing curves.
#' @export
#' @examples
#' # for all forms of curves initialized by base pattern 'R', with rotation 0, and on level 3
#' draw_multiple_curves(
#'     sfc_hilbert("R", "111"),
#'     sfc_hilbert("R", "112"),
#'     sfc_hilbert("R", "121"),
#'     sfc_hilbert("R", "122"),
#'     sfc_hilbert("R", "211"),
#'     sfc_hilbert("R", "212"),
#'     sfc_hilbert("R", "221"),
#'     sfc_hilbert("R", "222"),
#'     nrow = 2, title = TRUE)
#' 
#' # simply a list of sequences
#' # note they only contain I/R/L, so the base patterns I/R/L are internally used
#' draw_multiple_curves(
#'     sfc_sequence("IIII"),
#'     sfc_sequence("RRRR"),
#'     sfc_sequence("RRLL"),
#'     nrow = 1
#' )
draw_multiple_curves = function(..., nrow = NULL, ncol = NULL, extend = TRUE, 
	title = FALSE, closed = FALSE, padding = unit(0, "pt"),
	lwd = 4, col = NULL) {
	pl = list(...)

	n = length(pl)
	if(n < 1) {
		stop_wrap("No curve is specified.")
	}

	if(is.null(nrow) && is.null(ncol)) {
		nrow = floor(sqrt(n))
	}
	if(!is.null(nrow) && is.null(ncol)) {
		ncol = ceiling(n/nrow)
	} else if(is.null(nrow) && !is.null(ncol)) {
		nrow = ceiling(n/ncol)
	} else if(!is.null(nrow) && !is.null(ncol)) {
		ncol = ceiling(n/nrow)
	}

	if(length(title) == 1) {
		title = rep(title, n)
	}
	if(length(closed) == 1) {
		closed = rep(closed, n)
	}
	
	gbl = lapply(seq_along(pl), function(i) {
		if(inherits(pl[[i]], "sfc_sequence")) {
			sfc_grob(pl[[i]], extend = extend, title = title[i], closed = closed[i], lwd = lwd, col = col)
		} else if(inherits(pl[[i]], "matrix")) {
			sfc_grob(pl[[i]], title = title[i], closed = closed[i], lwd = lwd, col = col)
		} else if(inherits(pl[[i]], "grob")) {
			pl[[i]]$children[[1]]$gp$lwd = lwd
			if(!is.null(col)) {
				pl[[i]]$children[[1]]$gp$col = col
			}
			pl[[i]]
		} else if(inherits(pl[[i]], "sfc_base")) {
			sfc_grob(pl[[i]])
		} else {
			stop_wrap("Wrong data type.")
		}
	})
	
	grid.newpage()
	pushViewport(viewport(layout = grid.layout(nrow = nrow, ncol = ncol)))
	for(i in seq_len(nrow)) {
		for(j in seq_len(ncol)) {
			ind = (i-1)*ncol + j
			if(ind > n) {
				break
			}
			pushViewport(viewport(layout.pos.row = i, layout.pos.col = j))
			pushViewport(viewport(width = unit(1, "npc") - padding, height = unit(1, "npc") - padding))
			grid.draw(gbl[[ind]])
			popViewport()
			popViewport()
		}
	}
	popViewport()
}


rotate_coord = function(x, theta, center = c(0, 0)) {
	if(length(x) == 2) {
		dim(x) = c(1, 2)
	}

	y = x[, 2]
	x = x[, 1]

	x = x - center[1]
	y = y - center[2]

	theta = theta/180*pi
	x2 = x*cos(theta) - y*sin(theta)
	y2 = x*sin(theta) + y*cos(theta)

	x2 = x2 + center[1]
	y2 = y2 + center[2]

	cbind(x2, y2)
}

reverse_coord = function(x) {
	if(length(x) == 2) {
		dim(x) = c(1, 2)
	}
	x[seq(nrow(x), 1), , drop = FALSE]
}

move_coord = function(x, offset) {
	if(length(x) == 2) {
		dim(x) = c(1, 2)
	}
	x[, 1] = x[, 1] + offset[1]
	x[, 2] = x[, 2] + offset[2]
	x
}


get_circular_index = function(ind, n) {
	(ind-1) %% n + 1
}


stop_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    stop(x, call. = FALSE)
}

warning_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    warning(x, call. = FALSE)
}

message_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    message(x)
}

equal_to = function(x, y) {
	all(abs(x - y) < 1e-6)
}


convert_to_child_class = function(x, class) {
	x2 = new(class)
	for(nm in slotNames(x)) {
		slot(x2, nm) = slot(x, nm)
	}
	x2
}

in_range = function(x, range, others = NULL) {
	if(is.null(others)) {
		(x >= range[1] && x <= range[2])
	} else {
		(x >= range[1] && x <= range[2]) || x %in% others
	}
}
