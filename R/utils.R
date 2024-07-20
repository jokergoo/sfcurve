
#' Draw multiple curves
#' 
#' @param ... A list of `sfc_sequence` objects or objects in its child classes. The value
#'      can also be a list of two-column coordinate matrices.
#' @param nrow Number of rows in the layout.
#' @param ncol Number of columns in the layout.
#' @param extend Whether to draw the entering and leaving segments?
#' @param title Whether to add titles on each panel? The title is constructed in the form of `initial_seed|transverse_code`, e.g. `I|111`.
#' 
#' @details
#' This function is only for the demonstration purpose.
#' @export
#' @examples
#' # for all forms of curves initialized by base pattern 'R', with rotation 0, and on level 3
#' draw_multiple_curves(
#'     sfc_hilbert("R", code = c(1, 1, 1)),
#'     sfc_hilbert("R", code = c(1, 1, 2)),
#'     sfc_hilbert("R", code = c(1, 2, 1)),
#'     sfc_hilbert("R", code = c(1, 2, 2)),
#'     sfc_hilbert("R", code = c(2, 1, 1)),
#'     sfc_hilbert("R", code = c(2, 1, 2)),
#'     sfc_hilbert("R", code = c(2, 2, 1)),
#'     sfc_hilbert("R", code = c(2, 2, 2)),
#'     nrow = 2, title = TRUE)
#' 
#' # simply a list of sequences
#' draw_multiple_curves(
#'     sfc_sequence("IIII"),
#'     sfc_sequence("RRRR"),
#'     sfc_sequence("RRLL"),
#'     nrow = 1
#' )
draw_multiple_curves = function(..., nrow = 1, ncol = NULL, extend = TRUE, title = TRUE) {
	pl = list(...)

	n = length(pl)
	if(n < 1) {
		stop_wrap("No curve is specified.")
	}

	if(is.null(nrow) && is.null(ncol)) {
		nrow = ceiling(sqrt(n))
	}
	if(!is.null(nrow) && is.null(ncol)) {
		ncol = ceiling(n/nrow)
	} else if(is.null(nrow) && !is.null(ncol)) {
		nrow = ceiling(n/ncol)
	} else if(!is.null(nrow) && !is.null(ncol)) {
		ncol = ceiling(n/nrow)
	}

	if(inherits(pl[[1]], "sfc_sequence")) {
		gbl = lapply(pl, sfc_grob, extend = extend, title = title)
	} else {
		gbl = lapply(pl, sfc_grob)
	}

	grid.newpage()
	pushViewport(viewport(layout = grid.layout(nrow = nrow, ncol = ncol)))
	for(i in seq_len(nrow)) {
		for(j in seq_len(ncol)) {
			pushViewport(viewport(layout.pos.row = i, layout.pos.col = j))
			ind = (i-1)*ncol + j
			grid.draw(gbl[[ind]])
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
	abs(x - y) < 1e-6
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
