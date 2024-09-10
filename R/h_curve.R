
rotate_90 = function(pos) {
	n = as.integer(round(sqrt(nrow(pos))))
	pos2 = pos
	pos2[, 1] = -pos[, 2] + n - 1
	pos2[, 2] = pos[, 1]
	pos2
}

#' H-curve
#' @rdname h_curve
#' @param h The seed of the H-curve. The value should be one of [`H0`], [`H1`] or [`H2`].
#' @param iteration Number of iterations.
#' @param connect How the four subunits are connected to form the H-curve on the next level. See **Details**.
#' @param random Whether to generate subunits randomly on each iteration.
#' @details
#' An H-curve on level k is composed with four subunits on level k-1. If we number the four subunits in the following order:
#' 
#' ```
#' 2  3
#' 1  4
#' ```
#' 
#' where subunit 1 connects to subunit 2, subunit 2 connects to subunit 3, et al., and e.g. subunit 1 connects to subunit 2 
#' via its toprigth corner. Since H-curve can be thought of as a closed curve, to, e.g. let subunit 1 to connect to  
#' subunit 2, its topright corner needs to be opened. There are two segments on subunit 1 that can be removed/opened: the horizontal
#' segment and the vertical segment on the topright corner in subunit 1.
#' 
#' In this way, in `sfc_h()`, the argument `connect` only accepts a single value of `"h"` or `"v"` where the types of segments for 
#' all the four subunits are the same, i.e. whether all the horizontal corner segments are opened or whether all the vertical corner
#' segments are opened. In `expand_h()`, the argument `connect` can be set to a string with four letters or a vector of length four where the type of segments of the
#' four subunits can be set separately.
#' 
#' In the random mode, each subunit is generated randomly, the type of the open segment is choosen randomly, also each subunit has a probability of 0.5
#' to rotate by 90 degrees.
#' 
#' @return
#' A two-column matrix of coordinates of points on the curve.
#' @export
#' @importFrom stats runif
#' @examples
#' draw_multiple_curves(
#'     sfc_h(H0, iteration = 2),
#'     sfc_h(H2, iteration = 2),
#'     closed = TRUE, nrow =1
#' )
#' draw_multiple_curves(
#'     sfc_h(H1, iteration = 3, random = TRUE),
#'     sfc_h(H1, iteration = 3, random = TRUE),
#'     closed = TRUE, nrow = 1
#' )
sfc_h = function(h, iteration = 1, connect = c("h", "v"), random = FALSE) {

	if(!(identical(h, H0) || identical(h, H1) || identical(h, H2))) {
		stop_wrap("`h` can only take values from the three pre-defined objects: `H0/H1/H2`.")
	}

	if(random) {
		hl = list(h, h, h, h)
		cl = c("h", "v")

		for(i in seq_len(iteration)) {

			
			for(i in 1:4) {
				if(runif(1) > 0.5) {
					hl[[i]] = rotate_90(hl[[i]])
				}
			}
			h1 = expand_h(hl[[sample(4, 1)]], hl[[sample(4, 1)]], hl[[sample(4, 1)]], hl[[sample(4, 1)]], connect = sample(cl, 1, replace = TRUE))
			
			for(i in 1:4) {
				if(runif(1) > 0.5) {
					hl[[i]] = rotate_90(hl[[i]])
				}
			}
			h2 = expand_h(hl[[sample(4, 1)]], hl[[sample(4, 1)]], hl[[sample(4, 1)]], hl[[sample(4, 1)]], connect = sample(cl, 1, replace = TRUE))
			
			for(i in 1:4) {
				if(runif(1) > 0.5) {
					hl[[i]] = rotate_90(hl[[i]])
				}
			}
			h3 = expand_h(hl[[sample(4, 1)]], hl[[sample(4, 1)]], hl[[sample(4, 1)]], hl[[sample(4, 1)]], connect = sample(cl, 1, replace = TRUE))
			
			for(i in 1:4) {
				if(runif(1) > 0.5) {
					hl[[i]] = rotate_90(hl[[i]])
				}
			}
			h4 = expand_h(hl[[sample(4, 1)]], hl[[sample(4, 1)]], hl[[sample(4, 1)]], hl[[sample(4, 1)]], connect = sample(cl, 1, replace = TRUE))

			hl = list(h1, h2, h3, h4)
		}

		hl[[sample(4, 1)]]

	} else {
		connect = match.arg(connect)
		for(i in seq_len(iteration)) {
			h = expand_h(h, connect = connect)
		}
		h
	}
}

#' @rdname h_curve
#' @param h1 The first subunit on the bottom left.
#' @param h2 The second subunit on the top left.
#' @param h3 The third subunit on the top right.
#' @param h4 The fourth subunit on the bottom right.
#' @export
#' @examples
#' draw_multiple_curves(
#'     expand_h(H0, connect = "hvvh"),
#'     expand_h(H1, connect = "vvhh"),
#'     closed = TRUE, nrow = 1
#' )
#' 
#' # set the four subunits separately
#' h1 = expand_h(H0, connect = "hhhh")
#' h2 = expand_h(H0, connect = "vvvv")
#' h3 = expand_h(H0, connect = "hvhv")
#' h4 = expand_h(H0, connect = "hvvh")
#' expand_h(h1, h2, h3, h4, connect = "vhvh") |> 
#'     plot_segments(closed = TRUE)
#' 
#' fun = function(h, iteration) {
#'     for(i in 1:iteration) h = expand_h(h, connect = "vhvh")
#'     h
#' }
#' fun(H0, 4) |> plot_segments(closed = TRUE)
expand_h = function(h1, h2 = h1, h3 = h1, h4 = h1, connect = "hhhh") {

	n = as.integer(round(sqrt(nrow(h1))))

	
	if(length(connect) == 1) {
		connect = strsplit(connect, "")[[1]]
		if(length(connect) == 1) {
			connect = rep(connect, 4)
		}
	}
	if(length(connect) != 4) {
		stop_wrap("`connect` should be a string or a vector with four letters.")
	}

	h1 = open_h(h1, 3, connect[1])
	h2 = open_h(h2, 4, connect[2])
	h3 = open_h(h3, 1, connect[3])
	h4 = open_h(h4, 2, connect[4])

	h2[, 2] = h2[, 2] + n

	h3[, 1] = h3[, 1] + n
	h3[, 2] = h3[, 2] + n

	h4[, 1] = h4[, 1] + n

	rbind(h1, h2, h3, h4)
}


# h is a two-column matrix, coordinate clockwise, starts from center, the lower segment
# it returns a list of points with clockwise direction
#. 2 3
#. 1 4
open_h = function(h, where, how) {
	min_x = min(h[, 1])
	max_x = max(h[, 1])
	min_y = min(h[, 2])
	max_y = max(h[, 2])

	n = nrow(h)

	if(where == 1) {
		i = which(h[, 1] == min_x & h[, 2] == min_y)
		if(how == "h") {
			part1 = h[seq_len(i-1), , drop = FALSE]
			part2 = h[seq(i, n), , drop = FALSE]
		} else {
			part1 = h[seq(1, i), , drop = FALSE]
			part2 = h[i+seq_len(n-i), , drop = FALSE]
		}
		h2 = rbind(part2, part1)
	} else if(where == 2) {
		i = which(h[, 1] == min_x & h[, 2] == max_y)
		if(how == "h") {
			part1 = h[seq(1, i), , drop = FALSE]
			part2 = h[i+seq_len(n-i), , drop = FALSE]
		} else {
			part1 = h[seq_len(i-1), , drop = FALSE]
			part2 = h[seq(i, n), , drop = FALSE]
		}
		h2 = rbind(part2, part1)
	} else if(where == 3) {
		i = which(h[, 1] == max_x & h[, 2] == max_y)
		if(how == "h") {
			part1 = h[seq_len(i-1), , drop = FALSE]
			part2 = h[seq(i, n), , drop = FALSE]
		} else {
			part1 = h[seq(1, i), , drop = FALSE]
			part2 = h[i+seq_len(n-i), , drop = FALSE]
		}
		h2 = rbind(part2, part1)
	} else if(where == 4) {
		i = which(h[, 1] == max_x & h[, 2] == min_y)
		if(how == "h") {
			part1 = h[seq(1, i), , drop = FALSE]
			part2 = h[i+seq_len(n-i), , drop = FALSE]
		} else {
			part1 = h[seq_len(i-1), , drop = FALSE]
			part2 = h[seq(i, n), , drop = FALSE]
		}
		h2 = rbind(part2, part1)
	}

	h2
}




H0 = cbind(c(0L, 0L, 1L, 1L), 
	       c(0L, 1L, 1L, 0L))

H1 = expand_h(H0, connect = "h")
H2 = expand_h(H0, connect = "v")

#' Seed sequences of the H-curve
#' @rdname h_seed
#' @export
#' @details
#' The three objects simply contain coordinates of points on the three base H-curves.
#' @return Two-column matrices.
#' @examples
#' H0
#' draw_multiple_curves(H0, H1, H2, nrow = 1, closed = TRUE)
"H0"

#' @rdname h_seed
#' @export
"H1"

#' @rdname h_seed
#' @export
"H2"
