
rotate_90 = function(pos) {
	n = as.integer(round(sqrt(nrow(pos))))
	pos2 = pos
	pos2[, 1] = -pos[, 2] + n - 1
	pos2[, 2] = pos[, 1]
	pos2
}

#' H-curve
#' @rdname h_curve
#' @param h The seed. It should be one of [`H0`], [`H1`] or [`H2`].
#' @param connect How the four sub-units are connected.
#' @param iteration Number of interations.
#' @param random Whether generate units randomly.
#' @export
#' @examples
#' h_curve(H0, iteration = 2) |> plot(type = "l", asp = 1)
#' h_curve(H1, iteration = 2) |> plot(type = "l", asp = 1)
#' par(mfrow = c(1, 2))
#' h_curve(H1, iteration = 3, random = TRUE) |> plot(type = "l", asp = 1)
#' h_curve(H1, iteration = 3, random = TRUE) |> plot(type = "l", asp = 1)
h_curve = function(h, connect = c("h", "v"), iteration = 1, random = FALSE) {

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
#' @param h1 The first subunit.
#' @param h2 The second subunit.
#' @param h3 The third subunit.
#' @param h4 The fourth subunit.
#' @export
#' @examples
#' expand_h(H0, connect = "hvvh") |> plot(type = "l", asp = 1)
#' expand_h(H1, connect = "vvhh") |> plot(type = "l", asp = 1)
#' 
#' h1 = expand_h(H0, connect = "hhhh")
#' h2 = expand_h(H0, connect = "vvvv")
#' h3 = expand_h(H0, connect = "hvhv")
#' h4 = expand_h(H0, connect = "hvvh")
#' expand_h(h1, h2, h3, h4, connect = "vhvh") |> plot(type = "l", asp = 1)
#' 
#' fun = function(h, k) {
#'     for(i in 1:k) h = expand_h(h, connect = "vhvh")
#'     h
#' }
#' fun(H0, 4) |> plot(type = "l", asp = 1)
expand_h = function(h1, h2 = h1, h3 = h1, h4 = h1, connect = "vvvv") {

	n = as.integer(round(sqrt(nrow(h1))))

	
	if(length(connect) == 1) {
		connect = strsplit(connect, "")[[1]]
		if(length(connect) == 1) {
			connect = rep(connect, 4)
		}
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
#' 
#' @rdname h_seed
#' @export
"H0"

#' @rdname h_seed
#' @export
"H1"

#' @rdname h_seed
#' @export
"H2"
