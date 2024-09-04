
# adjust the segment, start point must be in the bottom-left unit and end point
# must be in the bottom-right unit

#' Shape of the curve
#' @aliases sfc_shape
#' @rdname sfc_shape
#' 
#' @param p An `sfc_2x2` object.
#' @details
#' The shape of the curve is defined as a form of the curve without considering entry/exit directions, rotation, 
#' flipping (reflection) nor reversing.
#' 
#' ## 2x2 curve
#' 
#' The process of selecting the shape segment of the curve denoted as `P` is:
#' 
#' 1. The entry-point should locate in the bottom left subunit and the exit-point should 
#'    locate in the bottom right subunit. We try the four rotations (0, 90, 180, 270), and 
#'    the four rotations on the horizontally flipped curve. Once we find the transformed curve
#'    that satisfies this criterion, we term it as `P2`.
#' 2. We also generate `P3` which is a horizontally flipped version of `rev(P2)`.
#' 3. We compare the first point `p` of `P2` and `P3`, and select the one whose `p` has the smaller
#'    x-coordinate (i.e. more to the left of the curve). If the x-coordinates of `p` are the same in
#'    `P2` and `P3`, we select the one whose `p` has the smaller y-coordinate.
#' 
#' @return
#' `sfc_shape()` returns a two-column data frame of the xy-coordinates of the shape curve.
#' @export
#' @examples
#' p1 = sfc_2x2("I", 11)
#' p2 = sfc_2x2("R", 22)
#' draw_multiple_curves(
#'     p1, p2, 
#'     sfc_shape(p1), sfc_shape(p2), 
#'     col = "black")
setMethod("sfc_shape", 
	signature = "sfc_2x2",
	definition = function(p) {

	loc = sfc_segments(p)
	np = nrow(loc)

	# center of the curve set to (0, 0)
	loc[, 1] = loc[, 1] - mean(loc[, 1])
	loc[, 2] = loc[, 2] - mean(loc[, 2])

	ok = FALSE
	for(theta in c(0, 90, 180, 270)) {
		loc2 = rotate_coord(loc, theta)
		if(loc2[1, 1] < 0 & loc2[1, 2] < 0 & loc2[np, 1] > 0 & loc2[np, 2] < 0) {
			ok = TRUE
			break
		}
	}

	if(!ok) {
		loc = hflip_coord(loc, 0)
		for(theta in c(0, 90, 180, 270)) {
			loc2 = rotate_coord(loc, theta)
			if(loc2[1, 1] < 0 & loc2[1, 2] < 0 & loc2[np, 1] > 0 & loc2[np, 2] < 0) {
				ok = TRUE
				break
			}
		}
	}

	loc_rev = hflip_coord(reverse_coord(loc2))

	if(equal_to(loc2[1, 1], loc_rev[1, 1])) {
		if(loc2[1, 2] <= loc_rev[1, 2]) {
			loc_final = loc2
		} else {
			loc_final = loc_rev
		}
	} else {
		if(loc2[1, 1] < loc_rev[1, 1]) {
			loc_final = loc2
		} else {
			loc_final = loc_rev
		}
	}

	loc_final[, 1] = round(loc_final[, 1] - min(loc_final[, 1]))
	loc_final[, 2] = round(loc_final[, 2] - min(loc_final[, 2]))
	loc_final = as.integer(loc_final)

	dim(loc_final) = c(nrow(loc), 2)
	loc_final
})

#' @rdname sfc_shape
#' @param level Level of the 2x2 curve.
#' @export
#' @return
#' `all_2x2_shapes()` returns a list of `n` two-column data frames where each data frame corresponds to
#' the xy-coordnates of the corresponding shape curve.
#' @examples
#' sl = all_2x2_shapes(2)
#' draw_multiple_curves(list = sl, lwd = 2, col = "black")
#' sl = all_2x2_shapes(3)
#' draw_multiple_curves(list = sl, lwd = 2, col = "black")
all_2x2_shapes = function(level = 2) {

	pl = list()
	for(bp in names(SFC_RULES_2x2@rules)) {
		pl = c(pl, lapply(1:2^level, function(x) {
			sfc_2x2(bp, code = int_to_binary(x-1, level)+1)
		}))
	}

	sl = lapply(pl, sfc_shape)

	hash = sapply(sl, digest::digest)
	l = !duplicated(hash)
	sl[l]
}

uid_3x3_peano = function(loc, mode) {

	v = c("vertical" = 1L, "horizontal" = 2L)

	unit_orientation = function(loc) {
		if(equal_to(loc[1, 1], loc[2, 1]) && equal_to(loc[2, 1], loc[3, 1])) {
	        "vertical"
	    } else {
	        "horizontal"
	    }
	}

	go_next_level = function(loc, res) {
		n = nrow(loc)
		if(n == 9) {
			return(res)
		} else {
			block_size = n/mode^2
			for(i in 1:9) {
				loc2 = loc[seq((i-1)*block_size+1, i*block_size), ]
				loc1 = sfc_reduce(loc2, to = 1)
				res = c(res, v[unit_orientation(loc1)])
				res = go_next_level(loc2, res)
			}
			return(res)
		}
	}

	if(nrow(loc) == 1) {
		return(integer(0))
	}

	if(nrow(loc) == 9) {
		return(v[unit_orientation(loc)])
	}

	loc1 = sfc_reduce(loc, to = 1)
	res = v[unit_orientation(loc1)]
	res = go_next_level(loc, res)

	paste(res, collapse = "")
}

#' @rdname sfc_shape
#' @export
#' @details
#' ## 3x3 Peano curve
#' 
#' The process of selecting the shape segment of the curve denoted as `P` is:
#' 
#' 1. The entry-point should locate in the bottom left subunit and the exit-point should 
#'    locate in the top right subunit. We try the four rotations (0, 90, 180, 270). Once we find the transformed curve
#'    that satisfies this criterion, we term it as `P2`.
#' 2. We also generate `P3` which is a 180 degrees rotation on the reversed `P2`.
#' 3. We calculate the "UID" of `P*` and pick the one with the smallest UID as the final curve.
#' 
#' The UID of a 3x3 Peano curve is based on the hierarchical indices of the units on it. The hierarchy of the
#' Peano curve is traversed in a depth-first manner. On each node, the orientation of the corresponding unit is
#' calculated where vertical is 1 and horizontal is 2. The digits are concatenated into a long string.
#' 
#' @examples
#' p = sfc_3x3_peano("I", 11)
#' draw_multiple_curves(
#'     p, sfc_reverse(p),
#'     sfc_shape(p), sfc_shape(sfc_reverse(p)),
#'     col = "black")
setMethod("sfc_shape", 
	signature = "sfc_3x3_peano",
	definition = function(p) {

	loc = sfc_segments(p)
	np = nrow(loc)

	# center of the curve set to (0, 0)
	loc[, 1] = loc[, 1] - mean(loc[, 1])
	loc[, 2] = loc[, 2] - mean(loc[, 2])

	ok = FALSE
	for(theta in c(0, 90, 180, 270)) {
		loc2 = rotate_coord(loc, theta)
		if(loc2[1, 1] < 0 & loc2[1, 2] < 0 & loc2[np, 1] > 0 & loc2[np, 2] > 0) {
			ok = TRUE
			break
		}
	}

	loc_flip1 = rotate_coord(reverse_coord(loc2), 180)
	
	uid1 = uid_3x3_peano(loc2, p@mode)
	uid2 = uid_3x3_peano(loc_flip1, p@mode)
	
	lt = list(loc2, loc_flip1)

	ind = which.min(c(uid1, uid2))
	loc_final = lt[[ind]]

	loc_final[, 1] = round(loc_final[, 1] - min(loc_final[, 1]))
	loc_final[, 2] = round(loc_final[, 2] - min(loc_final[, 2]))
	loc_final = as.integer(loc_final)

	dim(loc_final) = c(nrow(loc), 2)
	loc_final
})
