
# adjust the segment, start point must be in the bottom-left unit and end point
# must be in the bottom-right unit

#' Shape of the curve
#' @aliases sfc_shape
#' @rdname sfc_shape
#' 
#' @param p An `sfc_2x2` object.
#' @details
#' The shape of the curve is defined as a form of the curve without considering rotation, 
#' flipping (reflection) nor reversing.
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
setMethod("sfc_shape", 
	signature = "sfc_2x2",
	definition = function(p) {
	loc = sfc_segments(p)
	np = nrow(loc)

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
#' @param level Level of the Hilbert curve.
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



