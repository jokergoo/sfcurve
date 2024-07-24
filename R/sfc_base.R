


setClass("sfc_base",
	slots = c("letter" = "character",
		      "preceeding" = "numeric",
		      "current" = "numeric",
		      "succeeding" = "numeric",
		      "in_direction" = "numeric",
		      "out_direction" = "numeric",
		      "primary" = "logical",
		      "open" = "logical",
		      "grob" = "ANY"),
	prototype = list("current" = c(0, 0),
		             "grob" = gTree()))


#' Constructor of the sfc_base class
#' 
#' @param letter A single letter (not necessarily) to represent the base pattern.
#' @param in_direction The direction of the segment that enters the point, measured in the polar coordinate system, in degrees.
#' @param out_direction The direction of the segment that leaves the point, measured in the polar coordinate system, in degrees.
#' @param grob A [`grid::grob()`] object of this base pattern. If it is not set, it is generated according to `in_direction` and `out_direction`.
#' @param primary Currently, going forward, turning left and turning right can be set as primary base patterns because other high-level patterns
#'        can be built from them.
#' @param open Can the base pattern be connected to other base patterns?
#' 
#' @details
#' The "base pattern" is designed not only for single point but also for combination of points that form a "base curve". However,
#' currently, it is fixed to the single point base pattern. 
#' 
#' Currently, this package supports 2x2 and 3x3 space-filling curves that fills grids in 2D space constructed by the Gaussian integers.
#' And when the curve expands, we only allow the segments to go forward, backward, left and right. Thus there are the following base patterns
#' pre-defined in this package:
#' 
#' - [`BASE_I`]: go forward.
#' - [`BASE_J`]: go forward.
#' - [`BASE_R`]: turn right.
#' - [`BASE_L`]: turn left.
#' - [`BASE_U`]: go backward.
#' - [`BASE_B`]: leave the start point where the start point is closed.
#' - [`BASE_D`]: leave the start point where the start point is closed.
#' - [`BASE_P`]: return to the end point where the end point is closed.
#' - [`BASE_Q`]: return to the end point where the end point is closed.
#' - [`BASE_C`]: self-closed.
#' 
#' The base pattern determines the final form of the curve. 
#' 
#' @return
#' An `sfc_base` object.
#' 
#' @export
#' @import grid
#' @examples
#' BASE_I
sfc_base = function(letter, in_direction, out_direction, grob = NULL, 
	primary = TRUE, open = TRUE) {

	current = c(0, 0)

	in_direction = in_direction %% 360
	out_direction = out_direction %% 360

	x = current[1] - cos(in_direction/180*pi)
	y = current[2] - sin(in_direction/180*pi)

	preceeding = c(x, y)

	x = current[1] + cos(out_direction/180*pi)
	y = current[2] + sin(out_direction/180*pi)

	succeeding = c(x, y)
	
	b = new("sfc_base")
	b@letter = letter
	b@preceeding = preceeding
	b@current = current
	b@succeeding = succeeding
	b@in_direction = in_direction
	b@out_direction = out_direction
	b@primary = primary
	b@open = open

	if(is.null(grob)) {
		
		gbl = list()
		gbl[[1]] = segmentsGrob(b@preceeding[1], b@preceeding[2], b@current[1], b@current[2], default.units = "native", gp = gpar(col = "grey"))
		gbl[[2]] = segmentsGrob(b@current[1], b@current[2], b@succeeding[1], b@succeeding[2], default.units = "native", gp = gpar(col = "grey"), arrow = arrow(angle = 15, length = unit(0.1, "npc")))
		gbl[[3]] = pointsGrob(b@current[1], b@current[2], default.units = "native", pch = 16, size = unit(4, "pt"))
		gb = grobTree(gbl[[1]], gbl[[2]], gbl[[3]], vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1)))
		b@grob = gb
	} else {
		b@grob = grob
	}

	b
}


#' @export
plot.sfc_base = function(x, ...) {
	
	grid.newpage()
	pushViewport(viewport(width = unit(2, "cm"), height = unit(2, "cm")))
	grid.draw(x@grob)
	popViewport()
	
}

#' Print the object
#' 
#' @param object The corresponding object.
#' @rdname show
#' @export
setMethod("show",
	signature = "sfc_base",
	definition = function(object) {

	if(is.na(object@in_direction) && is.na(object@out_direction)) {
		cat("Base: ", object@letter, ", self-closed.\n", sep = "")
	} else if(is.na(object@in_direction) && !is.na(object@out_direction)) {
		cat("Base: ", object@letter, ", with in-closed and ", object@out_direction, " degrees out.\n", sep = "")
	} else if(!is.na(object@in_direction) && is.na(object@out_direction)) {
		cat("Base: ", object@letter, ", with ", object@in_direction, " degrees in and out-closed.\n", sep = "")
	} else {
		cat("Base: ", object@letter, ", with ", object@in_direction, " degrees in and ", object@out_direction, " degrees out.\n", sep = "")
	}
})

#' The graphics object
#' 
#' @rdname sfc_grob
#' @param p The corresponding object.
#' 
#' @return A [`grid::grob()`] object.
#' @export
#' @examples
#' sfc_grob(BASE_I)
setMethod("sfc_grob",
	signature = "sfc_base", 
	definition = function(p) {
	p@grob
})


#' The previous and the next point
#' @aliases sfc_previous_point
#' @rdname sfc_previous_point
#' @param p An `sfc_base` object.
#' @param x The coordinate of the current point.
#' @param rot Rotation of the current point.
#' @param length Length of the segment between the previous/next point and the current point.
#'
#' @return A vector of length two.
#' @export
#' @examples
#' sfc_previous_point(BASE_R, c(0, 0), 0)
#' sfc_previous_point(BASE_R, c(0, 0), 90)
#' sfc_previous_point(BASE_R, c(0, 0), 180)
#' sfc_previous_point(BASE_R, c(1, 0), 0)
#' sfc_previous_point(BASE_R, c(1, 0), 90)
#' sfc_previous_point(BASE_R, c(1, 0), 180)
setMethod("sfc_previous_point",
	signature = "sfc_base",
	definition = function(p, x, rot, length = 1) {

	preceeding = numeric(2)
	pos = rotate_coord(p@current, -rot)
	preceeding[1] = p@current[1] + length*cos(pi + p@in_direction/180*pi)
	preceeding[2] = p@current[2] + length*sin(pi + p@in_direction/180*pi)
	pos = rotate_coord(preceeding, rot)
	pos = move_coord(pos, x)
	pos

})


#' @aliases sfc_next_point
#' @rdname sfc_previous_point
#' @export
#' @examples
#' sfc_next_point(BASE_R, c(0, 0), 0)
#' sfc_next_point(BASE_R, c(0, 0), 90)
#' sfc_next_point(BASE_R, c(0, 0), 180)
#' sfc_next_point(BASE_R, c(1, 0), 0)
#' sfc_next_point(BASE_R, c(1, 0), 90)
#' sfc_next_point(BASE_R, c(1, 0), 180)
setMethod("sfc_next_point",
	signature = "sfc_base",
	definition = function(p, x, rot, length = 1) {

	succeeding = numeric(2)
	succeeding[1] = p@current[1] + length*cos(p@out_direction/180*pi)
	succeeding[2] = p@current[2] + length*sin(p@out_direction/180*pi)
	pos = rotate_coord(succeeding, rot)
	pos = move_coord(pos, x)
	pos
})


# are two base patterns horizontally or vertically connected?
connected_patterns = function(x1, rot1, pos1, x2, rot2, pos2, bases) {
	p2 = sfc_next_point(bases[[ x1 ]], pos1, rot1)
	p1 = sfc_previous_point(bases[[ x2 ]], pos2, rot2)

	all(equal_to(p1, pos1) & equal_to(p2, pos2))
}



# given a previous point, a next point, a current point, choose the base pattern and rotation
guess_base_pattern = function(bases, in_direction, out_direction) {
	diff = out_direction - in_direction
	diff = diff %% 360L
	for(i in seq_along(bases)) {
		b = bases[[i]]
		diff2 = (b@out_direction - b@in_direction) %% 360L
		if(equal_to(diff, diff2)) {
			return(list(letter = b@letter, rot = (in_direction - b@in_direction) %% 360L))
		}
	}
	return(NULL)
}


base_in_direction = function(b, rot) {
	(b@in_direction + rot) %% 360L
}

base_out_direction = function(b, rot) {
	(b@out_direction + rot) %% 360L
}
