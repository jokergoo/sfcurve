

expand_xx = function(fun, x, level = 1, ...) {
	loc = fun(x, ...)
	for(i in seq_len(level-1)) {
		loc = fun(loc, ...)
	}
	loc
}

h0 = cbind(c(-0.5, -0.5, 0.5, 0.5), c(-0.5, 0.5, 0.5, -0.5))

h1 = expand(h0, connect = 1)
h2 = expand(h0, connect = 2)

h0 = cbind(c(-0.5, -0.5, -1.5, -1.5, -0.5, -1.5, -1.5, -0.5, 0.5, 0.5, 1.5, 1.5, 0.5, 1.5, 1.5, 0.5),
	       c(-0.5, -1.5, -1.5, -0.5, 0.5, 0.5, 1.5, 1.5, 0.5, 1.5, 1.5, 0.5, -0.5, -0.5, -1.5, -1.5))

h0 = cbind(c(-0.5, -1.5, -0.5, -1.5, -1.5, -0.5, -1.5, -0.5, 0.5, 1.5, 0.5, 1.5, 1.5, 0.5, 1.5, 0.5),
	       c(-1.5, -1.5, -0.5, -0.5, 0.5, 0.5, 1.5, 1.5, 1.5, 1.5, 0.5, 0.5, -0.5, -0.5, -1.5, -1.5))

h0 = cbind(c(-0.5, -0.5, -1.5, -1.5, -1.5, -0.5, -1.5, -0.5, 0.5, 0.5, 1.5, 1.5, 1.5, 0.5, 1.5, 0.5),
	       c(-1.5, -0.5, -1.5, -0.5, 0.5, 0.5, 1.5, 1.5, 1.5, 0.5, 1.5, 0.5, -0.5, -0.5, -1.5, -1.5))



expand_h = function(h1, h2 = h1, h3 = h1, h4 = h1, connect = "vvvv") {

	n = sqrt(nrow(h1))

	connect = strsplit(connect, "")[[1]]
	if(length(connect) == 1) {
		connect = rep(connect, 4)
	}

	h2 = h2
	h3 = h3
	h4 = h4

	h1 = open_h(h1, 3, connect[1])
	h2 = open_h(h2, 4, connect[2])
	h3 = open_h(h3, 1, connect[3])
	h4 = open_h(h4, 2, connect[4])


	h1[, 1] = h1[, 1] - (n-1)/2 - 1/2
	h1[, 2] = h1[, 2] - (n-1)/2 - 1/2
	h2[, 1] = h2[, 1] - (n-1)/2 - 1/2
	h2[, 2] = h2[, 2] + (n-1)/2 + 1/2
	h3[, 1] = h3[, 1] + (n-1)/2 + 1/2
	h3[, 2] = h3[, 2] + (n-1)/2 + 1/2
	h4[, 1] = h4[, 1] + (n-1)/2 + 1/2
	h4[, 2] = h4[, 2] - (n-1)/2 - 1/2

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


h1 = h3 = cbind(c(0, 1, 2, 2, 1, 0), c(1, 1, 1, 0, 0, 0))
h2 = h4 = cbind(c(0, 0, 0, 1, 1, 1), c(0, 1, 2, 2, 1, 0))



expand_z = function(z1, z2 = z1, z3 = z1, z4 = z1, connect = "z") {

	n = sqrt(nrow(z1))


	z2 = z2
	z3 = z3
	z4 = z4


	if(connect == "z") {
		z1[, 1] = z1[, 1] - (n-1)/2 - 1/2
		z1[, 2] = z1[, 2] + (n-1)/2 + 1/2
		z2[, 1] = z2[, 1] + (n-1)/2 + 1/2
		z2[, 2] = z2[, 2] + (n-1)/2 + 1/2
		z3[, 1] = z3[, 1] - (n-1)/2 - 1/2
		z3[, 2] = z3[, 2] - (n-1)/2 - 1/2
		z4[, 1] = z4[, 1] + (n-1)/2 + 1/2
		z4[, 2] = z4[, 2] - (n-1)/2 - 1/2
	} else if(connect == "n") {
		z1[, 1] = z1[, 1] - (n-1)/2 - 1/2
		z1[, 2] = z1[, 2] + (n-1)/2 + 1/2
		z2[, 1] = z2[, 1] - (n-1)/2 - 1/2
		z2[, 2] = z2[, 2] - (n-1)/2 - 1/2
		z3[, 1] = z3[, 1] + (n-1)/2 + 1/2
		z3[, 2] = z3[, 2] + (n-1)/2 + 1/2
		z4[, 1] = z4[, 1] + (n-1)/2 + 1/2
		z4[, 2] = z4[, 2] - (n-1)/2 - 1/2
	}

	rbind(z1, z2, z3, z4)
}


z1 = cbind(c(-0.5, 0.5, -0.5, 0.5), c(0.5, 0.5, -0.5, -0.5))
plot(expand_z(expand_z(z1)), type = "l")


z1 = rbind(c(0, 0))
plot(expand_z(expand_z(z1, connect = "n"), connect = "n"), type = "l")

k = 0
z = cbind(0, 0)
zl = list(z, z, z, z)
while(k < 4) {
	z1 = 
}


expand_gosper = function(g) {

	radius = nrow(g)/7*6/2*3


	g1 = rotate_coord(g, offset1)
	g2 = rotate_coord(g, offset1)
	g3 = rotate_coord(g, offset1)
	g4 = rotate_coord(g, offset1)
	g5 = rotate_coord(g, offset1)
	g6 = rotate_coord(g, offset1)
	g7 = rotate_coord(g, offset1)

	rbind(z1, z2, z3, z4)
}

expand_triangle = function(t) {
	
	triangle = attr(t, "triangle")
	center = c( (max(triangle[, 1]) + min(triangle[, 1]))/2, min(triangle[, 2]) )
	h = max(triangle[, 2] - triangle[, 1])

	t = reverse_coord(t)
	t1 = rotate_coord(t, 135, center)
	t1 = move_coord(t1, c(-h, h))
	t2 = rotate_coord(t, 225, center)
	t2 = move_coord(t2, c(h, h))
	tt = rbind(t1, t2)
	attr(tt, "triangle") = cbind(c(center[1] - h*sqrt(2), center[1] + h*sqrt(2), center[1]),
		                         c(center[2], center[2], center[2] + h*sqrt(2)))
	tt
}

t = cbind(c(-1/sqrt(2)-1, -1, 1, 1+1/sqrt(2)), c(0, 1/sqrt(2), 1/sqrt(2), 0))
t = move_coord(t, c(0, 0.3))

attr(t, "triangle") = cbind(c(-2, 2, 0), c(0, 0, 2))

plot(expand_triangle(t), type = "l")


open_diagonal = function(p, where) {

	if(where == 1) {
		p2 = rotate_coord(p, -45)
	} else if(where == 2) {
		p2 = rotate_coord(p, -135)
	} else if(where == 3) {
		p2 = rotate_coord(p, 135)
	} else if(where == 4) {
		p2 = rotate_coord(p, 45)
	}

	n = nrow(p)
	i = which.max(p2[, 2])

	part1 = h[seq(1, i), , drop = FALSE]
	part2 = h[i+seq_len(n-i), , drop = FALSE]

	rbind(part2, part1)
}

p = cbind(c())



expand_equilateral_triangle = function(x1, x2 = x1, x3 = x1, x4 = x1, type = 1) {

	triangle_len = attr(x1, "triangle_len")

	if(type == 1) {
		t1 = move_coord(x1, c(0, triangle_len/sqrt(3)))

		t2 = rotate_coord(x2, -60)
		t2 = reverse_coord(t2)

		t3 = move_coord(x3, c(-triangle_len/2, -triangle_len/2/sqrt(3)))

		t4 = rotate_coord(x4, 120)
		t4 = reverse_coord(t4)
		t4 = move_coord(t4, c(triangle_len/2, -triangle_len/2/sqrt(3)))
	} else if(type == 2) {
		t1 = rotate_coord(x1, -120)
		t1 = reverse_coord(t1)
		t1 = move_coord(t1, c(0, triangle_len/sqrt(3)))

		t2 = move_coord(x2, c(-triangle_len/2, -triangle_len/2/sqrt(3)))

		t3 = rotate_coord(x3, 60)
		t3 = reverse_coord(t3)

		t4 = move_coord(x4, c(triangle_len/2, -triangle_len/2/sqrt(3)))
	}

	tt = rbind(t1, t2, t3, t4)
	attr(tt, "triangle_len") = 2*triangle_len
	tt
}

t = cbind(c(0, 0, -1/2, 1/2), c(1/sqrt(3), 0, -1/2/sqrt(3), -1/2/sqrt(3)))
attr(t, "triangle_len") = 2




open_tri_hexagon = function(p, where) {
	i1 = which.max(p[, 1])+1

	n = nrow(p)

	p1 = p[get_curcular_index(i1+1, i1+n), , drop = FALSE]
}



cat_fun_list = list(
	"z1" = function(p1, p2 = p1, p3 = p1, p4 = p1) {
		n = sqrt(nrow(p1))

		p1[, 1] = p1[, 1] - (n-1)/2 - 1/2
		p1[, 2] = p1[, 2] + (n-1)/2 + 1/2
		p2[, 1] = p2[, 1] + (n-1)/2 + 1/2
		p2[, 2] = p2[, 2] + (n-1)/2 + 1/2
		p3[, 1] = p3[, 1] - (n-1)/2 - 1/2
		p3[, 2] = p3[, 2] - (n-1)/2 - 1/2
		p4[, 1] = p4[, 1] + (n-1)/2 + 1/2
		p4[, 2] = p4[, 2] - (n-1)/2 - 1/2

		rbind(p1, p2, p3, p4)
	},
	"z2" = function(p1, p2, p3, p4) {
		n = sqrt(nrow(p1))
		p1[, 1] = p1[, 1] - (n-1)/2 - 1/2
		p1[, 2] = p1[, 2] + (n-1)/2 + 1/2
		p2[, 1] = p2[, 1] - (n-1)/2 - 1/2
		p2[, 2] = p2[, 2] - (n-1)/2 - 1/2
		p3[, 1] = p3[, 1] + (n-1)/2 + 1/2
		p3[, 2] = p3[, 2] + (n-1)/2 + 1/2
		p4[, 1] = p4[, 1] + (n-1)/2 + 1/2
		p4[, 2] = p4[, 2] - (n-1)/2 - 1/2
		rbind(p1, p2, p3, p4)
	}
)