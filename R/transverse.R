
# to know the position of the next point, we need two things
# 1. the segment from the previous point and current point
# 2. the action at the current point, i.e. to go straight, turn right or left
#
# position of the next point
next_point = function(prev, curr, action, length = 1L) {
	if(action == "I") {
		if(prev[1] == curr[1]) {
			if(prev[2] < curr[2]) {
				return(c(curr[1], curr[2] + length))
			} else {
				return(c(curr[1], curr[2] - length))
			}
		} else if(prev[2] == curr[2]) {
			if(prev[1] < curr[1]) {
				return(c(curr[1] + length, curr[2]))
			} else {
				return(c(curr[1] - length, curr[2]))
			}
		}
	} else if(action == "R") {
		if(prev[1] == curr[1]) {
			if(prev[2] < curr[2]) {
				return(c(curr[1] + length, curr[2]))
			} else {
				return(c(curr[1] - length, curr[2]))
			}
		} else if(prev[2] == curr[2]) {
			if(prev[1] < curr[1]) {
				return(c(curr[1], curr[2] - length))
			} else {
				return(c(curr[1], curr[2] + length))
			}
		}
	} else if(action == "L") {
		if(prev[1] == curr[1]) {
			if(prev[2] < curr[2]) {
				return(c(curr[1] - length, curr[2]))
			} else {
				return(c(curr[1] + length, curr[2]))
			}
		} else if(prev[2] == curr[2]) {
			if(prev[1] < curr[1]) {
				return(c(curr[1], curr[2] + length))
			} else {
				return(c(curr[1], curr[2] - length))
			}
		}
	}
}

sfc_transverse = function(p, start = c(0, 0)) {
	seq = p$seq
	rot = p$rot[1]
	n = length(seq)
	pos = matrix(NA_integer_, nrow = n, ncol = 2)

	# "virtual prevous point" of the sequence
	prev = sequence_prev_point(start, rot)

	pos[1, ] = start
	for(i in seq_len(n-1)) {
		if(i == 1) {
			pos[2, ] = next_point(prev, pos[1, ], seq[1])
		} else {
			pos[i+1, ] = next_point(pos[i-1, ], pos[i, ], seq[i])
		}
	}

	pos
}

# the "virtual previous point" of a sequence
# 
# only for B and D, the entering direction is not bottom-up,
# however, prev_point is not applied to the singlet base pattern, 
# so we do not need to consider the B/D scenario
#
# The implementation of this function assumes the "previous poinrt"
# is at the bottom of the first point in the sequence
sequence_prev_point = function(curr, rot, length = 1L) {
	if(rot == 0) {
		prev = c(0, -length) + curr
	} else if(rot == 90) {
		prev = c(-length, 0) + curr
	} else if(rot == 180) {
		prev = c(0, length) + curr
	} else if(rot == 270) {
		prev = c(length, 0) + curr
	}

	prev
}
