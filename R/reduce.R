

sfc_reduce = function(p, to = sfc_level(p) - 1) {
	level = sfc_level(p)

	if(level == 0 || to < 1) {
		return(p$initial)
	}

	type_code = p$type_code[seq_len(to)]

	sfc(p$initial, type = type_code)
}
