
rotate = function(p, rot) {
	if(! (rot %% 360) %in% c(0, 90, 180, 270)) {
		stop("`rot` should only be multiple of 90.")
	}
	p$rot = (p$rot + rot) %% 360
	p
}

`^.sfc_sequence` = function(e1, e2) {
	rotate(e1, e2)
}

hflip = function(p) {
	p2 = p
	p2$seq[p$seq == "L"] = "R"
	p2$seq[p$seq == "R"] = "L"

	p2$rot[p$rot == 90] = 270
	p2$rot[p$rot == 270] = 90

	p2$rot = p2$rot %% 360
	p2
}

vflip = function(p) {
	hflip(rotate(p, 180))
}

dflip = function(p, type = 1) {
	if(type == 1) {
		rotate(hflip(p), 90)
	} else {
		rotate(hflip(p), -90)
	}
}

reverse = function(p) {
	p2 = p
	p2$seq = rev(p2$seq)
	p2$rot = rev(p2$rot)

	l1 = p2$seq == "I"
	if(any(l1)) {
		p2$rot[l1] = p2$rot[l1] + 180
	}

	l2 = p2$seq == "R"
	l3 = p2$seq == "L"

	if(any(l2)) {
		p2$seq[l2] = "L"
		p2$rot[l2] = p2$rot[l2] - 90
	}
	if(any(l3)) {
		p2$seq[l3] = "R"
		p2$rot[l3] = p2$rot[l3] + 90
	}

	p2$rot = p2$rot %% 360
	p2
}

rev.sfc_pattern = function(x) {
	reverse(x)
}
