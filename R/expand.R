
# expand from level k to level k+1
sfc_expand = function(p, type = 1, increase = 2) {

	seq = p$seq
	rot = p$rot
	n = length(p$seq)

	base_pattern_set = get_base_pattern_set(increase)

	if(!(type == 1 || type == 2)) {
		stop("`type` can only take value in {1, 2}.")
	}

	pl = vector("list", n)
	pl[[1]] = base_pattern_set[[ seq[1] ]][[type]]
	pl[[1]] = rotate(pl[[1]], rot[1])
	tl = integer(n)
	tl[1] = type

	if(n > 1) {
		for(i in 2:n) {
			tl[i] = sfc_type(tl[i-1], rot[i-1], rot[i])
			pl[[i]] = base_pattern_set[[ seq[i] ]][[ tl[[i]] ]]
			pl[[i]] = rotate(pl[[i]], rot[i])
		}
	}

	p2 = do.call("c", pl)

	if(inherits(p, "sfc_initial")) {
		p2$initial = p
	}
	p2$type_code = c(p$type_code, type)
	class(p2) = union("sfc_expand", class(p2))
	p2
}

# t: previous type code
# r: prevous rotation
# r_next: next rotation
sfc_type = function(t, r, r_next) {
	if((r_next - r) %% 180 == 0) {
		t
	} else {
		c(2, 1)[t]
	}
}


library(Rcpp)

sourceCpp(code = "
#include <Rcpp.h>
using namespace Rcpp;

bool is_bit_one(int x, int pos) {
	bool l = x & (1 << (pos - 1));
	return l;
}

// [[Rcpp::export]]
IntegerVector int_to_binary(int x, int len) {
	IntegerVector b(len);
	for(int i = 0; i < len; i ++) {
		b[len - i - 1] = is_bit_one(x, i+1);
	}
	return b;
}
")


