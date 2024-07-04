#include <Rcpp.h>
using namespace Rcpp;

#include "utils.h"

void fold_peano(NumericVector x1, NumericVector y1,
	NumericVector x2, NumericVector y2,
	NumericVector x3, NumericVector y3,
	NumericVector x4, NumericVector y4,
	NumericVector x5, NumericVector y5,
	NumericVector x6, NumericVector y6,
	NumericVector x7, NumericVector y7,
	NumericVector x8, NumericVector y8,
	NumericVector x9, NumericVector y9, int type) {
	
	double offset = size(x1);

	if(is_bit_one(type, 1)) {
		diag_flip(x1, y1, 1);
	}

	hflip(x2, y2);
	if(is_bit_one(type, 2)) {
		diag_flip(x2, y2, -1);
	}
	vmove(x2, y2, offset);

	if(is_bit_one(type, 3)) {
		diag_flip(x3, y3, 1);
	}
	vmove(x3, y3, offset*2);

	vflip(x4, y4);
	if(is_bit_one(type, 4)) {
		diag_flip(x4, y4, -1);
	}
	move(x4, y4, offset, offset*2);

	turn(x5, y5, 180);
	if(is_bit_one(type, 5)) {
		diag_flip(x5, y5, 1);
	}
	move(x5, y5, offset, offset);

	vflip(x6, y6);
	if(is_bit_one(type, 6)) {
		diag_flip(x6, y6, -1);
	}
	hmove(x6, y6, offset);

	if(is_bit_one(type, 7)) {
		diag_flip(x7, y7, 1);
	}
	hmove(x7, y7, offset*2);

	hflip(x8, y9);
	if(is_bit_one(type, 8)) {
		diag_flip(x8, y8, -1);
	}
	move(x8, y8, offset*2, offset);

	if(is_bit_one(type, 9)) {
		diag_flip(x9, y9, 1);
	}
	move(x9, y9, offset*2, offset*2);

	return;
}

// [[Rcpp::export]]
List peano_curve_cpp(int level, int type = 0) {

	if(type > 511 || type < 0) {
		stop("type should be an integer between 0 and 512.");
	}

	// left, bottom, bottom, right
	if(level >= 2) {
		List pos = peano_curve_cpp(level - 1, type);
		NumericVector x = pos[0];
		NumericVector y = pos[1];

		NumericVector x1 = clone(x);
		NumericVector y1 = clone(y);
		NumericVector x2 = clone(x);
		NumericVector y2 = clone(y);
		NumericVector x3 = clone(x);
		NumericVector y3 = clone(y);
		NumericVector x4 = clone(x);
		NumericVector y4 = clone(y);
		NumericVector x5 = clone(x);
		NumericVector y5 = clone(y);
		NumericVector x6 = clone(x);
		NumericVector y6 = clone(y);
		NumericVector x7 = clone(x);
		NumericVector y7 = clone(y);
		NumericVector x8 = clone(x);
		NumericVector y8 = clone(y);
		NumericVector x9 = clone(x);
		NumericVector y9 = clone(y);
			
		fold_peano(x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8, x9, y9, type);

		NumericVector x_combine = c_vec(x1, x2, x3, x4, x5, x6, x7, x8, x9);
		NumericVector y_combine = c_vec(y1, y2, y3, y4, y5, y6, y7, y8, y9);

		List pos2 = List::create(x_combine, y_combine);
		return pos2;

	} else {
		NumericVector x;
		NumericVector y;

		x = NumericVector::create(0, 0, 0, 1, 1, 1, 2, 2, 2);
		y = NumericVector::create(0, 1, 2, 2, 1, 0, 0, 1, 2);
		
		List pos = List::create(x, y);

		return pos;
	}
}
