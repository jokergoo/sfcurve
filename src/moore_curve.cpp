#include <Rcpp.h>
using namespace Rcpp;

#include "utils.h"

void fold_moore(NumericVector x1, NumericVector y1,
	NumericVector x2, NumericVector y2,
	NumericVector x3, NumericVector y3,
	NumericVector x4, NumericVector y4,
	int type) {

	double offset = size(x1);

	turn(x1, y1, 90);
	if(is_bit_one(type, 1)) {
		diag_flip(x1, y1, 1);
	}

	turn(x2, y2, 90);
	vmove(x2, y2, offset);

	turn(x3, y3, -90);
	move(x3, y3, offset, offset);

	turn(x4, y4, -90);
	if(is_bit_one(type, 2)) {
		diag_flip(x4, y4, -1);
	}
	hmove(x4, y4, offset);
	
	return;
}

// [[Rcpp::export]]
List moore_curve_cpp(int level, int type = 0) {

	if(type > 3 || type < 0) {
		stop("type should be an integer between 0 and 3.");
	}

	// left, bottom, bottom, right
	if(level >= 2) {
		List pos = moore_curve_cpp(level - 1, type);
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
		
		fold_moore(x1, y1, x2, y2, x3, y3, x4, y4, type);
		
		NumericVector x_combine = c_vec(x1, x2, x3, x4);
		NumericVector y_combine = c_vec(y1, y2, y3, y4);

		List pos2 = List::create(x_combine, y_combine);
		return pos2;

	} else {
		// start phass: facing bottom, orientation from left to right
		NumericVector x = NumericVector::create(0, 0, 1, 1);
		NumericVector y = NumericVector::create(0, 1, 1, 0);
		List pos = List::create(x, y);

		return pos;
	}
}
