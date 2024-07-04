#include <Rcpp.h>
using namespace Rcpp;

#include "utils.h"


void fold_meander(NumericVector x1, NumericVector y1,
	NumericVector x2, NumericVector y2,
	NumericVector x3, NumericVector y3,
	NumericVector x4, NumericVector y4,
	NumericVector x5, NumericVector y5,
	NumericVector x6, NumericVector y6,
	NumericVector x7, NumericVector y7,
	NumericVector x8, NumericVector y8,
	NumericVector x9, NumericVector y9) {
		
	double offset = size(x1);
	if(int(log(offset)/log(3)) == 1) {
		vmove(x2, y2, offset);

		hflip(x3, y3);
		turn(x3, y3, -90);
		vmove(x3, y3, offset*2);

		hflip(x4, y4);
		turn(x4, y4, -90);
		move(x4, y4, offset, offset*2);

		hflip(x5, y5);
		turn(x5, y5, -90);
		move(x5, y5, offset*2, offset*2);

		vflip(x6, y6);
		turn(x6, y6, -90);
		move(x6, y6, offset*2, offset);

		turn(x7, y7, 180);
		move(x7, y7, offset, offset);

		turn(x8, y8, 180);
		hmove(x8, y8, offset);

		hflip(x9, y9);
		turn(x9, y9, -90);
		hmove(x9, y9, offset*2);
	} else {
		hflip(x2, y2);
		turn(x2, y2, -90);
		hmove(x2, y2, offset);

		hflip(x3, y3);
		turn(x3, y3, -90);
		move(x3, y3, offset, offset);

		turn(x4, y4, 180);
		vmove(x4, y4, offset);

		vmove(x5, y5, offset*2);

		move(x6, y6, offset, offset*2);

		move(x7, y7, offset*2, offset*2);

		hflip(x8, y8);
		turn(x8, y8, 90);
		move(x8, y8, offset*2, offset);

		hflip(x9, y9);
		turn(x9, y9, 90);
		hmove(x9, y9, offset*2);
	}

	return;
}

// [[Rcpp::export]]
List meander_curve_cpp(int level) {

	// left, bottom, bottom, right
	if(level >= 2) {
		List pos = meander_curve_cpp(level - 1);
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
			
		fold_meander(x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8, x9, y9);
		
		NumericVector x_combine = c_vec(x1, x2, x3, x4, x5, x6, x7, x8, x9);
		NumericVector y_combine = c_vec(y1, y2, y3, y4, y5, y6, y7, y8, y9);

		List pos2 = List::create(x_combine, y_combine);
		return pos2;

	} else {
		NumericVector x;
		NumericVector y;

		x = NumericVector::create(0, 1, 2, 2, 2, 1, 1, 0, 0);
		y = NumericVector::create(0, 0, 0, 1, 2, 2, 1, 1, 2);
		
		List pos = List::create(x, y);

		return pos;
	}
}
