#include <Rcpp.h>
using namespace Rcpp;

#include "utils.h"

void fold_z(NumericVector x1, NumericVector y1,
	NumericVector x2, NumericVector y2,
	NumericVector x3, NumericVector y3,
	NumericVector x4, NumericVector y4) {
	
	double offset = size(x1);

	vmove(x2, y2, offset);
	hmove(x3, y3, offset);
	move(x4, y4, offset, offset);

	return;
}


// [[Rcpp::export]]
List z_curve_cpp(int level) {

	// left, bottom, bottom, right
	if(level >= 2) {
		List pos = z_curve_cpp(level - 1);
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
		
		fold_z(x1, y1, x2, y2, x3, y3, x4, y4);
		
		NumericVector x_combine = c_vec(x1, x2, x3, x4);
		NumericVector y_combine = c_vec(y1, y2, y3, y4);

		List pos2 = List::create(x_combine, y_combine);
		return pos2;

	} else {
		// start phass: facing bottom, orientation from left to right
		NumericVector x = NumericVector::create(0, 0, 1, 1);
		NumericVector y = NumericVector::create(0, 1, 0, 1);
		List pos = List::create(x, y);

		return pos;
	}
}
