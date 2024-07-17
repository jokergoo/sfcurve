#include <Rcpp.h>
using namespace Rcpp;

#include "utils.h"

double size(NumericVector x) {
	return pow(x.size(), 1/3.0);
}


void reverse(NumericVector x, NumericVector y, NumericVector z) {
	NumericVector x2 = clone(x);
	NumericVector y2 = clone(y);
	NumericVector z2 = clone(z);

	x = rev(x2);
	y = rev(y2);
	z = rev(z2);
	return;
}


void move(NumericVector x, NumericVector y, NumericVector z, double x_offset, double y_offset, double z_offset) {

	x = x + x_offset;
	y = y + y_offset;
	z = z + z_offset;

	return;
}


void rotate_by_z_axis(NumericVector x, NumericVector y, NumericVector z, int angle) {
	double offset = (size(x) - 1.0)/2;

	NumericVector temp;
	if(angle == 90) {
		move(x, y, z, -offset, -offset, 0);
		temp = 0 + x;
		x = 0 + y;
		y = 0 + temp;
		x = 0 - x;
		move(x, y, z, offset, offset, 0);

	} else if(angle == -90) {
		move(x, y, z, -offset, -offset, 0);
		temp = 0 + x;
		x = 0 + y;
		y = 0 + temp;
		y = 0 - y;
		move(x, y, z, offset, offset, 0);

	} else if(angle == -180 || angle == 180) {
		move(x, y, z, -offset, -offset, 0);
		x = 0 - x;
		y = 0 - y;
		move(x, y, z, offset, offset, 0);

	}
	return;
}

void rotate_by_x_axis(NumericVector x, NumericVector y, NumericVector z, int angle) {
	double offset = (size(x) - 1.0)/2;

	NumericVector temp;
	if(angle == 90) {
		move(x, y, z, 0, -offset, -offset);
		temp = 0 + y;
		y = 0 + z;
		z = 0 + temp;
		y = 0 - y;
		move(x, y, z, 0, offset, offset);

	} else if(angle == -90) {
		move(x, y, z, 0, -offset, -offset);
		temp = 0 + y;
		y = 0 + z;
		z = 0 + temp;
		z = 0 - z;
		move(x, y, z, 0, offset, offset);

	} else if(angle == -180 || angle == 180) {
		move(x, y, z, 0, -offset, -offset);
		y = 0 - y;
		x = 0 - x;
		move(x, y, z, 0, offset, offset);

	}
	return;
}

void rotate_by_y_axis(NumericVector x, NumericVector y, NumericVector z, int angle) {
	double offset = (size(x) - 1.0)/2;

	NumericVector temp;
	if(angle == 90) {
		move(x, y, z, -offset, 0, -offset);
		temp = 0 + x;
		x = 0 + z;
		z = 0 + temp;
		x = 0 - x;
		move(x, y, z, offset, 0, offset);

	} else if(angle == -90) {
		move(x, y, z, -offset, 0, -offset);
		temp = 0 + x;
		x = 0 + z;
		z = 0 + temp;
		z = 0 - z;
		move(x, y, z, offset, 0, offset);

	} else if(angle == -180 || angle == 180) {
		move(x, y, z, -offset, 0, -offset);
		x = 0 - x;
		z = 0 - z;
		move(x, y, z, offset, 0, offset);

	}
	return;
}


void fold_hc(NumericVector x1, NumericVector y1, NumericVector z1,
	NumericVector x2, NumericVector y2, NumericVector z2,
	NumericVector x3, NumericVector y3, NumericVector z3,
	NumericVector x4, NumericVector y4, NumericVector z4,
	NumericVector x5, NumericVector y5, NumericVector z5,
	NumericVector x6, NumericVector y6, NumericVector z6,
	NumericVector x7, NumericVector y7, NumericVector z7,
	NumericVector x8, NumericVector y8, NumericVector z8) {
	
	double offset = size(x1);

	rotate_by_x_axis(x1, y1, z1, -90);
	rotate_by_y_axis(x1, y1, z1, 90);

	rotate_by_x_axis(x2, y2, z2, -90);
	rotate_by_z_axis(x2, y2, z2, 90);
	rotate_by_y_axis(x2, y2, z2, 180);
	move(x2, y2, z2, 0, offset, 0);

	rotate_by_x_axis(x3, y3, z3, -90);
	rotate_by_z_axis(x3, y3, z3, 90);
	rotate_by_y_axis(x3, y3, z3, 180);
	move(x3, y3, z3, offset, offset, 0);

	rotate_by_z_axis(x4, y4, z4, 180);
	move(x4, y4, z4, offset, 0, 0);

	rotate_by_z_axis(x5, y5, z5, 180);
	move(x5, y5, z5, offset, 0, offset);

	rotate_by_z_axis(x6, y6, z6, 90);
	rotate_by_y_axis(x6, y6, z6, 90);
	move(x6, y6, z6, offset, offset, offset);

	rotate_by_z_axis(x7, y7, z7, 90);
	rotate_by_y_axis(x7, y7, z7, 90);
	move(x7, y7, z7, 0, offset, offset);

	rotate_by_x_axis(x8, y8, z8, 90);
	rotate_by_y_axis(x8, y8, z8, -90);
	move(x8, y8, z8, 0, 0, offset);

	return;
}


// [[Rcpp::export]]
List hilbert_curve_3d_cpp(int level) {

	if(level >= 2) {
		List pos = hilbert_curve_3d_cpp(level - 1);
		NumericVector x = pos[0];
		NumericVector y = pos[1];
		NumericVector z = pos[2];

		NumericVector x1 = clone(x);
		NumericVector y1 = clone(y);
		NumericVector z1 = clone(z);
		NumericVector x2 = clone(x);
		NumericVector y2 = clone(y);
		NumericVector z2 = clone(z);
		NumericVector x3 = clone(x);
		NumericVector y3 = clone(y);
		NumericVector z3 = clone(z);
		NumericVector x4 = clone(x);
		NumericVector y4 = clone(y);
		NumericVector z4 = clone(z);
		NumericVector x5 = clone(x);
		NumericVector y5 = clone(y);
		NumericVector z5 = clone(z);
		NumericVector x6 = clone(x);
		NumericVector y6 = clone(y);
		NumericVector z6 = clone(z);
		NumericVector x7 = clone(x);
		NumericVector y7 = clone(y);
		NumericVector z7 = clone(z);
		NumericVector x8 = clone(x);
		NumericVector y8 = clone(y);
		NumericVector z8 = clone(z);
		
		fold_hc(x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, x5, y5, z5, x6, y6, z6, x7, y7, z7, x8, y8, z8);

		NumericVector x_combine = c_vec8(x1, x2, x3, x4, x5, x6, x7, x8);
		NumericVector y_combine = c_vec8(y1, y2, y3, y4, y5, y6, y7, y8);
		NumericVector z_combine = c_vec8(z1, z2, z3, z4, z5, z6, z7, z8);

		List pos2 = List::create(x_combine, y_combine, z_combine);
		return pos2;

	} else {
		// start phass: facing bottom, orientation from left to right
		NumericVector x = NumericVector::create(0, 0, 1, 1, 1, 1, 0, 0);
		NumericVector y = NumericVector::create(0, 1, 1, 0, 0, 1, 1, 0);
		NumericVector z = NumericVector::create(0, 0, 0, 0, 1, 1, 1, 1);
		List pos = List::create(x, y, z);

		return pos;
	}
}
