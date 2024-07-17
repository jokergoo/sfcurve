#ifndef __UTILS__
#define __UTILS__

NumericVector c_vec2(NumericVector x1, NumericVector x2);
NumericVector c_vec3(NumericVector x1, NumericVector x2, NumericVector x3);
NumericVector c_vec4(NumericVector x1, NumericVector x2, NumericVector x3, NumericVector x4);
NumericVector c_vec8(NumericVector x1, NumericVector x2, NumericVector x3, NumericVector x4,
	                 NumericVector x5, NumericVector x6, NumericVector x7, NumericVector x8);
bool is_bit_one(int x, int pos);
void rotate_coord(NumericVector x, NumericVector y, double theta, double x_center, double y_center);
void move_coord(NumericVector x, NumericVector y, double x_offset, double y_offset);

#endif
