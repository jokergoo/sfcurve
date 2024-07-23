#include <Rcpp.h>
using namespace Rcpp;


NumericVector c_vec2(NumericVector x1, NumericVector x2) {
	int n1 = x1.size();
	int n2 = x2.size();

	int n = n1 + n2;

	NumericVector x(n);
	int offset = 0;
	for(int i = 0; i < n1; i ++) {
		x[i] = x1[i];
	}

	offset = n1;
	for(int i = 0; i < n2; i ++) {
		x[offset + i] = x2[i];
	}

	return x;
}

NumericVector c_vec3(NumericVector x1, NumericVector x2, NumericVector x3) {
	int n1 = x1.size();
	int n2 = x2.size();
	int n3 = x3.size();

	int n = n1 + n2 + n3;

	NumericVector x(n);
	int offset = 0;
	for(int i = 0; i < n1; i ++) {
		x[i] = x1[i];
	}

	offset = n1;
	for(int i = 0; i < n2; i ++) {
		x[offset + i] = x2[i];
	}

	offset = n1 + n2;
	for(int i = 0; i < n3; i ++) {
		x[offset + i] = x3[i];
	}

	return x;
}


NumericVector c_vec4(NumericVector x1, NumericVector x2, NumericVector x3, NumericVector x4) {
	
	NumericVector v1 = c_vec2(x1, x2);
	NumericVector v2 = c_vec2(x3, x4);

	NumericVector x = c_vec2(v1, v2);

	return x;
}

NumericVector c_vec8(NumericVector x1, NumericVector x2, NumericVector x3, NumericVector x4,
	                 NumericVector x5, NumericVector x6, NumericVector x7, NumericVector x8) {
	
	NumericVector v1 = c_vec4(x1, x2, x3, x4);
	NumericVector v2 = c_vec4(x5, x6, x7, x8);

	NumericVector x = c_vec2(v1, v2);

	return x;
}



List c_list2(List pos1, List pos2) {
	NumericVector x1 = pos1[0];
	NumericVector x2 = pos2[0];

	NumericVector x = c_vec2(x1, x2);

	NumericVector y1 = pos1[1];
	NumericVector y2 = pos2[1];

	NumericVector y = c_vec2(y1, y2);

	List pos = List::create(x, y);
	return pos;
}


List c_list3(List pos1, List pos2, List pos3) {
	NumericVector x1 = pos1[0];
	NumericVector x2 = pos2[0];
	NumericVector x3 = pos3[0];

	NumericVector x = c_vec3(x1, x2, x3);

	NumericVector y1 = pos1[1];
	NumericVector y2 = pos2[1];
	NumericVector y3 = pos3[1];

	NumericVector y = c_vec3(y1, y2, y3);

	List pos = List::create(x, y);
	return pos;
}

bool is_bit_one2(int x, int pos) {
	bool l = x & (1 << (pos - 1));
	return l;
}

// [[Rcpp::export]]
IntegerVector int_to_binary(int x, int len) {
	IntegerVector b(len);
	for(int i = 0; i < len; i ++) {
		b[len - i - 1] = is_bit_one2(x, i+1);
	}
	return b;
}


void rotate_coord(NumericVector x, NumericVector y, double theta, double x_center, double y_center) {

	x = x - x_center;
	y = y - y_center;

	theta = theta/180*3.1415926;
	NumericVector x2 = x*cos(theta) - y*sin(theta);
	NumericVector y2 = x*sin(theta) + y*cos(theta);

	x2 = x2 + x_center;
	y2 = y2 + y_center;

	x = x2;
	y = y2;

	return;
}


void move_coord(NumericVector x, NumericVector y, double x_offset, double y_offset) {
	
	x = x + x_offset;
	y = y + y_offset;
	
	return;
}

