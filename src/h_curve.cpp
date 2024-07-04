#include <Rcpp.h>
using namespace Rcpp;

#include "utils.h"

List A_base();
List A(int level, double h_offset, double v_offset);
List B_base();
List B(int level, double h_offset, double v_offset);
List C_base();
List C(int level, double h_offset, double v_offset);
List D_base();
List D(int level, double h_offset, double v_offset);
List X1(int level, double h_offset, double v_offset);
List X2(int level, double h_offset, double v_offset);
List Y1(int level, double h_offset, double v_offset);
List Y2(int level, double h_offset, double v_offset);


List A_base() {
	NumericVector x = NumericVector::create(1, 1, 0, 0);
	NumericVector y = NumericVector::create(1, 0, 0, 1);

	List pos = List::create(x, y);
	return pos;
}

List A(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = A_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = X1(next, h_offset + offset, v_offset + offset);
		List pos2 = D(next, h_offset + offset, v_offset);
		List pos3 = A(next, h_offset, v_offset);
		List pos4 = B(next, h_offset, v_offset + offset);

		List pos11 = pos1[0];
		List pos12 = pos1[1];
		List pos = c_list(pos11, pos2, pos3, pos4, pos12);
		return pos;
	}
}

List B_base() {
	NumericVector x = NumericVector::create(0, 0, 1, 1);
	NumericVector y = NumericVector::create(0, 1, 1, 0);

	List pos = List::create(x, y);
	return pos;
}

List B(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = B_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = Y2(next, h_offset + offset, v_offset);
		List pos2 = A(next, h_offset, v_offset);
		List pos3 = B(next, h_offset, v_offset + offset);
		List pos4 = C(next, h_offset + offset, v_offset + offset);

		List pos11 = pos1[0];
		List pos12 = pos1[1];
		List pos = c_list(pos11, pos2, pos3, pos4, pos12);
		return pos;
	}
}


List C_base() {
	NumericVector x = NumericVector::create(0, 0, 1, 1);
	NumericVector y = NumericVector::create(0, 1, 1, 0);

	List pos = List::create(x, y);
	return pos;
}

List C(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = C_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = X2(next, h_offset, v_offset);
		List pos2 = B(next, h_offset, v_offset + offset);
		List pos3 = C(next, h_offset + offset, v_offset + offset);
		List pos4 = D(next, h_offset + offset, v_offset);

		List pos11 = pos1[0];
		List pos12 = pos1[1];
		List pos = c_list(pos11, pos2, pos3, pos4, pos12);
		return pos;
	}
}


List D_base() {
	NumericVector x = NumericVector::create(1, 1, 0, 0);
	NumericVector y = NumericVector::create(1, 0, 0, 1);

	List pos = List::create(x, y);
	return pos;
}

List D(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = D_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = Y1(next, h_offset, v_offset + offset);
		List pos2 = C(next, h_offset + offset, v_offset + offset);
		List pos3 = D(next, h_offset + offset, v_offset);
		List pos4 = A(next, h_offset, v_offset);

		List pos11 = pos1[0];
		List pos12 = pos1[1];
		List pos = c_list(pos11, pos2, pos3, pos4, pos12);
		return pos;
	}
}

List _X_base(int type = 1) {
	NumericVector x1 = NumericVector::create(1, 1);
	NumericVector y1 = NumericVector::create(1, 0);

	NumericVector x2 = NumericVector::create(0, 0);
	NumericVector y2 = NumericVector::create(0, 1);

	List pos1 = List::create(x1, y1);
	List pos2 = List::create(x2, y2);

	List pos;
	if(type == 1) {
		pos = List::create(pos1, pos2);
	} else {
		pos = List::create(pos2, pos1);
	}
	return pos;
}

List X1(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = _X_base(1);
		move(pos[0], h_offset, v_offset);
		move(pos[1], h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = X1(next, h_offset + offset, v_offset + offset);
		List pos2 = D(next, h_offset + offset, v_offset);
		List pos3 = X1(next, h_offset, v_offset);
		List pos4 = B(next, h_offset, v_offset + offset);

		List pos11 = pos1[0];
		List pos12 = pos1[1];
		List pos31 = pos3[0];
		List pos32 = pos3[1];
		
		List pos_1 = c_list(pos11, pos2, pos31);
		List pos_2 = c_list(pos32, pos4, pos12);

		List pos = List::create(pos_1, pos_2);
		return pos;
	}
}

List X2(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = _X_base(2);
		move(pos[0], h_offset, v_offset);
		move(pos[1], h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = X2(next, h_offset, v_offset);
		List pos2 = B(next, h_offset, v_offset + offset);
		List pos3 = X2(next, h_offset + offset, v_offset + offset);
		List pos4 = D(next, h_offset + offset, v_offset);

		List pos11 = pos1[0];
		List pos12 = pos1[1];
		List pos31 = pos3[0];
		List pos32 = pos3[1];
		
		List pos_1 = c_list(pos11, pos2, pos31);
		List pos_2 = c_list(pos32, pos4, pos12);

		List pos = List::create(pos_1, pos_2);
		return pos;
	}
}




List _Y_base(int type = 1) {
	List pos = _X_base(type);
	return pos;
}

List Y1(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = _Y_base(1);
		move(pos[0], h_offset, v_offset);
		move(pos[1], h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = Y1(next, h_offset, v_offset + offset);
		List pos2 = C(next, h_offset + offset, v_offset + offset);
		List pos3 = Y1(next, h_offset + offset, v_offset);
		List pos4 = A(next, h_offset, v_offset);

		List pos11 = pos1[0];
		List pos12 = pos1[1];
		List pos31 = pos3[0];
		List pos32 = pos3[1];
		
		List pos_1 = c_list(pos11, pos2, pos31);
		List pos_2 = c_list(pos32, pos4, pos12);

		List pos = List::create(pos_1, pos_2);
		return pos;
	}
}


List Y2(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = _Y_base(2);
		move(pos[0], h_offset, v_offset);
		move(pos[1], h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = Y2(next, h_offset + offset, v_offset);
		List pos2 = A(next, h_offset, v_offset);
		List pos3 = Y2(next, h_offset, v_offset + offset);
		List pos4 = C(next, h_offset + offset, v_offset + offset);

		List pos11 = pos1[0];
		List pos12 = pos1[1];
		List pos31 = pos3[0];
		List pos32 = pos3[1];
		
		List pos_1 = c_list(pos11, pos2, pos31);
		List pos_2 = c_list(pos32, pos4, pos12);

		List pos = List::create(pos_1, pos_2);
		return pos;
	}
}



// [[Rcpp::export]]
List h_curve_cpp(int level) {
	int offset = pow(2, level-1);
	int next = level-1;

	List pos;
		
	List pos1 = A(next, 0, 0);
	List pos2 = B(next, 0, offset);
	List pos3 = C(next, offset, offset);
	List pos4 = D(next, offset, 0);

	pos = c_list(pos1, pos2, pos3, pos4);
	
	return pos;

}
