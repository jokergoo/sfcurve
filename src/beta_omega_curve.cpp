#include <Rcpp.h>
using namespace Rcpp;

#include "utils.h"

List omega_base();
List omega(int level, double h_offset, double v_offset);
List omega_90(int level, double h_offset, double v_offset);
List omega_180(int level, double h_offset, double v_offset);
List omega_270(int level, double h_offset, double v_offset);

List omega2_base();
List omega2(int level, double h_offset, double v_offset);
List omega2_90(int level, double h_offset, double v_offset);
List omega2_180(int level, double h_offset, double v_offset);
List omega2_270(int level, double h_offset, double v_offset);

List beta_base();
List beta(int level, double h_offset, double v_offset);
List beta_90(int level, double h_offset, double v_offset);
List beta_180(int level, double h_offset, double v_offset);
List beta_270(int level, double h_offset, double v_offset);

List beta2_base();
List beta2(int level, double h_offset, double v_offset);
List beta2_90(int level, double h_offset, double v_offset);
List beta2_180(int level, double h_offset, double v_offset);
List beta2_270(int level, double h_offset, double v_offset);

List alpha_base();
List alpha(int level, double h_offset, double v_offset);
List alpha_90(int level, double h_offset, double v_offset);
List alpha_180(int level, double h_offset, double v_offset);
List alpha_270(int level, double h_offset, double v_offset);

List alpha2_base();
List alpha2(int level, double h_offset, double v_offset);
List alpha2_90(int level, double h_offset, double v_offset);
List alpha2_180(int level, double h_offset, double v_offset);
List alpha2_270(int level, double h_offset, double v_offset);


List omega_base() {
	NumericVector x = NumericVector::create(0, 0, 1, 1);
	NumericVector y = NumericVector::create(0, 1, 1, 0);

	List pos = List::create(x, y);
	return pos;
}

List omega(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = omega_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = alpha2_180(next, h_offset, v_offset);
		List pos2 = beta_90(next, h_offset, v_offset + offset);
		List pos3 = alpha_270(next, h_offset + offset, v_offset + offset);
		List pos4 = beta2_180(next, h_offset + offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}

List omega_90_base() {
	NumericVector x = NumericVector::create(1, 0, 0, 1);
	NumericVector y = NumericVector::create(0, 0, 1, 1);

	List pos = List::create(x, y);
	return pos;
}

List omega_90(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = omega_90_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = alpha2_270(next, h_offset + offset, v_offset);
		List pos2 = beta_180(next, h_offset, v_offset);
		List pos3 = alpha(next, h_offset, v_offset + offset);
		List pos4 = beta2_270(next, h_offset + offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List omega_180_base() {
	NumericVector x = NumericVector::create(1, 1, 0, 0);
	NumericVector y = NumericVector::create(1, 0, 0, 1);

	List pos = List::create(x, y);
	return pos;
}

List omega_180(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = omega_180_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = alpha2(next, h_offset + offset, v_offset + offset);
		List pos2 = beta_270(next, h_offset + offset, v_offset);
		List pos3 = alpha_90(next, h_offset, v_offset);
		List pos4 = beta2(next, h_offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List omega_270_base() {
	NumericVector x = NumericVector::create(0, 1, 1, 0);
	NumericVector y = NumericVector::create(1, 1, 0, 0);

	List pos = List::create(x, y);
	return pos;
}

List omega_270(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = omega_270_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = alpha2_90(next, h_offset, v_offset + offset);
		List pos2 = beta(next, h_offset + offset, v_offset + offset);
		List pos3 = alpha_180(next, h_offset + offset, v_offset);
		List pos4 = beta2_90(next, h_offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List omega2_base() {
	NumericVector x = NumericVector::create(1, 1, 0, 0);
	NumericVector y = NumericVector::create(0, 1, 1, 0);

	List pos = List::create(x, y);
	return pos;
}

List omega2(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = omega2_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = beta_180(next, h_offset + offset, v_offset);
		List pos2 = alpha2_270(next, h_offset + offset, v_offset + offset);
		List pos3 = beta2_90(next, h_offset, v_offset + offset);
		List pos4 = alpha_180(next, h_offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List omega2_90_base() {
	NumericVector x = NumericVector::create(1, 0, 0, 1);
	NumericVector y = NumericVector::create(1, 1, 0, 0);

	List pos = List::create(x, y);
	return pos;
}

List omega2_90(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = omega2_90_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = beta_270(next, h_offset + offset, v_offset + offset);
		List pos2 = alpha2(next, h_offset, v_offset + offset);
		List pos3 = beta2_180(next, h_offset, v_offset);
		List pos4 = alpha_270(next, h_offset + offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List omega2_180_base() {
	NumericVector x = NumericVector::create(0, 0, 1 ,1);
	NumericVector y = NumericVector::create(1, 0, 0, 1);

	List pos = List::create(x, y);
	return pos;
}

List omega2_180(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = omega2_180_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = beta(next, h_offset, v_offset + offset);
		List pos2 = alpha2_90(next, h_offset, v_offset);
		List pos3 = beta2_270(next, h_offset + offset, v_offset);
		List pos4 = alpha(next, h_offset + offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List omega2_270_base() {
	NumericVector x = NumericVector::create(0, 1, 1, 0);
	NumericVector y = NumericVector::create(0, 0, 1, 1);

	List pos = List::create(x, y);
	return pos;
}

List omega2_270(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = omega2_270_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = beta_90(next, h_offset, v_offset);
		List pos2 = alpha2_180(next, h_offset + offset, v_offset);
		List pos3 = beta2(next, h_offset + offset, v_offset + offset);
		List pos4 = alpha_90(next, h_offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}



List beta_base() {
	NumericVector x = NumericVector::create(0, 0, 1, 1);
	NumericVector y = NumericVector::create(0, 1, 1, 0);

	List pos = List::create(x, y);
	return pos;
}

List beta(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = beta_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = alpha2_180(next, h_offset, v_offset);
		List pos2 = beta_90(next, h_offset, v_offset + offset);
		List pos3 = alpha_270(next, h_offset + offset, v_offset + offset);
		List pos4 = omega_270(next, h_offset + offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List beta_90_base() {
	NumericVector x = NumericVector::create(1, 0, 0, 1);
	NumericVector y = NumericVector::create(0, 0, 1, 1);

	List pos = List::create(x, y);
	return pos;
}

List beta_90(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = beta_90_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = alpha2_270(next, h_offset + offset, v_offset);
		List pos2 = beta_180(next, h_offset, v_offset);
		List pos3 = alpha(next, h_offset, v_offset + offset);
		List pos4 = omega(next, h_offset + offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}



List beta_180_base() {
	NumericVector x = NumericVector::create(1, 1, 0, 0);
	NumericVector y = NumericVector::create(1, 0, 0, 1);

	List pos = List::create(x, y);
	return pos;
}

List beta_180(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = beta_180_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = alpha2(next, h_offset + offset, v_offset + offset);
		List pos2 = beta_270(next, h_offset + offset, v_offset);
		List pos3 = alpha_90(next, h_offset, v_offset);
		List pos4 = omega_90(next, h_offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List beta_270_base() {
	NumericVector x = NumericVector::create(0, 1, 1, 0);
	NumericVector y = NumericVector::create(1, 1, 0, 0);

	List pos = List::create(x, y);
	return pos;
}

List beta_270(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = beta_270_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = alpha2_90(next, h_offset, v_offset + offset);
		List pos2 = beta(next, h_offset + offset, v_offset + offset);
		List pos3 = alpha_180(next, h_offset + offset, v_offset);
		List pos4 = omega_180(next, h_offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}



List beta2_base() {
	NumericVector x = NumericVector::create(1, 1, 0, 0);
	NumericVector y = NumericVector::create(0, 1, 1, 0);

	List pos = List::create(x, y);
	return pos;
}

List beta2(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = beta2_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = omega2_270(next, h_offset + offset, v_offset);
		List pos2 = alpha2_270(next, h_offset + offset, v_offset + offset);
		List pos3 = beta2_90(next, h_offset, v_offset + offset);
		List pos4 = alpha_180(next, h_offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List beta2_90_base() {
	NumericVector x = NumericVector::create(1, 0, 0, 1);
	NumericVector y = NumericVector::create(1, 1, 0, 0);

	List pos = List::create(x, y);
	return pos;
}

List beta2_90(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = beta2_90_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = omega2(next, h_offset + offset, v_offset + offset);
		List pos2 = alpha2(next, h_offset, v_offset + offset);
		List pos3 = beta2_180(next, h_offset, v_offset);
		List pos4 = alpha_270(next, h_offset + offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List beta2_180_base() {
	NumericVector x = NumericVector::create(0, 0, 1, 1);
	NumericVector y = NumericVector::create(1, 0, 0, 1);

	List pos = List::create(x, y);
	return pos;
}

List beta2_180(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = beta2_180_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = omega2_90(next, h_offset, v_offset + offset);
		List pos2 = alpha2_90(next, h_offset, v_offset);
		List pos3 = beta2_270(next, h_offset + offset, v_offset );
		List pos4 = alpha(next, h_offset + offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List beta2_270_base() {
	NumericVector x = NumericVector::create(0, 1, 1, 0);
	NumericVector y = NumericVector::create(0, 0, 1, 1);

	List pos = List::create(x, y);
	return pos;
}

List beta2_270(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = beta2_270_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = omega2_180(next, h_offset, v_offset);
		List pos2 = alpha2_180(next, h_offset + offset, v_offset);
		List pos3 = beta2(next, h_offset + offset, v_offset + offset);
		List pos4 = alpha_90(next, h_offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List alpha_base() {
	NumericVector x = NumericVector::create(0, 0, 1, 1);
	NumericVector y = NumericVector::create(0, 1, 1, 0);

	List pos = List::create(x, y);
	return pos;
}

List alpha(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = alpha_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = omega_90(next, h_offset, v_offset);
		List pos2 = beta_90(next, h_offset, v_offset + offset);
		List pos3 = alpha_270(next, h_offset + offset, v_offset + offset);
		List pos4 = beta2_180(next, h_offset + offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}



List alpha_90_base() {
	NumericVector x = NumericVector::create(1, 0, 0, 1);
	NumericVector y = NumericVector::create(0, 0, 1, 1);

	List pos = List::create(x, y);
	return pos;
}

List alpha_90(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = alpha_90_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = omega_180(next, h_offset + offset, v_offset);
		List pos2 = beta_180(next, h_offset, v_offset);
		List pos3 = alpha(next, h_offset, v_offset + offset);
		List pos4 = beta2_270(next, h_offset + offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}



List alpha_180_base() {
	NumericVector x = NumericVector::create(1, 1, 0, 0);
	NumericVector y = NumericVector::create(1, 0, 0, 1);

	List pos = List::create(x, y);
	return pos;
}

List alpha_180(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = alpha_180_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = omega_270(next, h_offset + offset, v_offset + offset);
		List pos2 = beta_270(next, h_offset + offset, v_offset);
		List pos3 = alpha_90(next, h_offset, v_offset);
		List pos4 = beta2(next, h_offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}



List alpha_270_base() {
	NumericVector x = NumericVector::create(0, 1, 1, 0);
	NumericVector y = NumericVector::create(1, 1, 0, 0);

	List pos = List::create(x, y);
	return pos;
}

List alpha_270(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = alpha_270_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = omega(next, h_offset, v_offset + offset);
		List pos2 = beta(next, h_offset + offset, v_offset + offset);
		List pos3 = alpha_180(next, h_offset + offset, v_offset);
		List pos4 = beta2_90(next, h_offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List alpha2_base() {
	NumericVector x = NumericVector::create(1, 1, 0, 0);
	NumericVector y = NumericVector::create(0, 1, 1, 0);

	List pos = List::create(x, y);
	return pos;
}

List alpha2(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = alpha2_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = beta_180(next, h_offset + offset, v_offset);
		List pos2 = alpha2_270(next, h_offset + offset, v_offset + offset);
		List pos3 = beta2_90(next, h_offset, v_offset + offset);
		List pos4 = omega2_90(next, h_offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List alpha2_90_base() {
	NumericVector x = NumericVector::create(1, 0, 0, 1);
	NumericVector y = NumericVector::create(1, 1, 0, 0);

	List pos = List::create(x, y);
	return pos;
}

List alpha2_90(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = alpha2_90_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = beta_270(next, h_offset + offset, v_offset + offset);
		List pos2 = alpha2(next, h_offset, v_offset + offset);
		List pos3 = beta2_180(next, h_offset, v_offset);
		List pos4 = omega2_180(next, h_offset + offset, v_offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List alpha2_180_base() {
	NumericVector x = NumericVector::create(0, 0, 1, 1);
	NumericVector y = NumericVector::create(1, 0, 0, 1);

	List pos = List::create(x, y);
	return pos;
}

List alpha2_180(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = alpha2_180_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = beta(next, h_offset, v_offset + offset);
		List pos2 = alpha2_90(next, h_offset, v_offset);
		List pos3 = beta2_270(next, h_offset + offset, v_offset);
		List pos4 = omega2_270(next, h_offset + offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}


List alpha2_270_base() {
	NumericVector x = NumericVector::create(0, 1, 1, 0);
	NumericVector y = NumericVector::create(0, 0, 1, 1);

	List pos = List::create(x, y);
	return pos;
}

List alpha2_270(int level, double h_offset = 0, double v_offset = 0) {
	if(level == 1) {
		List pos = alpha2_270_base();
		move(pos, h_offset, v_offset);
		return pos;

	} else {
		double offset = pow(2, level-1);
		int next = level - 1;
		List pos1 = beta_90(next, h_offset, v_offset);
		List pos2 = alpha2_180(next, h_offset + offset, v_offset);
		List pos3 = beta2(next, h_offset + offset, v_offset + offset);
		List pos4 = omega2(next, h_offset, v_offset + offset);

		List pos;
		pos = c_list(pos1, pos2, pos3, pos4);
		return pos;
	}
}



// [[Rcpp::export]]
List beta_omega_curve_cpp(int level, int type = 1) {
	int offset = pow(2, level-1);
	int next = level-1;

	List pos;
		
	if(type == 1 || type == 11) {

		List pos1 = beta2_270(next, 0, 0);
		List pos2 = alpha(next, 0, offset);
		List pos3 = beta(next, offset, offset);
		List pos4 = alpha2_90(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 12) {

		List pos1 = omega2_270(next, 0, 0);
		List pos2 = alpha(next, 0, offset);
		List pos3 = beta(next, offset, offset);
		List pos4 = omega2_90(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 13) {

		List pos1 = beta2_270(next, 0, 0);
		List pos2 = alpha(next, 0, offset);
		List pos3 = beta(next, offset, offset);
		List pos4 = omega2_90(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 14) {

		List pos1 = omega2_270(next, 0, 0);
		List pos2 = alpha(next, 0, offset);
		List pos3 = beta(next, offset, offset);
		List pos4 = alpha2_90(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 2 || type == 21) {

		List pos1 = alpha_90(next, 0, 0);
		List pos2 = beta_90(next, 0, offset);
		List pos3 = alpha_270(next, offset, offset);
		List pos4 = beta_270(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 22) {

		List pos1 = omega_90(next, 0, 0);
		List pos2 = beta_90(next, 0, offset);
		List pos3 = alpha_270(next, offset, offset);
		List pos4 = omega_270(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 23) {

		List pos1 = alpha_90(next, 0, 0);
		List pos2 = beta_90(next, 0, offset);
		List pos3 = alpha_270(next, offset, offset);
		List pos4 = omega_270(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 24) {

		List pos1 = omega_90(next, 0, 0);
		List pos2 = beta_90(next, 0, offset);
		List pos3 = alpha_270(next, offset, offset);
		List pos4 = beta_270(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 3) {

		List pos1 = beta_180(next, 0, 0);
		List pos2 = alpha(next, 0, offset);
		List pos3 = beta(next, offset, offset);
		List pos4 = alpha_180(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 4) {

		List pos1 = alpha2_180(next, 0, 0);
		List pos2 = beta_90(next, 0, offset);
		List pos3 = alpha_270(next, offset, offset);
		List pos4 = beta2_180(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 5 || type == 51) {

		List pos1 = beta2_270(next, 0, 0);
		List pos2 = alpha(next, 0, offset);
		List pos3 = beta(next, offset, offset);
		List pos4 = alpha_180(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 52) {

		List pos1 = omega2_270(next, 0, 0);
		List pos2 = alpha(next, 0, offset);
		List pos3 = beta(next, offset, offset);
		List pos4 = alpha_180(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 6 || type == 61) {

		List pos1 = alpha2_180(next, 0, 0);
		List pos2 = beta_90(next, 0, offset);
		List pos3 = alpha_270(next, offset, offset);
		List pos4 = beta_270(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	} else if(type == 62) {
		
		List pos1 = alpha2_180(next, 0, 0);
		List pos2 = beta_90(next, 0, offset);
		List pos3 = alpha_270(next, offset, offset);
		List pos4 = omega_270(next, offset, 0);

		pos = c_list(pos1, pos2, pos3, pos4);
	}

	return pos;

}

