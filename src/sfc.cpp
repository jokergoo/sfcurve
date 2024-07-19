#include <Rcpp.h>
using namespace Rcpp;

#include "utils.h"

void fill_vector(IntegerVector vec, int start, IntegerVector v) {
	for(int i = 0; i < v.size(); i ++) {
		vec[start + i] = v[i];
	}
	return;
}

// [[Rcpp::export]]
List expand_by_rules_cpp(List rules, IntegerVector letters, IntegerVector code) {
	int n = letters.size();

	List lt1 = rules[0];
	List lt2 = lt1[0];
	IntegerVector v = lt2[0];
	int k = v.size();

	int n_code = code.size();

	IntegerVector seq(n*k);
	IntegerVector rot(n*k);

	for(int i = 0; i < n; i ++) {
		lt1 = rules[ letters[i]-1 ];
		if(n_code == 1) {
			lt2 = lt1[ code[0]-1 ];
		} else {
			lt2 = lt1[ code[i]-1 ];
		}
		fill_vector(seq, k*i, lt2[0]);
		fill_vector(rot, k*i, lt2[1]);
	}

	List lt = List::create(seq, rot);
	return lt;
}

// [[Rcpp::export]]
List expand_by_rules_2_cpp(List rules, List flip, IntegerVector letters, IntegerVector code, LogicalVector l_flip) {
	int n = letters.size();

	List lt1 = rules[0];
	List lt2 = lt1[0];
	IntegerVector v = lt2[0];
	int k = v.size();

	int n_code = code.size();

	IntegerVector seq(n*k);
	IntegerVector rot(n*k);

	for(int i = 0; i < n; i ++) {
		if(l_flip[i]) {
			lt1 = flip[ letters[i]-1 ];
		} else {
			lt1 = rules[ letters[i]-1 ];
		}

		if(n_code == 1) {
			lt2 = lt1[ code[0]-1 ];
		} else {
			lt2 = lt1[ code[i]-1 ];
		}
		fill_vector(seq, k*i, lt2[0]);
		fill_vector(rot, k*i, lt2[1]);
	}

	List lt = List::create(seq, rot);
	return lt;
}

void rotate_coord(NumericVector p, double theta) {

	double x = p[0];
	double y = p[1];

	theta = theta/180*3.14159265358979323846;

	p[0] = x*cos(theta) - y*sin(theta);
	p[1] = x*sin(theta) + y*cos(theta);

	return;
}


void move_coord(NumericVector p, double x_offset, double y_offset) {
	
	p[0] = p[0] + x_offset;
	p[1] = p[1] + y_offset;
	
	return ;
}

NumericVector sfc_next_point(S4 base, double x, double y, double rot, double length = 1) {

	NumericVector current = base.slot("current");
	double out_direction = base.slot("out_direction");

	out_direction = out_direction/180*3.14159265358979323846;
	NumericVector succeeding(2);
	succeeding[0] = current[0] + length*cos(out_direction);
	succeeding[1] = current[1] + length*sin(out_direction);

	rotate_coord(succeeding, rot);
	move_coord(succeeding, x, y);

	return succeeding;
}

// [[Rcpp::export]]
NumericMatrix sfc_segments_cpp(IntegerVector seq, NumericVector rot, List bases, NumericVector start) {
	int n = seq.size();

	NumericMatrix pos(n, 2);

	pos(0, 0) = start[0];
	pos(0, 1) = start[1];

	for(int i = 1; i < n; i ++) {
		S4 b = bases[ seq[i-1]-1 ];
		NumericVector current = sfc_next_point(b, pos(i-1, 0), pos(i-1, 1), rot[i-1]);
		pos(i, 0) = current[0];
		pos(i, 1) = current[1];
	}

	return pos;

}
