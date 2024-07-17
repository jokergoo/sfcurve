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


