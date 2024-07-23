library(testthat)

test_that("test standard curves", {
	loc1 = hilbert_curve(2, "Cpp")
	loc2 = hilbert_curve(2, "R")
	expect_equal(loc1, loc2)

	loc1 = hilbert_curve(3, "Cpp")
	loc2 = hilbert_curve(3, "R")
	expect_equal(loc1, loc2)

	loc1 = hilbert_curve(4, "Cpp")
	loc2 = hilbert_curve(4, "R")
	expect_equal(loc1, loc2)

	loc1 = peano_curve(2, "Cpp")
	loc2 = peano_curve(2, "R")
	expect_equal(loc1, loc2)

	loc1 = peano_curve(3, "Cpp")
	loc2 = peano_curve(3, "R")
	expect_equal(loc1, loc2)

	loc1 = peano_curve(4, "Cpp")
	loc2 = peano_curve(4, "R")
	expect_equal(loc1, loc2)
})
