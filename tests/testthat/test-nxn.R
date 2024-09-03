
library(testthat)

guess_unit_corner = sfcurve:::guess_unit_corner


.rotate = function(x, rot = 0, base = c("bottomleft", "bottomright", "topright", "topleft")) {
	i = which(base == x)
	j = get_circular_index(i+rot/90, 4)
	base[j]
}


test_that("test guess_unit_corner on SFC_RULES_2x2", {
	
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$I[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$I[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$I[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$I[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$R[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$R[[1]], rot), which = "last"), .rotate("bottomright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$R[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$R[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$L[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$L[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$L[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$L[[2]], rot), which = "last"), .rotate("bottomleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$U[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$U[[1]], rot), which = "last"), .rotate("bottomright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$U[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$U[[2]], rot), which = "last"), .rotate("bottomleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$B[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$B[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$B[[2]], rot), which = "first"), .rotate("topright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$B[[2]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$D[[1]], rot), which = "first"), .rotate("topleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$D[[1]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$D[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$D[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$P[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$P[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$P[[2]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$P[[2]], rot), which = "last"), .rotate("bottomright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$Q[[1]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$Q[[1]], rot), which = "last"), .rotate("bottomleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$Q[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$Q[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$C[[1]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$C[[1]], rot), which = "last"), .rotate("bottomleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$C[[2]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_2x2@rules$C[[2]], rot), which = "last"), .rotate("bottomright", rot))
	}
})

test_that("test guess_unit_corner on SFC_RULES_3x3_PEANO", {
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@rules$I[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@rules$I[[1]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@rules$J[[1]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@rules$J[[1]], rot), which = "last"), .rotate("topleft", rot))
	}
	
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@rules$R[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@rules$R[[1]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@rules$L[[1]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@rules$L[[1]], rot), which = "last"), .rotate("topleft", rot))
	}
})


test_that("test guess_unit_corner on SFC_RULES_3x3_PEANO, flip", {
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@flip$I[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@flip$I[[1]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@flip$J[[1]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@flip$J[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@flip$R[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@flip$R[[1]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@flip$L[[1]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_PEANO@flip$L[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

})


test_that("test guess_unit_corner on SFC_RULES_3x3_MEANDER", {
	
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$I[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$I[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$I[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$I[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$R[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$R[[1]], rot), which = "last"), .rotate("bottomright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$R[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$R[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$L[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$L[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$L[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$L[[2]], rot), which = "last"), .rotate("bottomleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$U[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$U[[1]], rot), which = "last"), .rotate("bottomright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$U[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$U[[2]], rot), which = "last"), .rotate("bottomleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$B[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$B[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$B[[2]], rot), which = "first"), .rotate("topright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$B[[2]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$D[[1]], rot), which = "first"), .rotate("topleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$D[[1]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$D[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$D[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$P[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$P[[1]], rot), which = "last"), .rotate("bottomright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$P[[2]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$P[[2]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$Q[[1]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$Q[[1]], rot), which = "last"), .rotate("bottomleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$Q[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$Q[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$C[[1]], rot), which = "first"), .rotate("topright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$C[[1]], rot), which = "last"), .rotate("bottomright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$C[[2]], rot), which = "first"), .rotate("topleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@rules$C[[2]], rot), which = "last"), .rotate("bottomleft", rot))
	}
})


test_that("test guess_unit_corner on SFC_RULES_3x3_MEANDER, flip", {
	
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$I[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$I[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$I[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$I[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$R[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$R[[1]], rot), which = "last"), .rotate("bottomright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$R[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$R[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$L[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$L[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$L[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$L[[2]], rot), which = "last"), .rotate("bottomleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$U[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$U[[1]], rot), which = "last"), .rotate("bottomright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$U[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$U[[2]], rot), which = "last"), .rotate("bottomleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$B[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$B[[1]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$B[[2]], rot), which = "first"), .rotate("topright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$B[[2]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$D[[1]], rot), which = "first"), .rotate("topleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$D[[1]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$D[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$D[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$P[[1]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$P[[1]], rot), which = "last"), .rotate("bottomright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$P[[2]], rot), which = "first"), .rotate("bottomleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$P[[2]], rot), which = "last"), .rotate("topleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$Q[[1]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$Q[[1]], rot), which = "last"), .rotate("bottomleft", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$Q[[2]], rot), which = "first"), .rotate("bottomright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$Q[[2]], rot), which = "last"), .rotate("topright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$C[[1]], rot), which = "first"), .rotate("topright", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$C[[1]], rot), which = "last"), .rotate("bottomright", rot))
	}

	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$C[[2]], rot), which = "first"), .rotate("topleft", rot))
	}
	for(rot in c(0, 90, 180, 270)) {
		expect_equal(guess_unit_corner(sfc_rotate(SFC_RULES_3x3_MEANDER@flip$C[[2]], rot), which = "last"), .rotate("bottomleft", rot))
	}
})
