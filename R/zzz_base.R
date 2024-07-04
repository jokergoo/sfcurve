
BASE_PATTERN_LEVELS = c("I", "R", "L", "U", "C", "B", "D", "P", "Q")


BASE_PATTERN_SET_2X2 = list()
BASE_PATTERN_SET_2X2[["I"]][[1]] = sfc_initial(c("R", "L", "L", "R"), rot = c(0, 90, 0, 270))
BASE_PATTERN_SET_2X2[["I"]][[2]] = sfc_initial(c("L", "R", "R", "L"), rot = c(0, 270, 0, 90))
BASE_PATTERN_SET_2X2[["R"]][[1]] = sfc_initial(c("I", "R", "R", "L"), rot = c(0, 0, 90, 180))
BASE_PATTERN_SET_2X2[["R"]][[2]] = sfc_initial(c("L", "R", "R", "I"), rot = c(0, 270, 0, 90))
BASE_PATTERN_SET_2X2[["L"]][[1]] = sfc_initial(c("R", "L", "L", "I"), rot = c(0, 90, 0, 270))
BASE_PATTERN_SET_2X2[["L"]][[2]] = sfc_initial(c("I", "L", "L", "R"), rot = c(0, 0, 270, 180))
BASE_PATTERN_SET_2X2[["U"]][[1]] = sfc_initial(c("I", "R", "R", "I"), rot = 0)
BASE_PATTERN_SET_2X2[["U"]][[2]] = sfc_initial(c("I", "L", "L", "I"), rot = 0)
BASE_PATTERN_SET_2X2[["C"]][[1]] = sfc_initial(c("L", "L", "L", "L"), rot = c(0, 270, 180, 90))
BASE_PATTERN_SET_2X2[["C"]][[2]] = sfc_initial(c("R", "R", "R", "R"), rot = c(0, 90, 180, 270))
BASE_PATTERN_SET_2X2[["B"]][[1]] = sfc_initial(c("R", "R", "R", "I"), rot = c(90, 180, 270, 0))
BASE_PATTERN_SET_2X2[["B"]][[2]] = sfc_initial(c("L", "L", "L", "R"), rot = c(180, 90, 0, 270))
BASE_PATTERN_SET_2X2[["D"]][[1]] = sfc_initial(c("L", "L", "L", "I"), rot = c(270, 180, 90, 0))
BASE_PATTERN_SET_2X2[["D"]][[2]] = sfc_initial(c("R", "R", "R", "L"), rot = c(180, 270, 0, 90))
BASE_PATTERN_SET_2X2[["P"]][[1]] = sfc_initial(c("I", "R", "R", "R"), rot = c(0, 0, 90, 180))
BASE_PATTERN_SET_2X2[["P"]][[2]] = sfc_initial(c("R", "L", "L", "L"), rot = c(0, 90, 0, 270))
BASE_PATTERN_SET_2X2[["Q"]][[1]] = sfc_initial(c("I", "L", "L", "L"), rot = c(0, 0, 270, 180))
BASE_PATTERN_SET_2X2[["Q"]][[2]] = sfc_initial(c("L", "R", "R", "R"), rot = c(0, 270, 0, 90))

BASE_PATTERN_SET_2X2 = BASE_PATTERN_SET_2X2[BASE_PATTERN_LEVELS]

class(BASE_PATTERN_SET_2X2) = "sfc_base_pattern_set"

print.sfc_basic_pattern_set = function(x, ...) {
	if(nchar(x[[1]][[1]]$seq) == 4) {
		dim = 2
	} else if (nchar(x[[1]][[1]]$seq) == 9) {
		dim = 3
	}
	cat("Basic patterns for 1x1 -> ", dim, "x", dim, ":\n", sep = "")
	for(nm in names(x)) {
		for(i in seq_along(x[[nm]])) {
			cat(nm, ": ", sep = "")
			cat(nm, i, " = ", paste(x[[nm]][[1]]$seq, "(", x[[nm]][[1]]$rot, ")", sep = "", collapse = ""), "\n", sep = "")
		}
	}
}

BASE_PATTERN_SET_3x3 = list()
BASE_PATTERN_SET_3x3[["I"]][["p0v"]] = sfc_initial("IILLIRRII", rot = 0)
BASE_PATTERN_SET_3x3[["I"]][["p0h"]] = sfc_initial("RILLIRRIL", rot = 0)
BASE_PATTERN_SET_3x3[["I"]][["p1"]] = sfc_initial("IRRLLILIR", rot = 0)
BASE_PATTERN_SET_3x3[["I"]][["p2"]] = sfc_initial("RILILLRRI", rot = 0)
BASE_PATTERN_SET_3x3[["I"]][["p3h"]] = sfc_initial("LIRRILLIR", rot = 0)
BASE_PATTERN_SET_3x3[["I"]][["p3v"]] = sfc_initial("IILLIRRII", rot = 0)

BASE_PATTERN_SET_3x3[["R"]][["p0v"]] = sfc_initial("IIRRILLIR", rot = 0)
BASE_PATTERN_SET_3x3[["R"]][["p0h"]] = sfc_initial("RILLIRRII", rot = 0)
BASE_PATTERN_SET_3x3[["R"]][["p1"]] = sfc_initial("IIRIRRLLI", rot = 0)
BASE_PATTERN_SET_3x3[["R"]][["p2"]] = sfc_initial("LIRIRRLLR", rot = 0)

BASE_PATTERN_SET_3x3[["L"]][["p0v"]] = hflip(BASE_PATTERN_SET_3x3[["R"]][["p0v"]])
BASE_PATTERN_SET_3x3[["L"]][["p0h"]] = hflip(BASE_PATTERN_SET_3x3[["R"]][["p0h"]])
BASE_PATTERN_SET_3x3[["L"]][["p2"]] = hflip(BASE_PATTERN_SET_3x3[["R"]][["p1"]])
BASE_PATTERN_SET_3x3[["L"]][["p1"]] = hflip(BASE_PATTERN_SET_3x3[["R"]][["p2"]])

BASE_PATTERN_SET_3x3[["U"]][["p1"]] = sfc_initial("IIRIRRLLR", rot = 180)
BASE_PATTERN_SET_3x3[["U"]][["p2"]] = sfc_initial("RLLRRIRII", rot = 180)
BASE_PATTERN_SET_3x3[["U"]][["p3"]] = sfc_initial("LRRLLILII", rot = 180)
BASE_PATTERN_SET_3x3[["U"]][["p4"]] = sfc_initial("IILILLRRL", rot = 180)

BASE_PATTERN_SET_3x3[["C"]][[1]] = sfc_initial("RRRIRIRIR", rot = 180)
BASE_PATTERN_SET_3x3[["C"]][[2]] = sfc_initial("IRRIRIRIR", rot = 270)
BASE_PATTERN_SET_3x3[["C"]][[3]] = sfc_initial("LRRIRIRIR", rot = 0)
BASE_PATTERN_SET_3x3[["C"]][[4]] = sfc_initial("LILILILLL", rot = 180)
BASE_PATTERN_SET_3x3[["C"]][[5]] = sfc_initial("LILILILLI", rot = 90)
BASE_PATTERN_SET_3x3[["C"]][[6]] = sfc_initial("LILILILLR", rot = 0)
BASE_PATTERN_SET_3x3[["C"]][[7]] = hflip(BASE_PATTERN_SET_3x3[["C"]][[1]])
BASE_PATTERN_SET_3x3[["C"]][[8]] = hflip(BASE_PATTERN_SET_3x3[["C"]][[2]])
BASE_PATTERN_SET_3x3[["C"]][[9]] = hflip(BASE_PATTERN_SET_3x3[["C"]][[3]])
BASE_PATTERN_SET_3x3[["C"]][[10]] = hflip(BASE_PATTERN_SET_3x3[["C"]][[4]])
BASE_PATTERN_SET_3x3[["C"]][[11]] = hflip(BASE_PATTERN_SET_3x3[["C"]][[5]])
BASE_PATTERN_SET_3x3[["C"]][[12]] = hflip(BASE_PATTERN_SET_3x3[["C"]][[6]])


get_base_pattern_set = function(k) {
	if(k == 2) {
		BASE_PATTERN_SET_2X2
	} else if(k == 3) {
		BASE_PATTERN_SET_3x3
	}
}
