

setClass("sfc_sequence",
	slots = c("seq" = "factor",
		      "rot" = "integer",
		      "universe" = "character"))

setClass("sfc_seed",
	contains = "sfc_sequence")

setClass("sfc_unit",
	slots = list("corner" = "integer"),
	contains = "sfc_sequence")

setClass("sfc_rules",
    slots = c("rules" = "list",
    	      "flip" = "list",
              "universe" = "character",
              "name" = "character",
              "bases" = "list"))

setClass("sfc_nxn",
	slots = c("seed" = "sfc_seed",
	          "rules" = "sfc_rules",
		      "expansion" = "integer",
		      "level" = "integer",
		      "n" = "integer"),
	contains = "sfc_sequence")

setClass("sfc_hilbert",
	contains = "sfc_nxn",
	prototype = list(n = 2L))

setClass("sfc_peano",
	contains = "sfc_nxn",
	prototype = list(n = 3L))

setClass("sfc_meander",
	contains = "sfc_nxn",
	prototype = list(n = 3L))


setGeneric('sfc_expand', function(p, ...) standardGeneric('sfc_expand'))
setGeneric('sfc_expand_by_rules', function(p, seq, ...) standardGeneric('sfc_expand_by_rules'))
setGeneric('sfc_grob', function(p, ...) standardGeneric('sfc_grob'))
setGeneric('sfc_rotate', function(p, ...) standardGeneric('sfc_rotate'))
setGeneric('sfc_hflip', function(p, ...) standardGeneric('sfc_hflip'))
setGeneric('sfc_vflip', function(p, ...) standardGeneric('sfc_vflip'))
setGeneric('sfc_dflip', function(p, ...) standardGeneric('sfc_dflip'))
setGeneric('sfc_reverse', function(p, ...) standardGeneric('sfc_reverse'))
setGeneric('sfc_segments', function(p, ...) standardGeneric('sfc_segments'))
setGeneric('sfc_vaidate', function(p, ...) standardGeneric('sfc_vaidate'))
setGeneric('sfc_to_base', function(p, ...) standardGeneric('sfc_to_base'))
setGeneric('sfc_level', function(p, ...) standardGeneric('sfc_level'))
setGeneric('sfc_mode', function(p, ...) standardGeneric('sfc_mode'))
setGeneric('sfc_universe', function(p, ...) standardGeneric('sfc_universe'))
setGeneric('sfc_validate', function(p, ...) standardGeneric('sfc_validate'))
setGeneric('sfc_previous_point', function(p, ...) standardGeneric('sfc_previous_point'))
setGeneric('sfc_next_point', function(p, ...) standardGeneric('sfc_next_point'))
setGeneric('sfc_is_compatible', function(p, ...) standardGeneric('sfc_is_compatible'))
setGeneric('sfc_index', function(p, ...) standardGeneric('sfc_index'))
setGeneric('sfc_flip_unit', function(p, ...) standardGeneric('sfc_flip_unit'))
setGeneric('sfc_apply', function(p, ...) standardGeneric('sfc_apply'))
setGeneric('sfc_reduce', function(p, ...) standardGeneric('sfc_reduce'))
setGeneric('level1_unit_orientation', function(p, ...) standardGeneric('level1_unit_orientation'))
setGeneric('change_level1_unit_orientation', function(p, ...) standardGeneric('change_level1_unit_orientation'))

