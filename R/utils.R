draw_multiple_curves = function(..., nrow = 1, ncol = NULL, extend = TRUE) {
	pl = list(...)

	n = length(pl)

	if(is.null(nrow) && is.null(ncol)) {
		nrow = ceiling(sqrt(n))
	}
	if(!is.null(nrow) && is.null(ncol)) {
		ncol = ceiling(n/nrow)
	} else if(is.null(nrow) && !is.null(ncol)) {
		nrow = ceiling(n/ncol)
	} else if(!is.null(nrow) && !is.null(ncol)) {
		ncol = ceiling(n/nrow)
	}

	gbl = lapply(pl, grob_sfc_sequence, extend = extend)

	plot_grid(plotlist = gbl, nrow = nrow, ncol = ncol)
}
