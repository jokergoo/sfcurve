% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfc_index.R
\name{[.sfc_nxn}
\alias{[.sfc_nxn}
\alias{sfc_index,sfc_nxn-method}
\alias{sfc_index}
\alias{test_sfc_index}
\title{Subunit in the curve}
\usage{
\method{[}{sfc_nxn}(x, i, j, ..., drop = TRUE)

\S4method{sfc_index}{sfc_nxn}(p, index = "")

test_sfc_index(p, index)
}
\arguments{
\item{x}{An \code{sfc_nxn} object.}

\item{i, index}{A string of digits representing the path on the hierarchy of the curve. The left side
corresponds to the top level and the right side corresponds to the bottom level on the curve. For the
2x2 curve, the digits can only be 1-4, and for the Peano and Meander curves, the digites can be 1-9.
The hierarchical index should be specified in a format of \code{i1:i2:i3:...}
where \code{:} can be replaced by any non-number character. For 2x2 and 3x3 curves, \code{:} can be omitted and the
hierarchical index can be specified as \code{i1i2i3...}. See the \strong{Examples} section.}

\item{j}{A value of \code{TRUE} or \code{FALSE} that controls whether to keep the \code{sfc_nxn} class or degenerate to the \code{sfc_sequence} class.}

\item{...}{Ignore.}

\item{drop}{A value of \code{TRUE} or \code{FALSE} that controls whether to keep the \code{sfc_nxn} class or degenerate to the \code{sfc_sequence} class.}

\item{p}{An \code{sfc_nxn} object.}
}
\value{
\code{sfc_index()} returns an integer vector.
}
\description{
Subunit in the curve
}
\details{
\code{sfc_index()} only works on square curves (i.e. a curve with a single base letter as seed.)

\code{test_sfc_index()} is a helper function for demonstrating \code{sfc_index()}.
}
\examples{
p = sfc_2x2("I", "11111")
p["3:2:1"]
# for 2x2 and 3x3 curves, ":" can be omitted
p["321"]
p["3:2:1", TRUE] # or p["3:2:1", drop = FALSE]
# only for testing
p = sfc_2x2("I", "11111")
om = par(no.readonly = TRUE)
par(mfrow = c(2, 2))
test_sfc_index(p, "3")
test_sfc_index(p, "3:2")
test_sfc_index(p, "3:2:1")
test_sfc_index(p, "3:2:1:1")
par(om)

p = sfc_3x3_meander("I", "11111")
om = par(no.readonly = TRUE)
par(mfrow = c(2, 2))
test_sfc_index(p, "7")
test_sfc_index(p, "7:5")
test_sfc_index(p, "7:5:9")
test_sfc_index(p, "7:5:9:2")
par(om)
}
