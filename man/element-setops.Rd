% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ranges-setops.R
\name{\%union\%}
\alias{\%union\%}
\alias{\%intersect\%}
\alias{\%setdiff\%}
\alias{between}
\alias{span}
\title{Row-wise set operations on Ranges objects}
\usage{
x \%union\% y

x \%intersect\% y

x \%setdiff\% y

between(x, y)

span(x, y)
}
\arguments{
\item{x, y}{Ranges objects}
}
\value{
A Ranges object
}
\description{
Row-wise set operations on Ranges objects
}
\details{
Each of these functions acts on the rows between pairs of
Ranges object.
The function \code{\%union\%()}.
will return the entire range between two ranges objects assuming there
are no gaps, if you would like to force gaps use \code{\link[=span]{span()}} instead.
The function \code{\%intersect\%()} will create a new ranges object
with a hit column indicating whether or not the two ranges intersect.
The function \code{\%setdiff\%()}will return the ranges for each
row in x that are not in the corresponding row of y.
The function \code{\link[=between]{between()}} will return the gaps between
two ranges.
}
\examples{
x <- as_iranges(data.frame(start = 1:10, width = 5))
# stretch x by 3 on the right
y <- stretch(anchor_start(x), 3)
# take the rowwise union
x \%union\% y
# take the rowwise intersection
x \%intersect\% y
# asymetric difference
y \%setdiff\% x
x \%setdiff\% y
# if there are gaps between the rows of each range use span
y <- as_iranges(data.frame(start = c(20:15, 2:5),
width = c(10:15,1:4)))
# fill in the gaps and take the rowwise union
span(x,y)
# find the gaps
between(x,y)
}
\seealso{
[IRanges::punion()][IRanges::pintersect()][IRanges::pgap()][IRanges::psetdiff()]
}
