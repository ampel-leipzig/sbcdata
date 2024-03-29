% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/counts.R
\name{count_cases}
\alias{count_cases}
\alias{count_cbc}
\title{Count Cases or Blood Counts}
\usage{
count_cases(x)

count_cbc(x, columns = c("HGB", "MCV", "PLT", "RBC", "WBC"), complete = FALSE)
}
\arguments{
\item{x}{\code{data.table}, in the format described in \code{\link{sbcdata}}.}

\item{columns}{\code{character}, name of columns from \code{\link{sbcdata}} that should
be treated as complete blood count (CBC).}

\item{complete}{\code{logical}, if \code{TRUE} the CBC is only count if none of the
\code{columns} is \code{NA}.}
}
\value{
\code{integer}, number of cases or blood counts
}
\description{
Function to count number of individual cases and blood counts.
}
\examples{
count_cases(sbcdata)
count_cbc(sbcdata)
}
\author{
Sebastian Gibb \href{mailto:mail@sebastiangibb.de}{mail@sebastiangibb.de}
}
