\name{getCodes}
\alias{getCodes}
\title{Obtain game codes needed to construct XML file names}
\usage{
getCodes(init.date = "05/26/2013", id = "00", offset = paste(0:10))
}
\arguments{
  \item{init.date}{String specifying the date (MM/DD/YYYY)
  to start obtaining game codes.}

  \item{id}{Character vector with any combination of '00',
  '10', and/or '20'. These codes stand for NBA, WNBA and
  D-League, respectively.}

  \item{offset}{Character vector of non-negative integers.
  This will control how many days to go beyond the
  \code{init.date}}
}
\description{
Obtain game codes needed to construct XML file names
}
\examples{
\dontrun{
codes <- getCodes()
codes <- getCodes(id=c("00", "10"))
}
}

