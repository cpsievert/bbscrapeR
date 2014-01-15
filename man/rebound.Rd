\name{rebound}
\alias{rebound}
\title{'Rebound' data from nba.com and wnba.com}
\usage{
rebound(first, last, codes, leagues = "nba", suffix = "shotchart_all.xml")
}
\arguments{
  \item{first}{String specifying the date "YYYYMMDD" to
  start obtaining game codes.}

  \item{last}{String specifying the date "YYYYMMDD" to stop
  obtaining game codes.}

  \item{codes}{Character vector of game codes with league
  ID as a prefix}

  \item{leagues}{Character vector with any combination of
  'nba', 'wnba', and/or 'd' (for D-League).}

  \item{suffix}{character vector with suffix of the XML
  files to be parsed. Currently supported options are:
  'boxscore.xml', 'pbp_all.xml', 'shotchart_all.xml'}
}
\description{
'Rebound' data from nba.com and wnba.com
}
\examples{
\dontrun{
#all available data between May 26th and June 1st of 2013
all.leagues <- c("nba", "wnba", "d")
files <- c("boxscore.xml", "pbp_all.xml", "shotchart_all.xml")
dat <- rebound(first="20130526", last="20130601",
               leagues = all.leagues, suffix = files)
}
}

