\name{rebound}
\alias{rebound}
\title{Obtain game codes needed to construct XML file names}
\usage{
rebound(init.date = "05/26/2013", id = "nba", offset = paste(0:10),
  suffix = "shotchart_all.xml")
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

  \item{suffix}{character vector with suffix of the XML
  files to be parsed. Currently supported options are:
  'boxscore.xml', 'pbp_all.xml', 'shotchart_all.xml'}
}
\description{
Obtain game codes needed to construct XML file names
}
\examples{
library(XML2R)
urls <- c("http://www.nba.com/games/game_component/dynamic/20130528/MIAIND/pbp_all.xml",
           "http://www.wnba.com/games/game_component/dynamic/20130527/CHIPHO/pbp_all.xml")
obs <- XML2Obs(urls, as.equiv=TRUE, xpath="//message")
obs <- add_key(obs, parent="message")
tables <- collapse(obs)
head(tables[["message//game//event"]], 10)


dat <- rebound()
}

