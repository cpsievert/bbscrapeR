\name{rebound}
\alias{rebound}
\title{'Rebound' data from nba.com and wnba.com}
\usage{
rebound(first, last, codes, leagues = "nba", suffix = "shotchart_all.xml",
  connect)
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

  \item{connect}{A database connection object. The class of
  the object should be "MySQLConnection" or
  "SQLiteConnection". If a valid connection is supplied,
  tables will be copied to the database, which will result
  in better memory management. If a connection is supplied,
  but the connection fails for some reason, csv files will
  be written to the working directory.}
}
\value{
Returns a list of data frames (or nothing if writing to a
database).
}
\description{
Obtain (and possibly store in a remote database) data from
nba.com and wnba.com. Currently there are support for three
file types. Here are some examples:
\href{http://www.nba.com/games/game_component/dynamic/20130526/MIAIND/shotchart_all.xml}{shotchart_all.xml}
\href{http://www.nba.com/games/game_component/dynamic/20130528/MIAIND/pbp_all.xml}{pbp_all.xml}
\href{http://www.nba.com/games/game_component/dynamic/20130526/MIAIND/boxscore.xml}{boxscore.xml}
}
\examples{
\dontrun{
#collect and store available data between May 26th and June 1st of 2013
library(dplyr)
db <- src_sqlite("bball.sqlite3") #creates database in current directory
all.leagues <- c("nba", "wnba", "d")
files <- c("boxscore.xml", "pbp_all.xml", "shotchart_all.xml")
dat <- rebound(first="20130526", last="20130601",
               leagues = all.leagues, suffix = files, connect=my$con)
}
}
\seealso{
If you want to add support for more file types, the
\code{XML2R} package is a good place to start.
}

