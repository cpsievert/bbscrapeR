#' Acquire player tracking data from http://stats.nba.com/
#' 
#' All the arguments to this function 
#' 
#' @param type Required type of tracking data. Eligible values are 'shot', 'rebound',
#' 'pass', 'shotdefend', 'reb', 'shots'. The most useful/interesting are 'shot' and 'rebound'.
#' @param PlayerID Required player ID. See \code{data(players)}
#' @param DateFrom Optionally set date to begin acquiring data (in the format 'YYYY/MM/DD')
#' @param DateTo Optionally set date to stop acquiring data (in the format 'YYYY/MM/DD')
#' @param GameSegment Optionally filter by 'First+Half', Second+Half', or 'Overtime'.
#' @param LastNGames Optionally filter by the last 'n' number of games.
#' @param LeagueID Character vector with any combination of '00', '10', and/or '20'. 
#' These codes stand for NBA, WNBA and D-League, respectively.
#' @param Location Optionally filter by 'Home' or 'Road' games?
#' @param Month Optionally filter by Month (use '1' for Oct, '2' for Nov, etc.)
#' @param OpponentTeamID Optionally filter by opposing team
#' @param Outcome Optionally filter by wins (use 'W')  or losses (use 'L')
#' @param Period Optionally filter by quarter/OT (use '1' for 1st Quarter, '2' for 2nd, etc.)
#' @param Season Required filter on season year
#' @param SeasonSegment Optionally filter by 'Pre All-Star' or 'Post All-Star'
#' @param SeasonType Required filter on either 'Regular Season' or 'Playoffs'
#' @param TeamID
#' @param VsConference
#' @param VsDivision
#' @param PerGame Aggregate 'PerGame' or 'Totals'. Only relevant for "dashboards"
#' 
#' @export
#' @examples
#' 
#' \dontrun{
#' # Defaults to Lebron's shots from the 2013-14 regular season
#' shots <- acquire()
#' # Lebron's rebounds from the 2013-14 regular season
#' rebounds <- acquire("rebound")
#' # The rest of these data types are "dashboards"
#' # That is, they show summary statistics that will change over 
#' # the season. For this reason, you probably don't want to
#' # store these in a database.
#' pass_dash <- acquire("pass")
#' defense_dash <- acquire("shotdefend")
#' reb_dash <- acquire("reb")
#' shot_dash <- acquire("shots")
#' 
#' # All the shots for the 2013-14 season!
#' ids <- players[, 'PlayerID']
#' lshots <- lapply(ids, function(x) acquire(PlayerID = x))
#' allshots <- do.call("rbind", lshots)
#' 
#' # All the rebounds for the 2013-14 season!
#' lrebounds <- lapply(ids, function(x) 
#'                  acquire(type = "rebound", PlayerID = x))
#' allrebs <- do.call("rbind", lrebounds)
#' }
#' 

# NOTE TO SELF:
# I determined these 'response parameters' from navigating to 
# http://stats.nba.com/player/#!/2544/tracking/shotslogs/
# From here, click on the wheel, change some parameters, then click 
# "Run it!". Next, open Firebug, then look at the GET response urls.
# The last one should point to a relevant json file.

acquire <- function(type = "shot", PlayerID = "2544", DateFrom = "", DateTo = "",
                    GameSegment = "", LastNGames = "0", LeagueID = "00", Location = "", 
                    Month = "0", OpponentTeamID = "0", Outcome = "", Period = "0", 
                    Season = "2013-14", SeasonSegment = "", SeasonType = "Regular+Season", 
                    TeamID = "0", VsConference = "", VsDivision = "", PerMode = "PerGame") {
  log <- ""
  # 'shot' and 'rebound' are "logs" meaning that new data is appended.
  # the other types are "dashboards" meaning that they are changed in place.
  if (type == "shot") log <- "log"
  if (type == "rebound") log <- "logs" #yay for consistency
  root <- paste0("http://stats.nba.com/stats/playerdashpt", type, log, "?")
  st <-paste0(root, "DateFrom=", DateFrom, "&DateTo=", DateTo, "&GameSegment=",
         GameSegment, "&LastNGames=", LastNGames, "&LeagueID=", LeagueID, "&Location=",
         Location, "&Month=", Month, "&OpponentTeamID=", OpponentTeamID, "&Outcome=",
         Outcome, "&Period=", Period, "&PlayerID=", PlayerID, "&Season=", Season,
         "&SeasonSegment=", SeasonSegment, "&SeasonType=", SeasonType, "&TeamID=",
         TeamID, "&VsConference=", VsConference, "&VsDivision=", VsDivision,
         "&PerMode=", PerMode)
  json <- fromJSON(st)
  # add headers to (possibly a list) of tables
  # note that a row of missing values is returned
  # if no data is returned
  tab <- mapply(function(x, y) { 
            if (!length(x)) 
              x <- matrix(rep(NA, length(y)), nrow = 1)
            `colnames<-`(x, y)
          }, 
    json$resultSets$rowSet, json$resultSets$headers, 
    SIMPLIFY = FALSE)
  # append a new column with the 'name' of the table
  tab <- mapply(function(x, y) cbind(x, TABLE_NAME = y), 
                 tab, as.list(json$resultSets$name), SIMPLIFY = FALSE)
  # combine the tables into one big table
  tab <- plyr::rbind.fill.matrix(tab)
  # Add the player ID?
  tab <- cbind(tab, PlayerID = PlayerID)
  tab
}
