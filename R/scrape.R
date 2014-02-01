#' 'Rebound' data from nba.com and wnba.com
#'
#'
#' @param first String specifying the date "YYYYMMDD" to start obtaining game codes.
#' @param last String specifying the date "YYYYMMDD" to stop obtaining game codes.
#' @param codes Character vector of game codes with league ID as a prefix
#' @param leagues Character vector with any combination of 'nba', 'wnba', and/or 'd' (for D-League).
#' @param suffix character vector with suffix of the XML files to be parsed. Currently supported options are: 'boxscore.xml', 'pbp_all.xml', 'shotchart_all.xml'
#' @import XML
#' @import XML2R
#' @export
#' @examples
#' 
#' \dontrun{
#' #all available data between May 26th and June 1st of 2013
#' all.leagues <- c("nba", "wnba", "d")
#' files <- c("boxscore.xml", "pbp_all.xml", "shotchart_all.xml")
#' dat <- rebound(first="20130526", last="20130601",
#'                leagues = all.leagues, suffix = files)
#'}



rebound <- function(first, last, codes, leagues = "nba", suffix="shotchart_all.xml") {
  #translate leagues to '00' for 'nba', '10' for 'wnba', and '20' for 'd'
  tmp <- sub("wnba", "10", leagues)
  tmp <- sub("nba", "00", tmp)
  leaguez <- sub("d", "20", tmp)
  if (missing(codes)) {
    scrape.env <- environment()
    data(codes, package="bbscrapeR", envir=scrape.env)
    ids <- substr(codes, 0, 2)
    codes2 <- codes[ids %in% leaguez]            #keep only codes that match league(s) of interest
    codez <- sub("^[0-9]{2}/", "", codes2)        #get rid of league code
    #datez <- sub("/[A-Z]+", "", codez)          #not needed as as.POSIXct will ignore text
    datez <- as.POSIXct(codez, format="%Y %m %d")
    firzt <- as.POSIXct(first, format="%Y %m %d")
    lazt <- as.POSIXct(last, format="%Y %m %d")
    codex <- codez[firzt <= datez & datez <= lazt]
  } else {
    message("Since codes were supplied, the 'first', 'last', and 'leagues' option will be ignored.")
    codex <- codes
  }
  ids <- substr(codex, 0, 2)
  game.codes <- sub("^[0-9]{2}/", "", codex)
  if (any(!ids %in% c("00", "10", "20"))) warning("The first two characters in each element of codes should be either '00', '10' or '20'.")
  NBAprefix <- game.codes[ids == "00"]
  if (length(NBAprefix) > 0) NBAprefix <- paste0("http://www.nba.com/games/game_component/dynamic/", NBAprefix)
  WNBAprefix <- game.codes[ids == "10"]
  if (length(WNBAprefix) > 0) WNBAprefix <- paste0("http://www.wnba.com/games/game_component/dynamic/", WNBAprefix)
  Dprefix <- game.codes[ids == "20"]
  if (length(Dprefix) > 0) Dprefix <- paste0("http://www.nba.com/games/game_component/dynamic/", Dprefix)
  prefixs <- c(NBAprefix, WNBAprefix, Dprefix)

  #check for valid inputs
  valid.suffix <- c("boxscore.xml", "pbp_all.xml", "shotchart_all.xml")
  if (!all(suffix %in% valid.suffix)) {
    warning("Currently supported file suffix are: 'boxscore.xml', 'pbp_all.xml', and 'shotchart_all.xml'")
    Sys.sleep(5) #time to read warning
  }
  
  if (any(grepl("shotchart_all.xml", suffix))) {
    files <- paste0(prefixs, "/shotchart_all.xml")
    obs <- XML2Obs(files, as.equiv=TRUE)
    #as with "pbp_all.xml" files, there is no need to add_key since 'message' & 'message//game' observations occur once per file
    tables <- setNames(collapse_obs(obs), paste0("shotchart//", c("message", "game", "event")))
    tables[["shotchart//event"]] <- format.shotchart.event(tables[["shotchart//event"]])
  }
  
  if (any(grepl("boxscore.xml", suffix))) {
    files <- paste0(prefixs, "/boxscore.xml")
    obs <- XML2Obs(files, as.equiv=TRUE)
    obs <- add_key(obs, parent="message//game//htm", recycle="tcd", quiet=TRUE)
    obs <- add_key(obs, parent="message//game//vtm", recycle="tcd", quiet=TRUE)
    obs <- re_name(obs, equiv = c("message//game//htm", "message//game//vtm"),
                   rename.as = "teams", diff.name="home_away", quiet=TRUE)
    obs <- re_name(obs, equiv = c("message//game//htm//pl", "message//game//vtm//pl"),
                    rename.as = "players", diff.name="home_away", quiet=TRUE)
    table.names <- paste0("boxscore//", c("message", "game", "officials", "players", "teams"))
    if (exists("tables")){
      tables <- c(tables, setNames(collapse_obs(obs), table.names))
    } else {
      tables <- setNames(collapse_obs(obs), table.names)
    }
  }

  if (any(grepl("pbp_all.xml", suffix))) {
    files <- paste0(prefixs, "/pbp_all.xml")
    #nba files are corrupt! Need a urlsToDocs() work-around
    female <- grepl("wnba", files)
    wnba.files <- files[female]
    nba.files <- files[!female]
    wnba.docs <- urlsToDocs(wnba.files)
    nba.docs <- pbpToDocs(nba.files)
    docs <- c(nba.docs, wnba.docs)
    #This is essentially copy-pasted from the guts of XML2Obs
    valid.urls <- sapply(docs, function(x) attr(x, "XMLsource"))
    nodes <- docsToNodes(docs, xpath="/") 
    rm(docs)
    l <- nodesToList(nodes)
    rm(nodes)
    obs <- listsToObs(l, urls=valid.urls, as.equiv=TRUE, url.map=FALSE)
    #message node is missing from names in nba obs...sigh
    #Also, note that there is only one message and one message//game observation per file (url can serve as key)
    nms <- names(obs) 
    nms[nms %in% "game//event"] <- "pbp//event"
    nms[nms %in% "game"] <- "pbp//game"
    nms[nms %in% "attrs"] <- "pbp//message"
    obs <- setNames(obs, nms)
    if (exists("tables")){
      tables <- c(tables, collapse_obs(obs))
    } else {
      tables <- collapse_obs(obs)
    }
    #transform descriptions in the XML value into nicely formatted variables
    desc <- tables[["pbp//event"]][,"XML_value"]
    #strip game clock (this is recorded in game_clock anyway)
    desc <- sub("^\\([0-9]{2}:[0-9]{2}\\)", "", desc)
    #some game clocks go to the tenth (or maybe hundreth?) of a second
    desc <- sub("^\\([0-9]{2}:[0-9]{2}\\.[0-9]+\\)", "", desc)
    #team abbreviation isn't useful either
    desc <- sub("^\\[[A-Z]{3}.*\\]", "", desc)
    #trim
    desc <- sub("^ ", "", desc)
    player_code <- tables[["pbp//event"]][,"player_code"]
    #If player_code is populated and the description is not a jump ball, 
    #then the first word in the description is useless!
    idx <- player_code != "" & !grepl("Jump Ball", desc)
    desc[idx] <- sub("^[A-Za-z]+ ", "", desc[idx])
    expr <- "Assist|Steal"
    indicies <- grep(expr, desc)
    mat <- sapply(strsplit(desc[indicies], expr), "[")
    desc[indicies] <- sub(" $", "", mat[1,])
    new <- rep("", length(desc))
    new[indicies] <- sub("^: |^:", "", mat[2,])
    tables[["pbp//event"]] <- cbind(tables[["pbp//event"]], cbind(event=desc, secondary_event=new))
  }
  return(tables)
}


#' Obtain game codes needed to construct XML file names
#'
#'
#' @param init.date String specifying the date (MM/DD/YYYY) to start obtaining game codes.
#' @param id Character vector with any combination of '00', '10', and/or '20'. These codes stand for NBA, WNBA and D-League, respectively.
#' @param offset Character vector of non-negative integers. This will control how many days to go beyond the \code{init.date}
#' @import jsonlite
#' @export
#' @examples
#' 
#' \dontrun{
#' codes <- getCodes()
#' codes <- getCodes(id=c("00", "10"))
#' #Takes awhile
#' codes <- getCodes(init.date="09/01/2012", id=c("00", "10", "20"), offset=paste(0:500))
#' }
#'

#Some of these ideas are adapted from https://github.com/knightsc/bbstats

getCodes <- function(init.date = "05/26/2013", id = "00", offset = paste(0:10)){
  codes <- NULL
  for (i in id){
    for (j in offset){
      url <- paste0("http://stats.nba.com/stats/scoreboard?LeagueID=", i, "&gameDate=", init.date, "&DayOffset=", j)
      scoreboards <- fromJSON(url)[[3]]
      idx <- which(scoreboards$name %in% "GameHeader")
      idx2 <- which(scoreboards$headers[[idx]] %in% "GAMECODE")
      sets <- scoreboards$rowSet[idx][[1]]
      code <- sapply(sets, function(x) x[idx2])
      if (length(code) > 0)  code <- paste0(i, "/", code)
      codes <- c(codes, code)
    }
  }
  return(codes)
}

#Made with help from -- http://stackoverflow.com/questions/21084400/xmltolist-does-not-return-xmlvalue
#and -- https://gist.github.com/cpsievert/8412484
pbpToDocs <- function(urls, quiet=FALSE){
  docs <- NULL
  for (i in urls) {
    if (!quiet) cat(i, "\n")
    con <- url(i)
    corrupt <- suppressWarnings(readLines(con))
    close(con)
    #Have to check if the url is an actual xml file
    if (any(grepl("Sorry, Page Not Found", corrupt))) {
      doc <- NULL
    } else {
    #Now fix the corrupt part
      tmp <- gsub("<![CDATA[", "", corrupt, fixed=TRUE) 
      file <- gsub("]]>", "", tmp, fixed=TRUE)
      doc <- xmlParseString(xml(file))
    }
    if (!is.null(doc)) {
      attr(doc, "XMLsource") <- i
      docs <- c(docs, doc) #Keep non-empty documents
    }
  }
  return(docs)
}


# Series of formatting functions to coerce variables to their proper (numeric) types
format.table <- function(dat, nums) {
  dat <- data.frame(dat, stringsAsFactors=FALSE)
  numz <- nums[nums %in% names(dat)] #error handling (just in case one of the columns doesn't exist)
  for (i in numz) dat[, i] <- as.numeric(dat[, i])
  return(dat)
}
format.shotchart.event <- function(dat) {
  event.nums <- c("x", "y")
  format.table(dat, nums=event.nums)
}
