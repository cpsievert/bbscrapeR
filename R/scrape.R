#' 'Rebound' data from nba.com and wnba.com
#'
#'  Obtain (and possibly store in a remote database) data from nba.com and wnba.com. Currently there are support for three
#'  file types. Here are some examples:
#'  \href{http://www.nba.com/games/game_component/dynamic/20130526/MIAIND/shotchart_all.xml}{shotchart_all.xml}
#'  \href{http://www.nba.com/games/game_component/dynamic/20130528/MIAIND/pbp_all.xml}{pbp_all.xml}
#'  \href{http://www.nba.com/games/game_component/dynamic/20130526/MIAIND/boxscore.xml}{boxscore.xml}
#'
#' @param first String specifying the date "YYYYMMDD" to start obtaining game codes.
#' @param last String specifying the date "YYYYMMDD" to stop obtaining game codes.
#' @param codes Character vector of game codes with league ID as a prefix
#' @param leagues Character vector with any combination of 'nba', 'wnba', and/or 'd' (for D-League).
#' @param suffix character vector with suffix of the XML files to be parsed. Currently supported options are: 'boxscore.xml', 'pbp_all.xml', 'shotchart_all.xml'
#' @param connect A database connection object. The class of the object should be "MySQLConnection" or "SQLiteConnection".
#' If a valid connection is supplied, tables will be copied to the database, which will result in better memory management.
#' If a connection is supplied, but the connection fails for some reason, csv files will be written to the working directory.
#' @seealso If you want to add support for more file types, the \code{XML2R} package is a good place to start.
#' @return Returns a list of data frames (or nothing if writing to a database).
#' @import XML
#' @import XML2R
#' @import dplyr
#' @export
#' @examples
#' 
#' \dontrun{
#' #collect and store available data between May 26th and June 1st of 2013
#' library(dplyr)
#' db <- src_sqlite("bball.sqlite3") #creates database in current directory
#' all.leagues <- c("nba", "wnba", "d")
#' files <- c("boxscore.xml", "pbp_all.xml", "shotchart_all.xml")
#' dat <- rebound(first="20130526", last="20130601",
#'                leagues = all.leagues, suffix = files, connect=db$con)
#'}



rebound <- function(first, last, codes, leagues = "nba", suffix="shotchart_all.xml", connect) {
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
    tables <- setNames(collapse_obs(obs), paste0("shotchart_", c("message", "game", "event")))
    #turn matrices into dfs and format variable types
    for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], name=i)
    if (!missing(connect)) {
      #Try to write tables to database, if that fails, write to csv. Then clear up memory
      for (i in names(tables)) export(connect, name=i, value=tables[[i]])
      rm(obs)
      rm(tables)
      message("Collecting garbage")
      gc() 
    }
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
    table.names <- paste0("boxscore_", c("message", "game", "officials", "players", "teams"))
    if (exists("tables")){
      tables <- c(tables, setNames(collapse_obs(obs), table.names))
    } else {
      tables <- setNames(collapse_obs(obs), table.names)
    }
    #turn matrices into dfs and format variable types
    for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], name=i)
    if (!missing(connect)) {
      #Try to write tables to database, if that fails, write to csv. Then clear up memory
      for (i in names(tables)) export(connect, name=i, value=tables[[i]])
      rm(obs)
      rm(tables)
      message("Collecting garbage")
      gc() 
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
    nms[nms %in% "game//event"] <- "pbp_event"
    nms[nms %in% "game"] <- "pbp_game"
    nms[nms %in% "attrs"] <- "pbp_message"
    obs <- setNames(obs, nms)
    if (exists("tables")){
      tables <- c(tables, collapse_obs(obs))
    } else {
      tables <- collapse_obs(obs)
    }
    #transform descriptions in the XML value into nicely formatted variables
    desc <- tables[["pbp_event"]][,"XML_value"]
    #strip game clock (this is recorded in game_clock anyway)
    desc <- sub("^\\([0-9]{2}:[0-9]{2}\\)", "", desc)
    #some game clocks go to the tenth (or maybe hundreth?) of a second
    desc <- sub("^\\([0-9]{2}:[0-9]{2}\\.[0-9]+\\)", "", desc)
    #team abbreviation isn't useful either
    desc <- sub("^\\[[A-Z]{3}.*\\]", "", desc)
    #trim
    desc <- sub("^ ", "", desc)
    player_code <- tables[["pbp_event"]][,"player_code"]
    #If player_code is populated and the description is not a jump ball, 
    #then the first word in the description is useless!
    idx <- player_code != "" & !grepl("Jump Ball", desc)
    desc[idx] <- sub("/^[a-z ,.'-]+$/i", "", desc[idx])
    #Now split the description into two parts if more than one player is involved
    expr <- "Assist|Steal"
    indicies <- grep(expr, desc)
    mat <- sapply(strsplit(desc[indicies], expr), "[")
    desc[indicies] <- sub(" $", "", mat[1,])
    new <- rep("", length(desc))
    new[indicies] <- sub("^: |^:", "", mat[2,])
    #extract the number of (cumulative) points  
    pts <- str_extract(desc, "\\([[:digit:]]+ PTS\\)")
    pts <- as.integer(str_extract(pts, "([[:digit:]]+)"))
    tables[["pbp_event"]] <- cbind(tables[["pbp_event"]], 
                                    cbind(event=desc, secondary_event=new, cum_points=pts,
                                          three_point=as.numeric(grepl("3pt", desc))))
    #turn matrices into dfs and format variable types
    for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], name=i)
    #turn the numbered action_type and msg_type variables into more informative response values
    scrape.env <- environment() #avoids bringing data objects into global environment
    data(actions, package="bbscrapeR", envir=scrape.env)
    data(msgs, package="bbscrapeR", envir=scrape.env)
    tables[["pbp_event"]] <- inner_join(x=tables[["pbp_event"]], y=actions, by=c("action_type", "msg_type"))
    tables[["pbp_event"]] <- inner_join(x=tables[["pbp_event"]], y=msgs, by="action_type")
    if (!missing(connect)) {
      #Try to write tables to database, if that fails, write to csv. Then clear up memory
      for (i in names(tables)) export(connect, name=i, value=tables[[i]])
      rm(obs)
      rm(tables)
      message("Collecting garbage")
      gc() 
    }
  }
  if (exists("tables")) {
    return(tables)
  } else {
    #Should I return the connection or something else instead?
    return(NULL)
  }
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

#Try to write tables to database connection, if that fails, write to csv.
export <- function(connect, name, value) {
  # '.' in table names are not good!
  names(value) <- sub("\\.", "_", names(value))
  #if url.map=FALSE, have to change 'url_key' to url
  names(value) <- sub("^url_key$", "url", names(value))
  current.fields <- names(value)
  #url should never be NA!
  throw <- is.na(value$url)
  if (any(throw)) value <- value[-throw,]
  if (dim(value)[1] == 0) return(NULL)
  #upload fields so we have table templates
  env2 <- environment()
  data(fields, package="bbscrapeR", envir=env2)
  #Try to find fields in an existing table
  prior.fields <- plyr::try_default(DBI::dbListFields(connect, name), default=NULL, quiet=TRUE)
  master.fields <- names(fields[[name]])
  if (!is.null(prior.fields)) {
    idx <- !master.fields %in% prior.fields
    if (any(idx)) warning(paste("The", name, "table in your database has fewer fields than the suggested set of fields! You might want to try adding these fields to this table:", paste(master.fields[idx], collaspe=", ")))
    new.fields <- prior.fields[!prior.fields %in% current.fields]
    types <- NULL
  } else {
    new.fields <- master.fields[!master.fields %in% current.fields]
    types <- fields[[name]]
  }
  #add any missing fields to value b4 trying to write to database
  if (length(new.fields) > 0) {
    new.mat <- matrix(rep(NA, length(new.fields)), nrow=1)
    value <- cbind(value, `colnames<-`(new.mat, new.fields))
    #must have columns ordered same way
    value <- value[master.fields]
  }
  success <- plyr::try_default(DBI::dbWriteTable(conn=connect, name=name, value=value, field.types=types,
                                                 append=TRUE, overwrite=FALSE, row.names=FALSE),
                               default=FALSE, quiet=TRUE)
  if (success) {
    message(paste("Successfully copied", name, "table to database connection."))
  } else {
    file.name <- gsub(" ", "-", gsub(":", "-", paste0(name, " ", Sys.time(), ".csv")))
    message(paste("Failed to copy", name, "table to database connection. Writing", file.name, "instead."))
    write.csv(value, file=file.name, row.names=FALSE)
  }
  return(success)
}




# Take a matrix and turn into data frame and turn relevant columns into numerics (or integers)
format.table <- function(dat, name) {
  nums <- NULL
  ints <- NULL
  switch(name,
         shotchart_event = ints <- c("act", "id", "prd", "pts", "x", "y", "time"),
         pbp_event = ints <- c("action_type", "msg_type", "eventid", "prd", "htms", "vtms")
  )
  dat <- data.frame(dat, stringsAsFactors=FALSE)
  numz <- nums[nums %in% names(dat)] #error handling (just in case one of the columns doesn't exist)
  intz <- ints[ints %in% names(dat)] #error handling (just in case one of the columns doesn't exist)
  for (i in numz) dat[, i] <- as.numeric(dat[, i])
  for (i in intz) dat[, i] <- as.integer(dat[, i])
  if (name == "shotchart_event") { #change some of the column names so they match columns in pbp_event
    names(dat) <- gsub("^id$", "eventid", names(dat))
    names(dat) <- gsub("^act$", "action_type", names(dat))
  }
  return(dat)
}

#silly function to work around stringsAsFactors=TRUE when using merge
merged <- function(x, y, by){
  dat <- merge(x=x, y=y, by=by, sort=FALSE)
  dat[] <- lapply(dat, function(x) as.character(x))
  return(dat)
}