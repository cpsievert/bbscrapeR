library(RSelenium)
library(XML)

# have to render http://stats.nba.com/players/ to get player names/ids
pJS <- phantom()
Sys.sleep(5) # give the binary a moment
remDr <- remoteDriver(browserName = 'phantomjs')
remDr$open(silent = TRUE)
remDr$setImplicitWaitTimeout(milliseconds = 10000)
remDr$navigate("http://stats.nba.com/players/")
elem <- remDr$findElements(using = "class name", "active")
# check that the names actually rendered
#remDr$screenshot(display=TRUE)
src <- remDr$getPageSource()[[1]]
doc <- htmlParse(src, asText = TRUE)
nodes <- getNodeSet(doc, path = "//a[@class='active']")
nms <- sapply(nodes, xmlValue)
links <- sapply(lapply(nodes, xmlAttrs), "[[", "href")
ids <- sub("/", "", sub("^/player/#!/", "", links))
players <- cbind(PlayerName = nms, PlayerID = ids)

# Just note that the working dir should be the path to bbscrapeR source
# setwd("~/Desktop/github/local/bbscrapeR/")
save(players, file = "data/players.rda")

# Shut down phantomjs
remDr$close()
pJS$stop()
