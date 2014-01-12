library(XML2R)
urls <- c("http://www.nba.com/games/game_component/dynamic/20130528/MIAIND/pbp_all.xml",
         "http://www.wnba.com/games/game_component/dynamic/20130527/CHIPHO/pbp_all.xml")
obs <- XML2Obs(urls)
obs <- add_key(obs, parent="url1//message")
tables <- collapse(obs)
tail(tables[["url1//message//game//event"]])
