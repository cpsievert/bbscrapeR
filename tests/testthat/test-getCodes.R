context("Game Codes")

exampleDate <- "12/01/14"

test_that("Type and dim are correct", {
  res <- getCodes(init.date = exampleDate, offset = "0")
  expect_equal(class(res), "character")
  expect_equal(length(res), 4)
  expect_false(any(grepl("NA", res)))

})

test_that("Codes refer to real web pages", {
  res <- getCodes(init.date = exampleDate, offset = "0")
  producedGamePages <- paste0("http://stats.nba.com/game/#!/", res)
  for(u in producedGamePages){
    con <- url(u)
    corrupt <- suppressWarnings(readLines(con))
    close(con)

    expect_false(any(grepl("Sorry, Page Not Found", corrupt)))
  }


})
