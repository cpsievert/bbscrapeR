knit_examples <- function() {
  old <- getwd()
  on.exit(setwd(old))
  if (basename(old) != 'examples') stop("basename(getwd()) != 'examples'")
  dirs <- dir()
  # keep only directories
  dirs <- dirs[file_test("-d", dirs)]
  # navigate into each example and knit individually
  for (i in dirs) {
    setwd(i)
    for (j in Sys.glob("*.Rmd")) {
      e <- try(rmarkdown::render(input = j, envir = new.env()))
    }
    setwd(old)
  }
}
knit_examples()