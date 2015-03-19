codes <- sapply(2000:2015, bbscrapeR:::get_codes)
codes <- do.call(c, codes)
codes <- paste0("00/", codes)
save(codes, file = "data/codes.rda")
