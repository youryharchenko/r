
library(WDI)

cache <- WDIcache()
head(
  WDIsearch(string = "SP.POP.TOTL",
  field = "indicator",
  short = TRUE,
  cache = cache)
, n = 30L)

head(
  WDIsearch(string = "NY.GDP.PCAP.PP.CD",
            field = "indicator",
            short = TRUE,
            cache = cache)
  , n = 30L)


