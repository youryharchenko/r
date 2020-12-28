library(tidymodels)
library(WDI)

year <- 2014L

vars <- c(
#  "AG.LND.TOTL.K2",
  "AG.LND.AGRI.ZS",
# "AG.LND.IRIG.AG.ZS",
#  "AG.LND.ARBL.ZS",
#  "AG.LND.ARBL.HA.PC",
#  "AG.YLD.CREL.KG",
#  "SP.RUR.TOTL",
  "SP.RUR.TOTL.ZS"
)
varNames <-c(
#  "Land area (sq km)",
  "Agricultural land (% of land area)",
#  "Agricultural irrigated land (% of total agricultural land)",
#  "Arable land (% of land area)",
#  "Arable land (hectares per person)",
#  "Cereal yield (kg per hectare)",
#  "Rural population",
  "Rural population (% of total population)"
)

df <- WDI(country="all", indicator=vars, start=year, end=year, extra = TRUE) %>%
  filter(region != "Aggregates") %>%
  select(iso2c, country, region, all_of(vars))

dfc <- select(df, country, all_of(vars)) %>%
  na.omit() # %>%
  #scale()

# wss <- (nrow(dfc)-1)*sum(apply(dfc,2,var))
#
# for (i in 2:15) wss[i] <- sum(kmeans(dfc, centers=i)$withinss)
#
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")

cl <- dfc %>% select(all_of(vars)) %>% kmeans(9)
plot(select(dfc, all_of(vars)), col = cl$cluster)
points(cl$centers, col = 1:length(cl$cluster), pch = 8, cex = 2)
a <- augment(cl, dfc)
head(a)
tidy(cl)

