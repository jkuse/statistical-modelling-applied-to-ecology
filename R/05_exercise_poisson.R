### exercise RIKZRichness poisson
library(car)

rikz_rich <- read.table(file.choose(), header = T, dec=".")
head(rikz_rich)
str(rikz_rich)

### possible explanatory variables (discrete): week, angle1, angle2, exposure, beach
### possible explanatory variables (continuous): salinity, temperature, NAP, penetrability, grainsize, humus

hist(rikz_rich$Richness)
qqPlot(rikz_rich$Richness, distribution = "pois", lambda = mean(rikz_rich$Richness))

### it almost fit in a poisson, but there are some points overdispersed
### maybe we could use negative binomial

lm(Richness ~ temperature, data = rikz_rich)
