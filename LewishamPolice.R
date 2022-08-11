getwd()
install.packages('readr')
install.packages('dplyr')
if(!requireNamespace("devtools")) install.packages("devtools") # installs devtools if not installed yet
devtools::install_github("dkahle/ggmap") # installs ggmap from GitHub
install.packages("bindrcpp")
packageVersion("ggmap")

library(readr)
library(dplyr)
library(devtools)
library(ggmap)
police1 <- read_csv("2019-01-metropolitan-street.csv")
police2 <- read_csv("2019-02-metropolitan-street.csv")
police3 <- read_csv("2019-03-metropolitan-street.csv")
police4 <- read_csv("2019-04-metropolitan-street.csv")
police5 <- read_csv("2019-05-metropolitan-street.csv")
police6 <- read_csv("2019-06-metropolitan-street.csv")
police7 <- read_csv("2019-07-metropolitan-street.csv")
police8 <- read_csv("2019-08-metropolitan-street.csv")
police9 <- read_csv("2019-09-metropolitan-street.csv")
police10 <- read_csv("2019-10-metropolitan-street.csv")
police11 <- read_csv("2019-11-metropolitan-street.csv")
police12 <- read_csv("2019-12-metropolitan-street.csv")

policex <- bind_rows(police1,police2,police3,police4,police5,police6,police7,police8,police9,police10,police11,police12)
policex
policex %>% 
  mutate(CrimeType = `Crime type`) %>% 
  select(Month, Longitude, Latitude, CrimeType) -> policex
policex

geocode("lewisham", source="dsk")
bbox <- c(-0.038, 51.45, 0.018, 51.47)
LewishamMap <- get_map(location = bbox, source = "stamen", maptype = "toner")  
ggmap(LewishamMap)

crimeLewisham <- filter(policex, bbox[1] <= Longitude, Longitude <= bbox[3],
                    bbox[2] <= Latitude,  Latitude  <= bbox[4])
crimeLewisham
table(crimeLewisham$CrimeType)
crimeLewisham %>% 
  filter(CrimeType %in% c("Anti-social behaviour", "Violence and sexual offences", 
                          "Other theft", "Vehicle crime")
  ) -> crimeLewisham
table(crimeLewisham$CrimeType)

MapPoints <- ggmap(LewishamMap) +
  geom_point(aes(x = Longitude, y = Latitude, colour = CrimeType, shape=CrimeType),
             size=4, data = crimeLewisham) + 
  theme(legend.position = "top")

MapPoints

MapCrimes <- ggmap(LewishamMap) +
  geom_point(aes(x = Longitude, y = Latitude, colour = CrimeType),
             size=3, data = crimeLewisham) +
  theme(legend.position = "none") +  
  facet_wrap(~ CrimeType)

MapCrimes
