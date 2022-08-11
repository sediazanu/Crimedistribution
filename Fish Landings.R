getwd()
install.packages('readr')
install.packages('.')

library(readr)
library(dplyr)
if(!requireNamespace("devtools")) install.packages("devtools") # installs devtools if not installed yet
devtools::install_github("dkahle/ggmap") # installs ggmap from GitHub
install.packages("bindrcpp")
library(devtools)
library(ggmap)


detach("package:stringi", unload = TRUE)

read_csv("Published_dataset_2014.csv")

FishLanding2014 <- read_csv("Published_dataset_2014.csv")
FishLanding2015 <- read_csv("Published_dataset_2015.csv")
FishLanding2016 <- read_csv("Published_dataset_2016.csv")
FishLanding2017 <- read_csv("Published_dataset_2017.csv")
FishLanding2018 <- read_csv("Published_dataset_2018.csv")
FishLanding2019 <- read_csv("Published_dataset_2019.csv")

FishLanding2019 <- rename(FishLanding2019, 'Live weight (tonnes)'='Sum of Live weight (tonnes)', 'Landed weight (tonnes)'='Sum of Landed weight (tonnes)', 'Value(£000s)'='Sum of Value(£)')

FishLanding2014 <-mutate(FishLanding2014, Test = 2014)
FishLanding2015 <-mutate(FishLanding2015, Test = 2015)
FishLanding2016 <-mutate(FishLanding2016, Test = 2016)
FishLanding2017 <-mutate(FishLanding2017, Test = 2017)
FishLanding2018 <-mutate(FishLanding2018, Test = 2018)
FishLanding2019 <-mutate(FishLanding2019, Test = 2019)

FishLanding2017 <-rename(FishLanding2017, "Value"="Value(£000s)")
FishLanding2018 <-rename(FishLanding2018, "Value"="Value(£000s)")

FishLanding2017 <-mutate(FishLanding2017, Value = Value/1000)
FishLanding2018 <-mutate(FishLanding2018, Value = Value/1000)

FishLanding2017 <-rename(FishLanding2017, "Value(£000s)"="Value")
FishLanding2018 <-rename(FishLanding2018, "Value(£000s)"="Value")

landings <- bind_rows(FishLanding2014,FishLanding2015,FishLanding2016,FishLanding2017,FishLanding2018,FishLanding2019, .id="Year")
landings

landings <-rename(landings, 'MonthLanded'='Month Landed')
landings <-mutate(landings, Time = as.Date(paste(Test, MonthLanded, 15, sep = "-")))

landings <-rename(landings, 'PortofLanding'='Port of Landing')
landings <-rename(landings, "LengthGroup"="Length Group")
landings <-rename(landings, "Landedweighttonnes"="Landed weight (tonnes)")
landings <-rename(landings, "Value"="Value(£000s)")
landings <-rename(landings, "SpeciesGroup"="Species Group")

WhitbyU10mLobs <- filter(landings, PortofLanding == "Whitby", LengthGroup =="10m&Under", Species == "Lobsters")
WhitbyU10mLobs

LandingPlot1 <-ggplot(data= WhitbyU10mLobs) + geom_point(mapping = aes(x = Time, y = Landedweighttonnes)) + scale_y_log10()
LandingPlot1

LandingPlot2 <-ggplot(data= WhitbyU10mLobs) + geom_point(mapping = aes(x = Time, y = Value)) + scale_y_log10()
LandingPlot2

Correction <-WhitbyU10mLobs %>%
  group_by(Year) %>%
  summarise(Value = mean(Value))

Correction

landings<- rename(landings, 'VesselNationality'='Vessel Nationality')

LandWeiByVessNat <-select(landings, Time, VesselNationality)
LandWeiByVessNat <-landings %>%
  group_by(Time,VesselNationality) %>%
  summarise(TotalLandedWeight = sum(Landedweighttonnes))

LandingPlot3 <-ggplot(data= LandWeiByVessNat) + geom_point(mapping = aes(x = Time, y = TotalLandedWeight)) + scale_y_log10() +geom_line(mapping = aes(x = Time, y = TotalLandedWeight)) 
          +facet_wrap(~ VesselNationality, scales = 'free')
LandingPlot3


Demersal1 <- select(landings, Time, MonthLanded, Species, SpeciesGroup,Landedweighttonnes)
Demersal1 <- filter(Demersal1,SpeciesGroup =="Demersal", MonthLanded ==12 | MonthLanded==1)
Demersal2 <-Demersal1 %>%
  group_by(Time,Species) %>%
  summarise(TotalLandedWeight = sum(Landedweighttonnes))

LandingPlot4 <-ggplot(Demersal2, aes(x=Species, y=TotalLandedWeight)) + geom_boxplot() + scale_y_log10()
LandingPlot4
