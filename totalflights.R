library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(ggthemes)

flights2018 <- read_csv('D:/R Study/HKU/COMP2501/Final Project/Combined_Flights_2018.csv')
flights2019 <- read_csv('D:/R Study/HKU/COMP2501/Final Project/Combined_Flights_2019.csv')
flights2020 <- read_csv('D:/R Study/HKU/COMP2501/Final Project/Combined_Flights_2020.csv')
flights2021 <- read_csv('D:/R Study/HKU/COMP2501/Final Project/Combined_Flights_2021.csv')
flights2022 <- read_csv('D:/R Study/HKU/COMP2501/Final Project/Combined_Flights_2022.csv')


airlines2018 <- unique(flights2018$Airline)
airlines2019 <- unique(flights2019$Airline)
airlines2020 <- unique(flights2020$Airline)
airlines2021 <- unique(flights2021$Airline)
airlines2022 <- unique(flights2022$Airline)

allairlines <- intersect(airlines2018, airlines2019)
allairlines <- intersect(allairlines, airlines2020)
allairlines <- intersect(allairlines, airlines2021)
allairlines <- intersect(allairlines, airlines2022)

flights2018 <- flights2018[flights2018$Airline %in% allairlines,]
flights2019 <- flights2019[flights2019$Airline %in% allairlines,]
flights2020 <- flights2020[flights2020$Airline %in% allairlines,]
flights2021 <- flights2021[flights2021$Airline %in% allairlines,]
flights2022 <- flights2022[flights2022$Airline %in% allairlines,]

flights2018info <- flights2018 |>
  select(c(Airline, Cancelled, Diverted, DepDelayMinutes, DepDel15))

flights2018info <- flights2018info[!(flights2018info$Diverted == TRUE),]
flights2018info <- flights2018info[!((flights2018info$Cancelled == TRUE) & !is.na(flights2018info$DepDel15)),]
flights2018info <- flights2018info[!((flights2018info$Cancelled == FALSE) & is.na(flights2018info$DepDel15)),]

flights2018totalflights <- as.data.frame(table(flights2018info$Airline))

flights2018totalcancel <- flights2018info |>
  group_by(Airline) |>
  summarise(Cancellations = sum(Cancelled))


flights2018info <- flights2018info |>
  drop_na(DepDelayMinutes)

flights2018totaldelays <- flights2018info |>
  group_by(Airline) |>
  summarise(OnTime = sum(DepDelayMinutes == 0),
            ShortDelays = sum(DepDelayMinutes > 0 & DepDelayMinutes < 15),
            MediumDelays = sum(DepDelayMinutes >= 15 & DepDelayMinutes < 45),
            LongDelays = sum(DepDelayMinutes >= 45)) |>
  mutate(Cancellations = flights2018totalcancel$Cancellations) |>
  mutate(Flights = flights2018totalflights$Freq)


flights2019info <- flights2019 |>
  select(c(Airline, Cancelled, Diverted, DepDelayMinutes, DepDel15))

flights2019info <- flights2019info[!(flights2019info$Diverted == TRUE),]
flights2019info <- flights2019info[!((flights2019info$Cancelled == TRUE) & !is.na(flights2019info$DepDel15)),]
flights2019info <- flights2019info[!((flights2019info$Cancelled == FALSE) & is.na(flights2019info$DepDel15)),]

flights2019totalflights <- as.data.frame(table(flights2019info$Airline))

flights2019totalcancel <- flights2019info |>
  group_by(Airline) |>
  summarise(Cancellations = sum(Cancelled))


flights2019info <- flights2019info |>
  drop_na(DepDelayMinutes)

flights2019totaldelays <- flights2019info |>
  group_by(Airline) |>
  summarise(OnTime = sum(DepDelayMinutes == 0),
            ShortDelays = sum(DepDelayMinutes > 0 & DepDelayMinutes < 15),
            MediumDelays = sum(DepDelayMinutes >= 15 & DepDelayMinutes < 45),
            LongDelays = sum(DepDelayMinutes >= 45)) |>
  mutate(Cancellations = flights2019totalcancel$Cancellations) |>
  mutate(Flights = flights2019totalflights$Freq)


flights2020info <- flights2020 |>
  select(c(Airline, Cancelled, Diverted, DepDelayMinutes, DepDel15))

flights2020info <- flights2020info[!(flights2020info$Diverted == TRUE),]
flights2020info <- flights2020info[!((flights2020info$Cancelled == TRUE) & !is.na(flights2020info$DepDel15)),]
flights2020info <- flights2020info[!((flights2020info$Cancelled == FALSE) & is.na(flights2020info$DepDel15)),]

flights2020totalflights <- as.data.frame(table(flights2020info$Airline))

flights2020totalcancel <- flights2020info |>
  group_by(Airline) |>
  summarise(Cancellations = sum(Cancelled))


flights2020info <- flights2020info |>
  drop_na(DepDelayMinutes)

flights2020totaldelays <- flights2020info |>
  group_by(Airline) |>
  summarise(OnTime = sum(DepDelayMinutes == 0),
            ShortDelays = sum(DepDelayMinutes > 0 & DepDelayMinutes < 15),
            MediumDelays = sum(DepDelayMinutes >= 15 & DepDelayMinutes < 45),
            LongDelays = sum(DepDelayMinutes >= 45)) |>
  mutate(Cancellations = flights2020totalcancel$Cancellations) |>
  mutate(Flights = flights2020totalflights$Freq)


flights2021info <- flights2021 |>
  select(c(Airline, Cancelled, Diverted, DepDelayMinutes, DepDel15))

flights2021info <- flights2021info[!(flights2021info$Diverted == TRUE),]
flights2021info <- flights2021info[!((flights2021info$Cancelled == TRUE) & !is.na(flights2021info$DepDel15)),]
flights2021info <- flights2021info[!((flights2021info$Cancelled == FALSE) & is.na(flights2021info$DepDel15)),]

flights2021totalflights <- as.data.frame(table(flights2021info$Airline))

flights2021totalcancel <- flights2021info |>
  group_by(Airline) |>
  summarise(Cancellations = sum(Cancelled))


flights2021info <- flights2021info |>
  drop_na(DepDelayMinutes)

flights2021totaldelays <- flights2021info |>
  group_by(Airline) |>
  summarise(OnTime = sum(DepDelayMinutes == 0),
            ShortDelays = sum(DepDelayMinutes > 0 & DepDelayMinutes < 15),
            MediumDelays = sum(DepDelayMinutes >= 15 & DepDelayMinutes < 45),
            LongDelays = sum(DepDelayMinutes >= 45)) |>
  mutate(Cancellations = flights2021totalcancel$Cancellations) |>
  mutate(Flights = flights2021totalflights$Freq)


flights2022info <- flights2022 |>
  select(c(Airline, Cancelled, Diverted, DepDelayMinutes, DepDel15))

flights2022info <- flights2022info[!(flights2022info$Diverted == TRUE),]
flights2022info <- flights2022info[!((flights2022info$Cancelled == TRUE) & !is.na(flights2022info$DepDel15)),]
flights2022info <- flights2022info[!((flights2022info$Cancelled == FALSE) & is.na(flights2022info$DepDel15)),]

flights2022totalflights <- as.data.frame(table(flights2022info$Airline))

flights2022totalcancel <- flights2022info |>
  group_by(Airline) |>
  summarise(Cancellations = sum(Cancelled))


flights2022info <- flights2022info |>
  drop_na(DepDelayMinutes)

flights2022totaldelays <- flights2022info |>
  group_by(Airline) |>
  summarise(OnTime = sum(DepDelayMinutes == 0),
            ShortDelays = sum(DepDelayMinutes > 0 & DepDelayMinutes < 15),
            MediumDelays = sum(DepDelayMinutes >= 15 & DepDelayMinutes < 45),
            LongDelays = sum(DepDelayMinutes >= 45)) |>
  mutate(Cancellations = flights2022totalcancel$Cancellations) |>
  mutate(Flights = flights2022totalflights$Freq)

totalflights <- flights2018totaldelays
totalflights$OnTime = flights2018totaldelays$OnTime + flights2019totaldelays$OnTime +
  flights2020totaldelays$OnTime + flights2021totaldelays$OnTime +
  flights2022totaldelays$OnTime


totalflights$ShortDelays = flights2018totaldelays$ShortDelays + flights2019totaldelays$ShortDelays +
  flights2020totaldelays$ShortDelays + flights2021totaldelays$ShortDelays +
  flights2022totaldelays$ShortDelays

totalflights$MediumDelays = flights2018totaldelays$MediumDelays + flights2019totaldelays$MediumDelays +
  flights2020totaldelays$MediumDelays + flights2021totaldelays$MediumDelays +
  flights2022totaldelays$MediumDelays

totalflights$LongDelays = flights2018totaldelays$LongDelays + flights2019totaldelays$LongDelays +
  flights2020totaldelays$LongDelays + flights2021totaldelays$LongDelays +
  flights2022totaldelays$LongDelays

totalflights$Cancellations = flights2018totaldelays$Cancellations + flights2019totaldelays$Cancellations +
  flights2020totaldelays$Cancellations + flights2021totaldelays$Cancellations +
  flights2022totaldelays$Cancellations

totalflights$Flights = flights2018totaldelays$Flights + flights2019totaldelays$Flights +
  flights2020totaldelays$Flights + flights2021totaldelays$Flights +
  flights2022totaldelays$Flights



totalflightspercentage <- totalflights |>
  summarise(Airline = Airline,
            OnTime = OnTime/Flights *100,
            ShortDelays = ShortDelays/Flights *100,
            MediumDelays = MediumDelays/Flights *100,
            LongDelays = LongDelays/Flights *100,
            Cancel = Cancellations/Flights * 100) |>
  arrange(OnTime) |>
  pivot_longer(c(OnTime, ShortDelays, MediumDelays, LongDelays, Cancel), names_to = "Type",
               values_to = "Percent")

totalflightspercentage$Type <- factor(totalflightspercentage$Type,
                                     levels = c("Cancel", "LongDelays", "MediumDelays", "ShortDelays", "OnTime"))

totalflightsorder <- totalflightspercentage[totalflightspercentage$Type =="OnTime",]

totalflightsorder <- factor(totalflightsorder$Airline, levels = unique(totalflightsorder$Airline))

graph <- totalflightspercentage |>
  ggplot(aes(fill = Type,
             y = Airline,
             x = Percent)) +
  geom_bar(position = "fill", stat= "identity") +
  scale_y_discrete(limits = levels(totalflightsorder)) +
  scale_fill_manual(name = "Status", labels = c("Cancelled", "Long Delays (45 > minutes)", "Medium Delays (15-45 minutes)", "Short Delays (0-15 minutes)", "On Time"),
                    values = c("#7f7f7f",
                                "#E69F00",
                                "#009E73",
                                "#56B4E9",
                                "#0072B2")) +
  ggtitle("Flight Status of US Marketing Carriers in the past 5 Years") +
  ylab("Airlines") +
  xlab("Percentage") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_base() +
  theme(plot.title = element_text(hjust = 0.5))
graph
