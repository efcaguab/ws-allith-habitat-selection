#!/usr/bin/env Rscript
library(optparse)
library(dplyr)
library(tibble)
library(magrittr)
library(foreach)
library(doMC)
library(magrittr)

source("code/functions/pres.abs.lag.R")

# bin window (weeks)

option_list = list(
  make_option(c("-w", "--window_length"), type="integer", default=2, 
              help="length of the binning window in weeks", metavar="character"),
  make_option(c("-nc", "--ncores"), type="integer", default=1, 
              help="number of cores", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

registerDoMC(cores = opt$ncores)

# READ DATA ---------------------------------------------------------------

load("./data/processed/AllDetections.RData")

det.ws <- as_tibble(DET.WS)  # Convert to tbl data frame
names (det.ws) <- names (det.ws) %>% tolower ()  # change names to lower case 

# Table that contains tagged whale shark data
ws.tags <- as_tibble(WS.TAGS)
names (ws.tags) <- names (ws.tags) %>% tolower ()
names (ws.tags)[3] <- "date.tag"

ws.tags %<>%
  mutate(ecocean = as.character(ecocean), 
         id = as.character(shark),
         transmitterid = as.character(transmitterid),
         ecocean = if_else(is.na(ecocean), transmitterid, ecocean))


# CALCULATE LAGS ----------------------------------------------------------

# Using only sharks present in the list of tagged sharks merge data frames
det.ws <- inner_join (det.ws, select (ws.tags, -(size), -(number)))

fake.sightings <- data_frame (datetime = seq(from = min(det.ws$datetime),
                                         to = max(det.ws$datetime), 
                                         by = paste(opt$window_length, 'week')), 
                              id = "XXX")

det.ws <- bind_rows(fake.sightings, det.ws)

# Clump in a weekly basis
det.ws <- mutate (det.ws, date.week = cut (datetime, paste(opt$window_length, 'week')) %>% as.Date ())


aco.week <- plyr::ddply (det.ws, "date.week", function (det){
  sharks <- !duplicated (det$id)
  per.week <- data.frame (id = det$id[sharks], 
                          sex = det$sex[sharks],
                          batch = det$batch[sharks], 
                          date.tag = det$date.tag[sharks])
}) %>% as_tibble()

# aco.week <- aco.week %>% filter (batch == "2012-1" | batch == "2012-2")

# # remove individuals that were only tagged in the last week, because pff
# aco.week %<>%
#   mutate(tim = difftime(max(date.week),date.tag,  units = "days"), 
#          tim = as.numeric(tim)) %>%
#   filter(tim > (7 * opt$window_length)) %>%
#   select(-tim)


# Calculate probabilities
PADet <- pres.abs.lag(start.date = min(aco.week$date.week), 
                      end.date = max (aco.week$date.week),
                      sightings = aco.week$id, 
                      dates = aco.week$date.week)
# Change "id" column to "ecocean"
# names (PADet)[5] <- "ecocean"



# Put shark info back into the data-frame
PADet <- ws.tags %>% 
  group_by(id) %>%
  arrange(batch) %>%
  summarise(sex = sex[1], 
            size = mean(size, na.rm = T), 
            batch = batch[1]) %>% 
  right_join (PADet)


# CALCULATE EFFORT --------------------------------------------------------

# Arrange by date
array.events <- ARRAY.EVENTS %>% tbl_df () %>%
  arrange (DATETIME) 
# Change the ugly names in uppercase to lowercase
names (array.events) <- names (array.events) %>% tolower ()
array.events %<>% mutate_at (vars(-datetime),funs (as.character))

# Change the format of the events by date to a format by station
times.in <- plyr::ddply (array.events, "stationname", function (x){
  # Determine when a receiver was deployed in the station
  deployed <- x %>% filter (event == "DEP", lead (event) == "RET")
  # Determine when a receiver was retrieved from the station
  retrieved <- x %>% filter (event == "RET", lag (event) == "DEP")
  if (nrow (deployed) > 0) {
    y <- data.frame (station = first (x$stationname), 
                     date.in = deployed$datetime, 
                     date.out = retrieved$datetime,
                     rec.in = deployed$receiverid,
                     rec.out = retrieved$receiverid)
    return (y)
  } else return (NULL)
}) %>%  
  # Generate weekly approximations
  mutate (w.date.in = cut (date.in, 'week'),
          w.date.out = cut (date.out, 'week')) %>% tbl_df()

# For each re-detection date calculate the number of working stations
pres <- plyr::ddply (PADet, "date.2", function (x, times.in){
  # Filter out stations active at that date
  stations.listening <- filter (times.in, x$date.2[1] >= as.Date (w.date.in), x$date.2[1] <= as.Date (w.date.out)) %>% 
    select (station) %>%
    unique() %>%
    extract2(1) %>%
    as.character() %>%
    # see if they are inshore or offshore
    grepl("WS", ., fixed = TRUE)
  # Create data frame with the list and the number of stations
  y <- data.frame (#configuration = do.call (paste, as.list(stations.listening$station)), 
    nStations_inshore = sum(stations.listening), 
    nStations_offshore = sum(!stations.listening))
  return (y)
}, times.in = times.in) %>% as.tibble ()

# Merge with the Probability of acoustic detection data frame
PADet <- inner_join (PADet, pres %>% select (date.2, nStations_inshore, nStations_offshore)) %>%
  # Simplify factors by making them integers
  mutate (week.1 = lubridate::week (date.1), 
          week.2 = lubridate::week (date.2), 
          date.random = (as.numeric (date.1) - as.numeric (min (date.1))) / 7, 
          date.id = paste (date.1, id)) %>%
  filter (id != "XXX")


# TAG SHEDDING ------------------------------------------------------------

shed <- read.csv("data/raw/shed_tag_Lith2.csv") %>%
  rename(id = Red.Sea.Numbering) %>%
  mutate_at(vars(c("Date.Sighted.Without.Tag", 
                   "Last.Detection.Before.Tag.Loss", 
                   "Retagging.Date")), 
            funs(as.POSIXct(., format = "%m/%d/%Y", tz = "Asia/Riyadh"))) %>%
  rename(last_det = Last.Detection.Before.Tag.Loss, 
         retag = Retagging.Date) %>%
  mutate(retag = if_else(is.na(retag), max(det.ws$datetime), retag)) %>%
  select(id, last_det, retag)


PADet %<>%
  left_join(shed) %>% 
  mutate(undetectable = (date.1 >= last_det & date.1 <= retag) | (date.2 >= last_det & date.2 <= retag), 
         undetectable = if_else(is.na(undetectable), FALSE, TRUE)) %>%
  filter(!undetectable)
  
saveRDS(PADet, "./data/processed/probability_acoustic_detection.rds")
