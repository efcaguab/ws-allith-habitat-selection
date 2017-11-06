#!/usr/bin/env Rscript
library(optparse)
library(dplyr)
library(tibble)
library(magrittr)
library(foreach)
library(doMC)
library(magrittr)

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

# FUNCTION ----------------------------------------------------------------

# Function to calculate the vectors of presence absence
pres.abs.lag <- function (start.date, end.date, sightings, dates){
  # Create a data frame with the detections
  sight <- data.frame (id = sightings, date = dates) %>%
    filter (date >= start.date, date <= end.date) %>%
    mutate (id = factor (id)) %>% 
    arrange (date)
  
  # For each shark we'll start with the first detection only
  individuals <- levels (sight$id)
  # Cycle trough each shark
  presence.absence <- 
    foreach (i=1:length (individuals), .combine = rbind) %dopar% {
      # message(individuals[i])
               # Find dates in which the shark was present
      dates.present <- sight$date[sight$id == individuals[i]] %>%
        as.numeric ()
      # Establish all dates in which it was tagged (only dates in which there was monitoring)
      dates.tagged <- unique(sight$date)[unique(sight$date) >= 
                                           sight$date[match (individuals[i], sight$id)]]
      if(length(dates.tagged)>1) {
        # Find all possible combinations of dates in which it was tagged
        dates.comb <- as.data.frame (t (combn (dates.tagged, 2))) %>%
          tbl_df()
        names (dates.comb) <- c ("date.1", "date.2")
        dates.comb <- mutate (dates.comb, lag = date.2 - date.1, # Find the lag between given dates
                              # Establish if it was present for in that lag
                              present = (date.1 %in% dates.present) &
                                (date.2 %in% dates.present), 
                              date.1 = as.Date(date.1, origin = "1970-01-01"), 
                              date.2 = as.Date(date.2, origin = "1970-01-01"), 
                              id = individuals[i])
        return (dates.comb)
      } else {
        return(NULL)
      }
      
    }
  return (presence.absence)
}


# READ DATA ---------------------------------------------------------------

load("./data/processed/AllDetections.RData")

det.ws <- as_tibble(DET.WS)  # Convert to tbl data frame
names (det.ws) <- names (det.ws) %>% tolower ()  # change names to lower case 

# Table that contains tagged whale shark data
ws.tags <- as_tibble(WS.TAGS)
names (ws.tags) <- names (ws.tags) %>% tolower ()
names (ws.tags)[2] <- "date.tag"

ws.tags %<>%
  mutate(ecocean = as.character(ecocean), 
         transmitterid = as.character(transmitterid),
         ecocean = if_else(is.na(ecocean), transmitterid, ecocean))


# CALCULATE LAGS ----------------------------------------------------------

# Using only sharks present in the list of tagged sharks merge data frames
det.ws <- inner_join (det.ws, select (ws.tags, -(size), -(number)))

# Clump in a weekly basis
det.ws <- mutate (det.ws, date.week = cut (datetime, paste(opt$window_length, 'week')) %>% as.Date ())
aco.week <- plyr::ddply (det.ws, "date.week", function (det){
  sharks <- !duplicated (det$ecocean)
  per.week <- data.frame (ecocean = det$ecocean[sharks], 
                          sex = det$sex[sharks],
                          batch = det$batch[sharks], 
                          date.tag = det$date.tag[sharks])
}) %>% as_tibble()

# aco.week <- aco.week %>% filter (batch == "2012-1" | batch == "2012-2")

# remove individuals that were only tagged in the last week, because pff
aco.week %<>%
  mutate(tim = difftime(max(date.week),date.tag,  units = "days"), 
         tim = as.numeric(tim)) %>%
  filter(tim > (7 * opt$window_length)) %>%
  select(-tim)


# Calculate probabilities
PADet <- pres.abs.lag (start.date = min (aco.week$date.week), 
                       end.date = max (aco.week$date.week),
                       sightings = aco.week$ecocean, 
                       dates = aco.week$date.week)
# Change "id" column to "ecocean"
names (PADet)[5] <- "ecocean"

# Put shark info back into the data-frame
PADet <- left_join (PADet, ws.tags %>% select (ecocean, sex, size, batch))


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
          date.id = paste (date.1, ecocean))

saveRDS(PADet, "./data/processed/probability_acoustic_detection.rds")
