suppressMessages({
  suppressPackageStartupMessages({
    library(tibble)
    library(optparse)
    library(dplyr)
    library(magrittr)
    library(foreach)
    library(doMC)
  })
})

source("code/functions/pres.abs.lag.R")


option_list = list(
  make_option(c("-w", "--window_length"), type="integer", default=2, 
              help="length of the binning window in weeks", metavar="character"),
  make_option(c("-nc", "--ncores"), type="integer", default=1, 
              help="number of cores", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

registerDoMC(cores = opt$ncores)

message("window length is ", opt$window_length)

load("./data/processed/AllDetections.RData")

# Table that contains tagged whale shark data
ws.tags <- as_tibble(WS.TAGS)
names (ws.tags) <- names (ws.tags) %>% tolower ()
names (ws.tags)[3] <- "date.tag"

ws.tags %<>%
  mutate(ecocean = as.character(ecocean), 
         id = as.character(shark),
         transmitterid = as.character(transmitterid),
         ecocean = if_else(is.na(ecocean), transmitterid, ecocean))

# ENCOUNTERS --------------------------------------------------------------

sightings <- readr::read_csv("data/raw/visual_sightings.csv") %>%
  mutate(date = as.Date(Date, format = "%d-%b-%y")) %>%
  rename(id = `Shark ID`) %>%
  select(date, id)


# SURVEYS -----------------------------------------------------------------

SURVEYS <- read.csv ("./data/raw/Survey_Effort_Lith.csv")
SURVEYS$START.TIME <- as.POSIXct(with(SURVEYS, paste(DATE, START.TIME)), format = "%m/%d/%Y %H:%M",  tz = "Asia/Riyadh")
SURVEYS$END.TIME <- as.POSIXct(with(SURVEYS, paste (DATE, END.TIME)), format = "%m/%d/%Y %H:%M",  tz = "Asia/Riyadh") 
SURVEYS$DATE <- as.Date (SURVEYS$DATE, format ='%m/%d/%Y')
SURVEYS$DURATION <- with (SURVEYS, END.TIME-START.TIME)
SURVEYS$DURATION[is.na (SURVEYS$DURATION)] <- round (mean (SURVEYS$DURATION, na.rm = TRUE))

surveys <- SURVEYS %>% tbl_df ()
names (surveys) <- tolower (names (surveys))

breaks <- seq(min(min(surveys$date), min(sightings$date))-opt$window_length*7, 
              max(max(surveys$date), max(sightings$date))+opt$window_length*7,
              by = paste(opt$window_length, 'week'))
# CALCULATE LAGS ----------------------------------------------------------

fake.sightings <- data_frame (date = surveys$date, 
                              id = "XXX")

sightings.tagged.sharks <- sightings %>%
  bind_rows(fake.sightings) %>%
  mutate (date.week = cut.Date(date,breaks = breaks) %>% as.Date ()) %>%
  group_by (date.week) %>%
  distinct (id) %>%
  group_by () %>% 
  arrange (date.week)

PAEnc <- pres.abs.lag (
  start.date = min (sightings.tagged.sharks$date.week),
  end.date = max (sightings.tagged.sharks$date.week),
  sightings = sightings.tagged.sharks$id,
  dates = sightings.tagged.sharks$date.week)
  
# ADD SURVEY DATA ---------------------------------------------------------

# Add survey information 
surveys.week <- surveys %>%
  # Group in 2 week bins
  mutate (date.week = cut (date, breaks =  breaks) %>% as.Date, 
          hours = duration %>% as.numeric %>% abs %>% divide_by (60)) %>%
  group_by (date.week) %>%
  # Calculate average trip length in the bin
  summarise (hours = mean (hours, na.rm = TRUE))

PAEnc %<>% 
  # Join with survey data for effort
  left_join (surveys.week %>% select (date.week, hours),
             by = c ("date.2" = "date.week")) %>%
  # Rename id by ecocean so its consistent
  # rename (ecocean = id) %>%
  # Remove the dummy shark for the trips with no shark
  filter (id != "XXX") %>%
  # For those surveys without trip length info fill with the mean
  mutate (hours = replace (hours, is.na (hours), mean (hours, na.rm = TRUE)),
          # Calculate week on the year for the sighting and resighting dates
          week.1 = lubridate::week (date.1),
          week.2 = lubridate::week (date.2),
          # Add a random variable for the first sighting date
          date.random = (as.numeric (date.1) - as.numeric (min (date.1))) / 7) %>%
  # Populatwe with shark details
  inner_join (ws.tags %>% select (id, sex, size), 
              by = "id")

saveRDS(PAEnc, "./data/processed/probability_visual_detection.rds")
