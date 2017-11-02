library (magrittr)
library (VTrack)
library(tibble)
library(ggplot2)
library(dplyr)

# READ FILES --------------------------------------------------------------

# Read CSV detection file and process it with VTrack
AL.DETECTIONS <- ReadInputData (
  read.csv ("./data/raw/AllDetections_Lith.csv"),
  iHoursToAdd = 3) %>% as_tibble()
AL.DETECTIONS$DATETIME <- as.POSIXct (
  as.POSIXlt (AL.DETECTIONS$DATETIME, tz="Asia/Riyadh"))

RECEIVER.EVENTS <- read.csv ("./data/raw/AllEvents_Lith.csv")
ARRAY.EVENTS <- read.csv ("./data/raw/ArrayEvents_Lith.csv")
WS.TAGS <- read.csv ("./data/raw/WSTags_Lith.csv")


# CORRECT TIME DRIFT ------------------------------------------------------

# To be done by Jesse
names (RECEIVER.EVENTS) <- c ("DATETIME", "RECEIVERID", "DESC", "DATA", "UNITS")
RECEIVER.EVENTS$DATETIME <- as.POSIXct (RECEIVER.EVENTS$DATETIME, 
                                        format = "%m/%d/%Y %H:%M", tz ="UTC")
RECEIVER.EVENTS$DATETIME <- as.POSIXct (
  as.POSIXlt (RECEIVER.EVENTS$DATETIME, tz="Asia/Riyadh"))
PC.TIMES <- RECEIVER.EVENTS [RECEIVER.EVENTS$DESC == "PC Time", c (1, 2, 4)]
# We manually checked that all PC times are in the time-zone GMT+3 so we can go ahead and convert the PC times to POSIXct class
PC.TIMES$DATA <- as.POSIXct (
  substr (as.character (PC.TIMES$DATA), 1, 19), tz="Asia/Riyadh") 

PC.TIMES %<>% as.tibble() %>%
  mutate(delay = difftime(DATA, DATETIME, units = "mins"), 
         delay = as.numeric(delay))

# filter(PC.TIMES, abs(delay) > 60) %>% arrange(delay) %T>% View %>% write.csv("problematic-pc-times.csv")

# # Plots to check for time zone and computer time mistakes
# ggplot(PC.TIMES) + 
#   geom_line (aes(x = DATA, y = delay)) + 
#   facet_wrap(~ RECEIVERID)


# ASSIGN DETECTIONS TO STATIONS -------------------------------------------

# Read and organize events (retrievals/deployments) data
ARRAY.EVENTS <- ARRAY.EVENTS %>% as_tibble () %>%
  mutate(EVENT = toupper(EVENT), 
         STATION = COMMENTS) %>%
  filter (EVENT == "DEP" | EVENT == "RET") %>%
  mutate (DATETIME = as.POSIXct (DATE, format = "%m/%d/%Y %H:%M", tz = "Asia/Riyadh"),
          STATIONNAME = as.factor (STATION),
          RECEIVERID = REC) %>%
  select (DATETIME, STATIONNAME, EVENT, RECEIVERID)

# Read stations file and assign detections to stations
STATIONS <- read.csv ("./data/raw/Stations_Lith.csv")
names(STATIONS) <- toupper(names(STATIONS)) 
STATIONS %<>%
  mutate(STATION = COMMENTS)
# Assign station and location

for (i in 1:nrow (ARRAY.EVENTS)){  # For each event
  # message(i)
  # If is a deployment change the station for the future
  if (ARRAY.EVENTS$EVENT[i] == "DEP"){  
    replace.index <- (as.character (ARRAY.EVENTS$RECEIVERID[i]) == as.character(AL.DETECTIONS$RECEIVERID)) & (AL.DETECTIONS$DATETIME >= ARRAY.EVENTS$DATETIME[i]) 
    # Include station
    AL.DETECTIONS$STATIONNAME [replace.index] <- as.character (ARRAY.EVENTS$STATIONNAME[i])
  }
  # If is a retrieval delete data for the future
  else {  
    replace.index <- (as.character (ARRAY.EVENTS$RECEIVERID[i]) == as.character(AL.DETECTIONS$RECEIVERID)) & 
      (AL.DETECTIONS$DATETIME >= ARRAY.EVENTS$DATETIME[i]) 
    AL.DETECTIONS$STATIONNAME[replace.index] <- NA
  }
}

# # check that discarded detections are actually artifacts
# AL.DETECTIONS %>% filter(STATIONNAME == "") %>% 
#   arrange(DATETIME, RECEIVERID) %>% 
#   filter(DATETIME > as.POSIXct("2010-01-01")) %>%
#   ggplot(aes(x = trunc(DATETIME, "days"))) + 
#   geom_histogram(binwidth = 3600*24*30)

#close (pb)
# Delete detections outside valid intervals
AL.DETECTIONS <- AL.DETECTIONS %>% filter (STATIONNAME != "Unknown", !is.na (STATIONNAME), STATIONNAME != "")

# FILTER DETECTIONS -------------------------------------------------------

# Read file with Whale Shark Tag lists
WS.TAGS$DATE <- as.POSIXct (WS.TAGS$DATE, format="%m/%d/%Y", tz = "Asia/Riyadh")
WS.TAGS$NAME <- WS.TAGS$COMMENT <- WS.TAGS$SHARK <- NULL

# Select only whale shark detections 
DET.WS <- AL.DETECTIONS[!is.na (match (AL.DETECTIONS$TRANSMITTERID, WS.TAGS$TRANSMITTERID)), ]

# Remove detections before 48 hours after tagging date
for (i in 1: nrow(WS.TAGS)){
  next2.days <- WS.TAGS$DATE + 60 * 60 * 24 * 2  # Add two days
  replace.index <- as.character (DET.WS$TRANSMITTERID) == as.character (WS.TAGS$TRANSMITTERID[i])
  # Delete rows that are in the tagging day
  DET.WS <- subset (DET.WS, ! (replace.index & (DET.WS$DATETIME < next2.days[i]))) 
}

# Remove unused tags from the factor list
DET.WS$TRANSMITTERID <- factor (DET.WS$TRANSMITTERID)

save (DET.WS, WS.TAGS, ARRAY.EVENTS, file ="./data/processed/AllDetections.RData")
