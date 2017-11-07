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
