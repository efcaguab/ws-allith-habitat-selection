suppressMessages({
  suppressPackageStartupMessages({
    library(gamm4)
    library(foreach)
    library(magrittr)
    library(optparse)
    library(doMC)
    library(dplyr)
  })
})

option_list = list(
  make_option(c("-nc", "--ncores"), type="integer", default=1, 
              help="number of cores", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

registerDoMC(cores = opt$ncores)

PADet <- readRDS("./data/processed/probability_acoustic_detection.rds") %>%
  mutate(lag_log = log(lag))


# RANDOM EFFECTS ----------------------------------------------------------

# Choose the random structure
ma.r.01 <- . %>% gamm4 (present ~ s (week.2, bs = "cc") + s (lag_log) + nStations_inshore + nStations_offshore + sex + size, family = "binomial", data = ., REML = TRUE)
ma.r.02 <- . %>% gamm4 (present ~ s (week.2, bs = "cc") + s (lag_log) + nStations_inshore + nStations_offshore + sex + size, family = "binomial", data = ., random= ~(1|id), REML = TRUE)

# ma.r.03 <- . %>% gamm4 (present ~ s (week.2, bs = "cc") + s(lag_log) + nStations_inshore + nStations_offshore + sex + size, family = "binomial", data = ., random= ~(1|id))

# ma.r.03 <- . %>% gamm4 (present ~ s (week.2, bs = "cc") + s (lag) + nStations_inshore + nStations_offshore + sex + size, family = "binomial", data = ., random= ~(1|ecocean/date.random))

# Join instruction together so that they can be evaluated in parallel
ma.r <- c (
  ma.r.01, 
  ma.r.02
  # ma.r.03
  )
# Evaluate the models
models.acoustic.random <- foreach (i = 1:length(ma.r)) %dopar% ma.r[[i]](sample_frac(PADet, 1))
# Save them so that we don't have to wait for so long each time...
saveRDS (models.acoustic.random, "./data/processed/models_acoustic_random.rds")

# models.acoustic.random <- readRDS("./data/processed/models_acoustic_random.rds")

