library (gamm4)
library(foreach)
library(magrittr)
library(optparse)
library(doMC)
library(dplyr)

option_list = list(
  make_option(c("-nc", "--ncores"), type="integer", default=1, 
              help="number of cores", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

registerDoMC(cores = opt$ncores)

PAEnc <- readRDS("./data/processed/probability_visual_detection.rds")

registerDoMC(cores = opt$ncores)

# RANDOM EFFECTS ----------------------------------------------------------

# Choose the random structure
ma.r.01 <- . %>% gamm4 (present ~ s (week.2, k = 4) + s (lag) + hours + sex + size, family = "binomial", data = .)
ma.r.02 <- . %>% gamm4 (present ~ s (week.2, k = 4) + s (lag) + hours + sex + size, family = "binomial", data = ., random= ~(1|id))
# ma.r.03 <- . %>% gamm4 (present ~ s (week.2, bs = "cc") + s (lag) + nStations_inshore + nStations_offshore + sex + size, family = "binomial", data = ., random= ~(1|ecocean/date.random))

# Join instruction together so that they can be evaluated in parallels
ma.r <- c (
  ma.r.01, 
  ma.r.02
  # ma.r.03
)
# Evaluate the models
models.visual.random <- foreach (i = 1:length(ma.r)) %dopar% ma.r[[i]](PAEnc)
# Save them so that we don't have to wait for so long each time...
saveRDS (models.visual.random, "./data/processed/models_visual_random.rds")

