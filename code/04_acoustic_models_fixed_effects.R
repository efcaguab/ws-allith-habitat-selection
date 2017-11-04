library (gamm4)
library(foreach)
library(magrittr)
library(optparse)
library(doMC)
library(dplyr)

option_list = list(
  make_option(c("-w", "--ncores"), type="integer", default=1, 
              help="number of cores", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

registerDoMC(cores = opt$ncores)

PADet <- readRDS("./data/processed/probability_acoustic_detection.rds")

# FIXED EFFECTS -----------------------------------------------------------

# We define the models assuming that the best random effect includes the shark id

v <- c("nStations_inshore", "nStations_offshore", "sex", "size")
s <- 1:4

f <- foreach(i=4:1, .combine = c) %do% {
  combn(v, i) %>%
    apply(2, paste, collapse = " + ") %>%
    paste("present ~ s(week.2, bs = 'cc') + s(lag)", ., sep = " + ")
}

models.acoustic.fixed <- foreach(i=1:length(f)) %dopar% {
  try({
    gamm4 (as.formula(f[i]), family = "binomial", data = PADet, random= ~(1|ecocean))
  })
}

saveRDS (models.acoustic.fixed, "./data/processed/models_acoustic_fixed.rds")
