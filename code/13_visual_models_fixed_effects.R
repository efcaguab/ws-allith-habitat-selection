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

PAEnc <- readRDS("./data/processed/probability_visual_detection.rds")

# FIXED EFFECTS -----------------------------------------------------------

# We define the models assuming that the best random effect includes the shark id

v <- c("hours", "sex", "size")
s <- 1:3

basic <- "present ~ s(week.2, bs = 'cc', k = 4) + s(lag, k = 4)"

f <- foreach(i=3:1, .combine = c) %do% {
  combn(v, i) %>%
    apply(2, paste, collapse = " + ") %>%
    paste(basic, ., sep = " + ")
} %>%
  c(basic)

models.visual.fixed <- foreach(i=1:length(f)) %dopar% {
  try({
    gamm4 (as.formula(f[i]), family = "binomial", data = PAEnc, random= ~(1|id))
  })
}

saveRDS (models.visual.fixed, "./data/processed/models_visual_fixed.rds")
