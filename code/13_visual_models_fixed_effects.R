suppressMessages({
  suppressPackageStartupMessages({
    library (mgcv)
    library(foreach)
    library(magrittr)
    library(optparse)
    library(doMC)
    library(dplyr)
  })
})

option_list = list(
  make_option(c("-w", "--ncores"), type="integer", default=1, 
              help="number of cores", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

registerDoMC(cores = opt$ncores)

PAEnc <- readRDS("./data/processed/probability_visual_detection.rds") %>%
  mutate(lag_log = log(lag))

# FIXED EFFECTS -----------------------------------------------------------

# We define the models assuming that the best random effect includes the shark id

v <- c("hours", "sex", "size")
s <- 1:3

basic <- paste0("present ~ s(week.2, k = ", floor(n_distinct(PAEnc$week.2)/2), ") + s(lag_log)")

f <- foreach(i=3:1, .combine = c) %do% {
  combn(v, i) %>%
    apply(2, paste, collapse = " + ") %>%
    paste(basic, ., sep = " + ")
} %>%
  c(basic)

models.visual.fixed <- foreach(i=1:length(f)) %dopar% {
  try({
    gamm(as.formula(f[i]), family = "binomial", data = PAEnc, random= list(id = ~1), method = "ML", niterPQL = 24)
  })
}

saveRDS (models.visual.fixed, "./data/processed/models_visual_fixed.rds")
