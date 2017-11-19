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
  make_option(c("-w", "--ncores"), type="integer", default=1, 
              help="number of cores", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

registerDoMC(cores = opt$ncores)

PADet <- readRDS("./data/processed/probability_acoustic_detection.rds") %>%
  mutate(lag_log = log(lag))

# FIXED EFFECTS -----------------------------------------------------------

# We define the models assuming that the best random effect includes the shark id

v <- c("nStations_inshore", "nStations_offshore", "sex", "size")
s <- 1:4

basic <- "present ~ s(week.2, bs = 'cc') + s(lag_log)"

f <- foreach(i=4:1, .combine = c) %do% {
  combn(v, i) %>%
    apply(2, paste, collapse = " + ") %>%
    paste(basic, ., sep = " + ")
} %>% c(basic)

dir.create("./data/processed/models_acoustic_fixed/")

for(i in 1:length(f)){
  try({
    mod <- gamm4(as.formula(f[i]), family = "binomial", data = PADet, random= ~ (1|id), REML = FALSE)
    saveRDS(mod, paste0("./data/processed/models_acoustic_fixed/model_", i, ".rds"))
    rm(mod)
  })
}


# saveRDS (models.acoustic.fixed, "./data/processed/models_acoustic_fixed.rds")
