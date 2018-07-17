library(dplyr)
library(foreach)
library(ggplot2)

ma <- readRDS("./data/processed/results_window6/models_acoustic_fixed.rds")
mv <- readRDS("data/processed/results_window6/models_visual_fixed.rds")

remove_errors <- . %>% plyr::llply(function(x){
  if(any(class(x) == "try-error")){
    return(NULL)
  } else (
    return(x)
  )
})

ma <- remove_errors(ma)
ma[sapply(ma, is.null)] <- NULL

mv <- remove_errors(mv)
mv[sapply(mv, is.null)] <- NULL

aictable <- . %>% plyr::ldply(function(x){
  data_frame(f = as.character(formula(x$gam)[3]),
             AIC = AIC(x[[1]]))
})

# ma[[aictable(ma)$AIC %>% which.min()]]

best_models <- list(
  acoustic = ma[[aictable(ma)$AIC %>% which.min()]], 
  visual = mv[[aictable(mv)$AIC %>% which.min()]]
)

fits <- foreach(i=1:length(best_models), .combine = rbind) %do% {
  y <- best_models[[i]]
  intercept <- y$gam$coefficients[1]
  size <- as.numeric(y$gam$coefficients["size"])
  if("nStations_inshore" %in% names(y$gam$coefficients)){
    effort <- as.numeric(y$gam$coefficients["nStations_inshore"])
  } else {
    effort <- 0
  }
  a <- plot(y$gam, pages = 1)
  plyr::ldply(a, function(x){
    data.frame(x = x$x, y = x$fit, se = x$se, var = x$xlab)
  }) %>%
    mutate(intercept = intercept, 
           size = size, 
           effort = effort,
           type = names(best_models)[i]) 
}

odds_plots <- fits %>% 
  mutate(x = if_else(var == "lag_log", exp(x)/365, x)) %>%
  plyr::dlply("var", function(x){
    x %>% ggplot(aes(x = x, y = exp(y), fill = type)) +
    geom_hline(yintercept = 1, linetype = 2, colour = "gray") +
    geom_ribbon(aes(ymin = exp(y-se*2), ymax = exp(y + se*2)), alpha = 0.2) +
    # coord_cartesian(ylim = c(0, 13)) +
    geom_line(aes( colour = type)) +
    xlab("") + ylab("odds") +
    # facet_wrap(~var, scales = "free_x") +
      scale_fill_brewer(palette = "Set1", name = "") +
      scale_color_brewer(palette = "Set1", name = "") +
      theme_bw()
})

odds_plots[[1]] <- odds_plots[[1]] +
  labs(subtitle = "Seasonality", 
       x = "week of the year")

odds_plots[[2]] <-  odds_plots[[2]] +
  labs(subtitle = "Lag", 
       x = "years")

cowplot::plot_grid(plotlist = odds_plots, ncol = 1)

ggsave("model_output.pdf", width = 7, height = 7/sqrt(2))

load("data/processed/AllDetections.RData")
pvd <- readRDS("data/processed/results_window6/probability_visual_detection.rds")
pad <- readRDS("data/processed/results_window6/probability_acoustic_detection.rds")


prediction_df <- expand.grid(date = seq(min(DET.WS$DATETIME), 
                                        max(DET.WS$DATETIME), by ="week"), 
            type = c("acoustic", "visual"),
            size = 4, 
            nStations_inshore = 30,
            nStations_offshore = 34) %>% 
  mutate(week.2 = lubridate::week(date), 
         lag = difftime(date, min(date), units = "days"),
         lag = as.numeric(lag),
         lag_log = log(as.numeric(lag))) %>%
  filter(lag > 0)

types <- c("acoustic", "visual")

prediction_df <- foreach(i=1:2, .combine = bind_rows) %dopar% {
  m <- best_models[[types[i]]]$gam
  d <- prediction_df %>% filter(type == types[i])
  p <- predict(m, d, se.fit = T)
  d %>% 
    mutate(y = p$fit, 
               se = p$se.fit)
}


prediction_df %>% 
  # filter(!(type == "visual" & (week.2 < min(pvd$week.2) | week.2 > max(pvd$week.2)))) %>%
  mutate(y = if_else(!(type == "visual" & (week.2 < min(pvd$week.2) | week.2 > max(pvd$week.2))),
                          y, NA_real_)) %>%
  # mutate(prob = y + intercept + size * 7 + effort * 20) %>%
  ggplot(aes(x = date, y = plogis(y), fill = type)) +
  # geom_hline(yintercept = 1, linetype = 2, colour = "gray") +
  geom_ribbon(aes(ymin = plogis(y-se*2), ymax = plogis( y + se*2)), alpha = 0.2) +
  # coord_cartesian(ylim = c(0, 0.00005)) +
  geom_line(aes( colour = type)) +
  xlab("date") + ylab("resighting probability") +
  labs(title = "Acoustic and visual resighting probability of a hypothetical shark",
       subtitle = "Hypothetical individual first observed during the first sampling season",
       caption = "Assumes maximum inshore acoustic coverage and mean observed individual size")+
  scale_fill_brewer(palette = "Set1", name = "") +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_bw()
  # facet_wrap(~var, scales = "free_x")

ggsave("probability_prediction.pdf", w = 7, height = 7/sqrt(2))




