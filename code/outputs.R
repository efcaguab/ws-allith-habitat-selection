library(dplyr)

ma <- readRDS("./data/processed/models_acoustic_fixed.rds")
mv <- readRDS("data/processed/models_visual_fixed.rds")


aictable <- . %>% plyr::ldply(function(x){
  data_frame(f = as.character(formula(x$gam)[3]),
             AIC = AIC(x$mer))
})

ma[[aictable(ma)$AIC %>% which.min()]]

best_models <- list(
  acoustic = ma[[aictable(ma)$AIC %>% which.min()]], 
  visual = mv[[aictable(mv)$AIC %>% which.min()]]
)

fits <- foreach(i=1:length(best_models), .combine = rbind) %do% {
  y <- best_models[[i]]
  intercept <- y$gam$coefficients[1]
  a <- plot(y$gam, pages = 1)
  plyr::ldply(a, function(x){
    data.frame(x = x$x, y = x$fit, se = x$se, var = x$xlab)
  }) %>%
    mutate(intercept = intercept, 
           type = names(best_models)[i]) 
}

fits %>% 
  ggplot(aes(x = x, y = exp(y), fill = type)) +
  geom_hline(yintercept = 1, linetype = 2, colour = "gray") +
  geom_ribbon(aes(ymin = exp(y-se*2), ymax = exp(y + se*2)), alpha = 0.2) +
  coord_cartesian(ylim = c(0, 8)) +
  geom_line(aes( colour = type)) +
  xlab("") + ylab("odds") +
  facet_wrap(~var, scales = "free_x")


fits %>% 
  ggplot(aes(x = x, y = plogis(y + intercept), fill = type)) +
  # geom_hline(yintercept = 1, linetype = 2, colour = "gray") +
  geom_ribbon(aes(ymin = plogis(y - se*2 + intercept), ymax = plogis(y + se*2 + intercept)), alpha = 0.2) +
  coord_cartesian(ylim = c(0, 0.00005)) +
  geom_line(aes( colour = type)) +
  xlab("") + ylab("resigting probability") +
  facet_wrap(~var, scales = "free_x")



