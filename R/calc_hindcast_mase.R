calc_hindcast_mase <- function(model, peel.max, horizon, drop, indices2calc = NULL, dir_figures) {
  hindcast <- vector("list", peel.max)
  for(p in 1:peel.max){
    print(paste0("hindcast peel ", p))
    
    hindcast[[p]] <- fit_hindcast(model, 
                                        peel=p, 
                                        drop=list(indices = drop$indices, 
                                                  index_paa = drop$index_paa))
    
  }
  
  # calculate prediction error and MASE
  
  n.h <- length(horizon) # number of prediction horizons
  n.p <- length(hindcast) # number of peels
  nyrs <- model$env$data$n_years_model
  
  # Need to drop years that were not surveyed. Also dropping zero observations
  # although that could change in the future.
  agg_indices <- model$env$data$agg_indices
  agg_indices[which(agg_indices < 0 | agg_indices == 0)] <- NA
  
  # 1. calculate predictions and residuals
  preds.colnames <- c("hindcast","horizon","peel", "observed", "predicted", "predicted_naive",
                      "err","err_naive")
  preds <- as.data.frame(matrix(NA, ncol = length(preds.colnames), nrow = 0))
  colnames(preds) <- preds.colnames
  
  for(h in 1:n.h){
    for(p in n.p:horizon[h]){
      year <- hindcast[[p]]$env$data$year1_model + nyrs - 1 - p + horizon[h]
      index <- paste0("index-", drop$indices)
      predicted <- exp(hindcast[[p]]$rep$pred_log_indices[nyrs-p+horizon[h], drop$indices])
      predicted_naive <- agg_indices[nyrs-p, drop$indices]
      observed <- agg_indices[nyrs-p+horizon[h], drop$indices]
      conv <- check_convergence(hindcast[[p]], ret = T)
      if(conv$convergence == 0){ #& conv$is_sdrep & isFALSE(conv$na_sdrep)){
        conv$converged <- TRUE
      } else conv$converged <- FALSE
      preds <- rbind(preds, data.frame(#hindcast=names(hindcast)[hc], 
        horizon=horizon[h], peel=p, year,
        index, observed, predicted, predicted_naive,
        err=observed-predicted, 
        err_naive=observed-predicted_naive,
        converged = conv$converged))
    }
  }
  
  if(is.null(indices2calc)){
    indices2calc <- 1:model$env$data$n_indices
  }
  
  preds <-
    preds %>%
    filter(index %in% paste0("index-", indices2calc)) 
  
  
  
  
  # Plot predicted vs observed
  pred_vs_obs <- 
    data.frame(year = model$years, agg_indices) %>%
    gather(index, index_value, -year) %>%
    mutate(index = paste0("index-", substr(index, 2, 3))) %>%
    filter(index %in% paste0("index-", indices2calc)) %>%
    left_join(preds) %>%
    mutate(`Terminal year` = as.factor(year - horizon))
  
  p <- ggplot(pred_vs_obs,
         aes(x = year)) +
    geom_line(aes(y = index_value), color = "black") +
    geom_point(aes(y = index_value), color = "black") +
    geom_line(data = pred_vs_obs %>% filter(!is.na(predicted)),
              aes(y = predicted, color = `Terminal year`)) +
    geom_point(data = pred_vs_obs %>% filter(!is.na(predicted)),
               aes(y = predicted, color = `Terminal year`)) +
    facet_wrap(~index, ncol = 1, scales = "free") +
    theme_bw() +
    xlab("Year") +
    ylab("Index value")
  
  print(p)
  
  ggsave(plot = p,
         paste0(dir_figures, "/predvobs_", 
                model$model_name, ".png"), 
         h = 6, w = 7)
  
  
  # Plot MASE
  mase <-
    preds %>%
    filter(if_all(c(err, err_naive), ~ !is.na(.))) %>% # exclude NA predictions
    group_by(index, horizon) %>%
    summarise(mase = mean(abs(err)) / mean(abs(err_naive)))
  
  
  p <- ggplot(mase, 
         aes(x = horizon, y = mase)) +
    geom_line() +
    geom_point() +
    facet_wrap(~index) +
    geom_hline(yintercept = 1) +
    theme_bw() +
    ylab("MASE") +
    xlab("Horizon") +
    ylim(c(0, NA))
  
  print(p)
  
  ggsave(plot = p, paste0(dir_figures, "/mase_", 
                model$model_name, ".png"), 
         h = 4, w = 5)
  
  # Make table of MASE over all horizons
  mase_allh <-
    preds %>%
    filter(if_all(c(err, err_naive), ~ !is.na(.))) %>% # exclude NA predictions
    group_by(index) %>%
    summarise(mase = mean(abs(err)) / mean(abs(err_naive))) %>%
    print
  
  
  # Calculate convergence rate of hindcasts
  conv_rate <-
    preds %>%
    distinct(peel, converged) %>%
    summarise(`convergence rate` = mean(converged)) %>%
    print()
  
  return(preds)
}

