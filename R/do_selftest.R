# This runs the self-test and makes plots

do_selftest <- function(mod, n_sim, n_sim2plot, dir_figures) {
  set.seed(123)
  sim_inputs <- replicate(n_sim, sim_fn(mod), simplify=F)
  res = list(reps = list(), par.est = list(), par.se = list(), 
             adrep.est = list(), adrep.se =list())
  #fits = list()
  j <- 1
  for(i in 1:length(sim_inputs)){
    cat(paste0("sim ", i, " of ", n_sim, "\n"))
    sim_inputs[[i]]$SIM_ID <- i
    tfit <- NULL
    tryCatch(expr = {tfit <- fit_wham(sim_inputs[[i]], do.osa = F,
                                     do.retro = F, MakeADFun.silent = T, do.sdrep = F);},
             error = function(e){
               message("An error occurred:\n", e)
             },
             finally = {
               if (is.null(tfit)) {

               } else {
                 
                 conv <- check_convergence(tfit, ret = T)
                 if(conv$convergence == 0){ #& conv$is_sdrep & isFALSE(conv$na_sdrep)){
                   conv$converged <- TRUE
                 } else conv$converged <- FALSE
                 res$reps[[j]] = tfit$rep
                 res$reps[[j]]$converged <- conv$converged
                 res$reps[[j]]$SIM_ID <- i
                 res$reps[[j]]$years <- tfit$years
                 j <- j + 1
               }

             })
    
    
    #fits[[i]] <- tfit
    #res$par.est[[i]] = as.list(tfit$sdrep, "Estimate")
    #res$par.se[[i]] = as.list(tfit$sdrep, "Std. Error")
    #res$adrep.est[[i]] = as.list(tfit$sdrep, "Estimate", report = TRUE)
    #res$adrep.se[[i]] = as.list(tfit$sdrep, "Std. Error", report = TRUE)
  }
  
  
  true_ssb = map_df(sim_inputs, function(x) data.frame(SIM_ID = x$SIM_ID, 
                                                       YEAR   = x$years, 
                                                       VAR    = "SSB", 
                                                       TRU    = x$data$SSB))
  true_f  = map_df(sim_inputs, function(x) data.frame(SIM_ID = x$SIM_ID, 
                                                      YEAR   = x$years, 
                                                      VAR    = "F", 
                                                      TRU    = x$data$`F`))
  
  true_r  = map_df(sim_inputs, function(x) data.frame(SIM_ID = x$SIM_ID, 
                                                      YEAR   = x$years, 
                                                      VAR    = "R", 
                                                      TRU    = x$data$NAA[,1]))
  
  est_ssb  = map_df(res$reps, function(x) data.frame(SIM_ID = x$SIM_ID,
                                                     YEAR   = x$years,
                                                     VAR    = "SSB",
                                                     EST    = x$SSB))
  est_f  = map_df(res$reps, function(x) data.frame(SIM_ID = x$SIM_ID,
                                                   YEAR   = x$years,
                                                   VAR    = "F",
                                                   EST    = x$`F`))
  
  est_r  = map_df(res$reps, function(x) data.frame(SIM_ID = x$SIM_ID,
                                                   YEAR   = x$years,
                                                   VAR    = "R",
                                                   EST    = x$NAA[,1]))
  
  conv_df <- map_df(res$reps, function(x) data.frame(SIM_ID = x$SIM_ID,
                                                     converged = x$converged))
  
  # Join all sims
  sim_out <- 
    left_join(true_ssb, est_ssb) %>%
    bind_rows({left_join(true_f, est_f)}) %>%
    bind_rows({left_join(true_r, est_r)}) %>%
    left_join(conv_df)
  
  selffits_converged <- sim_out %>% filter(converged)
  
  (convergence_rate <- length(unique(selffits_converged$SIM_ID))/n_sim)
  
  bias <- 
    selffits_converged %>%
    #filter(VAR == "F") %>%
    # mutate(EST = EST.1 + EST.2,
    #        TRU = TRU.1 + TRU.2) %>%
    select(SIM_ID, YEAR, VAR, TRU, EST) %>%
    bind_rows({selffits_converged %>% filter(VAR == "SSB") %>%
        select(SIM_ID, YEAR, VAR, TRU, EST)}) %>%
    mutate(percent_err = 100 * (EST - TRU)/TRU) %>%
    group_by(VAR, YEAR) %>%
    summarise(mpe = mean(percent_err),
              se  = sd(percent_err)/n()) %>%
    mutate(mpe_90lo = mpe - 1.645 * se,
           mpe_90hi = mpe + 1.645 * se)
  
  (bias_mean <- 
      bias %>%
      group_by(VAR) %>%
      summarise(`Mean Percent Bias` = round(mean(mpe),2)))
  
  write.csv(bias_mean, 
            file = paste0(dir_figures, "/self-test mean bias ", deparse(substitute(mod)), ".csv"), 
            row.names = F)
  
  # Plot example true vs estimated
  p <- ggplot(sim_out %>%
              select(-converged) %>%
              gather(SOURCE, VALUE, -SIM_ID, -YEAR, -VAR) %>%
              filter(SIM_ID %in% unique(SIM_ID)[1:n_sim2plot]) %>%
              mutate(SIM_ID = paste0("Simulation ", SIM_ID)),
         aes(x = YEAR, y = VALUE, color = SOURCE)) +
    geom_line() +
    facet_grid(VAR~SIM_ID, scales = "free_y") +
    theme_bw() +
    xlab("Year") +
    ylab("Value") +
    ggtitle(paste0("Example estimated vs. true (", 
            deparse(substitute(mod)),
            ")"))
  
  print(p)
  
  ggsave(p, filename = paste0(dir_figures, "/selftest_est_vs_tru_examples_", 
                deparse(substitute(mod)), ".png"), 
         h = 6, w = 7)
  
  
  # Plot mean true vs mean estimated
  selffits_converged_mean <-
    selffits_converged %>%
    group_by(VAR, YEAR) %>%
    summarise(`Mean estimate w/90% CI` = mean(EST),
              True = mean(TRU),
              EST_90HIGH = `Mean estimate w/90% CI` + 1.645 * sd(EST),
              EST_90LOW  = `Mean estimate w/90% CI` - 1.645 * sd(EST)) %>%
    gather(output, value, -VAR, -YEAR, -EST_90HIGH, -EST_90LOW)
  
  p <-
    ggplot(selffits_converged_mean,
           aes(x = YEAR, y = value, color = output)) +
    geom_line() +
    scale_color_manual(values = c("blue", "black")) +
    geom_ribbon(aes(x = YEAR, ymin = EST_90LOW, ymax = EST_90HIGH), 
                alpha = 0.1, fill = "blue", color = NA) +
    facet_wrap(~VAR, scales = "free", ncol = 1) +
    theme_bw() +
    theme(legend.title = element_blank()) +
    ggtitle(paste0("Mean estimated vs. true (", 
                   deparse(substitute(mod)),
                   ")")) +
    ylab("") +
    xlab("Year")
  
  print(p)
  
  ggsave(p, filename = paste0(dir_figures, "/selftest_est_vs_tru_mean_", deparse(substitute(mod)), ".png"), 
         h = 6, w = 7)
  
  
  # Plot overall bias
  p <- ggplot(bias, aes(x = YEAR, y = mpe)) +
    geom_line() +
    geom_ribbon(aes(ymin = mpe_90lo, ymax = mpe_90hi), alpha = 0.2) +
    geom_hline(yintercept = 0) +
    geom_hline(data = bias_mean, 
               aes(yintercept = `Mean Percent Bias`),
               color = "red") +
    facet_wrap(~VAR) +
    theme_bw() +
    ylab("Mean percent error (Est. - True)") +
    xlab("Year") +
    ggtitle(paste0("Self-test mean bias (", deparse(substitute(mod)), ")"))
  
  print(p)
  
  ggsave(p, filename = paste0(dir_figures, "/selftest_bias_", 
                              deparse(substitute(mod)), ".png"), 
         h = 6, w = 7)
  
  
  return(sim_out)
}
