# Fit hindcast model to data, dropping specified indices and years of data specified by the peel
fit_hindcast <- function(model, peel, drop){
  temp = list(data = model$env$data, par = model$parList, map = model$env$map, 
              random = unique(names(model$env$par[model$env$random])))
  nyrs = temp$data$n_years_model
  temp$data$use_indices[(nyrs - peel + 1):nyrs, drop$indices] = 0
  temp$data$use_index_paa[(nyrs - peel + 1):nyrs, drop$index_paa] = 0
  if (temp$data$Ecov_how == 1){
    temp$data$Ecov_use_obs[(nyrs - peel):nyrs,] <- 0  
  }
  
    mod <- fit_wham(temp, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = T, do.sdrep = F)
  
  return(mod)
}