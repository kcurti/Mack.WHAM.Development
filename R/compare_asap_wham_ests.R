# Function to create plots comparing a WHAM run to previous ASAP output
# input data.frames that must include Estimate (SSB, F, Rect), Year, est, low, hi columns

compare_asap_wham_ests <- function(asap.ests, wham.ests, output.dir) 
{
  # asap.ests <- mt2023.ests
  # wham.ests <- m0.ests
  # output.dir <- file.path(m0.dir, "Comparison.figures")
  
  # Create output directory if doesn't already exist
  if(!dir.exists(output.dir)) {dir.create(output.dir)}
  
  # Combine asap and wham estimates
  combined.ests <- bind_rows(ASAP = asap.ests, 
                             WHAM = wham.ests, 
                             .id = "Model")
  
  # Plot
  windows()
  # par(mfrow=c(3,1))
  combined.ests %>% ggplot(aes(Year, est)) +
    geom_line(aes(color=Model)) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill=Model), alpha = 0.2, linetype='blank') +
    xlab("Year") +
    ylab(" ") + 
    facet_wrap(~Estimate, scales='free', ncol = 1)
  ggsave(file.path(output.dir,'Estimates_Comparison.png'),device='png')
  
}

