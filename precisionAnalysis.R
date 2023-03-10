## This script will run a precision analysis to help with sample size planning

library(ggplot2)

# define function for computing margin of error (MOE) assuming a 95% confidence interval
marginOfError <- function(P, Z = 1.96, N){
  MOE <- Z*sqrt(P*(1-P)/N)
  return(MOE)
}

# define function to plot a precision curve for given proportion and sample size (95% confidence interval assumed).
precisionCurve <- function(P, Z = 1.96, N){
  vector <- NA # pre-load vector variable
  for(N in seq(1:N)){
    vector <- c(vector, Z*sqrt(P*(1-P)/N)) # add new value to vector
  }
  
  vector <- vector[!is.na(vector)] # remove the NA value added when variable was pre-loaded
  
  # build plot
  plot <- ggplot(data = data.frame(sampleSize = seq(1:N), precision = vector), aes(x = sampleSize, y = precision)) +
    geom_line(colour = 'black', size = .75) +
    theme_classic() +
    ylab('Margin of error') +
    xlab('Sample size') +
    scale_x_continuous(expand = c(0.0, 0), limits = c(0,N)) +
    scale_y_continuous(expand = c(0.0, 0), limits = c(0,1), breaks = seq(0,1,0.1)) +
    theme(panel.grid.major = element_line(colour="grey97", size=0.5))
  
  return(plot)
}

# compute the margin-of-error (moe) for a sample size of 100 using 0.5 as the expected proportion 
# NB using 0.5 is appropriate in situations where you do not know what to expect - this is the most conservative approach because it leads to a maximal sample size estimate. See Gelman & Hill (2006, p. 442).

expectedProportion <- 0.5
sampleSize <- 100

moe <- marginOfError(P = expectedProportion, N = sampleSize)

# plot a precision curve showing margin of error for a 95% confidence interval as a function of sample size (we'll use 300 to illustrate the diminishing returns) assuming a proportion of 0.5. 
# we'll also annotate the plot with the parameters above
precisionCurve(P = expectedProportion, N = sampleSize + 250) +
  annotate("segment", x = sampleSize, xend = sampleSize, y = 0, yend = moe, colour = "purple", size=1, alpha=0.6) +
  annotate("segment", x = 0, xend =  sampleSize, y = moe, yend = moe, colour = "purple", size=1, alpha=0.6) +
  annotate("label", x = 7.5, y = moe, colour = "purple", size=3, alpha=1, label = round(moe,2)) +
  annotate("label", x = sampleSize, y = 0.05, colour = "purple", size=3, alpha=1, label = sampleSize) 