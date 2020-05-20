# precision analysis for proportions

# NB For proportions, using 0.5 as the expected proportion is appropriate in situations where you do not know what to expect - this is the most conservative approach because it leads to a maximal sample size estimate. See Gelman & Hill (2006, p. 442).
                                                                                                                                                                                                                                          
library(tidyverse)

# formula for margin of error (MOE) calculation (95% confidence interval assumed)
marginOfError <- function(P, Z = 1.96, N){
  MOE <- Z*sqrt(P*(1-P)/N)
  return(MOE)
}

# example use - an MOE for a given proportion and sample size
marginOfError(P = .5, N = 100)

# formula for sample size calculation (95% confidence interval assumed)
sampleSize <- function(P, Z = 1.96, MOE){
  N <- P*(1-P)*(Z/MOE)^2
  return(N)
}

# alternative function for sample size calculation (can use as a sanity check)
# samplingbook::sample.size.prop(e = 0.05, P = 0.5)

# example use - calculate a sample size for a given effect size and MOE
sampleSize(P = .5, MOE = .1)

# Formula to plot a precision curve for given proportion and sample size (95% confidence interval assumed).
precisionCurve <- function(P, Z = 1.96, N){
  vector <- NA # pre-load vector variable
  for(N in seq(1:N)){
    vector <- c(vector, Z*sqrt(P*(1-P)/N)) # add new value to vector
  }

  vector <- vector[!is.na(vector)] # remove the NA value added when variable was pre-loaded

  # build plot
  plot <- ggplot(data = data.frame(sampleSize = seq(1:N), precision = vector), aes(x = sampleSize, y = precision)) +
    geom_line(colour = 'darkblue', size = 1) +
    theme_classic() +
    ylab('Margin of error') +
    xlab('Sample size') +
    scale_x_continuous(expand = c(0.01, 0), limits = c(0,N)) +
    scale_y_continuous(expand = c(0.01, 0), limits = c(0,1))

  return(plot)
}

# example - plot a precision curve
precisionCurve(P = .05, N = 330)
