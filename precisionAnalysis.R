# This script will run a precision analysis to help with sample size planning 
# when you wish to estimate a proportion using Wald confidence intervals

# Load libraries ---------------------------------

library(tidyverse)

# Define functions ---------------------------------

# Define function for computing margin of error (MOE) assuming a 95% confidence interval (Z = 1.96)
# If the population size is known, it can be specified and a finite population correction will be applied.
# Otherwise, if the population size in unknown, no correction is applied (the default).
marginOfError <- function(expectedProportion,criticalValue,populationSize = Inf,sampleSize){
  P <- expectedProportion
  Z <- criticalValue
  n <- sampleSize
  N <- populationSize
  MOE <- Z*sqrt(P*(1-P)/n) # compute margin of error
  if(N != Inf){ # if user has specified a population size
    FPC <- sqrt((N-n)/(N-1)) # calculate a finite population correction (FPC) factor
    MOE <- MOE*FPC # adjust the MOE with the FPC
  }
  return(MOE)
}

# define function to plot a precision curve for given proportion and sample size (95% confidence interval assumed).
precisionCurve <- function(expectedProportion,criticalValue,populationSize = Inf,sampleSize){
  sampleSizeVector <- seq(1:(sampleSize*2)) # vector of sample sizes for X axis (by default will show a curve for 2x the specified sample size)
  moeVector <- # computer a vector of margin-of-errors (MOEs)
    unlist( # map returns a list so this converts it to a vector
      map( # instead of a for loop
        .x = sampleSizeVector, # vector of values to input into function
        .f = marginOfError, # specify function. N will come from the vector above.
        expectedProportion = expectedProportion, # taking this value via precisionCurve
        criticalValue = criticalValue, # taking this value via precisionCurve
        populationSize = populationSize # taking this value via precisionCurve
      ) 
    ) 
  
  df <- data.frame(sampleSize = sampleSizeVector,precision = moeVector)
  
  # build plot
  plot <- ggplot(data = df, aes(x = sampleSize, y = precision)) +
    geom_line(colour = 'black', size = .75) +
    theme_classic() +
    ylab('Precision (margin of error)') +
    xlab('Sample size') +
    scale_x_continuous(expand = c(0.0, 0), limits = c(0,max(sampleSizeVector))) +
    scale_y_continuous(expand = c(0.0, 0), limits = c(0,1), breaks = seq(0,1,0.1)) +
    theme(panel.grid.major = element_line(colour="grey97", size=0.5))
  
  return(plot)
}

# Tests ---------------------------------

# As a sanity check on the moe formula above, moes can be checked against those produced by the samplingbook package
# NOT RUN
# library(samplingbook)
# samplingbook::sample.size.prop(
#   e <- 0.1,  # specify moe
#   P <-.5, # specify expected proportion
#   N <- Inf, # specify *population size*. NB — this is *not* the target sample size. It is Inf by default but can be changed if you know the size of the population. In which case a finite population correction will be applied.
#   level <- .95 # specify level of coverage (.95 for a 95% confidence interval)
# )

# Run a precision analysis  ---------------------------------

# Enter desired values here
expectedProportion <- 0.5 # use 0.5 for the most conservative sample size estimate (see Gelman & Hill, 2006, p. 442).
criticalValue <- 1.96 # use z = 1.96 for a 95% confidence interval
populationSize <- Inf # If population size is known, apply finite population correction, otherwise use Inf

sampleSize1 <- 100 # target sample size

# Run the code below to get the relevant margin of error
moe1 <- marginOfError(expectedProportion, criticalValue, populationSize, sampleSize1)
print(paste0(
'The margin of error of a 95% confidence interval for an expected proportion of ',expectedProportion,
' and a sample size of ',sampleSize1,' is ', round(moe1,3)))

# You can get another moe for comparison (you can obviously do this as many times as you like)

# Enter desired values here
sampleSize2 <- 50 # target sample size

moe2 <- marginOfError(expectedProportion, criticalValue, populationSize, sampleSize2)
print(paste0(
  'The margin of error of a 95% confidence interval for an expected proportion of ',expectedProportion,
  ' and a sample size of ',sampleSize2,' is ', round(moe2,3)))

# We can also plot a precision curve showing precision (margin of error) as a function of sample size.
# We'll annotate the plot with the parameters above

precisionCurve(expectedProportion, criticalValue, populationSize, sampleSize1) +
  # this code annotates the precision curve with the moes computed above
  annotate("segment", x = sampleSize1, xend = sampleSize1, y = 0, yend = moe1, colour = "purple", size=1, alpha=0.6) +
  annotate("segment", x = 0, xend =  sampleSize1, y = moe1, yend = moe1, colour = "purple", size=1, alpha=0.6) +
  annotate("label", x = 7, y = moe1, colour = "purple", size=3, alpha=1, label = round(moe1,3)) +
  annotate("label", x = sampleSize1, y = 0.02, colour = "purple", size=3, alpha=1, label = sampleSize1) + 

  annotate("segment", x = sampleSize2, xend = sampleSize2, y = 0, yend = moe2, colour = "green", size=1, alpha=0.6) +
  annotate("segment", x = 0, xend =  sampleSize2, y = moe2, yend = moe2, colour = "green", size=1, alpha=0.6) +
  annotate("label", x = 7, y = moe2, colour = "green", size=3, alpha=1, label = round(moe2,3)) +
  annotate("label", x = sampleSize2, y = 0.02, colour = "green", size=3, alpha=1, label = sampleSize2) 

# References  ---------------------------------

# Gelman, A., & Hill, J. (2007). Data analysis using regression and multilevel/hierarchical models. Cambridge University Press.
# Rothman, K. J., & Greenland, S. (2018). Planning study size based on precision rather than power. Epidemiology, 29(5), 599–603. https://doi.org/10.1097/EDE.0000000000000876
