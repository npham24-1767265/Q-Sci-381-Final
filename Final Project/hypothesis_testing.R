## This functon calculates critical value given the correct numbers
## Parameters:
## mean : sample mean
## hypo_val : hypothesized value
## std_dev : population standard deviation 
## sample_size : sample size 
## alpha : significance level
## return the test statistic z value.
test_stat <- function(mean, hypo_val, std_dev, sample_size, alpha) {
  #test statistic
  z <- (mean - hypo_val)/(std_dev/sqrt(sample_size)) 
}