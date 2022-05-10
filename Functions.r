# ------------------------------------------------------------------------
# CTRL-A COPY THIS ENTIRE THING AND PASTE IT INTO R
# FUNCTIONS RELY ON ONE ANOTHER, SAFEST JUST TO COPY THE ENTIRE THING
# ------------------------------------------------------------------------

#Calculates Standard Error for Sample Proportions
se_prop <- function(phat, n) {
	return (sqrt(phat * (1-phat) / n))
}

#Calculates Standard Error for Sample Means
se_mean <- function(s, n) {
	return (s/sqrt(n))
}

#Calculates Standard Error for Independent Groups 
se_igroups <- function(s1,s2,n1,n2) {
	return (sqrt((s1^2/n1) + (s2^2/n2)))
}

#Calculates Test Statistic for Sample Proportions
ts_prop <- function(phat, p0, n) {
    return ((phat-p0)/sqrt((p0*(1-p0))/n))
}

#Calculates Test Statistic for Sample Means (if sd known use pnorm)
ts_mean <- function(ybar, mew, se, n) {
	return ((ybar - mew)/se_mean(se,n))
} 

#Calculates Test Statistic for Independent Groups
ts_igroups <- function(ybar1, ybar2, se1, se2, n1, n2, delta) {
	return ((ybar1 - ybar2 - delta)/se_igroups(se1, se2, n1, n2))
}

#Calculates Test Statistic for Independent Groups
ts_igroups <- function(ybar1, ybar2, se, delta) {
	return ((ybar1 - ybar2 - delta)/se)
}

#Calculates Test Statistic for Dependent Groups
ts_dgroups <- function(dbar, SD, n, delta) {
	return ((dbar - delta)/(SD/(sqrt(n))))
}

#Calculates Test Statistic for Analysis of Variance
ts_var <- function(mst, mse) {
	return (mst/mse)
}

#Returns proportion of population for confidence interval
ci_up_bound <- function(conf_level) {
	return((conf_level) + ((1-conf_level)/2))
}

#Prints Confidence Intervals for Sample Proportions
ci_prop <- function (phat, n, conf_level) {	
	me <- me_prop(conf_level, se_prop(phat, n))
	
	print(paste("Upper bound: ", (phat + me)))
    print(paste("Lower bound: ", (phat - me)))	
}

#Prints Confidence Intervals for Sample Proportions (se given)
ci_prop_se <- function(phat, se, conf_level) {
	me <- me_prop(conf_level, se)

	print(paste("Upper bound: ", (phat + me)))
    print(paste("Lower bound: ", (phat - me)))
}

#Prints Confidence Intervals for Sample Means 
ci_mean <-function(ybar, s, n, conf_level) {
	t_star <- qt(ci_up_bound(conf_level), n-1)
	
	int <- (t_star * se_mean(s, n))
	
	print(paste("Upper bound: ", (ybar + int)))
    print(paste("Lower bound: ", (ybar - int)))	
}

#Prints Confidence Intervals for Sample Means (se given)
ci_mean_se <-function(ybar, se, n, conf_level) {
	t_star <- qt(ci_up_bound(conf_level), n-1)
	
	int <- (t_star * se)
	
	print(paste("Upper bound: ", (ybar + int)))
    print(paste("Lower bound: ", (ybar - int)))	
}


#Prints Confidence Intervals for Sample Means with known population sd
ci_mean_known <-function(ybar, sigma, n, conf_level) {
	z_star <- qnorm(ci_up_bound(conf_level), n-1)
	
	int <- (z_star * se_mean(sigma, n))
	
	print(paste("Upper bound: ", (ybar + int)))
    print(paste("Lower bound: ", (ybar - int)))	
}

#Prints Confidence Intervals for Independent Groups 
ci_igroups <- function(y1, y2, s1, s2, n1, n2, conf_level) {
    t_star <- 0
	confidence <- ci_up_bound(conf_level)    
		
    if (n1 < n2) {
        t_star <- qt(confidence, n1-1)
    } else {
        t_star <- qt(confidence, n2-1)
    }
	
    int <- (t_star* se_igroups(s1,s2,n1,n2))
    
    print(paste("Upper bound: ", ((y1-y2) + int)))
    print(paste("Lower bound: ", ((y1-y2) - int)))
}

#Prints Confidence Intervals for Independent Groups 
ci_igroups_se <- function(difference, se, nmin, conf_level) {
    t_star <- 0
	confidence <- ci_up_bound(conf_level)    
	
    t_star <- qt(confidence, nmin-1)

	
    int <- (t_star * se)
    
    print(paste("Upper bound: ", (difference + int)))
    print(paste("Lower bound: ", (difference - int)))
}

#Prints Confidence Intervals for Independent Groups 
ci_dgroups <- function(dbar, deviation, n, conf_level) {
	t_star <- qt(ci_up_bound(conf_level), n-1)
	
	int <- (t_star * (deviation/sqrt(n)))
	
	print(paste("Upper bound: ", (dbar + int)))
    print(paste("Lower bound: ", (dbar - int)))
}

#Returns slope (b1) from r sy and sx
rl_b1 <- function(r,sy,sx) {
    return(r*sy/sx)
}

#Returns y-intercept (b0) from slope(b1), xmean, and ymean
rl_b0 <- function(xmean,ymean,slope) {
    return (ymean-(slope*xmean))
}

#Predicts y-value from x, slope and intercept
rl_predicty <- function(x, b1, b0) {
    print(paste("predicted value of y: ", ((x*b1) + b0)))
}

#Predicts x-value from y, slope and intercept
rl_predictx <- function(y,b1,b0) {
    print(paste("approximate x value of: ", ((y-b0)/b1)))
}

#Returns the margin of error
me_prop <- function(conf_level,se) {
    z_star <- qnorm(ci_up_bound(conf_level))
    return (z_star * se)
}

#Returns appropriate n 
n_prop <- function(phat, conf_level, me) {
	z_star <- qnorm(ci_up_bound(conf_level))
	
	return (((z_star^2)*phat*(1-phat))/(me^2))
}