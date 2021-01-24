
#######################################################################################################################################
#1 Sample Proportion Z-test
#######################################################################################################################################

CI_2Sided_zvalue_prop <- function(x,n,conf,p.null = 0){
  
  p = ifelse(x >= 0 & x <= 1, x, x/n)
  se = sqrt(p * (1 - p) / n)
  t_stat = abs((p - p.null) / se)
  z_val =  qnorm(1- ((1 - conf)/2))
  moe = z_val * se
  ci_low = p - moe
  ci_high = p + moe
  p_val = 2 * pnorm(-abs(t_stat))
  output_list <- list("p" = p,
                      "sd" = sd,
                      "n" = n,
                      "conf" = conf,
                      "test_statistic" = t_stat,
                      "se" = se,
                      "z_critical_val" = z_val,
                      "CI_Low" = ci_low,
                      "CI_High" = ci_high,
                      "p" = p_val,
                      "MOE" = moe)
  output <-paste("One Sample t-test",
                 paste("alternative hypothesis: true proportion is not equal to ",paste0((p.null*100),"%")),
                 paste("test statistic = ", round(t_stat,3)),
                 paste("se = ", paste0(round(se*100,3),"%")),
                 paste("z critical value = ", round(z_val,3)),
                 paste(paste0(round(conf*100,0),"%"), " Confidence Interval : ", paste0(round(ci_low*100,3),"%"), " <= (µ) ",paste0(round((p*100),3),"%"), " <= ", paste0(round(ci_high*100,3),"%")),
                 paste("Margion of Error (Critical Value X SE) = ", paste0(round(moe*100,3),"%")),
                 paste("p_value = ", round(p_val,5)),
                 sep = "\n")
  
  cat(output)
  return(invisible(output_list))
}


#######################################################################################################################################
#2 Sample Proportion Z-test
#######################################################################################################################################

z_test_2sample_prop <- function(x1,x2,n1,n2,conf,pooled_variance=""){

  if (!(pooled_variance %in% c(TRUE, FALSE))){
      output <- paste("Please Specify Whether the Variances are Pooled or Not? TRUE/FALSE")
      return(cat(output))
}  

else {
  p1 = ifelse(x1 >= 0 & x1 <= 1, x1, x1/n1)
  p2 = ifelse(x2 >= 0 & x2 <= 1, x2, x2/n2)
  
  
  if (pooled_variance == FALSE){
    se = sqrt((p1 * (1 - p1)/n1) + (p2 * (1 - p2)/n2))
  }
  else if (pooled_variance == TRUE){
    p_se = (p1 * n1 + p2 *n2) / (n1 + n2)
    se = sqrt(p_se * ( 1 - p_se) * ((1/n1)+(1/n2)))
  }
  
  diff = p1 - p2
  t_stat = abs(diff/ se)
  z_val =  qnorm(1- ((1 - conf)/2))
  moe = z_val * se
  ci_low = diff - moe
  ci_high = diff + moe
  p_val = 2 * pnorm(-abs(t_stat))
  output_list <- list("diff" = diff,
                      "p1" = p1,
                      "p2" = p2,
                      "n1" = n1,
                      "n2" = n2,
                      "conf" = conf,
                      "test_statistic" = t_stat,
                      "se" = se,
                      "z_critical_val" = z_val,
                      "CI_Low" = ci_low,
                      "CI_High" = ci_high,
                      "p" = p_val,
                      "MOE" = moe)
  output <-paste(paste("Two sample Proportionl Z-test",ifelse(pooled_variance == TRUE, ": Pooled Variance",""),sep=" "),
                 "alternative hypothesis: true difference in proportion is not equal to 0",
                 paste("p1 - p2 =" , trimws(round(p1,4)), "-", trimws(round(p2,4)), "=", trimws(round(p1 - p2,4)),sep= " "),
                 paste("test statistic = ", round(t_stat,3)),
                 paste("se = ", round(se,4)),
                 paste("z critical value = ", round(z_val,3)),
                 paste(paste0(round(conf*100,0),"%"), " Confidence Interval : ", round(ci_low,4), " <= (µ) ",round(diff,4), " <= ", round(ci_high,4)),
                 paste("Margion of Error (Critical Value X SE) = ", round(moe,4)),
                 paste("p_value = ", round(p_val,5)),
                 sep = "\n")
  
  cat(output)
  return(invisible(output_list))
    }
}

#z_test_2sample_prop(59,63,300,275, 0.9, TRUE)


#######################################################################################################################################
#1 Sample Mean T-test
#######################################################################################################################################
  
  CI_2Sided_tvalue_mean <- function(mean,sd,n,conf){
    se = sd/sqrt(n)
    t_stat = mean / se
    df = n - 1
    t_val = (qt((1- ((1 - conf)/2)),df = df))
    moe = t_val * se
    ci_low = mean - moe
    ci_high = mean + moe
    p_val = 2*pt(-abs(t_stat),  df, lower=TRUE)
    output_list <- list("mean" = mean,
                        "sd" = sd,
                        "n" = n,
                        "conf" = conf,
                        "t" = t_stat,
                        "df" = df, 
                        "se" = se,
                        "t_critical_val" = t_val,
                        "CI_Low" = ci_low,
                        "CI_High" = ci_high,
                        "p" = p_val,
                        "MOE" = moe)
    output <-paste("One Sample t-test",
                   "alternative hypothesis: true mean is not equal to 0",
                   paste("t = ", round(t_stat,3)),
                   paste("df = ", round(df,3)), 
                   paste("se = ", round(se,3)),
                   paste("t critical value = ", round(t_val,3)),
                   paste(paste0(round(conf*100,0),"%"), " Confidence Interval : ", round(ci_low,3), " <= (µ) ",round((mean),3), " <= ", round(ci_high,3)),
                   paste("Margion of Error (Critical Value X SE) = ", round(moe,3)),
                   paste("p_value = ", p_val),
                   sep = "\n")
    
    cat(output)
    return(invisible(output_list))
  }
  
  

#######################################################################################################################################
#2 Sample Mean T-test
#######################################################################################################################################
#statistics is greater than critical value, reject null
t_test_2sample_mean <- function(mean1,mean2,sd1,sd2,n1,n2,conf,equal_variance=""){
  
  
  if (!(equal_variance %in% c(TRUE, FALSE))){
    output <- paste("Please Specify Whether the Variances are Assusmed to be Equal? TRUE/FALSE")
    return(cat(output))
  }  
  
  else {
    
    if (equal_variance == FALSE){
      se = sqrt(((sd1^2)/n1)+ ((sd2^2)/n2))
      df = (((sd1^2/n1)+ (sd2^2/n2))^2)/(((1/(n1-1))*(sd1^2/n1)^2) + ((1/(n2-1))*(sd2^2/n2)^2))
      
      
    }
    
    else if (equal_variance == TRUE){
      
      pooled_stddev = sqrt(((n1 - 1)*(sd1^2) + (n2 -1)*(sd2^2))/ (n1+n2-2))
      se = ((pooled_stddev)* sqrt((1/n1)+ (1/n2)))
      df = n1 + n2 - 2
      
    }
    
    t_stat = (mean1 - mean2) / se
    t_val = (qt((1- ((1 - conf)/2)),df = df))
    moe = t_val * se
    ci_low = mean1 - mean2 - moe
    ci_high = mean1 - mean2 + moe
    p_val = 2*pt(-abs(t_stat),  df, lower=TRUE) #lower TRUE for P[X <= x] otherwise P[X > x]
    output_list <- list("mean1" = mean1,
                        "mean2" = mean2,
                        "mean_diff" = mean1 - mean2,
                        "t" = t_stat,
                        "df" = df, 
                        "se" = se,
                        "t_critical_val" = t_val,
                        "CI_Low" = ci_low,
                        "CI_High" = ci_high,
                        "p" = p_val,
                        "MOE" = moe)
    
    output <-paste(paste("Independent t-test with",ifelse(equal_variance == TRUE, "Equal", "Unequal"),"Variances",sep=" "),
                   "alternative hypothesis: true difference in means is not equal to 0",
                   paste("t = ", round(t_stat,3)), 
                   paste("df = ", round(df,3)), 
                   paste("se = ", round(se,3)),
                   paste("t critical value = ", round(t_val,3)),
                   paste(paste0(round(conf*100,0),"%"), " Confidence Interval : ", round(ci_low,3), " <= (µ1 - µ2) ",round((mean1 - mean2),3), " <= ", round(ci_high,3)),
                   paste("Margion of Error (Critical Value X SE) = ", round(moe,3)),
                   paste("p = ",p_val),
                   sep = "\n")
    cat(output)
    return(invisible(output_list))
  }
}

#t_test_2sample_mean(20,30,5,7,100,120,0.95,FALSE)
#t_test_2sample_mean(5.9,6.3,1.5,1.3,300,275, 0.9, TRUE)


