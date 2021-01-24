#Get the specific variable WOE to capture the lower bound an upper bounds to use for binning the variable
#retunrs a List of binned values with _WOEIV appended to the name of the variable
#Needs to be column binded with the original dataset as a dataframe to get a data frame

WOEIV_bin <- function (df, varname, WOEdataset ) {
if(!require(dplyr)){instalowerlimit.packages("dplyr")}


#Namelist - character
namelist <- paste(substitute(WOEdataset), "Tables", as.character(varname),as.character(varname), sep ="$")

#eval(parse(text=character)) evaluates a character value
get_bin_labels <- data.frame(eval(parse(text = namelist)))
colnames(get_bin_labels) <- c("var")

#Creates bins breaks and labels
get_bin_labels <- get_bin_labels %>%
  mutate(var1 =  gsub(c("\\]"),"",gsub(("\\["), "", var)),
         position_comma = regexpr(",",as.character(var1)),
         pl = position_comma - 1,
         no_of_character = nchar(as.character(var1)),
         subset_characters = no_of_character - position_comma,
         lowerlimit =substr(var1, 1 , position_comma - 1),
         upperlimit = substr(var1, position_comma+1, nchar(var1))) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(lowerlimit_mod = ifelse(lowerlimit > 0, lowerlimit -1, lowerlimit)) %>%
  arrange(lowerlimit) %>%
  select(var, lowerlimit, upperlimit, lowerlimit_mod) %>%
  filter(var != "NA")


 #Define the variables and the dataframe
  name = "WOEIV"
  breakpoints <- dput(sort(unlist(list(get_bin_labels$lowerlimit_mod, max(get_bin_labels$upperlimit[!is.na(get_bin_labels$upperlimit)])))))
  var_labels <- dput(as.character(unlist(list(get_bin_labels$var))))
  includelowest = FALSE
 
 
  tmp_Data2 <- setNames(lapply(df[varname], 
                        function(x) cut(x,breaks = breakpoints, labels=var_labels, include.lowest = includelowest)),
                       paste(names(df[varname]), name, sep='_')) 

 return(tmp_Data2)
 }



#Weight of Evidence
#The Weight of Evidence or WoE value is a widely used measure of the "strength" of a grouping for 
#separating good and bad risk (default). It is computed from the basic odds ratio:
#(Distribution of Good Credit Outcomes) / (Distribution of Bad Credit Outcomes)
#Or the ratios of Distr Goods / Distr Bads for short, where Distr refers to the proportion of Goods or Bads 
#in the respective group, relative to the column totals, i.e., expressed as relative proportions of the total number of 
#Goods and Bads
#Specifically, the Weight of Evidence value for a group consisting of n observations is computed as:
#WOE = [ln(Dist Good/Dist Bads)]*100
#The value of WoE will be 0 if the odds of Distribution Goods / Distribution Bads is equal to 1. 
#If the Distribution Bads in a group is greater than the Distribution Goods, 
#the odds ratio will be less than 1 and the WoE will be a negative number; 
#if the number of Goods is greater than the Distribution Bads in a group, the WoE value will be a positive number.

#Information Value
#The Information Value (IV) of a predictor is related to the sum of the (absolute) values 
#for WoE over all groups. Thus, it expresses the amount of diagnostic information of a predictor variable 
#for separating the Goods from the Bads. Specifically, given a predictor with n groups, 
#each with a certain Distribution of Goods and Bads, 
#the Information Value (IV) for that predictor can be computed as:
#IV = Sum(i -1 to n) [ (Distr goods (i) - Distr Bads (i)) * ln(Distr Goods/ Distr Bads)] 
# According to Siddiqi (2006), by convention the values of the IV statistic can be interpreted as follows. 
#If the IV statistic is:
#Less than 0.02, then the predictor is not useful for modeling (separating the Goods from the Bads)
#0.02 to 0.1, then the predictor has only a weak relationship to the Goods/Bads odds ratio
#0.1 to 0.3, then the predictor has a medium strength relationship to the Goods/Bads odds ratio
#0.3 or higher, then the predictor has a strong relationship to the Goods/Bads odds ratio.
