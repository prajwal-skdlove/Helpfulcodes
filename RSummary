function_qntl <- function(df){
  num_cols <- unlist(lapply(df, is.numeric))
  #num_cols <- which( sapply(df, is.numeric ))   
  df1 <- df[, num_cols, with = FALSE]
  df2 <- df1[, lapply(.SD, quantile, probs = c(0,.10, .25, .5,.75, .90, .95, .99, 1), na.rm = TRUE), .SDcols = num_cols]
  df_perc <- data.table(Percentile = c(0,.10, .25, .5,.75, .90, .95, .99,1))
  df3 <- cbind(df_perc,df2)
  return(df3)
}


do.call(cbind, lapply(df, summary))
