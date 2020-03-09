compare <- function(df1, df2) { 

  # find common and uncommon columns
  # change type of different columns
  # cut uneven number of rows
  # produce comparison result
 
  in_df1_in_df2 <- colnames(df1)[colnames(df1) %in% colnames(df2)]
  no_df1_in_df2 <- colnames(df2)[!c(colnames(df2) %in% colnames(df1))]
  in_df1_no_df2 <- colnames(df1)[!c(colnames(df1) %in% colnames(df2))]
  
  if(length(no_df1_in_df2) > 0) {
    print(paste("There are no columns", no_df1_in_df2, "in df1. 
                These columns will not be compared."))
  }
  
  if(length(in_df1_no_df2) > 0) {
    print(paste("There are no columns", in_df1_no_df2, "in df2. 
                These columns will not be compared."))
  }
  
  
       
}

df1 <- data.frame(
  x = rep(1, 5),
  y = rep(2, 5),
  z = rep(3, 5)
)

df2 <- data.frame(
  x = rep(1, 5),
  y = rep(2, 5),
  v = rep(3, 5)
)
