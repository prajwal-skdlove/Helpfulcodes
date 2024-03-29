#####################################################################################################################################################   
This code inputs an excel file with variable names, both old and new that it can be changed to with a lot of other information
Can use this to update/clean dataset while keeping all the changes central in excel
#####################################################################################################################################################   

#Get the Master Excel Var file to Change/Rename Variable
file_vnm <- "01_Master_File_Variables.xlsx" #variable name file
df_vnm <- read.xlsx(paste0(path_root, file_vnm))

#functions to align the colnames to the ones in excel
get_proper_col_name <- function(df, colnamefile, colnamevar, newnamevar){
  vnm_01 <- as.data.table(colnamefile)[!is.na(colnamevar)
                                       ][, paste("Variable") := lapply(.SD, function(x) trimws(x)), .SDcols = newnamevar][, c("Variable", colnamevar), with = FALSE]
  
  names(df) <- vnm_01$Variable[match(trimws(names(df)),vnm_01[[colnamevar]])]
  return(df)
}


varnamefile <- df_vnm
vname <- "Var_Qualtrics"
nname <- "VarName"
df <- dfCmb
df_colnew <- get_proper_col_name(df, varnamefile, vname, nname)

keep_Colnames <- as.data.table(df_vnm)[Var_Keep == 1, VarName] 
df_Final <- df_colnew[,keep_Colnames]

#####################################################################################################################################################    
#Clean the dataset and add date variables
#####################################################################################################################################################     
scl_10pts <- as.data.table(df_vnm)[Scl_10pts_1_10 == 1 & Var_Keep == 1, VarName] 

scl_10pts_name910 <- paste0(scl_10pts, "9_10")
scl_10pts_name910_breaks <- c(0,8,10)
scl_10pts_name910_labels <-  c(2,1)

scl_10pts_name16 <- paste0(scl_10pts, "1_6")
scl_10pts_name16_breaks <- c(0,6,10)
scl_10pts_name16_labels <-  c(1,2)


scl_10pts_name78 <- paste0(scl_10pts, "7_8")
scl_10pts_name78_breaks <- c(0,6,8,10)
scl_10pts_name78_labels <-  c(2,1,2)


scl_2pts <- as.data.table(df_vnm)[Scl_2pts_1_2 == 1 & Var_Keep == 1, VarName] 
scl_2pts_name <- paste(scl_2pts, "1", sep="_")
