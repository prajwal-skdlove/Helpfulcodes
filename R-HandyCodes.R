####################################################################################################
#Strip Date and Time
####################################################################################################
df[, date_time := as.POSIXct(date_time,'%Y-%m-%d %H:%M:%S',tz = "")
                    ][, `:=`(Dt = as.Date(date_time),
                             Tm = as.ITime(strftime(date_time,"%H:%M:%S")))]
                             
                             
####################################################################################################
#Combinations and Permutations
####################################################################################################                       
com <- function(n,r) {return(factorial(n)/(factorial(r)*(factorial(n-r))))}
per <- function(n,r) {return(factorial(n)/(factorial(n-r)))}


####################################################################################################
#Correlation coefficient
####################################################################################################
corr_eqn <- function(x,y,digits = 2) {
  corr_coef <- round(cor(x, y,use = "complete.obs"), digits = digits)
  val <- cor.test(x,y)
  paste("r = ", corr_coef, ", p = ",round(val$p.value,4))
}


####################################################################################################
#Get Random Phrases
####################################################################################################
Randomphrases <- function(n, l) {
  a <- do.call(paste0, replicate(l, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

####################################################################################################
#Get variable names matching paterns
####################################################################################################

varnames <- function (charactermatch, datafile){
  
  
  pattern <- charactermatch
  varnames <- colnames(datafile)
  var_Assign <- vector()
  pattern_match <- grep(pattern, varnames)
  
  
  for (i in (pattern_match)) {
    temp <- as.vector(varnames[i])
    var_Assign <- append(var_Assign, temp)
    
  }
  


  var_Assign <- as.vector(var_Assign)
  return(var_Assign)
}

####################################################################################################
#Function to substring the character, provided the start position and how  many characters to get
#Use Positive to get Left to Right, Use Negative to get Right to Left
####################################################################################################

substrfntbck <- function(x,start,howmany) {
  if (howmany >= 0) 
  {substr(x,start, start + howmany -1)}
  else 
  {substr(x, start - (- howmany) + 1, start )}
 }

####################################################################################################
#Function to calculate distance in miles provided with a pair of latitudes & longitudes
#Assumes a flat earth and calculates the shortest possible distance
#Good for distances less than 100
####################################################################################################
distance_in_miles <- function(lat1, lon1, lat2, lon2) {
  
  #radius of the earth; default = 6378137 m, divide 1609.344t to convert it in to miles
  r <- (6378137/1609.344)
  
  
  lat1pi <- ((pi/2) - (2*(pi/2)/180) * lat1)
  lat2pi <- ((pi/2) - (2*(pi/2)/180) * lat2)
  lon2pi_lon1pi <- ((pi/2) - (2*(pi/2)/180) * lon2) - ((pi/2) - (2*(pi/2)/180) * lon1)
  
  dist_miles <- r * (((lat1pi^2) + (lat2pi^2) - (2 * lat1pi * lat2pi * cos(lon2pi_lon1pi)))^0.5)
  return(dist_miles)}


####################################################################################################
#Function to calculate distance in miles provided with a pair of latitudes & longitudes
  #The shortest distance between two points (i.e., the 'great-circle-distance' or 'as the crow flies'),
  #according to the 'Vincenty (ellipsoid)' method. 
  #This method uses an ellipsoid and the results are very accurate. 
  # The method is computationally more intensive than the other great-circled methods in this package
####################################################################################################

get_geo_distance<- function(long1, lat1, long2, lat2, units = "miles"){
  if(!require(geosphere)){install.packages("geosphere") 
    require(geosphere)}

  distance_m = distVincentyEllipsoid(cbind(long1,lat1), cbind(long2,lat2), a=6378137, b=6356752.3142, f=1/298.257223563)

  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
    # This will return in meter
  }
  distance
}


####################################################################################################
#Source File to source all the functions that have been written
#Function can be called by their original function name
####################################################################################################
pathname <-"C:/R"

source_all_files <- function(path){
  currentwd <- getwd()
  setwd(path)
  file.sources=list.files(pattern=".[rR]")
  file.sources = file.sources[file.sources != "All_Functions_Source.R"]
  sapply(file.sources, source)
  setwd(currentwd)
}

source_all_files(pathname)

####################################################################################################
#R Pull data from Databases
####################################################################################################
library(DBI)
library(sqldf)
con <- dbconnet(odbc::odbc(), "con")

test <- dbGetQuery(con, paste("select * from table a"))

####################################################################################################
#R Copy Files to and From
####################################################################################################
file_rename <- paste(substr(files_in_folder, nchar(files_in_folder) - 13,nchar(files_in_folder)-4), filename, sep = "_")
file_rename

mapply(function(x,y) file.rename(from = paste(path_from,x,sep="/"),to = paste(path_to,y, sep="/")), x = files_in_folder, y = file_rename)

####################################################################################################
#Fix Column Names
####################################################################################################
name_list <- function(x){ #Function to remove . & spaces from name
  names(x) <- gsub(".", "_",gsub(" ", "",trimws(names(x)), fixed = TRUE), fixed = TRUE)
  return(x)
}


####################################################################################################
#Insane Date Convert
####################################################################################################
var_dt <- colnames(df_total02)[grep(glob2rx("*DATE*"), toupper(colnames(df_total02)))]
var_dt

df_total02 <- df_total01[, paste(var_dt) := lapply(.SD, 
                                                function(x)
                                                  ifelse(is.na(tryCatch(as.Date(x, format = "%m/%d/%Y",origin="1899-12-30"), error = function(e) e)),
                                                         format(as.Date(x,origin="1899-12-30"), "%Y-%m-%d"),
                                                         format(as.Date(x, format = "%m/%d/%Y",origin="1899-12-30"),"%Y-%m-%d"))),.SDcols = var_dt
                                  ]


####################################################################################################
#Run R with log
####################################################################################################
Logfile <- paste0(as.character(Sys.Date()),"_Log", ".log")
Logpath <- "path"
logloc <- paste0(Logpath, Logfile)


tryCatch({
  # Code to attempt
  source("Codes.R")
  
}, error = function(err.msg){
  # Add error message to the error log file
  write(toString(err.msg), log.path, append=TRUE)
}
)

####################################################################################################

#write log
con <- file(logloc)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

# Restore output to console
sink() 
sink(type="message")

####################################################################################################
#Send Email
####################################################################################################

library(RDCOMClient)
library(xtable)
#Create output to email
y <- print(xtable(dtlist_file), type="html", print.results = FALSE)
body <- paste0("<html>", "Min and Max Date of the Master File and the Files Aggregated","<br>","<br>","<br>","<br>",
               y,"<br>","<br>","<br>","<br>",
               "Email Sent Automatically through R", "<br>",
               "Log file located at: ", logloc, 
               "</html>")
#  glue("Some sentence with"),

OutApp <- COMCreate("Outlook.Application")
outMail = OutApp$CreateItem(0)
outMail[["To"]] = paste("email add",
                        sep = ";")
outMail[["subject"]] = paste(as.character(Sys.Date()),"Status", sep = " ")
outMail[["HTMLbody"]] = body
try(outMail$Send())## send it  

