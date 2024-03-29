Logfile1 <- paste0(as.character(Sys.Date()),"_Log_SearchEmailDownload", ".log")
Logpath <- "C:/File_Logs/"
logloc1 <- paste0(Logpath, Logfile1)
logloc1

con <- file(logloc1)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")



#####################################################################################################################################################                          
library(RDCOMClient)
library(openxlsx)
library(data.table)

#url <- "http://www.omegahat.net/R/bin/windows/contrib/3.5.1/RDCOMClient_0.93-0.zip"

#install.packages(url, repos=NULL, type="binary")
#####################################################################################################################################################                          
#Get the last date the email was searched/last file was downloaded
path_indate <- "C:/R_code/"
fnm_indate <- "Last_Date.xlsx"
df_indate <- read.xlsx(paste0(path_indate, fnm_indate))

#this is still wonky
var_lastdate <- as.Date(unique(df_indate$Last.Date), origin = "1899-12-31") #be careful with this - https://community.rstudio.com/t/number-to-date-problem-excel-to-r/40075/4
var_lastdate
#####################################################################################################################################################                          
#Setup outlook parameters

# lib = LoadTypeLib(outlook_app)
# names(outlook_app)

#set up search string
pattern1 <- "%Dashboard%"
inbox_search_string1 <- paste("urn:schemas:httpmail:subject like", paste0("'", pattern1,"'"), sep = " ")
inbox_search_string1

pattern2 <- "3611%"
inbox_search_string2 <- paste("urn:schemas:httpmail:subject like", paste0("'", pattern2,"'"), sep = " ")
inbox_search_string2


pattern3 <- var_lastdate
inbox_search_string3 <- paste("urn:schemas:httpmail:datereceived  >", paste0("'", pattern3,"'"), sep = " ")
inbox_search_string3


inbox_search_string <- paste(inbox_search_string1, inbox_search_string2, inbox_search_string3, sep = " & ")
inbox_search_string



#####################################################################################################################################################      
#Do the search
outlook_app <- COMCreate("Outlook.Application") #create a COM 

#Setup outlook parameters
search <- outlook_app$AdvancedSearch("Inbox",inbox_search_string)


#####################################################################################################################################################    
#Function to download and save attachements

function_download_save_emailattachments <- function(results,download_file_pattern,outpath){
  n1 <- results$Attachments()$Count()
  if (n1 > 0){
  Recieved_Time <- as.character(as.Date("1899-12-30") + floor(results$ReceivedTime()))
  for (j in 1:results$Attachments()$Count()){ #go through all the attachments
    print(results$Attachments(j)$DisplayName())
    if (grepl(download_file_pattern,results$Attachments(j)$DisplayName()) == TRUE) { #pattern match the attachments that you need
      attachment_file <- results$Attachments(j) #attachment file
      attachment_name <- attachment_file$DisplayName() #name of the attachment as int he email
      print(attachment_name)
      output_file <-  paste0(outpath, attachment_name) #location where to save the attachment
      attachment_file$SaveAsFile(output_file) #save the attachment
    }
  }
  return(Recieved_Time)}
}


#####################################################################################################################################################    
#Run the code to find the email, attachment and download it

srch_results <- search$Results()
Sys.sleep(10) #hold the fort down a little bit. takes time for it to retrieve results

n <- srch_results$Count()
n
dt <- if (n >0) {rbindlist(lapply(1:n, function(x) {data.table(Subject = srch_results$Item(x)$Subject(),
                                    RecievedTime = as.character(as.Date("1899-12-30") + floor(srch_results$Item(x)$ReceivedTime())))}))}
dt
#Run Only if the it finds emails that meet our criteria
if (n > 0) {
  #Print the results of the search
  dld_fileptrn <- glob2rx("Summary*")
  out_path <- "C:/Files_Unloaded/"
  
  out_file_function <- sapply(1:n, function(x)function_download_save_emailattachments(srch_results$Item(x),dld_fileptrn,out_path))
  out_Dt <- data.frame(Last.Date = max(as.Date(out_file_function)))
  out_Dt
  outpath_date <- "C:/R_code/"
  fnm_date <- "Last_Date.xlsx"
  write.xlsx(out_Dt,paste0(outpath_date,fnm_date), overwrite = TRUE)
}


#####################################################################################################################################################    
#Email if no attachments

if (n == 0 | !(exists("out_Dt"))| (exists("out_Dt") && is.na(out_Dt))) {

  #send email
  #Create output to email

  body <- paste0("<html>", "No Attachment Found after ", as.Date(unique(df_indate$Last.Date), origin = "1899-12-30"),"<br>","<br>","<br>","<br>",
                 "Email Sent Automatically through R", "<br>",
                 "Log file located at: ", logloc1, "<br>",
                 "Please check the log file if you see issues",
                 "</html>")
  #  glue("Some sentence with"),
  
  OutApp <- COMCreate("Outlook.Application")
  outMail = OutApp$CreateItem(0)
  outMail[["To"]] = paste("prajwal.koirala@email.com",                          
                          sep = ";")
  outMail[["subject"]] = paste(as.character(Sys.Date()),"Search Email and Download Status", sep = " ")
  outMail[["HTMLbody"]] = body
  outMail[["Attachments"]]$Add(logloc1)
  try(outMail$Send())## send it  
}

# Restore output to console
sink() 
sink(type="message")


#####################################################################################################################################################    
#Run the combine code if there are files and attachments found
if (n > 0) {
source("C:/R_code/R02_Code_to_Combine_Daily_Files.R")}
#####################################################################################################################################################    
  # if (date_diff < 0 | date_diff1 >= 0) {
  #   Continue <-  rstudioapi::showPrompt("CONTINUE? Enter TRUE or FALSE"
  #                                       ,paste(paste("Data on the Master File up to", as.Date(max_date_u01, origin = "1899-12-30"),sep=" ")
  #                                              ,paste("Data on the File to be added starts at", as.Date(min_date_d01, origin = "1899-12-30"),sep=" ")
  #                                              ,paste("Data on the File to be added ends at", as.Date(max_date_d01, origin = "1899-12-30"), sep = " ")
  #                                              ,sep="\n"))
  # }
  # 
  # else {
  #   Continue = TRUE
  # }
  # 
  # if (Continue == TRUE){
  
#####################################################################################################################################################     
#send email
library(RDCOMClient)
library(xtable)
#Create output to email
y <- print(xtable(dtlist_file), type="html", print.results = FALSE)
z <- print(xtable(dt), type="html", print.results = FALSE)
body <- paste0("<html>", "Files Downloaded from Email and Appended","<br>","<br>","<br>","<br>",
               z, "<br>","<br>","<br>","<br>",
               "Min and Max Date of the Master File and the Files Aggregated","<br>","<br>","<br>","<br>",
               y,"<br>","<br>","<br>","<br>",
               "Email Sent Automatically through R", "<br>",
               "Log files  located at: ", "<br>",
               logloc1, " & ", logloc,
               "</html>")
#  glue("Some sentence with"),

OutApp <- COMCreate("Outlook.Application")
outMail = OutApp$CreateItem(0)
outMail[["To"]] = paste("prajwal.koirala@email.com",                       
                        sep = ";")
outMail[["subject"]] = paste(as.character(Sys.Date()),"Status", sep = " ")
outMail[["HTMLbody"]] = body
try(outMail$Send())## send it  

#https://docs.microsoft.com/en-us/office/vba/api/outlook.attachment.saveasfile
