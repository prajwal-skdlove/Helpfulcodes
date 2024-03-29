#Crontab

#https://www.covermymeds.com/main/insights/articles/running-cron-on-a-specific-day-of-the-month/
#https://serverfault.com/questions/449651/why-is-my-crontab-not-working-and-how-can-i-troubleshoot-it

 #  +------------------------------------------------------------------------------------------------+
 #  |  Prajwal                                                                            |
 #  +------------------------------------------------------------------------------------------------+

 # Example of job definition:
 # .---------------- minute (0 - 59)
 # |  .------------- hour (0 - 23)
 # |  |  .---------- day of month (1 - 31)  
 # |  |  |  .------- month (1 - 12) OR jan,feb,mar,apr ...
 # |  |  |  |  .---- day of week (0 - 6) (Sunday=0 or 7) OR sun,mon,tue,wed,thu,fri,sat  
 # |  |  |  |  |  # *  *  *  *  * user-name command to be executed

#Crontab specfic day of the month

05     11       1-7     */3     *          [[ $(date +\%A) == "Monday" ]] && code to run
#last day of the month
[ `date +%d` -eq `echo $(cal | awk '{print $1}' | awk '{print $NF}' | grep -v '^$' | tail -n 1)` ] && code to run

#Email Using Mailx
 echo "something" | mailx -s "-- Completed successfully -- Please review summary and attached processing logs/outputs" \
 -a file email@email.com

#Email Using Sendmail
 (
    echo "To: user@gmail.com"
    echo "Subject: The Subject"
    echo "Content-Type: text/html"
    echo
    cat test.html
) | sendmail -t


 
#Setup Keytabfile
#New Keytabfile can only be created after deleting the old one and re creating the new one
ktutil
addent -password -p emailaddress@email.com -k 1 -e aes256-cts
Password for emailaddress@email.com  [enter your password]
wkt /home/username/.ident/username.keytab
quit

#Run a keytab file
x 'user=$(whoami); kinit -k -t /home/${user}/.ident/${user}.keytab -p ${user}@email.com';


#if then
if [ ! -d $log_dir ]; 
then mkdir -m775 -p $log_dir 
fi

#link a file
ln -s file_to_link name_of_the_link
#force relink a file
ln -nsf file_to_link name_of_the_link


#kill a session
screen -XS [session # you want to quit] quit


#Error from end of file /bin/bash^M: bad interpreter: No such file or directory
sed -i -e 's/\r$//' filename.sh

#run a unix tempfile
. $HOME/tempscript
source $HOME/tempscript

#change permissions
chmod 775 filename

#Make file executable
chmod +x filename

#delete mail. hit quit at the end
d* to delete all
d 134 delete 1,3,4 messages
d 1-4 delete 1-4 messages
d delete current
quit



chmod +x your_script.sh
./your_script.sh your_source_file > output.html

#find Files
find $file_loc -type f -mtime -50 -iname  "bk_final*" -ls

#delete files
find $log_dir -type f -mtime +31 -iname  "bk_avm*" -ls -delete

#copy files
cp [OPTION] source dest

#date modification
dttime_stamp=$(date +%Y%m%d_%H%M%S)
dt_stamp=$(date +%Y%m%d)
dt_stamp=$(date +%Y%m%d -d "-4 days")
first=$(date --date="$(date +'%Y-%m-01') - 1 month" +%Y%m%d)
last=$(date --date="$(date +'%Y-%m-01') - 1 second" +%Y%m%d)

#get patterns delimiter
`awk -F '.'  '{print $1}'  <<<  "abc.sas"`


#check for whether a variable is null or exists
if [[ -z $loc ]] #variable doesnt exists or is null
if [[ -n $loc ]] #variable exists and has value


#gzip
gzip -c filename > newname.gz #compress files
gzip -d newname.gz #decompress files

#sftp
sftp username@server
#input passsword when asked or create a keypair file
#get will grab files

#VIM
Insert Mode - i
Quit without saving - q!
Save and Quit - wq

#move tabs in vim
open new tab - :tabnew filename
next tab - g t
prior tab - g T
Numbered tab - nnn g t

#Capturing log of bash output
bashlogloc= /Logs
bashlogname= bashlog
bashlog_date=$(date +%Y%m%d_%H%M%S)
bashlog_file=$bashlogloc/"$bashlogname"_$bashlog_date.log

# Save standard output and standard error
exec 3>&1 4>&2
# Redirect standard output to a log file
exec 1>$bashlog_file
# Redirect standard error to a log file
exec 2>$bashlog_file

# Restore original stdout/stderr
exec 1>&3 2>&4
# Close the unused descriptors
exec 3>&- 4>&-
# Now the output of all commands goes to the original standard output & error

#Creating a keypair in a server
#in the .ssh folder create the keypair and name it
#This will create two files. Copy the one with .pub extension to the remote server
mkdir ~/.ssh
ssh-keygen -t rsa
#Move the file to the server
ssh-copy-id -i keypair_file.pub  user@server
cat keypair_file.pub | ssh user@server "mkdir -p ~/.ssh && cat >>  ~/.ssh/authorized_keys"
scp keypair_file.pub <user>@<yourhost>:.ssh/authorized_keys
#Validate it works
ssh -i ~/.ssh/keypair_file user@server


#Move files between servers
scp path_to_file path_to_destination
scp path_to_file username@server:path_to_destination #Move file from B to A while logged into B
scp username@server:path_to_file path_to_destination #Move file from B to A while logged into A

#count columns each row in a file
cat filename.txt| awk -F"|" '{print NR, NF}' > test.csv
#lowest column count
cat filename.txt| awk -F"|" '{print  NF}' | sort -nu | head -n 1
#highest column count
cat filename.txt| awk -F"|" '{print  NF}' | sort -nu | tail -n 1

#Row count
cat filename.txt | wc -l

#print specific line form a file
sed -n 2p  filename.txt

#find processes run by CRON
ps fauxww | grep -A 1 '[C]RON'


