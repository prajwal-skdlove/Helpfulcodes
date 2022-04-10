
if [[ -n $SYSERR ]] && [[ $SYSERR<=4 ]]
then

file_b1=b1
file_b2=b2
file_b3=b3


ttl_b1="B1"
ttl_b2="B2"
ttl_b3="B3"


tbl_scrpt= /Csv2HtmlTable.sh


for i in b1 b2 b3 
do
fvar=file_$i
tvar=ttl_$i
cat $data_dir${!fvar}.csv| $tbl_scrpt -d "|" -t "$ttl_tbl" -h "${!tvar}" > $data_dir${!fvar}.html
done

#Send an email out
{
  echo "To: $email_list"
  echo "Subject: $(eval echo $dt_stamp) --  **$process_title -- Completed successfully -- Please review summary and attached processing logs/outputs"
  echo 'Content-Type: multipart/mixed; boundary="-q1w2e3r4t5"'
  echo ""
  echo '---q1w2e3r4t5'
  echo "Content-Type: text/html"
  echo "Content-Disposition: inline";
  echo ""
  echo "<h2 style='text-align:left;background-color:White;color:#009E66;'> $process_title -- Completed Successfully $dt_stamp</h2>"
  echo ""
  for i in b1 b2 b3 
do
 fvar=file_$i
  echo ""
  cat $data_dir${!fvar}.html
  echo ""
 done
  echo '---q1w2e3r4t5'
  echo 'Content-Type: application; name="'$(basename $log_fl)'"'
  echo "Content-Transfer-Encoding: base64"
  echo 'Content-Disposition: attachment; filename="'$(basename $log_fl)'"'
  echo ""
  base64 $log_fl
  #mimencode --base64 $log_fl $(basename $log_fl)
  #echo '---q1w2e3r4t5'
  #echo 'Content-Type: application; name="'$(basename $bashlog_file)'"'
  #echo "Content-Transfer-Encoding: base64"
  #echo 'Content-Disposition: attachment; filename="'$(basename $bashlog_file)'"'
  #echo ""
  #base64 $bashlog_file
  #mimencode --base64 $bashlog_file $(basename $bashlog_file)
  }|/usr/sbin/sendmail -t

find $log_dir -type f -mtime +120 -iname  "test*" -ls -delete
find $data_dir -type f -mtime +120 -iname  "test*" -ls -delete

#echo -e "$process_title -- Completed Successfully $dt_stamp \n$body"\
#| mailx -s "$(eval echo $dt_stamp) --  **$process_title -- Completed successfully -- Please review summary and attached processing logs/outputs" \
#-a $log_fl $email_list
else
echo -e "$process_title -- Errors Detected $dt_stamp \n$body"\
| mailx -s "$(eval echo $dt_stamp) --  **$process_title -- Errors Detected -- Please review summary and attached processing logs/outputs" \
-a $log_fl $email_list
fi
