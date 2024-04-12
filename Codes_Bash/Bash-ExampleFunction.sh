#!/bin/bash

################################################################################
# Help                                                                         #
################################################################################
Help()
{
   # Display Help
   echo ""
   echo ""
   echo "	Quick SAS Functions for Proc Print, Freq, Univariate and Contents"
   echo
   echo "	Syntax: sasqf [-h|t|d|l|v|c|b|o]"
   echo "	options:"
   echo "	h     Print this Help."
   echo "	t     Sas operation"
   echo "		pp 	proc print"
   echo "		pf  	proc freq"
   echo	"		pu  	proc univariate"
   echo "		pc 	proc contents"
   echo "	d 	Dataset to run. Local - provide a location using -l;"
   echo "		Database : refer to dbase with dbase.data (dbase is assigned in autoexec.sas)"
   echo "	l  	Location for the local file Only if it is in a different libary"
   echo "	v 	Variables: Freq & Univariate"
   echo "	c 	Where condition if needed: Print,Freq and Univariate"
   echo "	b  	By Variable: Univariate"
   echo "	o 	Number of Observations: Print"	
   echo
   echo "       Usage Example:"
   echo "        sasqf -t pp -d dataset -l /home/ -o 10"
   echo
   echo
}

################################################################################
################################################################################
# Main program                                                                 #
################################################################################
################################################################################

while getopts ":d:t:c:l:o:v:b:h" option; do
    case "${option}" in
        d)
            dataset=${OPTARG};;
        c)
            condition=${OPTARG};;
        l)
            location=${OPTARG};;
        o)
            obs=${OPTARG};;
	t)
	    task=${OPTARG};;
	v)  
	    var=${OPTARG};;
	b)
	    byvar=${OPTARG};;
	h) 
            Help
            exit;;
     esac
done


if [[ -z $location ]] && [[ -f $dataset.sas7bdat ]]; then

         sasdir=`pwd`
		 fulldata=sasdata.$dataset

  elif [[ -n $location ]] && [[ -f $location/$dataset.sas7bdat ]]; then

        sasdir=$location
		fulldata=sasdata.$dataset

  elif [[ $(expr index "." $dataset) -ge 1 ]]; then
  
		sasdir=`pwd`
		fulldata=$dataset
		dataset=`awk -F '.'  '{print $2}'  <<<  $dataset`
		echo "Dataset is in database: `awk -F '.'  '{print $1}'  <<<  $dataset`"

   else
        echo "Dataset: $dataset not found. Exiting ... "
        exit
fi

if [[ -z $obs ]]; then
	observations=";"
else 
	observations="(obs=$obs);"
fi


	
# Begin constructing the SAS program in the temporary directory $HOME
echo "%*include 'init.sas' ;" >! $HOME/pp$$.sas
echo "libname sasdata '$sasdir' ;" >> $HOME/pp$$.sas
# Continue constructing the program
echo "options nodate nocenter nofmterr pagesize=74 linesize=140 ;" >> $HOME/pp$$.sas
echo "proc printto file='$HOME/pp$$.lst' ;" >> $HOME/pp$$.sas

if [[ $task == "pp" ]]; then

	echo "title Printing dataset: %nrbquote($dataset) Date: &sysdate. Time: &systime.;" >> $HOME/pp$$.sas
	echo "data $dataset; set $fulldata $observations" >> $HOME/pp$$.sas
		if [[ -n $condition ]]; then
		echo "where $condition;">> $HOME/pp$$.sas
		fi
	echo "run;" >> $HOME/pp$$.sas
	echo " " >> $HOME/pp$$.sas


	echo "proc print data=$dataset;" >> $HOME/pp$$.sas
		if [[ -n $var ]]; then
		echo "var $var;">> $HOME/pp$$.sas
		fi
	echo "run;" >> $HOME/pp$$.sas
	echo " " >> $HOME/pp$$.sas
	echo "Running proc print on dataset: $dataset ($sasdir)"

elif [[ $task == "pf" ]]; then

	if [[ -n $var ]]; then
	echo "title Proc Freq on dataset: %nrbquote($dataset) Date: &sysdate. Time: &systime.;" >> $HOME/pp$$.sas
	echo "proc freq data=$fulldata;" >> $HOME/pp$$.sas
	echo "tables $var /missing misprint;">> $HOME/pp$$.sas
		if [[ -n $condition ]]; then
		echo "where $condition;">> $HOME/pp$$.sas
		fi
	echo "run;" >> $HOME/pp$$.sas
	echo " " >> $HOME/pp$$.sas
	echo "Running proc freq on dataset: $dataset ($sasdir)"
	else
	echo "No Tables Variable provided"
	exit
	fi
	
elif [[ $task == "pc" ]]; then

	echo "title Proc Contents on dataset: %nrbquote($dataset) Date: &sysdate. Time: &systime.;" >> $HOME/pp$$.sas
	
	echo "proc contents data = $fulldata ;" >> $HOME/pp$$.sas
	echo "run;" >> $HOME/pp$$.sas

	echo " " >> $HOME/pp$$.sas
	echo "Running proc contents on dataset: $dataset ($sasdir)"

	
elif [[ $task == "pu" ]]; then

		echo "title Proc Univariate dataset: %nrbquote($dataset) Date: &sysdate. Time: &systime.;" >> $HOME/pp$$.sas
		echo "data $dataset; set $fulldata $observations" >> $HOME/pp$$.sas
			if [[ -n $condition ]]; then
			echo "where $condition;">> $HOME/pp$$.sas
			fi
		echo "run;" >> $HOME/pp$$.sas
		echo " " >> $HOME/pp$$.sas

		if [[ -n $byvar ]]; then
		 echo "proc sort data= $dataset;" >> $HOME/pp$$.sas
		 echo " by $byvar;" >> $HOME/pp$$.sas
		 echo "run;" >> $HOME/pp$$.sas
		 echo " " >> $HOME/pp$$.sas
		fi
	 

		 if [[ -n $var ]]; then
			echo "proc univariate data=$dataset;" >> $HOME/pp$$.sas
			echo "var $var;">> $HOME/pp$$.sas
				if [[ -n $condition ]]; then
				echo "where $condition;">> $HOME/pp$$.sas
				fi
				if [[ -n $byvar ]]; then
				echo "by $byvar;">> $HOME/pp$$.sas
				fi
			echo "run;" >> $HOME/pp$$.sas
			echo " " >> $HOME/pp$$.sas
			echo "Running proc univariate on dataset: $dataset ($sasdir)"
		 
		 else
			echo "No variable provided"
			exit
		 fi
		
else
	echo "No SAS function specified"
	exit
fi

# Run the program in the temporary directory .$HOME
cd $HOME
sas pp$$.sas
echo " " >> $HOME/pp$$.lst

grep -i "sas stopped" $HOME/pp$$.log
test -f $HOME/pp$$.lst
stat=$?

if [[ $stat == 0 ]]; then
lines=`head -10 $HOME/pp$$.lst | wc -l`

	 if [[ $lines -le 2 ]]; then
	 echo No output lst file was created.
	 view $HOME/pp$$.log
	 else
	 view $HOME/pp$$.lst
	 fi
else
echo No output lst file was created.
view $HOME/pp$$.log
fi
rm -rf $HOME/pp*
exit
