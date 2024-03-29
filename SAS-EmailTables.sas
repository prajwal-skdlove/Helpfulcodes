%let path = ;
%put &path.;
%let SAS_LogDateTimeStamp   = SAS_%sysfunc(putn(%sysfunc(date()),date9.))_%sysfunc(translate(%sysfunc(putn(%sysfunc(time()),time12.)),.,:));
%put &SAS_LogDateTimeStamp;
%let LogOutFile  = &path./Log/&SAS_LogDateTimeStamp..log;
%put &logoutFile.;
filename logfile "&LogOutFile";

proc printto log=logfile;
run;

/* Insert custom code after server connection here */
options
	debug = dbms_timers
	sastrace=',,,d'
	/*Request SAS generate code trace for all SQL steps & provides timing information*/
sastraceloc=Saslog no$stsuffix fullstimer;

/*Tells SAS to write it to the program log*/
options mprint symbolgen spool mlogic macrogen;

/*SAS Macro debugging*/
Options Sqlgeneration = dbms dbidirectexec sql_ip_trace=source;
libname sid 'path';
%let sid = &sysuserid.;


