## Functions common to many model jobs
## 03.03.2003 Ole Vignes ..... initial version
## 11.03.2003 Ole Vignes ..... added elapsed time
## 24.01.2007 Ole Vignes ..... added waitfor

## Write runstatus file
function Runstatus {
   if [ "x$starttime" = "x" ]; then
      starttime=`perl -e 'print time;'`
      text="JOB=$LOADL_STEP_ID"
   else
      text="T="`$HOME/bin/elapsed.pl $starttime`
   fi
   now=`date`
   echo "$1 $now .... $jobname $2 $text" >> $HOME/status/runstatus
}

## Wait for a given file, with timeout
function waitfor {
    file=$1
    maxrun=${2-600}
    start=`perl -e 'print time'`
    OK=no
    while [ "$OK" != yes ]; do
        if [ -e $file ]; then
	    echo "Found $file"
            OK=yes
	else
	    now=`perl -e 'print time'`
	    if [ $(($now - $start)) -gt $maxrun ]; then
		echo "$file not found in $maxrun seconds - continue"
		OK=yes
	    else
		sleep 60
	    fi
	fi
    done
}
