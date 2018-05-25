#!/bin/sh
### BEGIN INIT INFO
# Provides:          filenotifier
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Something
# Description:       Something else
### END INIT INFO


MONITOR_DIR="/opt/apache/tomcat-9.0.6/update/cron"
SCRIPT_FILE="/opt/apache/tomcat-9.0.6/update/script.sh"
TOMCAT_HOME=/opt/apache/tomcat-9.0.6
TOMCAT_BIN=$TOMCAT_HOME/bin
TOMCAT_WEBAPPS=$TOMCAT_HOME/webapps
TOMCAT_UPDATE_LOG=$TOMCAT_HOME/update/log.txt
TOMCAT_UPDATE_POOL=$TOMCAT_HOME/update/pool


inotifywait -m -r -e create --format '%w%f' "${MONITOR_DIR}" | while read NEWFILE

do
   echo "---------------------------------------------" >> $TOMCAT_UPDATE_LOG

   echo "`date` Start running script to upgrade..." >> $TOMCAT_UPDATE_LOG

   if test "$(ls -A "$TOMCAT_UPDATE_POOL")"; then

     echo "$TOMCAT_UPDATE_POOL is not empty" >> $TOMCAT_UPDATE_LOG

     echo "Trigger killing tomcat..." >> $TOMCAT_UPDATE_LOG

     #kill -9 | grep catalina
     #pkill -9 -f catalina
     ps -ef | grep catalina | awk '{print $2}' | xargs kill -9
     sleep 5

     echo "Tomcat stopped !!!!" >> $TOMCAT_UPDATE_LOG

     echo "Deleting old log files..."  >> $TOMCAT_UPDATE_LOG

     rm -rf $TOMCAT_HOME/logs/catalina.out
     find $TOMCAT_HOME/logs/ -mtime +30 -type f -delete
     find $TOMCAT_HOME/logs/webapps/ -mtime +30 -type f -delete

     for entry in "$TOMCAT_UPDATE_POOL"/*
      do
        echo "Found file $entry" >> $TOMCAT_UPDATE_LOG
    	appName=$(basename "$entry" ".war")
        fileName=$(basename "$entry")

        echo "Remove $TOMCAT_WEBAPPS/$appName" >> $TOMCAT_UPDATE_LOG
        rm -rf $TOMCAT_WEBAPPS/$appName

        echo "Remove $TOMCAT_WEBAPPS/$fileName" >> $TOMCAT_UPDATE_LOG
        rm -rf $TOMCAT_WEBAPPS/$fileName

        echo "Move $entry to $TOMCAT_WEBAPPS/$fileName" >> $TOMCAT_UPDATE_LOG
        mv $entry $TOMCAT_WEBAPPS/$fileName

        echo "System is reset now..."  >> $TOMCAT_UPDATE_LOG
        echo "---------------------------------------------" >> $TOMCAT_UPDATE_LOG

	reboot

      done
    else
       echo "$TOMCAT_UPDATE_POOL is empty" >> $TOMCAT_UPDATE_LOG
    fi

    echo "`date` Finish running script to upgrade..." >> $TOMCAT_UPDATE_LOG

done
