#!/bin/sh

TOMCAT_HOME=/opt/apache/tomcat-9.0.4
TOMCAT_BIN=$TOMCAT_HOME/bin
TOMCAT_WEBAPPS=$TOMCAT_HOME/webapps
TOMCAT_UPDATE_LOG=$TOMCAT_HOME/update/log.txt
TOMCAT_UPDATE_POOL=$TOMCAT_HOME/update/pool
TOMCAT_STOP_TRIGGER=$TOMCAT_HOME/update/stop/trigger.txt

echo "`date` Start running script to upgrade..." >> $TOMCAT_UPDATE_LOG

if test "$(ls -A "$TOMCAT_UPDATE_POOL")"; then

  echo "$TOMCAT_UPDATE_POOL is not empty" >> $TOMCAT_UPDATE_LOG

  echo "Trigger killing tomcat..." >> $TOMCAT_UPDATE_LOG

  echo "`date` Trigger killing tomcat..."  >> $TOMCAT_STOP_TRIGGER

  sleep 10

  echo "Deleting old log files..."  >> $TOMCAT_UPDATE_LOG

  rm -rf $TOMCAT_HOME/logs/catalina.out
  find $TOMCAT_HOME/logs/ -mtime +7 -type f -delete
  find $TOMCAT_HOME/logs/webapps/ -mtime +7 -type f -delete

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

    echo "Starting tomcat..."  >> $TOMCAT_UPDATE_LOG
    $TOMCAT_BIN/startup.sh
    echo "Finish updating tomcat..."  >> $TOMCAT_UPDATE_LOG

  done
else
   echo "$TOMCAT_UPDATE_POOL is empty" >> $TOMCAT_UPDATE_LOG 
fi

echo "`date` Finish running script to upgrade..." >> $TOMCAT_UPDATE_LOG
