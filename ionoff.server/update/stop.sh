#!/bin/sh

TOMCAT_UPDATE_LOG=/opt/apache/tomcat-9.0.4/update/log.txt

echo "[stop.sh] Stopping tomcat ..." >> $TOMCAT_UPDATE_LOG
#kill -9 | grep catalina
pkill -9 -f catalina
#ps -ef | grep tomcat | awk '{print $2}' | xargs kill -9
echo "[stop.sh] Tomcat stopped !!!!" >> $TOMCAT_UPDATE_LOG


