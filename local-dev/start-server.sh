#!/bin/bash

export JAVA_HOME=/usr/lib/jvm/jdk1.8.0_291
java -version
rm -rf tomcat-9/webapps/*
cp ../ionoff.server/target/icenter.war tomcat-9/webapps/icenter.war
./tomcat-9/bin/catalina.sh run
