## Welcome to IOnOff!

### What Is It?
IOnOff is a comprehensive system to control on/off electric device remotely using mobile device over internet.
To simply replace the dump switches by the IOnOff switches (i-switches) and register the i-switches to IOnOff software.

### Introduction
This project integrates:

A frontend web interface for user interaction.
A Java-based backend server for business logic and API handling.
Embedded C code deployed on a microcontroller to control devices remotely.
It enables users to control devices (e.g., turning them on/off) over the internet, leveraging IoT principles.

### Features
Web-based user interface for device management.
Secure backend APIs for data handling and device communication.
Embedded C firmware to interface with hardware for device control.
Real-time synchronization between UI and devices.

### Architecture Overview
Frontend: Developed using GWT, connects to backend APIs to interact with devices.
Backend: Java-based application built with Spring Boot, JPA, MySQL database. Backend manages business logic and handles REST APIs.
Embedded System: Microcontroller (PIC 18F4550 for ethernet wire connection i-switch and ESP8266 for wifi connection i-switch) code written in C.

### Getting Started
To get started, clone the repository and navigate to the repo folder
Folder Structure

```
.
├── ionoff.client/        # Web application code writiing in Java using GWT
├── ionoff.server/        # Spring boot web service, Java code
├── ionoff.things/        # Embedded C code for the microcontroller
├── ...                  
└── README.md             # This README file
```

### Installation and Setup
#### Frontend
##### Setup GWT Super Dev Mode (IntelliJ)
Run -> Edit Configurations -> + -> Application
```
Name:           ionoff.client
Run on:         Local machine
Build and run:  Runtime:    Java 1.8
                Mudule:     -cp ionoff.client
                Main class: com.google.gwt.dev.codeserver.CodeServer
                Arguments: -src src/main/java -launcherDir /$your_path_to_ionoff.client/src/main/webapp net.ionoff.center.IOnOff
                Working directory: /$your_path_to_ionoff.client
                Environment variable: 
```

##### Setup web application in tomcat
- Install tomcat (simply extract `ionoff.tomcat/tomcat9.zip`)
- Copy `ionoff.client/ionoff.xml` to `$your_tomcat_folder/conf/Catalina/localhost`
- Open `ionoff.xml` and change `docBase` to `$your_path_to_ionoff.client/src/main/webapp`

##### Start front-end
- CD to tomcat folder
- Run `export JAVA_HOME='/usr/lib/jvm/jdk1.8.0_291'`
- Run `./bin/catalina.sh run`
- Start tomcat on console 
- Start GWT Super Dev Mode by start the IntelliJ run config `ionoff.client`

#### Backend
- Run `mvn clean install`
- Start `net.ionoff.center.server.CenterServerApplication`

