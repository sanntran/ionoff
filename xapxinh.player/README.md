XapXinh is a media system includes XapXinh Media Player (XMP) and XapXinh Center Server (XCS).

XMP player based on VLC and VLCj and XMP is to control remotely from smartphone, tablet or PC

Many thanks to VLC and VLCj.

Features:

- Play local files

- Search and play Youtube videos

- Search and play albums that hosted on XCS

By default, after started, XMP connects to XCS and get a ID. Then you can use the ID to login to XCS web page to control the XPM

Current XMP release version is 1.0.1. You can download here

Steps to install (Window XP, 7, 8, 10 64bit)

1. Download and unzip to a folder

2. Run xmp.exe

3. Find id and password from taskbar icon info message

4. Use the id and password to login to xapxinh.net

Steps to setup project (Eclipse)

0. Extract vlc.zip from xapxinh.player\store to xapxinh.player (after extracted folder structure is xapxinh.player\vlc)
   
   Copy file xapxinh.player\store\nircmd_64.exe to xapxinh.player\nir\nircmd.exe

1. Import project

2. Install maven dependency by runing script in mvn-install-jshortcut.bat

3. Run AppMain.java as Java Application

4. Right click to AppMain.java -> Run > Run Configurations...

5. Add new Variable Environment: 

- Name: VLC_PLUGIN_PATH 

- Value: Full path to xapxinh.player\vlc

Happy coding!


XapXinh Media Player
====================

XapXinh is a media system including XapXinh Media Player (XMP), IOnOff Music Player (IMP) and XapXinh Media Server

XapXinh helps users control media player remotely via smartphone. With XapXinh, users can use smartphone to browse local files, search albums in cloud server and search youtube video, and play selected media in remote media player (XMP or IMP)

Steps to setup project in eclipse:

1. Import project to eclipse

2. Run command 
   mvn install:install-file -Dfile=jshortcut-0.4.jar -DgroupId=jshortcut -DartifactId=jshortcut -Dversion=0.4 -Dpackaging=jar
   Or open folder xapxinh.player\\player and double click to file mvn-install-jshortcut.bat
 
   (Make sure maven has been installed)

3. Unzip xapxinh.player\store\vlc.zip to folder xapxinh.player

4. To run XMP on Windows from Eclipse:
   Right click to file AppMain.java and Run As -> Java Application

   Right click to file AppMain.java and Run As -> Run Configurations

   Click to tab Environent and add new variable:

   VLC_PLUGIN_PATH Path to vlc folder inside git folder


LICENSE

The xmp project is provided under the GPL, version 3 or later.



