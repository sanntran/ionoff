@ECHO off
SET EXE=javaw.exe
SET APP=XapXinh Media Player
ECHO -----------------------------------------------------
FOR /F %%X IN ('tasklist /FI "WINDOWTITLE eq %APP%"') DO IF %%X == %EXE% GOTO ProcessFound
GOTO ProcessNotFound
:ProcessFound
ECHO %APP% is running
GOTO END
:ProcessNotFound
ECHO %APP% is not running 
IF EXIST "%~dp0ext" GOTO UpdateNewVersion
GOTO StartApp
:UpdateNewVersion
ECHO Updating jar...
IF EXIST "%~dp0ext\jar" (
    RMDIR "%~dp0jar" /s /q
	MOVE "%~dp0ext\jar" "%~dp0"
)
ECHO Updating vlc...
IF EXIST "%~dp0ext\vlc" (
    RMDIR "%~dp0vlc" /s /q
	MOVE "%~dp0ext\vlc" "%~dp0"
)
ECHO Updating jre...
IF EXIST "%~dp0ext\jre" (
    RMDIR "%~dp0jre" /s /q
	MOVE "%~dp0ext\jre" "%~dp0"
)
:StartApp
SET VLC_PLUGIN_PATH=%~dp0vlc
echo %VLC_PLUGIN_PATH%
start "XapXinh" "%~dp0jre\bin\javaw" -Xms512m -Xmx1024m -XX:MaxPermSize=512m -Dfile.encoding=UTF-8 -jar jar\xapxinh-player.jar
GOTO END
:END
ECHO -----------------------------------------------------