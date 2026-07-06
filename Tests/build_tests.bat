@echo off
setlocal
call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
set LOG=%~dp0build_log.txt
msbuild "%~dp0MultiDialog4FMX.Tests.dproj" /t:Build /p:Config=Debug /p:Platform=Win32 /nologo /v:minimal /clp:NoSummary > "%LOG%" 2>&1
set ERR=%ERRORLEVEL%
type "%LOG%"
exit /b %ERR%
