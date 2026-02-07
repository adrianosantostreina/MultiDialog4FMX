@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
msbuild MultiDialog4FMX.Tests.dproj /p:Config=Debug /p:Platform=Win32 /v:minimal
