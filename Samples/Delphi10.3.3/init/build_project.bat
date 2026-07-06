@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
msbuild "d:\2.2 GitHub Adriano Santos\MultiDialog4FMX\Samples\init\Proj1.dproj" /t:Build /p:Config=Debug /p:Platform=Android64
