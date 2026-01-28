@echo off
setlocal
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
msbuild TestAttributeRunner.dproj /t:Clean;Build /p:Config=Debug /p:Platform=Win32
if errorlevel 1 goto error
echo Build Success
exit /b 0

:error
echo Build Failed
exit /b 1
