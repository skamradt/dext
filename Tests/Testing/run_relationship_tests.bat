@echo off
setlocal
call "c:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
echo Building TestORMRelationships...
msbuild TestORMRelationships.dproj /t:Clean;Build /p:Config=Debug /p:Platform=Win32 > build.log 2>&1
if errorlevel 1 goto error
echo Running Tests...
..\Output\TestORMRelationships.exe > test_results_12.log 2>&1
exit /b 0

:error
echo Build Failed. Check build.log
exit /b 1
