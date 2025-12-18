@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
set SEARCH_PATH=%~dp0..\Output;%~dp0..\Sources\Core;%~dp0..\Sources\Data;%~dp0..\Sources\Web;%~dp0..\Sources\Core\Base;%~dp0..\Sources\Core\Specifications
echo Building Inheritance Test...
dcc32 ..\Tests\TestOrmInheritance.dpr -U"%SEARCH_PATH%" -I"..\Output" -N"..\Output" -E"..\Output" > build_tests.log 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo Inheritance Test Build FAILED
    type build_tests.log
    exit /b 1
)
echo Building JSON Test...
dcc32 ..\Tests\TestJsonCore.dpr -U"%SEARCH_PATH%" -I"..\Output" -N"..\Output" -E"..\Output" >> build_tests.log 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo JSON Test Build FAILED
    type build_tests.log
    exit /b 1
)

echo.
echo Running Inheritance Test...
..\Output\TestOrmInheritance.exe
echo.
echo Running JSON Test...
..\Output\TestJsonCore.exe
