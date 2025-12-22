@echo off
setlocal

echo Setting up Delphi environment...
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"

echo.
echo ==========================================
echo Building Dext CLI Tool
echo ==========================================
echo.

set OUTPUT_PATH=%~dp0..\Output
set SOURCE_PATH=%~dp0..\Sources

if not exist "%OUTPUT_PATH%" mkdir "%OUTPUT_PATH%"

dcc32 "%~dp0..\Apps\CLI\DextTool.dpr" -B -E"%OUTPUT_PATH%" -N"%OUTPUT_PATH%" -I"%SOURCE_PATH%" -U"%SOURCE_PATH%;%SOURCE_PATH%\Core;%SOURCE_PATH%\Core\Base;%SOURCE_PATH%\Data;%SOURCE_PATH%\Hosting\CLI;%SOURCE_PATH%\Hosting\CLI\Commands;%SOURCE_PATH%\Core\Json" -NS"System;System.Generics;Data;Data.Win;Winapi"

if %ERRORLEVEL% NEQ 0 goto Error

rem Rename to desired executable name
if exist "%OUTPUT_PATH%\DextTool.exe" move /Y "%OUTPUT_PATH%\DextTool.exe" "%OUTPUT_PATH%\dext.exe"

echo.
echo ==========================================
echo CLI Build Completed Successfully!
echo ==========================================
if not "%1"=="--no-wait" pause
exit /b 0

:Error
echo.
echo ==========================================
echo CLI BUILD FAILED!
echo ==========================================
if not "%1"=="--no-wait" pause
exit /b 1
