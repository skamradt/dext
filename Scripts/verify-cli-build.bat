@echo off
setlocal

echo Setting up Delphi environment...
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"

echo.
echo ==========================================
echo Building Dext CLI
echo ==========================================
echo.

REM Build configuration
set BUILD_CONFIG=Debug
set PLATFORM=Win32

REM Extract ProductVersion from BDS (e.g., 37.0)
for %%i in ("%BDS%") do set PRODUCT_VERSION=%%~nxi

REM Output paths
set DEXT=%~dp0..
set OUTPUT_PATH=%DEXT%\Output\%PRODUCT_VERSION%_%PLATFORM%_%BUILD_CONFIG%

echo Output directory: %OUTPUT_PATH%
echo.

cd "%~dp0..\Apps\CLI"

echo Building DextTool.dproj...
msbuild "DextTool.dproj" /t:Clean;Build /p:Configuration=%BUILD_CONFIG% /p:Platform=%PLATFORM% /p:DCC_DcuOutput="%OUTPUT_PATH%" /p:DCC_UnitSearchPath="%OUTPUT_PATH%" /v:minimal
if %ERRORLEVEL% NEQ 0 goto Error

echo.
echo ==========================================
echo Build Completed Successfully!
echo ==========================================
exit /b 0

:Error
echo.
echo ==========================================
echo BUILD FAILED!
echo ==========================================
exit /b 1
