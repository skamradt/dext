@echo off
setlocal

echo Setting up Delphi environment...
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"

echo.
echo ==========================================
echo Building Dext Framework Packages (Win32)
echo ==========================================
echo.

REM Build configuration
set BUILD_CONFIG=Debug
set PLATFORM=Win32

REM Extract ProductVersion from BDS (e.g., 37.0)
for %%i in ("%BDS%") do set PRODUCT_VERSION=%%~nxi

REM Output paths matching .dproj configuration: $(dext)\Output\$(ProductVersion)_$(Platform)_$(Config)
set DEXT=%~dp0..
set OUTPUT_PATH=%DEXT%\Output\%PRODUCT_VERSION%_%PLATFORM%_%BUILD_CONFIG%

REM Create output directories if they don't exist
if not exist "%DEXT%\Output" mkdir "%DEXT%\Output"
if not exist "%OUTPUT_PATH%" mkdir "%OUTPUT_PATH%"

echo Output directory: %OUTPUT_PATH%
echo.

cd "%~dp0..\Sources\Dashboard"
echo Building Dashboard Resources...
powershell -NoProfile -ExecutionPolicy Bypass -File "build-resources.ps1" -OutputPath "%OUTPUT_PATH%"
if %ERRORLEVEL% NEQ 0 goto Error

cd "%~dp0..\Sources"

echo Building Dext.Core...
msbuild "Dext.Core.dproj" /t:Clean;Build /p:Configuration=%BUILD_CONFIG% /p:Platform=%PLATFORM% /p:DCC_DcuOutput="%OUTPUT_PATH%" /p:DCC_UnitSearchPath="%OUTPUT_PATH%" /v:minimal
if %ERRORLEVEL% NEQ 0 goto Error

echo.
echo Building Dext.EF.Core...
msbuild "Dext.EF.Core.dproj" /t:Clean;Build /p:Configuration=%BUILD_CONFIG% /p:Platform=%PLATFORM% /p:DCC_DcuOutput="%OUTPUT_PATH%" /p:DCC_UnitSearchPath="%OUTPUT_PATH%" /v:minimal
if %ERRORLEVEL% NEQ 0 goto Error

echo.
echo Building Dext.Web.Core...
msbuild "Dext.Web.Core.dproj" /t:Clean;Build /p:Configuration=%BUILD_CONFIG% /p:Platform=%PLATFORM% /p:DCC_DcuOutput="%OUTPUT_PATH%" /p:DCC_UnitSearchPath="%OUTPUT_PATH%" /v:minimal
if %ERRORLEVEL% NEQ 0 goto Error

echo.
echo Building Dext.Web.Hubs...
msbuild "Dext.Web.Hubs.dproj" /t:Clean;Build /p:Configuration=%BUILD_CONFIG% /p:Platform=%PLATFORM% /p:DCC_DcuOutput="%OUTPUT_PATH%" /p:DCC_UnitSearchPath="%OUTPUT_PATH%" /v:minimal
if %ERRORLEVEL% NEQ 0 goto Error

echo.
echo Building Dext.Hosting...
msbuild "Dext.Hosting.dproj" /t:Clean;Build /p:Configuration=%BUILD_CONFIG% /p:Platform=%PLATFORM% /p:DCC_DcuOutput="%OUTPUT_PATH%" /p:DCC_UnitSearchPath="%OUTPUT_PATH%" /v:minimal
if %ERRORLEVEL% NEQ 0 goto Error

echo.
echo Building Dext.Testing...
msbuild "Dext.Testing.dproj" /t:Clean;Build /p:Configuration=%BUILD_CONFIG% /p:Platform=%PLATFORM% /p:DCC_DcuOutput="%OUTPUT_PATH%" /p:DCC_UnitSearchPath="%OUTPUT_PATH%" /v:minimal
if %ERRORLEVEL% NEQ 0 goto Error

echo.
echo Building Dext.UI...
msbuild "Dext.UI.dproj" /t:Clean;Build /p:Configuration=%BUILD_CONFIG% /p:Platform=%PLATFORM% /p:DCC_DcuOutput="%OUTPUT_PATH%" /p:DCC_UnitSearchPath="%OUTPUT_PATH%" /v:minimal
if %ERRORLEVEL% NEQ 0 goto Error

echo.
echo Building Dext.Net...
msbuild "Dext.Net.dproj" /t:Clean;Build /p:Configuration=%BUILD_CONFIG% /p:Platform=%PLATFORM% /p:DCC_DcuOutput="%OUTPUT_PATH%" /p:DCC_UnitSearchPath="%OUTPUT_PATH%" /v:minimal
if %ERRORLEVEL% NEQ 0 goto Error

echo.
echo ==========================================
echo Build Completed Successfully!
echo Output: %OUTPUT_PATH%
echo ==========================================
if not "%1"=="--no-wait" pause
cd ..
exit /b 0

:Error
echo.
echo ==========================================
echo BUILD FAILED!
echo ==========================================
if not "%1"=="--no-wait" pause
cd ..
exit /b 1

