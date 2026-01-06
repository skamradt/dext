@echo off
setlocal

echo Setting up Delphi environment...
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"

echo.
echo ==========================================
echo Building Dext Framework Packages
echo ==========================================
echo.

REM Build configuration
set BUILD_CONFIG=Debug
set PLATFORM=Win32

REM Output paths matching .dproj configuration: ..\Output\$(Platform)\$(Config)
set BASE_OUTPUT=%~dp0..\Output
set OUTPUT_PATH=%BASE_OUTPUT%\%PLATFORM%\%BUILD_CONFIG%

REM Create output directories if they don't exist
if not exist "%BASE_OUTPUT%" mkdir "%BASE_OUTPUT%"
if not exist "%BASE_OUTPUT%\%PLATFORM%" mkdir "%BASE_OUTPUT%\%PLATFORM%"
if not exist "%OUTPUT_PATH%" mkdir "%OUTPUT_PATH%"

echo Output directory: %OUTPUT_PATH%
echo.

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

