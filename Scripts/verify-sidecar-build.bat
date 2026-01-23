@echo off
setlocal

echo Setting up Delphi environment...
if exist "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat" (
    call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
) else (
    echo "rsvars.bat not found at expected location. Checking default..."
    REM Fallback or just try to continue if BDS is already set
)

if "%BDS%"=="" (
    echo BDS environment variable is not set. Cannot continue.
    exit /b 1
)

echo.
echo ==========================================
echo Building Dext Sidecar
echo ==========================================
echo.

REM Build configuration
set BUILD_CONFIG=Debug
set PLATFORM=Win32

REM Extract ProductVersion from BDS (e.g., 22.0, 23.0, etc.)
for %%i in ("%BDS%\.") do set PRODUCT_VERSION=%%~nxi

REM Output paths
set DEXT=%~dp0..
set OUTPUT_PATH=%DEXT%\Output\%PRODUCT_VERSION%_%PLATFORM%_%BUILD_CONFIG%

echo Output directory: %OUTPUT_PATH%
echo.

cd "%~dp0..\Apps\Sidecar"

echo Building DextSidecar.dproj...
msbuild "DextSidecar.dproj" /t:Clean;Build /p:Config=%BUILD_CONFIG% /p:Platform=%PLATFORM% /p:DCC_DcuOutput="%OUTPUT_PATH%" /p:DCC_UnitSearchPath="%OUTPUT_PATH%" /v:minimal
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
