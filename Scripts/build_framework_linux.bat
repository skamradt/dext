@echo off
setlocal

echo Setting up Delphi environment...
REM Try to find rsvars.bat in common locations
set RS_VARS="C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
if not exist %RS_VARS% set RS_VARS="C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
if not exist %RS_VARS% set RS_VARS="C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"

if not exist %RS_VARS% (
    echo [ERROR] rsvars.bat not found. Please edit this script with your Delphi bin path.
    exit /b 1
)

call %RS_VARS%

echo.
echo ==========================================
echo Building Dext Framework Packages (Linux64)
echo ==========================================
echo.

REM Build configuration
set BUILD_CONFIG=Release
set PLATFORM=Linux64

REM Extract ProductVersion from BDS (e.g., 37.0)
for %%i in ("%BDS%") do set PRODUCT_VERSION=%%~nxi

REM Repo root and paths
set DEXT=%~dp0..
set OUTPUT_PATH=%DEXT%\Output\%PRODUCT_VERSION%_%PLATFORM%_%BUILD_CONFIG%
set SEARCH_PATH=%OUTPUT_PATH%

REM Create output directories
if not exist "%DEXT%\Output" mkdir "%DEXT%\Output"
if not exist "%OUTPUT_PATH%" mkdir "%OUTPUT_PATH%"

echo Output directory: %OUTPUT_PATH%
echo.

echo Building Dashboard Resources...
powershell -NoProfile -ExecutionPolicy Bypass -File "%DEXT%\Sources\Dashboard\build-resources.ps1" -OutputPath "%OUTPUT_PATH%"
if errorlevel 1 goto Error
echo.

REM Clean output
echo Cleaning output directory...
del /Q "%OUTPUT_PATH%\*.dcu" 2>nul
del /Q "%OUTPUT_PATH%\*.o" 2>nul
del /Q "%OUTPUT_PATH%\*.so" 2>nul
del /Q "%OUTPUT_PATH%\*.dcp" 2>nul

cd "%DEXT%\Sources"

REM Packages in dependency order
set PACKAGES=Dext.Core.dproj Dext.Net.dproj Dext.EF.Core.dproj Dext.Web.Core.dproj Dext.Web.Hubs.dproj Dext.Hosting.dproj Dext.Testing.dproj

for %%P in (%PACKAGES%) do (
    echo Building %%P...
    msbuild "%%P" /t:Build /p:Configuration=%BUILD_CONFIG% /p:Platform=%PLATFORM% /p:DCC_DcuOutput="%OUTPUT_PATH%" /p:DCC_BplOutput="%OUTPUT_PATH%" /p:DCC_DcpOutput="%OUTPUT_PATH%" /p:DCC_UnitSearchPath="%SEARCH_PATH%" /v:minimal
    if errorlevel 1 goto Error
    echo.
)

echo.
echo ==========================================
echo BUILD SUCCESSFUL! (Linux64)
echo Output: %OUTPUT_PATH%
echo ==========================================
if not "%1"=="--no-wait" pause
exit /b 0

:Error
echo.
echo ==========================================
echo BUILD FAILED!
echo ==========================================
if not "%1"=="--no-wait" pause
exit /b 1
