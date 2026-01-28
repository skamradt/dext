@echo off
setlocal

if "%~1"=="" (
    echo Usage: build_project.bat "path\to\project.dproj" [Make|Build] [Config] [Platform]
    echo Default target: Make
    echo Default config: Debug
    echo Default platform: Win32
    exit /b 1
)

call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"

set PROJECT=%~1
if /I "%PROJECT:~-4%"==".dpr" (
    set PROJECT=%PROJECT:~0,-4%.dproj
)

if not exist "%PROJECT%" (
    echo.
    echo ERROR: Project file not found: %PROJECT%
    exit /b 1
)

set TARGET=%~2
if "%TARGET%"=="" set TARGET=Make

set CONFIG=%~3
if "%CONFIG%"=="" set CONFIG=Debug

set PLATFORM=%~4
if "%PLATFORM%"=="" set PLATFORM=Win32

echo.
echo ==========================================
echo Building Project: %PROJECT%
echo Target: %TARGET% - Config: %CONFIG% - Platform: %PLATFORM%
echo ==========================================
echo.

msbuild "%PROJECT%" /t:%TARGET%  /p:Config=%CONFIG%  /p:Platform=%PLATFORM% /v:minimal /nologo

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo BUILD FAILED!
    exit /b %ERRORLEVEL%
)

echo.
echo BUILD SUCCESSful!
endlocal
