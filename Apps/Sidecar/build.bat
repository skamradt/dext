@echo off
setlocal enabledelayedexpansion

echo Finding rsvars.bat...

set "RSVARS="

REM Search for rsvars.bat in likely locations (newest first)
for %%v in (24.0 23.0 22.0 21.0 20.0 19.0) do (
    if exist "C:\Program Files (x86)\Embarcadero\Studio\%%v\bin\rsvars.bat" (
        set "RSVARS=C:\Program Files (x86)\Embarcadero\Studio\%%v\bin\rsvars.bat"
        goto :Found
    )
)

:Found
if "%RSVARS%"=="" (
    where rsvars.bat >nul 2>nul
    if !errorlevel! equ 0 (
        set "RSVARS=rsvars.bat"
        echo Found in PATH
    ) else (
        echo Error: rsvars.bat not found. Please ensure Delphi is installed.
        exit /b 1
    )
)

echo Using environment: "%RSVARS%"
call "%RSVARS%"

echo.
echo ==========================================
echo Building Dext Sidecar (Debug/Win32)...
echo ==========================================
echo.

msbuild "DextSidecar.dproj" /t:Build /p:Config=Debug /p:Platform=Win32

if %errorlevel% neq 0 (
    echo.
    echo ❌ Build Failed!
    exit /b 1
)

echo.
echo ✅ Build Success!
echo Output: ..\..\Output\DextSidecar.exe
