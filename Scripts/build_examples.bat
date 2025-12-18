@echo off
setlocal enabledelayedexpansion

echo ==========================================
echo Building Dext Examples
echo ==========================================
echo.

set FAILED_PROJECTS=
set SUCCESS_COUNT=0
set FAIL_COUNT=0
set PROCESSED=

REM Find all .dproj files in Examples folder
for /r "%~dp0..\Examples" %%f in (*.dproj) do (
    set "PROJECT_PATH=%%f"
    set "PROJECT_NAME=%%~nf"
    
    REM Skip if already processed (avoid duplicates)
    echo !PROCESSED! | findstr /i "!PROJECT_NAME!" >nul
    if !ERRORLEVEL! NEQ 0 (
        set PROCESSED=!PROCESSED! !PROJECT_NAME!
        
        echo Building: !PROJECT_NAME!
        echo ------------------------------------------
        
        REM Build using MSBuild
        msbuild "!PROJECT_PATH!" /p:Config=Release /p:Platform=Win32 /v:minimal /nologo
        
        if !ERRORLEVEL! EQU 0 (
            echo [SUCCESS] !PROJECT_NAME!
            set /a SUCCESS_COUNT+=1
        ) else (
            echo [FAILED] !PROJECT_NAME!
            set FAILED_PROJECTS=!FAILED_PROJECTS! !PROJECT_NAME!
            set /a FAIL_COUNT+=1
        )
        echo.
    )
)

echo ==========================================
echo Build Summary
echo ==========================================
echo Success: %SUCCESS_COUNT%
echo Failed:  %FAIL_COUNT%

if not "%FAILED_PROJECTS%"=="" (
    echo.
    echo Failed Projects:
    for %%p in (%FAILED_PROJECTS%) do echo   - %%p
    echo.
    echo ==========================================
    echo BUILD COMPLETED WITH ERRORS
    echo ==========================================
    pause
    exit /b 1
) else (
    echo.
    echo ==========================================
    echo ALL EXAMPLES BUILT SUCCESSFULLY!
    echo ==========================================
    pause
    exit /b 0
)
