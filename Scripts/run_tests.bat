@echo off
setlocal enabledelayedexpansion

echo ==========================================
echo Building and Running Dext Tests
echo ==========================================
echo.

REM Setup Delphi environment
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"

set FAILED_TESTS=
set SUCCESS_COUNT=0
set FAIL_COUNT=0
set SKIPPED_COUNT=0
set BUILD_FAIL_COUNT=0

echo ==========================================
echo Step 1: Building All Tests
echo ==========================================
echo.
set /A buildnbr=0
set /A buildcnt=0

for /r "%~dp0..\Tests" %%f in (*.dproj) do (
   set /A buildcnt+=1
)

for /r "%~dp0..\Tests" %%f in (*.dproj) do (
    SET /A buildnbr+=1
    title Building Test !buildnbr! of %buildcnt%
    set "PROJECT_NAME=%%~nf"
    set "PROJECT_FILE=%%f"
    
    echo !PROJECT_NAME! | findstr /i "test" >nul
    if !ERRORLEVEL! EQU 0 (
        call :build_project "!PROJECT_NAME!" "!PROJECT_FILE!"
    )
)

echo.
echo ==========================================
echo Step 2: Running All Tests
echo ==========================================
echo.
set /A testnbr=0
set /A testcnt=0

for /r "%~dp0..\Tests" %%f in (*.dproj) do (
   set /A testcnt+=1
)

for /r "%~dp0..\Tests" %%f in (*.dproj) do (
    SET /A testnbr+=1
    title Executing Test !testnbr! of %testcnt%
    set "PROJECT_NAME=%%~nf"
    set "PROJECT_DIR=%%~dpf"
    
    echo !PROJECT_NAME! | findstr /i "test" >nul
    if !ERRORLEVEL! EQU 0 (
        call :run_project "!PROJECT_NAME!" "!PROJECT_DIR!"
    )
)

echo.
echo ==========================================
echo Test Summary
echo ==========================================
title   
echo Build Failures: %BUILD_FAIL_COUNT%
echo Passed:  %SUCCESS_COUNT%
echo Failed:  %FAIL_COUNT%
echo Skipped: %SKIPPED_COUNT%

if not "%FAILED_TESTS%"=="" (
    echo.
    echo Failed Tests:
    for %%p in (%FAILED_TESTS%) do echo   - %%p
    echo.
    echo ==========================================
    echo TESTS COMPLETED WITH FAILURES
    echo ==========================================
    exit /b 1
) else (
    if %BUILD_FAIL_COUNT% GTR 0 (
        echo.
        echo ==========================================
        echo SOME TESTS FAILED TO BUILD
        echo ==========================================
        exit /b 1
    ) else (
        echo.
        echo ==========================================
        if %SUCCESS_COUNT% GTR 0 (
            echo ALL TESTS PASSED!
        ) else (
            echo NO TESTS FOUND TO RUN
        )
        echo ==========================================
        exit /b 0
    )
)

goto :eof

REM ---------------------------------------------------------------------------
REM Subroutines
REM ---------------------------------------------------------------------------

:build_project
    set "P_NAME=%~1"
    set "P_FILE=%~2"
    
    echo Building: !P_NAME!
    REM Use OutputPath override to ensure all tests go to Tests\Output
    msbuild "!P_FILE!" /t:Make /p:Config=Debug /p:Platform=Win32 /p:DCC_ExeOutput="%~dp0..\Tests\Output" /v:minimal /nologo
    
    if !ERRORLEVEL! NEQ 0 (
        echo [BUILD FAILED] !P_NAME!
        set /a BUILD_FAIL_COUNT+=1
    ) else (
        echo [BUILD OK] !P_NAME!
    )
    echo.
    exit /b 0

:run_project
    set "P_NAME=%~1"
    set "P_DIR=%~2"
    
    echo.
    echo ==========================================
    echo Testing: !P_NAME!
    echo ==========================================
    
    set "EXE_FOUND="
    set "EXE_PATH="
    
    REM Priority 1: Check shared Tests\Output (as requested by user)
    if exist "%~dp0..\Tests\Output\!P_NAME!.exe" (
        set "EXE_PATH=%~dp0..\Tests\Output\!P_NAME!.exe"
        set "EXE_FOUND=1"
    )
    
    REM Priority 2: Check local Output
    if not defined EXE_FOUND (
        if exist "!P_DIR!Output\!P_NAME!.exe" (
            set "EXE_PATH=!P_DIR!Output\!P_NAME!.exe"
            set "EXE_FOUND=1"
        )
    )
    
    REM Priority 3: Check standard Delphi output folders
    if not defined EXE_FOUND (
        if exist "!P_DIR!Win32\Debug\!P_NAME!.exe" (
            set "EXE_PATH=!P_DIR!Win32\Debug\!P_NAME!.exe"
            set "EXE_FOUND=1"
        )
    )

    if defined EXE_FOUND (
        echo Running: !EXE_PATH!
        "!EXE_PATH!" -no-wait
        
        if !ERRORLEVEL! EQU 0 (
            echo [PASSED] !P_NAME!
            set /a SUCCESS_COUNT+=1
        ) else (
            echo [FAILED] !P_NAME! (Exit code: !ERRORLEVEL!)
            set FAILED_TESTS=!FAILED_TESTS! !P_NAME!
            set /a FAIL_COUNT+=1
        )
    ) else (
        echo [SKIPPED] !P_NAME! - Executable not found
        set /a SKIPPED_COUNT+=1
    )
    exit /b 0
