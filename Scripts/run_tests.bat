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
set BUILD_FAIL_COUNT=0
set BUILD_SUCCESS_COUNT=0
set "OUTPUT_DIR=%~dp0..\Tests\Output"

echo ==========================================
echo Step 1: Building All Tests
echo ==========================================
echo.
set /A buildnbr=0
set /A buildcnt=0

REM Count test projects first
for /r "%~dp0..\Tests" %%f in (*.dproj) do (
   set "PROJ_NAME=%%~nf"
   echo !PROJ_NAME! | findstr /i "test" >nul
   if !ERRORLEVEL! EQU 0 (
      set /A buildcnt+=1
   )
)

REM Build each test project
for /r "%~dp0..\Tests" %%f in (*.dproj) do (
    set "PROJECT_NAME=%%~nf"
    set "PROJECT_FILE=%%f"
    
    echo !PROJECT_NAME! | findstr /i "test" >nul
    if !ERRORLEVEL! EQU 0 (
        SET /A buildnbr+=1
        title Building Test !buildnbr! of %buildcnt%
        call :build_project "!PROJECT_NAME!" "!PROJECT_FILE!"
    )
)

echo.
echo ==========================================
echo Step 2: Running All Tests
echo ==========================================
echo.
set /A testnbr=0

REM Run each test that was successfully built
for /r "%~dp0..\Tests" %%f in (*.dproj) do (
    set "PROJECT_NAME=%%~nf"
    
    echo !PROJECT_NAME! | findstr /i "test" >nul
    if !ERRORLEVEL! EQU 0 (
        SET /A testnbr+=1
        title Executing Test !testnbr!
        call :run_project "!PROJECT_NAME!"
    )
)

echo.
echo ==========================================
echo Test Summary
echo ==========================================
title Dext Tests Complete
echo Build Success:  %BUILD_SUCCESS_COUNT%
echo Build Failures: %BUILD_FAIL_COUNT%
echo.
echo Tests Passed:   %SUCCESS_COUNT%
echo Tests Failed:   %FAIL_COUNT%

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
REM Build subroutine
REM ---------------------------------------------------------------------------
:build_project
    set "P_NAME=%~1"
    set "P_FILE=%~2"
    
    echo Building: %P_NAME%
    msbuild "%P_FILE%" /t:Make /p:Config=Debug /p:Platform=Win32 /p:DCC_ExeOutput="%OUTPUT_DIR%" /v:minimal /nologo
    
    if %ERRORLEVEL% NEQ 0 (
        echo [BUILD FAILED] %P_NAME%
        set /a BUILD_FAIL_COUNT+=1
    ) else (
        echo [BUILD OK] %P_NAME%
        set /a BUILD_SUCCESS_COUNT+=1
    )
    echo.
    goto :eof

REM ---------------------------------------------------------------------------
REM Run subroutine  
REM ---------------------------------------------------------------------------
:run_project
    set "P_NAME=%~1"
    set "EXE_FILE=%OUTPUT_DIR%\%P_NAME%.exe"
    
    if not exist "%EXE_FILE%" goto :eof
    
    echo.
    echo ==========================================
    echo Testing: %P_NAME%
    echo ==========================================
    echo Running: %EXE_FILE%
    
    "%EXE_FILE%" -no-wait
    
    if %ERRORLEVEL% EQU 0 (
        echo [PASSED] %P_NAME%
        set /a SUCCESS_COUNT+=1
    ) else (
        echo [FAILED] %P_NAME% - Exit code: %ERRORLEVEL%
        set "FAILED_TESTS=%FAILED_TESTS% %P_NAME%"
        set /a FAIL_COUNT+=1
    )
    goto :eof
