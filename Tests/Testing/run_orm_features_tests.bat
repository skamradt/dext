@echo off
setlocal

echo ============================================
echo   Running ORM Features Integration Tests
echo ============================================
echo.

REM Initialize Delphi environment
call "c:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"

echo Building TestORMFeaturesIntegration...
msbuild ..\Entity\TestORMFeaturesIntegration.dproj /t:Clean;Build /p:Config=Debug /p:Platform=Win32 /v:minimal > build_orm_features.log 2>&1

if errorlevel 1 (
    echo.
    echo ============================================
    echo   BUILD FAILED - Check build_orm_features.log
    echo ============================================
    type build_orm_features.log
    exit /b 1
)

echo Build successful!
echo.
echo Running Tests...
echo.

..\Output\TestORMFeaturesIntegration.exe

echo.
echo ============================================
echo   Tests Complete
echo ============================================

exit /b 0
