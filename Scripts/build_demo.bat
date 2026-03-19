@echo off
setlocal

set DEXT=C:\apps_delphi\Comp12\dext
set STUDIO=C:\Program Files (x86)\Embarcadero\Studio\23.0
set LOG=%DEXT%\Scripts\demo_build.log

call "%STUDIO%\bin\rsvars.bat"
if ERRORLEVEL 1 (
  echo ERROR: rsvars.bat not found
  exit /b 1
)

msbuild "%DEXT%\Examples\EventsBus.Demo\Core.EventBusDemo.dproj" ^
  /t:Build ^
  /p:Config=Debug ^
  /p:Platform=Win32 ^
  /v:normal ^
  /nologo ^
  > "%LOG%" 2>&1

if %ERRORLEVEL% EQU 0 (
  echo [OK] Demo build successful.
) else (
  echo [FAIL] Demo build failed.
)
echo.
findstr /i "error\|warning\|hint\|succeeded\|failed" "%LOG%"

endlocal
