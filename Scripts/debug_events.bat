@echo off
setlocal

set DEXT=C:\apps_delphi\Comp12\dext
set STUDIO=C:\Program Files (x86)\Embarcadero\Studio\23.0
set OUTPUT=%DEXT%\Output\23.0_Win32_Debug
set LOG=%DEXT%\Scripts\events_build.log

call "%STUDIO%\bin\rsvars.bat"
if ERRORLEVEL 1 (
  echo ERROR: rsvars.bat not found at %STUDIO%\bin\rsvars.bat
  exit /b 1
)

echo Building Dext.Events.dpk [Win32 Debug]...
echo.

msbuild "%DEXT%\Sources\Dext.Events.dproj" ^
  /t:Build ^
  /p:Config=Debug ^
  /p:Platform=Win32 ^
  /p:DCC_UnitSearchPath="%OUTPUT%;%DEXT%\Output;%DEXT%\Sources\Events;%DEXT%\Sources" ^
  /v:normal ^
  /nologo ^
  > "%LOG%" 2>&1

if %ERRORLEVEL% EQU 0 (
  echo [OK] Build successful.
  echo Output: %OUTPUT%
) else (
  echo [FAIL] Build failed. Errors:
  echo.
  findstr /i "error\|warning\|fatal" "%LOG%"
  echo.
  echo Full log: %LOG%
)

endlocal
