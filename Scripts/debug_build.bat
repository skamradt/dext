@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
dcc32 -M -Q -U"..\Sources\Data;..\Sources\Core;..\Output" -I"..\Sources\Data;..\Sources\Core;..\Output;..\Sources" -N"..\Output" -E"..\Output" ..\Tests\TestOrmInheritance.dpr > dcc_output.txt 2>&1
if %ERRORLEVEL% EQU 0 (
  echo Build Successful. Running tests...
  ..\Output\TestOrmInheritance.exe
) else (
  echo Build Failed.
  type dcc_output.txt
)
