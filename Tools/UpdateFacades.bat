@echo off
set "TOOL_DIR=%~dp0DextFacadeGenerator"
set "EXE=%TOOL_DIR%\DextFacadeGenerator.exe"
set "SRC_ROOT=%~dp0..\Sources"
set "LOG_FILE=%~dp0update_log.txt"

echo Recompiling Generator... > "%LOG_FILE%"
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" >> "%LOG_FILE%" 2>&1
dcc32 "%TOOL_DIR%\DextFacadeGenerator.dpr" -B -Q ^
  -I"%~dp0..\..\Libs\DelphiAST-SkliarOleksandr\Source;%~dp0..\..\Libs\DelphiAST-SkliarOleksandr\Source\Lexers;%~dp0..\Sources" ^
  -U"%~dp0..\..\Libs\DelphiAST-SkliarOleksandr\Source;%~dp0..\..\Libs\DelphiAST-SkliarOleksandr\Source\Lexers;%~dp0..\Sources;%~dp0..\Sources\Core;%~dp0..\Sources\Data;%~dp0..\Sources\Web;%~dp0..\Sources\Core\Base;%~dp0..\Sources\Core\Json;%~dp0..\Sources\Web\Hosting;%~dp0..\Sources\Web\Routing;%~dp0..\Sources\Web\Formatters;%~dp0..\Sources\Web\Mvc;%~dp0..\Sources\Web\Indy;%~dp0..\Sources\Web\Caching;%~dp0..\Sources\Hosting\CLI;%~dp0..\Sources\Hosting\CLI\Commands;%~dp0..\Sources\Web\Middleware" ^
  -NSSystem;System.Win;Winapi;Vcl;Data;FireDAC.Phys.PG >> "%LOG_FILE%" 2>&1

if errorlevel 1 (
   echo Compilation Failed! Check log.
   type "%LOG_FILE%"
   exit /b 1
)

echo Using Generator: %EXE% >> "%LOG_FILE%"

echo. >> "%LOG_FILE%"
echo ======================================================= >> "%LOG_FILE%"
echo Updating Dext (Core) >> "%LOG_FILE%"
echo ======================================================= >> "%LOG_FILE%"
"%EXE%" "%SRC_ROOT%\Core" "%SRC_ROOT%\Core" "Dext" "Dext" >> "%LOG_FILE%" 2>&1

echo. >> "%LOG_FILE%"
echo ======================================================= >> "%LOG_FILE%"
echo Updating Dext.Entity >> "%LOG_FILE%"
echo ======================================================= >> "%LOG_FILE%"
"%EXE%" "%SRC_ROOT%\Data" "%SRC_ROOT%\Data" "Dext.Entity" "Dext.Entity" >> "%LOG_FILE%" 2>&1

echo. >> "%LOG_FILE%"
echo ======================================================= >> "%LOG_FILE%"
echo Updating Dext.Web >> "%LOG_FILE%"
echo ======================================================= >> "%LOG_FILE%"
"%EXE%" "%SRC_ROOT%\Web" "%SRC_ROOT%\Web" "Dext.Web" "Dext.Web" >> "%LOG_FILE%" 2>&1

echo. >> "%LOG_FILE%"
echo All Done. >> "%LOG_FILE%"
type "%LOG_FILE%"
