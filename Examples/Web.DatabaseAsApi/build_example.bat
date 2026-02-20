@echo off
set "DEXT_ROOT=%~dp0..\.."
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"

dcc32 DatabaseAsApi.dpr -B -Q ^
  -I"%DEXT_ROOT%\Sources;%DEXT_ROOT%\Sources\Core;%DEXT_ROOT%\Sources\Data;%DEXT_ROOT%\Sources\Web" ^
  -U"%DEXT_ROOT%\Sources;%DEXT_ROOT%\Sources\Core;%DEXT_ROOT%\Sources\Data;%DEXT_ROOT%\Sources\Web;%DEXT_ROOT%\Sources\Core\Base;%DEXT_ROOT%\Sources\Core\Json;%DEXT_ROOT%\Sources\Web\Hosting;%DEXT_ROOT%\Sources\Web\Routing;%DEXT_ROOT%\Sources\Web\Formatters;%DEXT_ROOT%\Sources\Web\Mvc;%DEXT_ROOT%\Sources\Web\Indy;%DEXT_ROOT%\Sources\Web\Caching;%DEXT_ROOT%\Sources\Hosting\CLI;%DEXT_ROOT%\Sources\Hosting\CLI\Commands;%DEXT_ROOT%\Sources\Web\Middleware" ^
  -NSSystem;System.Win;Winapi;Vcl;Data;FireDAC.Phys.PG

if errorlevel 1 (
   echo Compilation Failed
   exit /b 1
)

echo Compilation Success
DatabaseAsApi.exe --no-wait
