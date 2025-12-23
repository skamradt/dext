@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
set SEARCH_PATH=%~dp0..\Output;%~dp0..\Sources;%~dp0..\Sources\Core;%~dp0..\Sources\Core\Base;%~dp0..\Sources\Core\Json;%~dp0..\Sources\Core\Specifications;%~dp0..\Sources\Data;%~dp0..\Sources\Hosting\CLI;%~dp0..\Sources\Hosting\CLI\Commands;%~dp0..\Sources\Web;%~dp0..\Sources\Web\Hosting;%~dp0..\Sources\Web\Middleware;%~dp0..\Sources\Web\Mvc;%~dp0..\Examples\Orm.EntityDemo
msbuild "..\Examples\Orm.EntityDemo\Orm.EntityDemo.dproj" /t:Rebuild /p:Config=Debug /p:Platform=Win32 /p:DCC_UnitSearchPath="%SEARCH_PATH%" /v:minimal > build_demo_output.txt 2>&1
type build_demo_output.txt
