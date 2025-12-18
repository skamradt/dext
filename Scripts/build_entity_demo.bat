@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
set SEARCH_PATH=%~dp0..\Output;%~dp0..\Sources\Core;%~dp0..\Sources\Data;%~dp0..\Sources\Web;%~dp0..\Sources\Core\Base;%~dp0..\Sources\Core\Specifications
msbuild "..\Examples\Orm.EntityDemo\Orm.EntityDemo.dproj" /p:Config=Debug /p:Platform=Win32 /p:DCC_UnitSearchPath="%SEARCH_PATH%" /v:minimal > build_demo_output.txt 2>&1
type build_demo_output.txt
