program Orm.EntityStyles;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  EntityStyles.Demo in 'EntityStyles.Demo.pas';

begin
  SetConsoleCharSet;
  RunDemo;
  
  // Only pause if not running in automated mode
  if not FindCmdLineSwitch('no-wait', True) then
    ReadLn;
end.
