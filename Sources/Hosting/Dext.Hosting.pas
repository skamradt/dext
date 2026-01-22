unit Dext.Hosting;

interface

uses
  System.SysUtils,
  Dext,
  // {BEGIN_DEXT_USES}
  // Generated Uses
  Dext.Hosting.CLI.Args,
  Dext.Hosting.CLI.Config,
  Dext.Hosting.CLI,
  Dext.Hosting.CLI.Registry,
  Dext.Hosting.CLI.Tools.CodeCoverage,
  Dext.Hosting.CLI.Tools.Sonar,

  Dext.Hosting.CLI.Logger,
  Dext.Hosting.CLI.Tools.FacadeGenerator
  // {END_DEXT_USES}
  ;

type
  // {BEGIN_DEXT_ALIASES}
  // Generated Aliases

  // Dext.Hosting.CLI
  TDextCLI = Dext.Hosting.CLI.TDextCLI;

  // Dext.Hosting.CLI.Args
  TCommandLineArgs = Dext.Hosting.CLI.Args.TCommandLineArgs;
  IConsoleCommand = Dext.Hosting.CLI.Args.IConsoleCommand;

  // Dext.Hosting.CLI.Config
  TDextTestConfig = Dext.Hosting.CLI.Config.TDextTestConfig;
  TDextEnvironment = Dext.Hosting.CLI.Config.TDextEnvironment;
  TDextConfig = Dext.Hosting.CLI.Config.TDextConfig;
  TDextGlobalConfig = Dext.Hosting.CLI.Config.TDextGlobalConfig;

  // Dext.Hosting.CLI.Logger
  TConsoleHubLogger = Dext.Hosting.CLI.Logger.TConsoleHubLogger;
  TConsoleHubLoggerProvider = Dext.Hosting.CLI.Logger.TConsoleHubLoggerProvider;

  // Dext.Hosting.CLI.Registry
  TProjectInfo = Dext.Hosting.CLI.Registry.TProjectInfo;
  TProjectRegistry = Dext.Hosting.CLI.Registry.TProjectRegistry;

  // Dext.Hosting.CLI.Tools.CodeCoverage
  TCodeCoverageTool = Dext.Hosting.CLI.Tools.CodeCoverage.TCodeCoverageTool;



  // Dext.Hosting.CLI.Tools.FacadeGenerator
  TOrdinalIgnoreCaseComparer = Dext.Hosting.CLI.Tools.FacadeGenerator.TOrdinalIgnoreCaseComparer;
  TExtractedUnit = Dext.Hosting.CLI.Tools.FacadeGenerator.TExtractedUnit;
  TFacadeGenerator = Dext.Hosting.CLI.Tools.FacadeGenerator.TFacadeGenerator;

  // Dext.Hosting.CLI.Tools.Sonar
  TSonarConverter = Dext.Hosting.CLI.Tools.Sonar.TSonarConverter;
  // {END_DEXT_ALIASES}

implementation

end.
