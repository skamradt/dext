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
  Dext.Hosting.CLI.Commands.Configuration,
  Dext.Hosting.CLI.Commands.Doc,
  Dext.Hosting.CLI.Commands.Facade,
  Dext.Hosting.CLI.Commands.MigrateDown,
  Dext.Hosting.CLI.Commands.MigrateGenerate,
  Dext.Hosting.CLI.Commands.MigrateList,
  Dext.Hosting.CLI.Commands.MigrateUp,
  Dext.Hosting.CLI.Commands.Scaffold,
  Dext.Hosting.CLI.Commands.Test,
  Dext.Hosting.CLI.Commands.UI,
  Dext.Hosting.CLI.Tools.DocGen,
  Dext.Hosting.CLI.Hubs.Dashboard,
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

  // Dext.Hosting.CLI.Commands.Configuration
  TConfigInitCommand = Dext.Hosting.CLI.Commands.Configuration.TConfigInitCommand;
  TEnvScanCommand = Dext.Hosting.CLI.Commands.Configuration.TEnvScanCommand;

  // Dext.Hosting.CLI.Commands.Doc
  TDocCommand = Dext.Hosting.CLI.Commands.Doc.TDocCommand;

  // Dext.Hosting.CLI.Commands.Facade
  TFacadeCommand = Dext.Hosting.CLI.Commands.Facade.TFacadeCommand;

  // Dext.Hosting.CLI.Commands.MigrateDown
  TMigrateDownCommand = Dext.Hosting.CLI.Commands.MigrateDown.TMigrateDownCommand;

  // Dext.Hosting.CLI.Commands.MigrateGenerate
  TMigrateGenerateCommand = Dext.Hosting.CLI.Commands.MigrateGenerate.TMigrateGenerateCommand;

  // Dext.Hosting.CLI.Commands.MigrateList
  TMigrateListCommand = Dext.Hosting.CLI.Commands.MigrateList.TMigrateListCommand;

  // Dext.Hosting.CLI.Commands.MigrateUp
  TMigrateUpCommand = Dext.Hosting.CLI.Commands.MigrateUp.TMigrateUpCommand;

  // Dext.Hosting.CLI.Commands.Scaffold
  TScaffoldCommand = Dext.Hosting.CLI.Commands.Scaffold.TScaffoldCommand;

  // Dext.Hosting.CLI.Commands.Test
  TTestCommand = Dext.Hosting.CLI.Commands.Test.TTestCommand;

  // Dext.Hosting.CLI.Commands.UI
  TUICommand = Dext.Hosting.CLI.Commands.UI.TUICommand;

  // Dext.Hosting.CLI.Config
  TDextTestConfig = Dext.Hosting.CLI.Config.TDextTestConfig;
  TDextEnvironment = Dext.Hosting.CLI.Config.TDextEnvironment;
  TDextConfig = Dext.Hosting.CLI.Config.TDextConfig;
  TDextGlobalConfig = Dext.Hosting.CLI.Config.TDextGlobalConfig;

  // Dext.Hosting.CLI.Hubs.Dashboard
  TDashboardHub = Dext.Hosting.CLI.Hubs.Dashboard.TDashboardHub;

  // Dext.Hosting.CLI.Logger
  TConsoleHubLogger = Dext.Hosting.CLI.Logger.TConsoleHubLogger;
  TConsoleHubLoggerProvider = Dext.Hosting.CLI.Logger.TConsoleHubLoggerProvider;

  // Dext.Hosting.CLI.Registry
  TProjectInfo = Dext.Hosting.CLI.Registry.TProjectInfo;
  TProjectRegistry = Dext.Hosting.CLI.Registry.TProjectRegistry;

  // Dext.Hosting.CLI.Tools.CodeCoverage
  TCodeCoverageTool = Dext.Hosting.CLI.Tools.CodeCoverage.TCodeCoverageTool;

  // Dext.Hosting.CLI.Tools.DocGen
  TMemberInfo = Dext.Hosting.CLI.Tools.DocGen.TMemberInfo;
  TMethodInfo = Dext.Hosting.CLI.Tools.DocGen.TMethodInfo;
  TPropertyInfo = Dext.Hosting.CLI.Tools.DocGen.TPropertyInfo;
  TTypeInfo = Dext.Hosting.CLI.Tools.DocGen.TTypeInfo;
  TClassInfo = Dext.Hosting.CLI.Tools.DocGen.TClassInfo;
  TUnitInfo = Dext.Hosting.CLI.Tools.DocGen.TUnitInfo;
  TDocRegistry = Dext.Hosting.CLI.Tools.DocGen.TDocRegistry;
  TDextDocGenerator = Dext.Hosting.CLI.Tools.DocGen.TDextDocGenerator;

  // Dext.Hosting.CLI.Tools.FacadeGenerator
  TOrdinalIgnoreCaseComparer = Dext.Hosting.CLI.Tools.FacadeGenerator.TOrdinalIgnoreCaseComparer;
  TExtractedUnit = Dext.Hosting.CLI.Tools.FacadeGenerator.TExtractedUnit;
  TFacadeGenerator = Dext.Hosting.CLI.Tools.FacadeGenerator.TFacadeGenerator;

  // Dext.Hosting.CLI.Tools.Sonar
  TSonarConverter = Dext.Hosting.CLI.Tools.Sonar.TSonarConverter;
  // {END_DEXT_ALIASES}

implementation

end.
