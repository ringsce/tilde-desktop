program KayteBuildSystem;

{$mode objfpc}{$H+}

uses
  System, JS, Web, SysUtils, Classes,
  KayteTypes, KayteUI, KayteBuild, KayteGit, KaytePackage;

type
  TKayteApp = class
  private
    FUI: TKayteUI;
    FBuildSystem: TKayteBuildSystem;
    FGitManager: TKayteGitManager;
    FPackageManager: TKaytePackageManager;
    FCurrentTheme: string;
    
    procedure InitializeUI;
    procedure SetupEventHandlers;
    procedure OnThemeToggle(Sender: TObject);
    procedure OnProjectOpen(Sender: TObject; const AFileName: string);
    procedure OnBuildStart(Sender: TObject);
    procedure OnGitAction(Sender: TObject; const AAction: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

{ TKayteApp }

constructor TKayteApp.Create;
begin
  inherited Create;
  FCurrentTheme := 'dark';
  
  FUI := TKayteUI.Create;
  FBuildSystem := TKayteBuildSystem.Create;
  FGitManager := TKayteGitManager.Create;
  FPackageManager := TKaytePackageManager.Create;
  
  InitializeUI;
  SetupEventHandlers;
end;

destructor TKayteApp.Destroy;
begin
  FPackageManager.Free;
  FGitManager.Free;
  FBuildSystem.Free;
  FUI.Free;
  inherited Destroy;
end;

procedure TKayteApp.InitializeUI;
begin
  FUI.Initialize;
  FUI.SetTheme(FCurrentTheme);
  FUI.UpdateStatus('Ready');
end;

procedure TKayteApp.SetupEventHandlers;
begin
  FUI.OnThemeToggle := @OnThemeToggle;
  FUI.OnProjectOpen := @OnProjectOpen;
  FUI.OnBuildStart := @OnBuildStart;
  FUI.OnGitAction := @OnGitAction;
end;

procedure TKayteApp.OnThemeToggle(Sender: TObject);
begin
  if FCurrentTheme = 'dark' then
    FCurrentTheme := 'light'
  else
    FCurrentTheme := 'dark';
    
  FUI.SetTheme(FCurrentTheme);
end;

procedure TKayteApp.OnProjectOpen(Sender: TObject; const AFileName: string);
var
  ProjectInfo: TProjectInfo;
begin
  FUI.AddOutput('Opening project: ' + AFileName, otNormal);
  
  ProjectInfo := FBuildSystem.LoadProject(AFileName);
  FUI.SetProjectInfo(ProjectInfo);
  FUI.UpdateProjectExplorer(ProjectInfo);
  
  // Auto-build
  FUI.AddOutput('Auto-building project...', otNormal);
  OnBuildStart(Self);
end;

procedure TKayteApp.OnBuildStart(Sender: TObject);
var
  BuildConfig: TBuildConfig;
  BuildResult: TBuildResult;
  PackagePath: string;
begin
  BuildConfig := FUI.GetBuildConfig;
  
  FUI.SwitchToOutputTab;
  FUI.AddOutput('--- Building ' + BuildConfig.ProjectName + ' (' + BuildConfig.BuildTarget + ') ---', otNormal);
  
  BuildResult := FBuildSystem.Build(BuildConfig);
  
  if BuildResult.Success then
  begin
    FUI.AddOutput('Build successful!', otSuccess);
    FUI.UpdateStatus('Build complete');
    
    // Auto-create package
    PackagePath := FPackageManager.CreatePackage(BuildConfig.ProjectName, BuildConfig.OutputDir);
    FUI.AddPackageToList(PackagePath);
    FUI.SwitchToProjectsTab;
  end
  else
  begin
    FUI.AddOutput('Build failed: ' + BuildResult.ErrorMessage, otError);
    FUI.UpdateStatus('Build failed');
  end;
end;

procedure TKayteApp.OnGitAction(Sender: TObject; const AAction: string);
var
  GitResult: TGitResult;
begin
  FUI.SwitchToOutputTab;
  FUI.AddOutput('--- Git: ' + AAction + ' ---', otNormal);
  
  case AAction of
    'init': GitResult := FGitManager.Init;
    'status': GitResult := FGitManager.Status;
    'add': GitResult := FGitManager.Add('.');
    'commit': GitResult := FGitManager.Commit(FUI.GetCommitMessage);
    'push': GitResult := FGitManager.Push;
    'pull': GitResult := FGitManager.Pull;
  end;
  
  FUI.AddOutput(GitResult.Output, otNormal);
  if GitResult.Success then
    FUI.UpdateGitStatus(GitResult)
  else
    FUI.AddOutput('Error: ' + GitResult.ErrorMessage, otError);
end;

procedure TKayteApp.Run;
begin
  FUI.AddOutput('Kayte Build System v1.0.0', otNormal);
  FUI.AddOutput('Ready to build...', otNormal);
  
  // Auto-detect compilers
  FBuildSystem.DetectCompilers;
  FUI.UpdateCompilerPaths(FBuildSystem.GetCompilerPaths);
end;

var
  App: TKayteApp;

begin
  App := TKayteApp.Create;
  try
    App.Run;
  except
    on E: Exception do
      window.alert('Error: ' + E.Message);
  end;
end.
