program NAPITest;

uses
  Forms,
  UMain in 'UMain.pas' {FMain},
  UNTTS in 'TTS\UNTTS.pas',
  UNAPIClient in 'Common\UNAPIClient.pas',
  UNAPIVAR in 'Common\UNAPIVAR.pas',
  UNMap in 'MAP\UNMap.pas',
  UNMapGeocode in 'MAP\UNMapGeocode.pas',
  UNSearch in 'SEARCH\UNSearch.pas',
  UNSearchModel in 'SEARCH\UNSearchModel.pas';

{$R *.res}

begin
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
  {$WARN SYMBOL_PLATFORM ON}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
