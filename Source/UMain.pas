unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, IniFiles, UNTTS, UNMap;

type
  TFMain = class(TForm)
    pcMain: TPageControl;
    tsInit: TTabSheet;
    tsTTS: TTabSheet;
    Panel1: TPanel;
    lbSpeed: TLabel;
    btTTS: TButton;
    rgSpeeker: TRadioGroup;
    tbSpeed: TTrackBar;
    memoTTS: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    etNAPIClientID: TEdit;
    etNAPIClientSecret: TEdit;
    tsMap: TTabSheet;
    Panel2: TPanel;
    btShowMap: TButton;
    Button2: TButton;
    Button3: TButton;
    panNMap: TPanel;
    Label3: TLabel;
    etNAPIWebServiceURL: TEdit;
    Label4: TLabel;
    lbLog: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btTTSClick(Sender: TObject);
    procedure tbSpeedChange(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pcMainChanging(Sender: TObject; var AllowChange: Boolean);
    procedure btShowMapClick(Sender: TObject);
  private
    { Private declarations }
    procedure logM(Msg: string);
    procedure saveIni;
    procedure loadIni;
  public
    { Public declarations }
    procedure onTTSResult(Sender:TObject);
  end;

var
  FMain: TFMain;

implementation

uses UNAPIVAR;

{$R *.dfm}

{ TForm40 }

procedure TFMain.btTTSClick(Sender: TObject);
var
  Text: string;
begin
  Text:= memoTTS.Text;

  logM('getTTS : ' + Text);
  getTTS(Text, onTTSResult, TNTTSSpeeker(rgSpeeker.ItemIndex), 0 - tbSpeed.Position);
end;

procedure TFMain.btShowMapClick(Sender: TObject);
var
  Lat, Lng: Double;
begin
  Lat:= 37.566535;
  Lng:= 126.9779692;

  logM('getNMap : ' + FloatToStr(Lat) + ', ' + FloatToStr(Lng));
  getNMap(panNMap, Lat, Lng);
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  logM('Application Start');

  pcMain.ActivePage:= tsInit;
  loadIni;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  logM('Application Stop');
//  lbLog.Items.SaveToFile('NAPI' + FormatDateTime('YYYYMMDDHHNNSS', Now) + '.log', TEncoding.Unicode);
end;

procedure TFMain.loadIni;
var
  localIni: TIniFile;
begin
  localIni:= TIniFile.Create(ChangeFileExt(Application.ExeName, '.Ini'));
  try
    etNAPIClientID.Text     := localIni.ReadString('Init', 'NAPI Client ID', '');
    etNAPIClientSecret.Text := localIni.ReadString('Init', 'NAPI Client Secret', '');
    etNAPIWebServiceURL.Text:= localIni.ReadString('Init', 'NAPI Web Service URL', '');
  finally
    localIni.Free;
  end;
end;

procedure TFMain.logM(Msg: string);
var
  Memos: TStringList;
  nMax, idx: Integer;
begin
  Memos:= TStringList.Create;
  try
    Memos.Text:= Msg;
    nMax:= Memos.Count;
    for idx:= 0 to nMax - 1 do
      lbLog.Items.Append('[' + FormatDateTime('HH:NN:SS', Time) + '] ' + Memos[idx]);
  finally
    Memos.Free;
  end;
  lbLog.ItemIndex:= lbLog.Items.Count - 1;
end;

procedure TFMain.onTTSResult(Sender: TObject);
begin
  if not Assigned(Sender) then
    Exit;

  logM(TNTTS(Sender).OutMsg);
end;

procedure TFMain.pcMainChange(Sender: TObject);
begin
  if TPageControl(Sender).ActivePage = tsTTS then
  begin
    tbSpeedChange(tbSpeed);
    memoTTS.Clear;
  end;
end;

procedure TFMain.pcMainChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if TPageControl(Sender).ActivePage = tsInit then
  begin
    saveIni;
    AllowChange:= (NAPIClientID <> '') and (NAPIClientSecret <> '');
    if not AllowChange then
      ShowMessage('Client ID or Client Secret is Empty !!');
  end;
end;

procedure TFMain.saveIni;
var
  localIni: TIniFile;

begin
  NAPIClientID     := Trim(etNAPIClientID.Text);
  NAPIClientSecret := Trim(etNAPIClientSecret.Text);
  NAPIWebServiceURL:= Trim(etNAPIWebServiceURL.Text);

  localIni:= TIniFile.Create(ChangeFileExt(Application.ExeName, '.Ini'));
  try
    localIni.WriteString('Init', 'NAPI Client ID', NAPIClientID);
    localIni.WriteString('Init', 'NAPI Client Secret', NAPIClientSecret);
    localIni.WriteString('Init', 'NAPI Web Service URL', NAPIWebServiceURL);
  finally
    localIni.Free;
  end;
  logM('Init Ini was Saved');
end;

procedure TFMain.tbSpeedChange(Sender: TObject);
begin
  lbSpeed.Caption:= lbSpeed.Hint + ' (' + IntToStr(TTrackBar(Sender).Position) + ')';
end;

end.
