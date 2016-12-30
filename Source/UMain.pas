unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, IniFiles;

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
    btMapShow: TButton;
    btMapGeoAddr: TButton;
    btMapUp: TButton;
    panNMap: TPanel;
    Label3: TLabel;
    etNAPIWebServiceURL: TEdit;
    Label4: TLabel;
    lbLog: TListBox;
    btMapRight: TButton;
    btMapLeft: TButton;
    btMapDn: TButton;
    btMapGeoCode: TButton;
    rgMapType: TRadioGroup;
    btMapClickEvent: TButton;
    Button2: TButton;
    tsSearch: TTabSheet;
    Panel3: TPanel;
    btSearch: TButton;
    etSearch: TEdit;
    cbSearchType: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btTTSClick(Sender: TObject);
    procedure tbSpeedChange(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pcMainChanging(Sender: TObject; var AllowChange: Boolean);
    procedure btMapShowClick(Sender: TObject);
    procedure btMapGeoAddrClick(Sender: TObject);
    procedure btMapUpClick(Sender: TObject);
    procedure btMapLeftClick(Sender: TObject);
    procedure rgMapTypeClick(Sender: TObject);
    procedure btMapGeoCodeClick(Sender: TObject);
    procedure btMapClickEventClick(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure etSearchKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    procedure logM(Msg: string);
    procedure saveIni;
    procedure loadIni;
    procedure parseNBlog;
    procedure parseNNews;
    procedure parseNBookHD;
    procedure parseNBookDT;
    procedure parseNAdultWord;
    procedure parseNEncyc;
    procedure parseNMovie;
    procedure parseNCafeArticle;
    procedure parseNKin;
    procedure parseNLocal;
    procedure parseNErrata;
    procedure parseNWeb;
    procedure parseNImage;
    procedure parseNShop;
    procedure parseNDoc;
  public
    { Public declarations }
    procedure onTTSResult(Sender:TObject);
  end;

var
  FMain: TFMain;

implementation

uses UNAPIVAR, UNTTS, UNMap, UNMapGeocode, UNSearch, UNSearchModel;

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

procedure TFMain.etSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    btSearch.Click;
end;

procedure TFMain.btMapClickEventClick(Sender: TObject);
begin
  if not Assigned(FNMap) then
    Exit;
  if TButton(Sender).Tag = 0 then
  begin
    FNMap.AddClickEvent;
    TButton(Sender).Tag:= 1;
  end
  else
  begin
    FNMap.RemoveClickEvent;
    TButton(Sender).Tag:= 0;
  end;
end;

procedure TFMain.btMapGeoCodeClick(Sender: TObject);
var
  zAddress: TzNAPIAddress;
  Address: TNAPIAddress;
begin
  if not Assigned(FNMap) then
    Exit;
  zAddress:= TzNAPIAddress.Create;
  try
    FNMap.GetGeoCode('ºÒÁ¤·Î 6', zAddress);

    for Address in zAddress do
      logM(FloatToStr(Address.geo_y) + ',' + FloatToStr(Address.geo_x));
  finally
    FreeAndNil(zAddress);
  end;
end;

procedure TFMain.btMapGeoAddrClick(Sender: TObject);
var
  zAddress: TzNAPIAddress;
  Address: TNAPIAddress;
begin
  if not Assigned(FNMap) then
    Exit;
  zAddress:= TzNAPIAddress.Create;
  try
    FNMap.GetGeoAddress(37.3595316, 127.1052133, zAddress);

    for Address in zAddress do
      logM(Address.address);
  finally
    FreeAndNil(zAddress);
  end;
end;

procedure TFMain.btMapLeftClick(Sender: TObject);
begin
  if not Assigned(FNMap) then
    Exit;
  FNMap.MovePan(TButton(Sender).Tag, 0);
end;

procedure TFMain.btMapUpClick(Sender: TObject);
begin
  if not Assigned(FNMap) then
    Exit;
  FNMap.MovePan(0, TButton(Sender).Tag);
end;

procedure TFMain.btSearchClick(Sender: TObject);
begin
  logM('getNSearch : ' + cbSearchType.Text + '|' + etSearch.Text);
  case TNSearchType(cbSearchType.ItemIndex) of
    nstBlog:        parseNBlog;
    nstNews:        parseNNews;
    nstBookHD:      parseNBookHD;
//    nstBookDT:      parseNBookDT;
    nstAdultWord:   parseNAdultWord;
    nstEncyc:       parseNEncyc;
    nstMovie:       parseNMovie;
    nstCafeArticle: parseNCafeArticle;
    nstKin:         parseNKin;
    nstLocal:       parseNLocal;
    nstErrata:      parseNErrata;
    nstWeb:         parseNWeb;
    nstImage:       parseNImage;
    nstShop:        parseNShop;
    nstDoc:         parseNDoc;
  end;
end;

procedure TFMain.btMapShowClick(Sender: TObject);
var
  Lat, Lng: Double;
begin
  Lat:= 37.566535;
  Lng:= 126.9779692;

  logM('getNMap : ' + FloatToStr(Lat) + ', ' + FloatToStr(Lng));
  rgMapType.ItemIndex:= 0;
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

procedure TFMain.parseNAdultWord;
var
  NSearch: TNAdultWord;
  zResult: TzNAdultWord;

  nMax, idx: Integer;
  AResult: TNSearchAdultWord;
begin
  NSearch:= TNAdultWord.Create;
  zResult:= TzNAdultWord.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        if AResult.adult then
          logM(etSearch.Text + ' is Adult Word')
        else
          logM(etSearch.Text + ' is Not Adult Word');
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNBlog;
var
  NSearch: TNBlog;
  zResult: TzNBlog;

  nMax, idx: Integer;
  AResult: TNSearchBlog;
begin
  NSearch:= TNBlog.Create;
  zResult:= TzNBlog.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.description);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNBookDT;
var
  NSearch: TNBookDT;
  zResult: TzNBookDT;

  nMax, idx: Integer;
  AResult: TNSearchBookDT;
begin
  NSearch:= TNBookDT.Create;
  zResult:= TzNBookDT.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.link);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNBookHD;
var
  NSearch: TNBookHD;
  zResult: TzNBookHD;

  nMax, idx: Integer;
  AResult: TNSearchBookHD;
begin
  NSearch:= TNBookHD.Create;
  zResult:= TzNBookHD.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.link);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNCafeArticle;
var
  NSearch: TNCafe;
  zResult: TzNCafe;

  nMax, idx: Integer;
  AResult: TNSearchCafeArticle;
begin
  NSearch:= TNCafe.Create;
  zResult:= TzNCafe.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.link);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNDoc;
var
  NSearch: TNDoc;
  zResult: TzNDoc;

  nMax, idx: Integer;
  AResult: TNSearchDoc;
begin
  NSearch:= TNDoc.Create;
  zResult:= TzNDoc.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.link);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNEncyc;
var
  NSearch: TNEncyc;
  zResult: TzNEncyc;

  nMax, idx: Integer;
  AResult: TNSearchEncyc;
begin
  NSearch:= TNEncyc.Create;
  zResult:= TzNEncyc.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.link);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNErrata;
var
  NSearch: TNErrata;
  zResult: TzNErrata;

  nMax, idx: Integer;
  AResult: TNSearchErrata;
begin
  NSearch:= TNErrata.Create;
  zResult:= TzNErrata.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        if AResult.errata = ''  then
          logM(etSearch.Text + ' is Correct Word')
        else
          logM(etSearch.Text + ' is not Correct Word => ' + AResult.errata);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNImage;
var
  NSearch: TNImage;
  zResult: TzNImage;

  nMax, idx: Integer;
  AResult: TNSearchImage;
begin
  NSearch:= TNImage.Create;
  zResult:= TzNImage.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.link);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNKin;
var
  NSearch: TNKin;
  zResult: TzNKin;

  nMax, idx: Integer;
  AResult: TNSearchKin;
begin
  NSearch:= TNKin.Create;
  zResult:= TzNKin.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.link);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNLocal;
var
  NSearch: TNLocal;
  zResult: TzNLocal;

  nMax, idx: Integer;
  AResult: TNSearchLocal;
begin
  NSearch:= TNLocal.Create;
  zResult:= TzNLocal.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.link);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNMovie;
var
  NSearch: TNMovie;
  zResult: TzNMovie;

  nMax, idx: Integer;
  AResult: TNSearchMovie;
begin
  NSearch:= TNMovie.Create;
  zResult:= TzNMovie.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.link);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNNews;
var
  NSearch: TNNews;
  zResult: TzNNews;

  nMax, idx: Integer;
  AResult: TNSearchNews;
begin
  NSearch:= TNNews.Create;
  zResult:= TzNNews.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.description);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNShop;
var
  NSearch: TNShop;
  zResult: TzNShop;

  nMax, idx: Integer;
  AResult: TNSearchShop;
begin
  NSearch:= TNShop.Create;
  zResult:= TzNShop.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.link);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.parseNWeb;
var
  NSearch: TNWeb;
  zResult: TzNWeb;

  nMax, idx: Integer;
  AResult: TNSearchWeb;
begin
  NSearch:= TNWeb.Create;
  zResult:= TzNWeb.Create;
  try
    if NSearch.getNSearch(etSearch.Text, zResult) then
    begin
      logM('OK' + NSearch.OutMsg);
      nMax:= zResult.count;
      for idx:= 0 to nMax - 1 do
      begin
        AResult:= zResult.zSearchItem[idx];
        logM(AResult.title + ' => ' + AResult.link);
      end;
    end
    else
      logM('Fail' + NSearch.OutMsg);
  finally
    zResult.Free;
    NSearch.Free;
  end;
end;

procedure TFMain.pcMainChange(Sender: TObject);
begin
  if TPageControl(Sender).ActivePage = tsTTS then
  begin
    tbSpeedChange(tbSpeed);
    memoTTS.Clear;
  end
  else if TPageControl(Sender).ActivePage = tsMap then
  begin
    btMapShow.Click;
  end
  else if TPageControl(Sender).ActivePage = tsSearch then
  begin
    cbSearchType.ItemIndex:= 0;
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

procedure TFMain.rgMapTypeClick(Sender: TObject);
begin
  if not Assigned(FNMap) then
  begin
    TRadioGroup(Sender).ItemIndex:= 0;
    TRadioGroup(Sender).Tag      := 0;
    Exit;
  end;

  if TRadioGroup(Sender).ItemIndex <> TRadioGroup(Sender).Tag then
  begin
    TRadioGroup(Sender).Tag:= TRadioGroup(Sender).ItemIndex;
    FNMap.SetMapType(TNMapType(TRadioGroup(Sender).Tag));
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
