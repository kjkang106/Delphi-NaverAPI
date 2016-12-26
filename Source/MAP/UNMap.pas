unit UNMap;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, SHDocVw, MSHTML, ActiveX,
  Registry, Variants;

const
  MAX_WAIT_MSEC = 3000;
  MAX_ADDRESS_INDEX = 29;

type
  TWbEmulation = (wbIE11, wbIE10, wbIE9, wbIE8, wbIE7);
  TNMap = class(TWebBrowser)
    private
      FHTMLWindow2: IHTMLWindow2;
      FMAX_WAIT_MSEC: Cardinal;
      DefLat, DefLng: Double;

      FLat, FLng: array[0 .. MAX_ADDRESS_INDEX] of Double;

      function GetLat(Index: Integer): Double;
      function GetLng(Index: Integer): Double;

      procedure clear(const Document: IHTMLDocument2);

      procedure setWebBrowserEmulation(value: TWbEmulation);
    public
      constructor Create(AOwner: TComponent); overload;
      constructor Create(AOwner: TComponent; Lat, Lng: Double); overload;
      destructor Destroy; override;

      procedure loadScript;

      property Lat[Index : integer]: Double read GetLat;
      property Lng[Index : integer]: Double read GetLng;
  end;

procedure getNMap(AOwner: TWinControl; Lat, Lng: Double);

var
  FNMap: TNMap;

implementation

uses UNAPIVAR;

procedure getNMap(AOwner: TWinControl; Lat, Lng: Double);
begin
  if Assigned(FNMap) then
    FreeAndNil(FNMap);

  FNMap:= TNMap.Create(AOwner, Lat, Lng);
  FNMap.ParentWindow:= TWinControl(AOwner).Handle;
  while FNMap.ReadyState <> READYSTATE_COMPLETE do
    Application.ProcessMessages;
  FNMap.loadScript;
end;

{ TNMap }

procedure TNMap.clear(const Document: IHTMLDocument2);
begin
  Document.write(PSafeArray(VarArrayAsPSafeArray(VarArrayOf([WideString('')]))));
  Document.close;
end;

constructor TNMap.Create(AOwner: TComponent);
var
  index: integer;
begin
  inherited;

  FMAX_WAIT_MSEC:= MAX_WAIT_MSEC;
  setWebBrowserEmulation(wbIE11);

  //Naver Application에 등록된 웹 서비스 URL만 API 인증이 성공함
//  Navigate('about:blank');
  Navigate(NAPIWebServiceURL);
  Left:= 0;
  Top:= 0;
  Width:= TWinControl(AOwner).ClientWidth;
  Height:= TWinControl(AOwner).ClientHeight;

  for index:= 0 to MAX_ADDRESS_INDEX do
  begin
    FLat[index]:= DefLat;
    FLng[index]:= DefLng;
  end;
end;

constructor TNMap.Create(AOwner: TComponent; Lat, Lng: Double);
begin
  DefLat:= Lat;
  DefLng:= Lng;

  Create(AOwner);
end;

destructor TNMap.Destroy;
begin

  inherited;
end;

function TNMap.GetLat(Index: Integer): Double;
begin
  if (Index >= Low(FLat)) and (Index <= High(FLat)) then
    result:= FLat[Index]
  else
    result:= -1;
end;

function TNMap.GetLng(Index: Integer): Double;
begin
  if (Index >= Low(FLng)) and (Index <= High(FLng)) then
    result:= FLng[Index]
  else
    result:= -1;
end;

procedure TNMap.loadScript;
var
  HTMLStr: UTF8String;
  aStream: TMemoryStream;
  index: integer;
begin
  HTMLStr:=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '    <meta charset="UTF-8">' +
    '    <meta http-equiv="X-UA-Compatible" content="IE=edge">' +
    '    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0, user-scalable=no">' +
    '    <title>간단한 지도 표시하기</title>' +
    '    <script type="text/javascript" src="' + MAP_URL + '?clientId=' + NAPIClientID + '"></script>' +
    '</head>' +
    '<body>' +
    '<div id="map" style="width:100%;height:400px;"></div>' +
    '' +
    '<script>' +
    'var mapOptions = {' +
    '    center: new naver.maps.LatLng(' + FloatToStr(FLat[0]) + ', ' + FloatToStr(FLng[0]) + '),' +
    '    zoom: 10' +
    '};' +
    '' +
    'var map = new naver.maps.Map(' + QuotedStr('map') + ', mapOptions);' +
    '</script>' +
    '</body>' +
    '</html> ';
  if Assigned(Document) then
  begin
    clear(IHTMLDocument2(Document));

    aStream := TMemoryStream.Create;
    try
      aStream.WriteBuffer(Pointer(HTMLStr)^, Length(HTMLStr));
      aStream.Seek(0, soFromBeginning);
      (Document as IPersistStreamInit).Load(TStreamAdapter.Create(aStream));
    finally
      aStream.Free;
    end;
    FHTMLWindow2 := (Document as IHTMLDocument2).parentWindow;
  end;
end;

procedure TNMap.setWebBrowserEmulation(value: TWbEmulation);
var
  nRegVal: Integer;

  appName: string;
  regKey: TRegistry;
begin
  case value of
    wbIE11: nRegVal:= 11001;
    wbIE10: nRegVal:= 10001;
    wbIE9:  nRegVal:= 9999;
    wbIE8:  nRegVal:= 8888;
    wbIE7:  nRegVal:= 7000;
  end;

  appName:= ExtractFileName(Forms.Application.ExeName);
  regKey:= TRegistry.Create;
  try
    regKey.RootKey := HKEY_CURRENT_USER;
    regKey.OpenKey('SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION', True);
    regKey.WriteInteger(appName, nRegVal);
  finally
    regKey.Free;
  end;
end;

end.
