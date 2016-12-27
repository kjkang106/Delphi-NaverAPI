unit UNMap;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, SHDocVw, MSHTML, ActiveX,
  Registry, Variants, UNMapGeocode;

const
  MAX_WAIT_MSEC = 3000;
  MAX_ADDRESS_INDEX = 29;

type
  TWbEmulation = (wbIE11, wbIE10, wbIE9, wbIE8, wbIE7);
  TNMapType = (mtNormal, mtSatellite, mtHybrid, mtTerrain);  //일반, 위성, 겹침, 지형도
  TNMap = class(TWebBrowser)
    private
      FHTMLWindow2: IHTMLWindow2;
      DefLat, DefLng: Double;

      FMAX_WAIT_MSEC: Cardinal;

      FStatus: string;
      FAddressCnt: integer;
      FAddrItems: array[0 .. MAX_ADDRESS_INDEX] of string;
      function GetAddrItems(Index: Integer): string;

      procedure clear(const Document: IHTMLDocument2);
      procedure loadScript;
      procedure geoResultInit;
      function getGeoResult: boolean;

      function mtToStr(mt:TNMapType): string;
      function GetIdValue(const Id : string): string;
      function SetIdValue(const Id,Value : string): boolean;
      procedure setWebBrowserEmulation(value: TWbEmulation);
    public
      constructor Create(AOwner: TComponent); overload;
      constructor Create(AOwner: TComponent; Lat, Lng: Double); overload;
      destructor Destroy; override;

      property status: string read FStatus;
      property WAIT_MSEC: Cardinal read FMAX_WAIT_MSEC write FMAX_WAIT_MSEC;

      property AddrItems[Index : integer]: string read GetAddrItems;
      property AddressCnt: integer read FAddressCnt;

      procedure SetMapType(mt: TNMapType);
      procedure MovePan(toRight, toDown: Integer);
      procedure GotoLatLng(geo_x,geo_y: double; zoom: integer= -1);
      function GetGeoCode(address: string; var zAddress: TzNAPIAddress): Integer;
      function GetGeoAddress(geo_x,geo_y: double; var zAddress: TzNAPIAddress): Integer;
      procedure AddClickEvent;
      procedure RemoveClickEvent;

      procedure ClearMarkers;
      procedure PutMarker(geo_x,geo_y: double);
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

function waitForGeoResult(NMap: TNMap): boolean;
var
  fContinue:Boolean;
  Start:Cardinal;
begin
  fContinue := True;
  Start := GetTickCount;

  while fcontinue do
  begin
    if NMap.getGeoResult then
    begin
      fContinue := True;
      break;
    end;

    Application.ProcessMessages;
    if Start + NMap.WAIT_MSEC < GetTickCount then
      fContinue := False;
  end;
  result:= fContinue;
  if not Result then
  begin
    NMap.WAIT_MSEC:= NMap.WAIT_MSEC + 500;
    if NMap.WAIT_MSEC > 10000 then
      NMap.WAIT_MSEC:= 10000;
  end;
end;

{ TNMap }

procedure TNMap.AddClickEvent;
begin
  FHTMLWindow2.execScript('AddClickEvent()', 'JavaScript');
end;

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
    FAddrItems[index]:= '';
end;

procedure TNMap.ClearMarkers;
begin
  FHTMLWindow2.execScript('ClearMarkers()', 'JavaScript');
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

procedure TNMap.geoResultInit;
var
  index: integer;
begin
  SetIdValue('status','NULL');
  for index:= 0 to MAX_ADDRESS_INDEX do
    SetIdValue('address'+IntToStr(index), '');
end;

function TNMap.GetAddrItems(Index: Integer): string;
begin
  if (Index >= Low(FAddrItems)) and (Index <= High(FAddrItems)) then
    result:= FAddrItems[Index]
  else
    result:= '';
end;

function TNMap.GetGeoAddress(geo_x,geo_y: double; var zAddress: TzNAPIAddress): Integer;
var
  index: Integer;
begin
  Result:= -1;
  if not Assigned(zAddress) then
    Exit;
  zAddress.Clear;
  geoResultInit;

  FHTMLWindow2.execScript(Format('codeLatLng(%3.7f, %3.7f, true)', [geo_x, geo_y]), 'JavaScript');
  waitForGeoResult(Self);

  Result:= AddressCnt;
  for index:= 0 to AddressCnt - 1 do
  begin
    if AddrItems[index] = '' then
      Break;

    addAddressList(AddrItems[index], zAddress);
  end;
end;

function TNMap.GetGeoCode(address: string; var zAddress: TzNAPIAddress): Integer;
var
  index: Integer;
begin
  Result:= -1;
  if not Assigned(zAddress) then
    Exit;
  zAddress.Clear;
  geoResultInit;

  address := StringReplace(StringReplace(Trim(address), #13, ' ', [rfReplaceAll]), #10, ' ', [rfReplaceAll]);
  FHTMLWindow2.execScript(Format('codeAddress(%s)',[QuotedStr(address)]), 'JavaScript');
  waitForGeoResult(Self);

  Result:= AddressCnt;
  for index:= 0 to AddressCnt - 1 do
  begin
    if AddrItems[index] = '' then
      Break;

    addAddressList(AddrItems[index], zAddress);
  end;
end;

function TNMap.getGeoResult: boolean;
var
  index: integer;
  addr_items: string;
begin
  if GetIdValue('status') = 'NULL' then
  begin
    result:= false;
    exit;
  end;

  FAddressCnt:= 0;
  try
    for index:= 0 to MAX_ADDRESS_INDEX do
    begin
      addr_items := GetIdValue('addr_items' + IntToStr(index));
      if (addr_items <> '') then
      begin
        FAddrItems[FAddressCnt]:= addr_items;
        inc(FAddressCnt);
      end
      else
        break;
    end;
    FStatus:= OleObject.Document.All.status.value;
  except
  end;
  Result:= True;
end;

function TNMap.GetIdValue(const Id: string): string;
var
  ABody     : IHTMLElement2;

  Tag      : IHTMLElement;
  TagsList : IHTMLElementCollection;
  Index    : Integer;
begin
  Result:='';
  if not Supports(IHTMLDocument2(Document).body, IHTMLElement2, ABody) then
    exit;

  TagsList := ABody.getElementsByTagName('input');
  for Index := 0 to TagsList.length-1 do
  begin
    Tag:=TagsList.item(Index, EmptyParam) As IHTMLElement;
    if CompareText(Tag.id,Id)=0 then
      Result := Tag.getAttribute('value', 0);
  end;
end;

procedure TNMap.GotoLatLng(geo_x, geo_y: double; zoom: integer);
begin
  FHTMLWindow2.execScript('GotoLatLng('+FloatToStr(geo_x)+','+FloatToStr(geo_y)+','+IntToStr(zoom)+')', 'JavaScript');
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
    '<meta charset="UTF-8">' +
    '<meta http-equiv="X-UA-Compatible" content="IE=edge">' +
    '<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0, user-scalable=no">' +
//    '<title>간단한 지도 표시하기</title>' +
//    '<script type="text/javascript" src="' + MAP_URL + '?clientId=' + NAPIClientID + '"></script>' +
    '<script type="text/javascript" src="' + MAP_URL + '?clientId=' + NAPIClientID + '&submodules=geocoder"></script>' +
    '<script type="text/javascript"> ' +
    '  var map;  ' +
    '  var markersArray = [];' +
    '  var listener;' +
    '' +
    '  function initialize() { ' +
    '    var mapOptions = {' +
    '      center: new naver.maps.LatLng(' + FloatToStr(DefLat) + ', ' + FloatToStr(DefLng) + '),' +
    '      zoom: 10' +
    '    };' +
    '    map = new naver.maps.Map(' + QuotedStr('map') + ', mapOptions);' +
    '    PutMarker(' + FloatToStr(DefLat) + ', ' + FloatToStr(DefLng) + ');' +
    '  }' +
    '' +
    '  function AddClickEvent() { ' +
    '    if(listener) {' +
    '      RemoveClickEvent();' +
    '    }' +
    '    listener = naver.maps.Event.addListener(map, "click", function(e)' +
    '      {' +
    '        codeLatLng(e.coord.lat(), e.coord.lng(), false);' +
    '      } ' +
    '    ); ' +
    '    map.setCursor(' + QuotedStr('pointer') + ');' +
    '  }' +
    '' +
    '  function RemoveClickEvent() { ' +
    '    if (listener) {'+
    '      naver.maps.Event.removeListener(listener); ' +
    '      map.setCursor(' + QuotedStr('open') + ');' +
    '    }' +
    '  }' +
    '' +
    '  function GotoLatLng(Lat, Lng, Zoom) { ' +
    '    var latlng = new naver.maps.LatLng(Lat, Lng);' +
    '    map.setCenter(latlng);' +
    '    if (Zoom > -1) {' +
    '      map.setZoom(Zoom);' +
    '    }' +
    '    PutMarker(Lat, Lng);' +
    '  }' +
    '' +
    '  function MovePan(toRight, toDown) { ' +
    '    map.panBy(new naver.maps.Point(toRight, toDown));' +
    '  }' +
    '' +
    '  function SetMapType(mapTypeId) { ' +
    '    map.setMapTypeId(mapTypeId);' +
    '  }' +
    ''+
    '  function PutMarker(Lat, Lng) { ' +
    '    var marker = new naver.maps.Marker({' +
    '      position: new naver.maps.LatLng(Lat, Lng),' +
    '      map: map' +
    '    });' +
    '    markersArray.push(marker); ' +
    '  }' +
    '' +
    '  function ClearMarkers() {  ' +
    '    if (markersArray) {        ' +
    '      for (i in markersArray) {  ' +
    '        markersArray[i].setMap(null); ' +
    '      } ' +
    '    } ' +
    '  }' +
    '' +
    '  function codeAddress(address) { ' +
    '    naver.maps.Service.geocode({' +
    '      address: address' +
    '      }, function(status, response) { ' +
    '        document.getElementById("status").value= status;' +
    '        if (status == naver.maps.Service.Status.OK) {' +
    '          var result = response.result;' +
    '          var items = result.items;';
  for index:= 0 to MAX_ADDRESS_INDEX do
    HTMLStr:= HTMLStr +
    '          if (items['+IntToStr(index)+']) { ' +
    '            document.getElementById("addr_items' + IntToStr(index) + '").value = JSON.stringify(items['+IntToStr(index)+']);' +
    '          }';
  HTMLStr:= HTMLStr +
    '          var Lat = items[0].point.y;' +
    '          var Lng = items[0].point.x;' +
    '          map.setCenter(new naver.maps.LatLng(Lat, Lng));' +
    '          PutMarker(Lat, Lng);' +
    '        } else {' +
    '          alert("Geocode was not successful for the following reason: " + status);' +
    '        }' +
    '      });' +
    '  }' +
    '' +
    '  function codeLatLng(Lat, Lng, bPutMarker) {  ' +
    '    var latlng = new naver.maps.LatLng(Lat, Lng);' +
    '    naver.maps.Service.reverseGeocode({' +
    '        location: latlng' +
    '      }, function(status, response){' +
    '        document.getElementById("status").value= status;' +
    '        if (status == naver.maps.Service.Status.OK) {' +
    '          var result = response.result;' +
    '          var items = result.items;';
  for index:= 0 to MAX_ADDRESS_INDEX do
    HTMLStr:= HTMLStr +
    '          if (items['+IntToStr(index)+']) { ' +
    '            document.getElementById("addr_items' + IntToStr(index) + '").value = JSON.stringify(items['+IntToStr(index)+']);' +
    '          }';
  HTMLStr:= HTMLStr +
    '          map.setCenter(latlng);' +
    '          if (bPutMarker) { '+
    '            PutMarker(Lat, Lng);' +
    '          }' +
    '        } else {' +
    '          alert("Geocoder failed due to: " + status);' +
    '        }' +
    '      });' +
    '  }' +
    '' +
    '</script>' +
    '' +
    '</head>' +
    '' +
    '<body onload="initialize()" style="margin:0;padding:0"> ' +
    '<div id="map" style="width:100%; height:' + IntToStr(Self.Height) + 'px"></div>';
  for index:= 0 to MAX_ADDRESS_INDEX do
    HTMLStr:= HTMLStr +
    '  <input id="addr_items' + IntToStr(index) + '" type="hidden"></input>';
  HTMLStr:= HTMLStr +
    '  <input id="status" type="hidden"></input> ' +
    '</body>' +
    '' +
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

procedure TNMap.MovePan(toRight, toDown: Integer);
begin
  FHTMLWindow2.execScript('MovePan('+FloatToStr(toRight)+','+FloatToStr(toDown)+')', 'JavaScript');
end;

function TNMap.mtToStr(mt: TNMapType): string;
begin
  case mt of
    mtNormal:    Result:= 'NORMAL';
    mtSatellite: Result:= 'SATELLITE';
    mtHybrid:    Result:= 'HYBRID';
    mtTerrain:   Result:= 'TERRAIN';
    else
      Result:= 'NORMAL';
  end;
  Result:= 'naver.maps.MapTypeId.' + Result;
end;

procedure TNMap.PutMarker(geo_x, geo_y: double);
begin
  FHTMLWindow2.execScript('PutMarker('+FloatToStr(geo_x)+','+FloatToStr(geo_y) + ')', 'JavaScript');
end;

procedure TNMap.RemoveClickEvent;
begin
  FHTMLWindow2.execScript('RemoveClickEvent()', 'JavaScript');
end;

function TNMap.SetIdValue(const Id, Value: string): boolean;
var
  ABody     : IHTMLElement2;

  Tag      : IHTMLElement;
  TagsList : IHTMLElementCollection;
  Index    : Integer;
begin
  Result:= False;
  if not Supports(IHTMLDocument2(Document).body, IHTMLElement2, ABody) then
    exit;
  TagsList := ABody.getElementsByTagName('input');
  for Index := 0 to TagsList.length-1 do
  begin
    Tag:=TagsList.item(Index, EmptyParam) As IHTMLElement;
    if CompareText(Tag.id,Id) = 0 then
    begin
      Tag.setAttribute('value', Value, 0);
      Result:= True;
    end;
  end;
end;

procedure TNMap.SetMapType(mt: TNMapType);
begin
  FHTMLWindow2.execScript('SetMapType(' + mtToStr(mt) + ')', 'JavaScript');
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
