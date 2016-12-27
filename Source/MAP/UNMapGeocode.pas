unit UNMapGeocode;

interface

uses
  SysUtils, Generics.Collections, DBXJSON;

type
  TNAPIAddress = record
    address: string;
    addr_country: string;
    addr_sido: string;
    addr_sigugun: string;
    addr_dongmyun: string;
    addr_rest: string;
    isRoadAddress: Boolean;
    geo_x: double;
    geo_y: double;
  end;
  TzNAPIAddress = TList<TNAPIAddress>;

function addAddressList(AddrItem: string; var zAddress: TzNAPIAddress): Boolean;

implementation

function addAddressList(AddrItem: string; var zAddress: TzNAPIAddress): Boolean;
var
  Address: TNAPIAddress;
  JoAddress, JoAddrDetail: TJSONObject;
begin
  JoAddress:= TJSONObject(TJSONObject.ParseJSONValue(AddrItem));
  try
    Address:= Default(TNAPIAddress);
    Address.address      := JoAddress.Get('address').JsonValue.Value;
    Address.isRoadAddress:= (JoAddress.Get('isRoadAddress').JsonValue.ClassType = TJSONTrue);
    Address.geo_x        := TJSONNumber(TJSONObject(JoAddress.Get('point').JsonValue).Get('x').JsonValue).AsDouble;
    Address.geo_y        := TJSONNumber(TJSONObject(JoAddress.Get('point').JsonValue).Get('y').JsonValue).AsDouble;

    JoAddrDetail:= TJSONObject(JoAddress.Get('addrdetail').JsonValue);
    Address.addr_country := JoAddrDetail.Get('country').JsonValue.Value;
    Address.addr_sido    := JoAddrDetail.Get('sido').JsonValue.Value;
    Address.addr_sigugun := JoAddrDetail.Get('sigugun').JsonValue.Value;
    Address.addr_dongmyun:= JoAddrDetail.Get('dongmyun').JsonValue.Value;
    Address.addr_rest    := JoAddrDetail.Get('rest').JsonValue.Value;

    zAddress.Add(Address);
  finally
    FreeAndNil(JoAddress);
  end;
end;

end.
