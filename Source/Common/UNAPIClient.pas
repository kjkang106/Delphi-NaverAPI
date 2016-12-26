unit UNAPIClient;

interface

uses
  Classes, SysUtils, IdHTTP, IdSSLOpenSSL;

type
  TNAPIClient = class
    private
      FIdHTTP: TIdHTTP;
      FIdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;

      FHost: string;
      FOutMsg: string;
      function SendWork(Msg: AnsiString; var RcvMsg: TMemoryStream): Boolean;
    public
      constructor Create;
      destructor Destroy; override;

      class function SendMsg(Host: string; Msg: AnsiString;
        var OutMsg: string; var RcvMsg: TMemoryStream): Boolean;

      property OutMsg: string read FOutMsg;
  end;

implementation

uses UNAPIVAR;

{ TNAPIClient }

constructor TNAPIClient.Create;
begin
  inherited;

  FIdHTTP:= TIdHTTP.Create;
  FIdSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

  FHost  := '';
  FOutMsg:= '';

  FIdHTTP.IOHandler:= FIdSSLIOHandlerSocketOpenSSL;
end;

destructor TNAPIClient.Destroy;
begin
  FIdHTTP.Disconnect;
  FreeAndNil(FIdSSLIOHandlerSocketOpenSSL);
  FreeAndNil(FIdHTTP);

  inherited;
end;

class function TNAPIClient.SendMsg(Host: string; Msg: AnsiString;
  var OutMsg: string; var RcvMsg: TMemoryStream): Boolean;
var
  NAPIClient : TNAPIClient;
begin
  Result:= False;
  Host:= Trim(Host);
  if Host = '' then
    Exit;

  NAPIClient:= TNAPIClient.Create;
  NAPIClient.FIdHTTP.ConnectTimeout:= 5000;
  NAPIClient.FHost:= Host;

  RcvMsg.SetSize(0);
  try
    Result:= NAPIClient.SendWork(Msg, RcvMsg);
    OutMsg:= OutMsg + NAPIClient.OutMsg;
  finally
    FreeAndNil(NAPIClient);
  end;
end;

function TNAPIClient.SendWork(Msg: AnsiString; var RcvMsg: TMemoryStream): Boolean;
var
  srSend: TStringStream;
begin
  Result:= False;
  if FIdHTTP.Connected then
    FIdHTTP.Disconnect;

  FOutMsg:= FOutMsg + '[Send] ' + Msg + #13#10;

  srSend:= TStringStream.Create(Msg, TEncoding.UTF8);
  try
    FIdHTTP.Request.ContentType := 'application/x-www-form-urlencoded';
    FIdHTTP.Request.CharSet     := 'UTF-8';
    FIdHTTP.Request.CustomHeaders.AddValue('X-Naver-Client-Id', NAPIClientID);
    FIdHTTP.Request.CustomHeaders.AddValue('X-Naver-Client-Secret', NAPIClientSecret);
    try
      FIdHTTP.Post(FHost, srSend, RcvMsg);
      FOutMsg:= FOutMsg + '[Recv] ' + FIdHTTP.Response.ResponseText + #13#10;
      if FIdHTTP.Response.ResponseCode = 200 then
        Result := True;
    except
      on E: exception do
        FOutMsg:= FOutMsg + '[ErrM] ' + E.Message + #13#10;
    end;
  finally
    FreeAndNil(srSend);
  end;
end;

end.
