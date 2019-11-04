unit UNAPIClient;

interface

uses
  Classes, SysUtils, IdHTTP, IdSSLOpenSSL, IdURI, IdGlobal;

type
  TNAPIClient = class
    private
      FIdHTTP: TIdHTTP;
      FIdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;

      FHost: string;
      FOutMsg: string;

      function getUTF8String(RcvMsg: TMemoryStream): string;
      function PostWork(Msg: AnsiString; var RcvMsg: TMemoryStream): Boolean;
      function GetWork(Msg: AnsiString; var RcvMsg: TMemoryStream): Boolean;
    public
      constructor Create;
      destructor Destroy; override;

      class function PostMsg(Host: string; Msg: AnsiString;
        var OutMsg: string; var RcvMsg: TMemoryStream): Boolean;
      class function GetMsg(Host: string; Msg: AnsiString;
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

class function TNAPIClient.GetMsg(Host: string; Msg: AnsiString;
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
    Result:= NAPIClient.GetWork(Msg, RcvMsg);
    OutMsg:= OutMsg + NAPIClient.OutMsg;
  finally
    FreeAndNil(NAPIClient);
  end;
end;

function TNAPIClient.getUTF8String(RcvMsg: TMemoryStream): string;
var
  rbStr :RawByteString;
begin
  rbStr := '';
  RcvMsg.Position := 0;
  SetLength(rbStr, RcvMsg.Size);
  RcvMsg.ReadBuffer(rbStr[1], RcvMsg.Size);
  SetCodePage(rbstr, 65001, False);

  Result:= String(rbStr);
end;

function TNAPIClient.GetWork(Msg: AnsiString;
  var RcvMsg: TMemoryStream): Boolean;
begin
  Result:= False;
  if FIdHTTP.Connected then
    FIdHTTP.Disconnect;

  FOutMsg:= FOutMsg + '[Send] ' + Msg + #13#10;

  FIdHTTP.Request.ContentType := 'application/x-www-form-urlencoded';
  FIdHTTP.Request.CharSet     := 'UTF-8';
  FIdHTTP.Request.CustomHeaders.AddValue('X-Naver-Client-Id', NAPIClientID);
  FIdHTTP.Request.CustomHeaders.AddValue('X-Naver-Client-Secret', NAPIClientSecret);
  try
    //FIdHTTP.Get(TIdURI.URLEncode(FHost + '?' + Msg, TEncoding.UTF8), RcvMsg);
    FIdHTTP.Get(TIdURI.URLEncode(FHost + '?' + Msg, IndyTextEncoding_UTF8), RcvMsg);
    FOutMsg:= FOutMsg + '[Recv] ' + FIdHTTP.Response.ResponseText + #13#10;
    if FIdHTTP.Response.ResponseCode = 200 then
    begin
      Result := True;
//      FOutMsg:= FOutMsg + '[Data] ' + getUTF8String(RcvMsg) + #13#10;
    end;
  except
    on E: exception do
      FOutMsg:= FOutMsg + '[ErrM] ' + E.Message + #13#10;
  end;
end;

class function TNAPIClient.PostMsg(Host: string; Msg: AnsiString;
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
    Result:= NAPIClient.PostWork(Msg, RcvMsg);
    OutMsg:= OutMsg + NAPIClient.OutMsg;
  finally
    FreeAndNil(NAPIClient);
  end;
end;

function TNAPIClient.PostWork(Msg: AnsiString;
  var RcvMsg: TMemoryStream): Boolean;
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
      begin
        Result := True;
//        FOutMsg:= FOutMsg + '[Data] ' + getUTF8String(RcvMsg) + #13#10;
      end;
    except
      on E: exception do
        FOutMsg:= FOutMsg + '[ErrM] ' + E.Message + #13#10;
    end;
  finally
    FreeAndNil(srSend);
  end;
end;

end.
