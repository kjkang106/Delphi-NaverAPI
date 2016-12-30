unit UNTTS;

interface

uses
  Windows, Classes, Messages, SysUtils, MMsystem;

type
  TNTTSSpeeker = (mijin, jinho, clara, matt, yuri, shinji, meimei); //mijin:미진(한국어, 여성), jinho:진호(한국어, 남성), clara:클라라(영어, 여성), matt:매튜(영어, 남성), yuri:유리(일본어, 여성), shinji:신지(일본어, 남성), meimei:메이메이(중국어, 여성))

  TNTTS = class(TThread)
    private
      FWinHandle: HWND;
      FOnResult: TNotifyEvent;
      FDeviceID: DWORD;

      FMp3File: string;
      FSpeeker: TNTTSSpeeker;
      FSpeed: Integer;         //-5 ~ 5 사이 정수로 -5면 0.5배 빠른, 5면 0.5배 느린, 0이면 정상 속도의 목소리로 합성
      FTtsText: string;
      FOutMsg: string;

      procedure DeallocateHWnd(Wnd: HWND);

      function saveToFile(srcStream: TMemoryStream; tgtFileName: string): Boolean;
      function mp3Play(srcStream: TMemoryStream): Boolean;
      function spToStr(sp: TNTTSSpeeker): string;

      procedure mciCheck(R: Cardinal);

      function getTTS: Boolean;

      procedure setFSpeed(value: Integer);
    protected
      procedure Execute; override;
      procedure WndProc(var msg: TMessage);
    public
      constructor Create;
      destructor Destroy; override;

      procedure notiResult;
      property speed: Integer read FSpeed write setFSpeed;
      property speeker: TNTTSSpeeker read FSpeeker write FSpeeker;
      property ttsText: string read FTtsText write FTtsText;
      property OutMsg: string read FOutMsg write FOutMsg;
      property OnResult: TNotifyEvent read FOnResult write FOnResult;
  end;

procedure getTTS(Text: string; onTTSResult: TNotifyEvent = nil;
  speeker:TNTTSSpeeker = mijin; speed:Integer = 0);

implementation

uses
  UNAPIVAR, UNAPIClient;

procedure getTTS(Text: string; onTTSResult: TNotifyEvent = nil;
  speeker:TNTTSSpeeker = mijin; speed:Integer = 0);
var
  ANTTS: TNTTS;
begin
  ANTTS:= TNTTS.Create;
  ANTTS.speeker := speeker;
  ANTTS.speed   := speed;
  ANTTS.ttsText := Text;
  ANTTS.OnResult:= onTTSResult;
  ANTTS.Start;
end;

{ TNTTS }

constructor TNTTS.Create;
begin
  FWinHandle := AllocateHWND(WndProc);
  inherited Create( true );

  FMp3File:= 'TTSOUT' + IntToStr(Self.ThreadID) + '.mp3';

  FSpeeker:= mijin;
  FSpeed  := 0;
  FTtsText:= '';
end;

procedure TNTTS.DeallocateHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));
  if Instance <> @DefWindowProc then
    // make sure we restore the old, original windows procedure before leaving
    SetWindowLong(Wnd, GWL_WNDPROC, Longint(@DefWindowProc));
  FreeObjectInstance(Instance);
  DestroyWindow(Wnd);
end;

destructor TNTTS.Destroy;
begin
  if FileExists(FMp3File) then
    DeleteFile(FMp3File);
  OnResult:= nil;
  DeallocateHWnd(FWinHandle);

  inherited;
end;

procedure TNTTS.Execute;
begin
  FreeOnTerminate := True;
  if getTTS then
  begin
    while not Terminated do
      Sleep(100);
    mciCheck(mciSendCommand(FDeviceID, MCI_CLOSE, 0, 0)); //연주중지
  end;
  Synchronize(notiResult);
end;

function TNTTS.getTTS: Boolean;
var
  outStream: TMemoryStream;
  PostMsg: string;
begin
  Result:= False;

  PostMsg:= '';
  PostMsg:= PostMsg + 'speaker=' + spToStr(speeker) + '&';
  PostMsg:= PostMsg + 'speed=' + IntToStr(speed) + '&';
  PostMsg:= PostMsg + 'text=' + ttsText + '&';
  Delete(PostMsg, Length(PostMsg), 1);

  outStream:= TMemoryStream.Create;
  try
    OutMsg:= OutMsg + '- Send Start:' + FormatDateTime('HH:NN:SS.zzz',time) + #13#10;
    Result:= TNAPIClient.PostMsg(TTS_URL, PostMsg, FOutMsg, outStream);
    OutMsg:= OutMsg + '- Send End  :' + FormatDateTime('HH:NN:SS.zzz',time) + #13#10;
    if Result then
      Result:= mp3Play(outStream);
  finally
    FreeAndNil(outStream);
  end;
end;

procedure TNTTS.mciCheck(R: Cardinal);
var
  S: Array[0..1023] of Char;
begin
  if R = 0 then
    exit;

  mciGetErrorString(R, S, SizeOf(S)-1);
  OutMsg:= OutMsg + '- mciCheck Error:' + S + #13#10;
  Synchronize(notiResult);

  raise Exception.Create(S);
end;

function TNTTS.mp3Play(srcStream: TMemoryStream): Boolean;
var
  mciOpen : MCI_OPEN_PARMS;
  mciPlay : MCI_PLAY_PARMS;

  mciError: Cardinal;
begin
  Result:= False;
  OutMsg:= OutMsg + '- Save MP3:' + FormatDateTime('HH:NN:SS.zzz',time) + #13#10;

  if saveToFile(srcStream, FMp3File) then
  begin
    OutMsg:= OutMsg + '- Play Start:' + FormatDateTime('HH:NN:SS.zzz',time) + #13#10;
    mciOpen:= Default(MCI_OPEN_PARMS);
    mciOpen.lpstrDeviceType := 'MPEGVideo';
    mciOpen.lpstrElementName:= PWideChar(FMp3File);

    mciError:= mciSendCommand(0, MCI_OPEN, MCI_OPEN_ELEMENT or MCI_OPEN_TYPE, DWORD(@mciOpen));
    if mciError = MCIERR_DEVICE_OPEN then
    begin
      FDeviceID := mciGetdeviceID(PWideChar(FMp3File));
      mciSendCommand(FDeviceID, MCI_CLOSE, 0, 0);
      mciError:= mciSendCommand(0, MCI_OPEN, MCI_OPEN_ELEMENT or MCI_OPEN_TYPE, DWORD(@mciOpen));
    end;

    if mciError = MMSYSERR_NOERROR then
    begin
      FDeviceID := mciOpen.wDeviceID;
      mciPlay.dwCallback := FWinHandle;
      mciError:= mciSendCommand(FDeviceID, MCI_PLAY, MCI_NOTIFY, DWORD(@mciPlay));

      Result:= (mciError = MMSYSERR_NOERROR);
    end;

    if Result then
    else
      mciCheck(mciError);
  end
  else
    OutMsg:= OutMsg + '- MP3 Save Fail' + #13#10;
end;

procedure TNTTS.notiResult;
begin
  if Assigned(OnResult) then
    OnResult(Self);
end;

function TNTTS.saveToFile(srcStream: TMemoryStream;
  tgtFileName: string): Boolean;
begin
  Result:= False;
  if FileExists(tgtFileName) then
    DeleteFile(tgtFileName);

  try
    srcStream.SaveToFile(tgtFileName);
    Result:= True;
  except
    on E: exception do
      OutMsg:= OutMsg + '- saveToFile Fail ' + E.Message + #13#10;
  end;
end;

procedure TNTTS.setFSpeed(value: Integer);
begin
  if value < -5 then
    value:= -5
  else if value > 5 then
    value:= 5;

  FSpeed:= value;
end;

function TNTTS.spToStr(sp: TNTTSSpeeker): string;
begin
  Result:= '';
  case sp of
    mijin:  Result:= 'mijin';
    jinho:  Result:= 'jinho';
    clara:  Result:= 'clara';
    matt:   Result:= 'matt';
    yuri:   Result:= 'yuri';
    shinji: Result:= 'shinji';
    meimei: Result:= 'meimei';
  end;
end;

procedure TNTTS.WndProc(var msg: TMessage);
begin
  if msg.Msg = MM_MCINOTIFY then
  begin
    OutMsg:= OutMsg + '- Play End  :' + FormatDateTime('HH:NN:SS.zzz',time) + #13#10;
    Self.Terminate;
  end
  else
    Msg.Result := DefWindowProc(FWinHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

end.
