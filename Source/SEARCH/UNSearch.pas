unit UNSearch;

interface

uses
  Classes, SysUtils, UNSearchModel;

type
  TNSearch<TzList: class> = class
    private
      FDisplayCnt: Integer;
      FStartNo: Integer;
      FNSearchSort: TNSearchSort;
      FOutMsg: string;

      function NSearchTypeToStr(nst: TNSearchType): string;
      function NSearchSortToStr(nss: TNSearchSort): string;

      procedure setFDisplayCnt(value: Integer);
      procedure setFStartNo(value: Integer);
      function getNSearchType: TNSearchType;
    public
      constructor Create;
      destructor Destroy; override;

      function getNSearch(const searchTxt: string; var SearchResult: TNSearchResult<TzList>): Boolean;

      property NSearchType: TNSearchType read getNSearchType;
      property StartNo: Integer read FStartNo write setFStartNo;
      property displayCnt: Integer read FDisplayCnt write setFDisplayCnt;
      property NSearchSort: TNSearchSort read FNSearchSort write FNSearchSort;
      property OutMsg: string read FOutMsg write FOutMsg;
  end;
  TNBlog = TNSearch<TzNSearchBlog>;
  TNNews = TNSearch<TzNSearchNews>;
  TNBookHD = TNSearch<TzNSearchBookHD>;
  TNBookDT = TNSearch<TzNSearchBookDT>;
  TNAdultWord = TNSearch<TzNSearchAdultWord>;
  TNEncyc = TNSearch<TzNSearchEncyc>;
  TNMovie = TNSearch<TzNSearchMovie>;
  TNCafe = TNSearch<TzNSearchCafeArticle>;
  TNKin = TNSearch<TzNSearchKin>;
  TNLocal = TNSearch<TzNSearchLocal>;
  TNErrata = TNSearch<TzNSearchErrata>;
  TNWeb = TNSearch<TzNSearchWeb>;
  TNImage = TNSearch<TzNSearchImage>;
  TNShop = TNSearch<TzNSearchShop>;
  TNDoc = TNSearch<TzNSearchDoc>;

implementation

uses UNAPIClient, UNAPIVAR;


{ TNSearch<T> }

constructor TNSearch<TzList>.Create;
begin
  FDisplayCnt := 10;
  FStartNo    := 1;
  FNSearchSort:= nssSim;
  FOutMsg     := '';
end;

destructor TNSearch<TzList>.Destroy;
begin

  inherited;
end;

function TNSearch<TzList>.getNSearch(const searchTxt: string; var SearchResult: TNSearchResult<TzList>): Boolean;
var
  outStream: TMemoryStream;
  PostMsg: string;
begin
  Result:= False;

  PostMsg:= '';
  PostMsg:= PostMsg + 'query=' + searchTxt + '&';
  PostMsg:= PostMsg + 'display=' + IntToStr(displayCnt) + '&';
  PostMsg:= PostMsg + 'start=' + IntToStr(StartNo) + '&';
  PostMsg:= PostMsg + 'sort=' + NSearchSortToStr(NSearchSort) + '&';
  Delete(PostMsg, Length(PostMsg), 1);

  outStream:= TMemoryStream.Create;
  try
    OutMsg:= OutMsg + '- Send Start:' + FormatDateTime('HH:NN:SS.zzz',time) + #13#10;
    Result:= TNAPIClient.GetMsg(SEARCH_URL + NSearchTypeToStr(NSearchType) + '.json',
      PostMsg, FOutMsg, outStream);
    OutMsg:= OutMsg + '- Send End  :' + FormatDateTime('HH:NN:SS.zzz',time) + #13#10;
    if Result then
      Result:= SearchResult.parseResult(NSearchType, outStream, FOutMsg);
  finally
    FreeAndNil(outStream);
  end;
end;

function TNSearch<TzList>.getNSearchType: TNSearchType;
begin
  Result:= nstKin;  //default
       if TypeInfo(TzList) = TypeInfo(TzNSearchBlog) then
    Result:= nstBlog
  else if TypeInfo(TzList) = TypeInfo(TzNSearchNews) then
    Result:= nstNews
  else if TypeInfo(TzList) = TypeInfo(TzNSearchBookHD) then
    Result:= nstBookHD
//  else if TypeInfo(TzList) = TypeInfo(TzNSearchBookDT) then
//    Result:= nstBookDT
  else if TypeInfo(TzList) = TypeInfo(TzNSearchAdultWord) then
    Result:= nstAdultWord
  else if TypeInfo(TzList) = TypeInfo(TzNSearchEncyc) then
    Result:= nstEncyc
  else if TypeInfo(TzList) = TypeInfo(TzNSearchMovie) then
    Result:= nstMovie
  else if TypeInfo(TzList) = TypeInfo(TzNSearchCafeArticle) then
    Result:= nstCafeArticle
  else if TypeInfo(TzList) = TypeInfo(TzNSearchKin) then
    Result:= nstKin
  else if TypeInfo(TzList) = TypeInfo(TzNSearchLocal) then
    Result:= nstLocal
  else if TypeInfo(TzList) = TypeInfo(TzNSearchErrata) then
    Result:= nstErrata
  else if TypeInfo(TzList) = TypeInfo(TzNSearchWeb) then
    Result:= nstWeb
  else if TypeInfo(TzList) = TypeInfo(TzNSearchImage) then
    Result:= nstImage
  else if TypeInfo(TzList) = TypeInfo(TzNSearchShop) then
    Result:= nstShop
  else if TypeInfo(TzList) = TypeInfo(TzNSearchDoc) then
    Result:= nstDoc;
end;

function TNSearch<TzList>.NSearchSortToStr(nss: TNSearchSort): string;
begin
  case nss of
    nssSim:  Result:= 'sim';
    nssDate: Result:= 'date';
    else
      Result:= '';
  end;
end;

function TNSearch<TzList>.NSearchTypeToStr(nst: TNSearchType): string;
begin
  case nst of
    nstBlog:        Result:= 'blog';
    nstNews:        Result:= 'news';
    nstBookHD:      Result:= 'book';
//    nstBookDT:      Result:= 'book_adv';  //xml만 존재
    nstAdultWord:   Result:= 'adult';  //성인 검색어 판별
    nstEncyc:       Result:= 'encyc';  //백과사전
    nstMovie:       Result:= 'movie';
    nstCafeArticle: Result:= 'cafearticle';
    nstKin:         Result:= 'kin';
    nstLocal:       Result:= 'local';
    nstErrata:      Result:= 'errata';  //오타변환
    nstWeb:         Result:= 'webkr';
    nstImage:       Result:= 'image';
    nstShop:        Result:= 'shop';
    nstDoc:         Result:= 'doc';
    else
      Result:= '';
  end;
end;

procedure TNSearch<TzList>.setFDisplayCnt(value: Integer);
begin
  if value <= 0 then
    value:= 10
  else if value > 100 then
    value:= 100;
  FDisplayCnt:= value;
end;

procedure TNSearch<TzList>.setFStartNo(value: Integer);
begin
  if value <= 0 then
    value:= 1
  else if value > 1000 then
    value:= 1000;
  FStartNo:= value;
end;

end.
