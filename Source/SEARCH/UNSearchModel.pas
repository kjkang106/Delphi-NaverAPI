unit UNSearchModel;

interface

uses
  Classes, SysUtils, Generics.Collections, DBXJSON, Rtti, TypInfo;

type
  TNSearchType = (nstBlog, nstNews, nstBookHD, {nstBookDT,} nstAdultWord, nstEncyc,
    nstMovie, nstCafeArticle, nstKin, nstLocal, nstErrata, nstWeb, nstImage,
    nstShop, nstDoc);
  TNSearchSort = (nssSim, nssDate);

  TNSearchHead = record
    lastBuildDate: string;
    total: Integer;
    start: Integer;
    display: Integer;
  end;

  TNSearchBlog = record
    title: string;
    link: string;
    description: string;
    bloggername: string;
    bloggerlink: string;
    postdate: string;
  end;
  TzNSearchBlog = TList<TNSearchBlog>;
  TNSearchNews = record
    title: string;
    originallink: string;
    link: string;
    description: string;
    pubDate: string;
  end;
  TzNSearchNews = TList<TNSearchNews>;
  TNSearchBookHD = record
    title: string;
    link: string;
    image: string;
    author: string;
    price: Real;
    discount: Real;
    publisher: string;
    pubdate: string;
    isbn: string;
    description: string;
  end;
  TzNSearchBookHD = TList<TNSearchBookHD>;
  TNSearchBookDT = record
    title: string;
    link: string;
  end;
  TzNSearchBookDT = TList<TNSearchBookDT>;
  TNSearchAdultWord = record
    adult: Boolean;    //0: 일반 검색어, 1: 성인 검색어
  end;
  TzNSearchAdultWord = TList<TNSearchAdultWord>;
  TNSearchEncyc = record
    title: string;
    link: string;
    description: string;
    thumbnail: string;
  end;
  TzNSearchEncyc = TList<TNSearchEncyc>;
  TNSearchMovie = record
    title: string;
    link: string;
    image: string;
    subtitle: string;
    pubDate: string;
    director: string;
    actor: string;
    userRating: Real;
  end;
  TzNSearchMovie = TList<TNSearchMovie>;
  TNSearchCafeArticle = record
    title: string;
    link: string;
    description: string;
    cafename: string;
    cafeurl: string;
  end;
  TzNSearchCafeArticle = TList<TNSearchCafeArticle>;
  TNSearchKin = record
    title: string;
    link: string;
    description: string;
  end;
  TzNSearchKin = TList<TNSearchKin>;
  TNSearchLocal = record
    title: string;
    link: string;
    category: string;
    description: string;
    telephone: string;
    address: string;
    roadAddress: string;
    mapx: Integer;
    mapy: Integer;
  end;
  TzNSearchLocal = TList<TNSearchLocal>;
  TNSearchErrata = record
    errata: string;
  end;
  TzNSearchErrata = TList<TNSearchErrata>;
  TNSearchWeb = record
    title: string;
    link: string;
    description: string;
  end;
  TzNSearchWeb = TList<TNSearchWeb>;
  TNSearchImage = record
    title: string;
    link: string;
    thumbnail: string;
    sizeheight: Integer;
    sizewidth: Integer;
  end;
  TzNSearchImage = TList<TNSearchImage>;
  TNSearchShop = record
    title: string;
    link: string;
    image: string;
    lprice: Real;
    hprice: Real;
    mallName: string;
    productId: string;
    productType: string;
  end;
  TzNSearchShop = TList<TNSearchShop>;
  TNSearchDoc = record
    title: string;
    link: string;
    description: string;
  end;
  TzNSearchDoc = TList<TNSearchDoc>;

  TNSearchResult<TzList: class> = class
    private
      FSerarchHead: TNSearchHead;
      FzSearchItem: TzList;
      FCount: Integer;

      procedure parseParam(const doc:TJSONObject; name: string; var value: string); overload;
      procedure parseParam(const doc:TJSONObject; name: string; var value: Real); overload;
      procedure parseParam(const doc:TJSONObject; name: string; var value: Integer); overload;
      procedure parseParams(const doc:TJSONObject; ATypeInfo, tgtInstance: Pointer);

      procedure parseNBlog(zItems: TJSONArray);
      procedure parseNNews(zItems: TJSONArray);
      procedure parseNBookHD(zItems: TJSONArray);
      procedure parseNBookDT(zItems: TJSONArray);
      procedure parseNAdultWord(JoResult: TJSONObject);
      procedure parseNEncyc(zItems: TJSONArray);
      procedure parseNMovie(zItems: TJSONArray);
      procedure parseNCafeArticle(zItems: TJSONArray);
      procedure parseNKin(zItems: TJSONArray);
      procedure parseNLocal(zItems: TJSONArray);
      procedure parseNErrata(JoResult: TJSONObject);
      procedure parseNWeb(zItems: TJSONArray);
      procedure parseNImage(zItems: TJSONArray);
      procedure parseNShop(zItems: TJSONArray);
      procedure parseNDoc(zItems: TJSONArray);
    public
      constructor Create;
      destructor Destroy; override;

      function parseResult(NSearchType: TNSearchType; srcStream: TMemoryStream;
        var OutMsg: string): Boolean;

      property count: Integer read FCount write FCount;
      property zSearchItem: TzList read FzSearchItem;
  end;
  TzNBlog = TNSearchResult<TzNSearchBlog>;
  TzNNews = TNSearchResult<TzNSearchNews>;
  TzNBookHD = TNSearchResult<TzNSearchBookHD>;
  TzNBookDT = TNSearchResult<TzNSearchBookDT>;
  TzNAdultWord = TNSearchResult<TzNSearchAdultWord>;
  TzNEncyc = TNSearchResult<TzNSearchEncyc>;
  TzNMovie = TNSearchResult<TzNSearchMovie>;
  TzNCafe = TNSearchResult<TzNSearchCafeArticle>;
  TzNKin = TNSearchResult<TzNSearchKin>;
  TzNLocal = TNSearchResult<TzNSearchLocal>;
  TzNErrata = TNSearchResult<TzNSearchErrata>;
  TzNWeb = TNSearchResult<TzNSearchWeb>;
  TzNImage = TNSearchResult<TzNSearchImage>;
  TzNShop = TNSearchResult<TzNSearchShop>;
  TzNDoc = TNSearchResult<TzNSearchDoc>;

implementation

{ TNSearchResult<T> }

constructor TNSearchResult<TzList>.Create;
begin
  inherited;

  FCount:= 0;
       if TypeInfo(TzList) = TypeInfo(TzNSearchBlog) then
    FzSearchItem:= TzList(TzNSearchBlog.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchNews) then
    FzSearchItem:= TzList(TzNSearchNews.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchBookHD) then
    FzSearchItem:= TzList(TzNSearchBookHD.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchBookDT) then
    FzSearchItem:= TzList(TzNSearchBookDT.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchAdultWord) then
    FzSearchItem:= TzList(TzNSearchAdultWord.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchEncyc) then
    FzSearchItem:= TzList(TzNSearchEncyc.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchMovie) then
    FzSearchItem:= TzList(TzNSearchMovie.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchCafeArticle) then
    FzSearchItem:= TzList(TzNSearchCafeArticle.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchKin) then
    FzSearchItem:= TzList(TzNSearchKin.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchLocal) then
    FzSearchItem:= TzList(TzNSearchLocal.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchErrata) then
    FzSearchItem:= TzList(TzNSearchErrata.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchWeb) then
    FzSearchItem:= TzList(TzNSearchWeb.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchImage) then
    FzSearchItem:= TzList(TzNSearchImage.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchShop) then
    FzSearchItem:= TzList(TzNSearchShop.Create)
  else if TypeInfo(TzList) = TypeInfo(TzNSearchDoc) then
    FzSearchItem:= TzList(TzNSearchDoc.Create);
end;

destructor TNSearchResult<TzList>.Destroy;
begin
  FreeAndNil(FzSearchItem);

  inherited;
end;

procedure TNSearchResult<TzList>.parseParam(const doc: TJSONObject; name: string;
  var value: string);
begin
  value:= '';
  if doc.Get(name) = nil then
    Exit;
  value:= doc.Get(name).JsonValue.Value;
end;

procedure TNSearchResult<TzList>.parseParam(const doc: TJSONObject; name: string;
  var value: Real);
begin
  value:= 0;
  if doc.Get(name) = nil then
    Exit;
  if doc.Get(name).JsonValue.Value <> '' then
    value:= TJSONNumber(doc.Get(name).JsonValue).AsDouble;
end;

procedure TNSearchResult<TzList>.parseNAdultWord(JoResult: TJSONObject);
var
  ASearch: TNSearchAdultWord;
  nTemp: Integer;
begin
  ASearch:= Default(TNSearchAdultWord);
  parseParam(JoResult, 'adult', nTemp);
  ASearch.adult:= (nTemp = 1);

  TzNSearchAdultWord(FzSearchItem).Add(ASearch);
  Inc(FCount);
end;

procedure TNSearchResult<TzList>.parseNBlog(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchBlog;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchBlog);
    parseParams(JoItems, TypeInfo(TNSearchBlog), @ASearch);
    TzNSearchBlog(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNBookDT(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchBookDT;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchBookDT);
    parseParams(JoItems, TypeInfo(TNSearchBookDT), @ASearch);
    TzNSearchBookDT(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNBookHD(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchBookHD;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchBookHD);
    parseParams(JoItems, TypeInfo(TNSearchBookHD), @ASearch);
    TzNSearchBookHD(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNCafeArticle(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchCafeArticle;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchCafeArticle);
    parseParams(JoItems, TypeInfo(TNSearchCafeArticle), @ASearch);
    TzNSearchCafeArticle(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNDoc(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchDoc;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchDoc);
    parseParams(JoItems, TypeInfo(TNSearchDoc), @ASearch);
    TzNSearchDoc(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNEncyc(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchEncyc;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchEncyc);
    parseParams(JoItems, TypeInfo(TNSearchEncyc), @ASearch);
    TzNSearchEncyc(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNErrata(JoResult: TJSONObject);
var
  ASearch: TNSearchErrata;
begin
  ASearch:= Default(TNSearchErrata);
  parseParam(JoResult, 'errata', ASearch.errata);
  TzNSearchErrata(FzSearchItem).Add(ASearch);
  Inc(FCount);
end;

procedure TNSearchResult<TzList>.parseNImage(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchImage;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchImage);
    parseParams(JoItems, TypeInfo(TNSearchImage), @ASearch);
    TzNSearchImage(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNKin(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchKin;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchKin);
    parseParams(JoItems, TypeInfo(TNSearchKin), @ASearch);
    TzNSearchKin(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNLocal(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchLocal;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchLocal);
    parseParams(JoItems, TypeInfo(TNSearchLocal), @ASearch);
    TzNSearchLocal(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNMovie(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchMovie;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchMovie);
    parseParams(JoItems, TypeInfo(TNSearchMovie), @ASearch);
    TzNSearchMovie(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNNews(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchNews;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchNews);
    parseParams(JoItems, TypeInfo(TNSearchNews), @ASearch);
    TzNSearchNews(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNShop(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchShop;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchShop);
    parseParams(JoItems, TypeInfo(TNSearchShop), @ASearch);
    TzNSearchShop(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseNWeb(zItems: TJSONArray);
var
  JoItems: TJSONObject;
  nMax, idx: Integer;
  ASearch: TNSearchWeb;
begin
  nMax:= zItems.Size;
  for idx:= 0 to nMax - 1 do
  begin
    JoItems:= TJSONObject(zItems.Get(idx));

    ASearch:= Default(TNSearchWeb);
    parseParams(JoItems, TypeInfo(TNSearchWeb), @ASearch);
    TzNSearchWeb(FzSearchItem).Add(ASearch);
    Inc(FCount);
  end;
end;

procedure TNSearchResult<TzList>.parseParam(const doc: TJSONObject; name: string;
  var value: Integer);
begin
  value:= 0;
  if doc.Get(name) = nil then
    Exit;
  if doc.Get(name).JsonValue.Value <> '' then
    value:= TJSONNumber(doc.Get(name).JsonValue).AsInt;
end;

procedure TNSearchResult<TzList>.parseParams(const doc: TJSONObject; ATypeInfo,
  tgtInstance: Pointer);
var
  rttiContext: TRttiContext;
  rttiType: TRttiType;
  fields: TArray<TRttiField>;
  i: Word;

  name: string;
  iValue: Integer;
  nValue: Real;
  sValue: string;
begin
  rttiType:= rttiContext.GetType(ATypeInfo);
  fields  := rttiType.GetFields;
  try
    for i:= low(fields) to high(fields) do
    begin
      name:= fields[i].Name;
      if fields[i].FieldType.TypeKind = tkInteger then
      begin
        parseParam(doc, name, iValue);
        fields[i].SetValue(tgtInstance, iValue);
      end
      else if fields[i].FieldType.TypeKind = tkFloat then
      begin
        parseParam(doc, name, nValue);
        fields[i].SetValue(tgtInstance, nValue);
      end
      else if fields[i].FieldType.TypeKind = tkClass then
      else if fields[i].FieldType.TypeKind = tkRecord then
      else
      begin
        parseParam(doc, name, sValue);
        fields[i].SetValue(tgtInstance, sValue);
      end;
    end;
  finally
    Finalize(fields);
  end;
end;

function TNSearchResult<TzList>.parseResult(NSearchType: TNSearchType;
  srcStream: TMemoryStream; var OutMsg: string): Boolean;
var
  srByte: TBytesStream;
  JoResult: TJSONObject;
  zItems: TJSONArray;
begin
  Result:= False;

  srByte:= TBytesStream.Create;
  srByte.LoadFromStream(srcStream);
  JoResult:= TJSONObject(TJSONObject.ParseJSONValue(srByte.Bytes, 0, srByte.Size));
  try
    if not Assigned(JoResult) then
      Exit;
//    OutMsg:= OutMsg + JoResult.ToString + #13#10;
    if NSearchType = nstAdultWord then
    begin
      parseNAdultWord(JoResult);
    end
    else if NSearchType = nstErrata then
    begin
      parseNErrata(JoResult);
    end
    else
    begin
      parseParams(JoResult, TypeInfo(TNSearchHead), @FSerarchHead);
      zItems:= TJSONArray(JoResult.Get('items').JsonValue);
      case NSearchType of
        nstBlog:        parseNBlog(zItems);
        nstNews:        parseNNews(zItems);
        nstBookHD:      parseNBookHD(zItems);
  //      nstBookDT:      parseNBookDT(zItems);
        nstEncyc:       parseNEncyc(zItems);
        nstMovie:       parseNMovie(zItems);
        nstCafeArticle: parseNCafeArticle(zItems);
        nstKin:         parseNKin(zItems);
        nstLocal:       parseNLocal(zItems);
        nstWeb:         parseNWeb(zItems);
        nstImage:       parseNImage(zItems);
        nstShop:        parseNShop(zItems);
        nstDoc:         parseNDoc(zItems);
      end;
    end;
  finally
    srByte.Free;
    if Assigned(JoResult) then
      JoResult.Free;
  end;
  Result:= (FCount > 0);
end;

end.
