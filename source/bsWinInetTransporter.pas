(******************************************************)
(*                                                    *)
(*            Besa Software Classes Library           *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// WinInet Transporter Library 
/// </summary>
unit bsWinInetTransporter;
{$I Besa.inc}
{TWinINet from https://github.com/synopse/mORMot/blob/master/SynCrtSock.pas}

interface
uses
  Windows, WinInet, SysUtils, Classes, bsTransporter, bsClasses;

type
  TWinINet = class(TInterfacedObject)
  protected
    fServer: RawByteString;
    fUserName:RawByteString;
    fPassword:RawByteString;
    fProxyName: RawByteString;
    fProxyByPass: RawByteString;
    fPort: cardinal;
    fHttps: boolean;
    fKeepAlive: cardinal;
    fSession, fConnection, fRequest: HINTERNET;
    fUserAgent : RawByteString;
    fIgnoreInvalidCerts:Boolean;
    procedure InternalConnect(SendTimeout,ReceiveTimeout: DWORD);
    procedure InternalRequest(const method, aURL: RawByteString);
    procedure InternalCloseRequest;
    procedure InternalAddHeader(const hdr: RawByteString);
    procedure InternalSendRequest(const aData: RawByteString);
    function InternalGetInfo(Info: DWORD): RawByteString;
    function InternalGetInfo32(Info: DWORD): DWORD;
    function InternalReadData(var Data: RawByteString; Read: integer): cardinal;
    class function InternalREST(const url,method,data,header: RawByteString): RawByteString;
    // those internal methods will raise an EWinINet exception on error

  public
    /// connect to http://aServer:aPort or https://aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy
    // - you can customize the default client timeouts by setting appropriate
    // SendTimeout and ReceiveTimeout parameters (in ms) - note that after
    // creation of this instance, the connection is tied to the initial
    // parameters, so we won't publish any properties to change those
    // initial values once created
    constructor Create(const aServer, aPort: RawByteString; aHttps: boolean;
      const aUserName: RawByteString=''; const aPassword: RawByteString='';
      const aProxyName: RawByteString=''; const aProxyByPass: RawByteString='';
      SendTimeout: DWORD=HTTP_DEFAULT_SENDTIMEOUT;
      ReceiveTimeout: DWORD=HTTP_DEFAULT_RECEIVETIMEOUT);
    /// relase the connection
    destructor Destroy; override;

    /// low-level HTTP/1.1 request
    // - after an Create(server,port), return 200,202,204 if OK,
    // http status error otherwise
    function Request(const url, method: RawByteString; KeepAlive: cardinal;
      const InHeader, InData, InDataType: RawByteString;
      out OutHeader, OutData: RawByteString): integer; virtual;

    /// wrapper method to retrieve a resource via an HTTP GET
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - it will internally create a TWinHttpAPI inherited instance: do not use
    // TWinHttpAPI.Get() but either TWinHTTP.Get() or TWinINet.Get() methods
    class function Get(const aURI: RawByteString;
      const aHeader: RawByteString=''): RawByteString;
    /// wrapper method to create a resource via an HTTP POST
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - the supplied aData content is POSTed to the server, with an optional
    // aHeader content
    // - it will internally create a TWinHttpAPI inherited instance: do not use
    // TWinHttpAPI.Post() but either TWinHTTP.Post() or TWinINet.Post() methods
    class function Post(const aURI, aData: RawByteString;
      const aHeader: RawByteString=''): RawByteString;
    /// wrapper method to update a resource via an HTTP PUT
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - the supplied aData content is PUT to the server, with an optional
    // aHeader content
    // - it will internally create a TWinHttpAPI inherited instance: do not use
    // TWinHttpAPI.Put() but either TWinHTTP.Put() or TWinINet.Put() methods
    class function Put(const aURI, aData: RawByteString;
      const aHeader: RawByteString=''): RawByteString;
    /// wrapper method to delete a resource via an HTTP DELETE
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - it will internally create a TWinHttpAPI inherited instance: do not use
    // TWinHttpAPI.Delete() but either TWinHTTP.Delete() or TWinINet.Delete()
    class function Delete(const aURI: RawByteString;
      const aHeader: RawByteString=''): RawByteString;

    /// the remote server host name, as stated specified to the class constructor
    property Server: RawByteString read fServer;
    /// the remote server port number, as specified to the class constructor
    property Port: cardinal read fPort;
    /// if the remote server uses HTTPS, as specified to the class constructor
    property Https: boolean read fHttps;
    /// the remote server optional proxy, as specified to the class constructor
    property ProxyName: RawByteString read fProxyName;
    /// the remote server optional proxy by-pass list, as specified to the class
    // constructor
    property ProxyByPass: RawByteString read fProxyByPass;
    property IgnoreInvalidCerts :Boolean read fIgnoreInvalidCerts write fIgnoreInvalidCerts;
    property Username : RawByteString read fUserName write fUserName;
    property Password : RawByteString read fPassword write fPassword;
  end;

  /// <summary>
  ///   <para>
  ///     <b>TbsWinINetTransporter</b> uses the Microsoft WinInet library to
  ///     make HTTP requests.
  ///   </para>
  ///   <para>
  ///     This uses will automatically make use of options such as proxy and
  ///     firewall settings, and the online/offline state configured for
  ///     Internet Explorer or using the Internet Control Panel. It is the
  ///     most commonly used and recommended transporter component.
  ///   </para>
  /// </summary>
  TbsWinINetTransporter=class(TbsHttpTransporter)
  public
    function Execute(Request : TStream):TStream; override;
  end;

  EWinInetException=class(ETransporterException)
  public
    constructor Create; override;
  end;

implementation


{ TbsWinINetTransporter }

function TbsWinINetTransporter.Execute(Request:TStream): TStream;
var
  sResponse: RawByteString;
  WinINet: TWinINet;
  URI: TbsURI;
  outHeaders: RawByteString;
  aData : RawByteString;
  ProxyName: RawByteString;
begin
  if not URI.From(TargetURL) then Exit;

  ProxyName:=ProxyServer;
  if Length(ProxyPort)>0 then
    ProxyName:=ProxyName+':'+ProxyPort;

  WinINet:= TWinINet.Create(URI.Server, URI.Port,
    URI.Https, Username,Password, ProxyName,ProxyPassword, SendTimeout, ReceiveTimeout);
  WinINet.IgnoreInvalidCerts:=IgnoreInvalidCerts;

    try
      aData:=TStringStream(Request).DataString;
      WinINet.Request(URI.Address,'POST',0,GetHeaders.Text, aData,'',outHeaders,sResponse);
      Result :=TStringStream.Create( sResponse );
    finally
      WinINet.Free;
    end;

end;

{ TWinINet }

constructor TWinINet.Create(const aServer, aPort: RawByteString; aHttps: boolean;
      const aUserName: RawByteString=''; const aPassword: RawByteString='';
      const aProxyName: RawByteString=''; const aProxyByPass: RawByteString='';
      SendTimeout: DWORD=HTTP_DEFAULT_SENDTIMEOUT;
      ReceiveTimeout: DWORD=HTTP_DEFAULT_RECEIVETIMEOUT);
begin
  fPort := StrToIntDef(aPort,0);
  if fPort=0 then
    if aHttps then
      fPort := INTERNET_DEFAULT_HTTPS_PORT else
      fPort := INTERNET_DEFAULT_HTTP_PORT;
  fUserName:=aUserName;
  fPassword:=aPassword;
  fServer := aServer;
  fHttps := aHttps;
  fProxyName := aProxyName;
  fProxyByPass := aProxyByPass;
  InternalConnect(SendTimeout,ReceiveTimeout); // should raise an exception on error
  fIgnoreInvalidCerts:=False;
end;

class function TWinINet.Delete(const aURI,
  aHeader: RawByteString): RawByteString;
begin
  result := InternalREST(aURI,'DELETE','',aHeader);
end;

destructor TWinINet.Destroy;
begin
  if fConnection<>nil then
    InternetCloseHandle(FConnection);
  if fSession<>nil then
    InternetCloseHandle(FSession);
  inherited;
end;

class function TWinINet.Get(const aURI, aHeader: RawByteString): RawByteString;
begin
  result := InternalREST(aURI,'GET','',aHeader);
end;

procedure TWinINet.InternalAddHeader(const hdr: RawByteString);
begin
  if (hdr<>'') and not HttpAddRequestHeadersA(fRequest,
     Pointer(hdr), length(hdr), HTTP_ADDREQ_FLAG_COALESCE) then
    raise EWinInetException.Create;
end;

procedure TWinINet.InternalCloseRequest;
begin
  if fRequest<>nil then begin
    InternetCloseHandle(fRequest);
    fRequest := nil;
  end;
end;


procedure TWinINet.InternalConnect(SendTimeout, ReceiveTimeout: DWORD);
var
  OpenType: integer;
  sbuf : DWORD;
  ZipFlag: LongBool;
begin
  if fProxyName='' then
   OpenType := INTERNET_OPEN_TYPE_PRECONFIG else
   OpenType := INTERNET_OPEN_TYPE_PROXY;
  fSession := InternetOpenA(Pointer(fUserAgent), OpenType,
    pointer(fProxyName), pointer(fProxyByPass), 0);
  if fSession=nil then
    raise EWinInetException.Create;

  fConnection := InternetConnectA(fSession, pointer(fServer), fPort, Pointer(UserName), Pointer(Password),
    INTERNET_SERVICE_HTTP, 0, 0);
  if fConnection=nil then
    raise EWinInetException.Create;

  InternetSetOption(fConnection,INTERNET_OPTION_SEND_TIMEOUT,
    @SendTimeout,SizeOf(SendTimeout));
  InternetSetOption(fConnection,INTERNET_OPTION_RECEIVE_TIMEOUT,
    @ReceiveTimeout,SizeOf(ReceiveTimeout));

  ZipFlag := True;
  InternetSetOption(fConnection, INTERNET_OPTION_HTTP_DECODING,  PChar(@ZipFlag), SizeOf(ZipFlag));

  {
  if fUserName<>'' then
   if not InternetSetOption(fConnection,
               INTERNET_OPTION_USERNAME,
               Pointer(fUserName),
               Length(fUserName)) then
     raise Exception.Create(SysErrorMessage(Windows.GetLastError));

   if fPassword<>'' then
  if not InternetSetOption(fConnection,
               INTERNET_OPTION_PASSWORD,
               Pointer(fPassword),
               Length(fPassword)) then
     raise Exception.Create(SysErrorMessage(Windows.GetLastError));
  }
end;


function TWinINet.InternalGetInfo(Info: DWORD): RawByteString;
var dwSize, dwIndex: DWORD;
begin
  result := '';
  dwSize := 0;
  dwIndex := 0;
  if not HttpQueryInfoA(fRequest, Info, nil, dwSize, dwIndex) and
     (GetLastError=ERROR_INSUFFICIENT_BUFFER) then begin
    SetLength(result,dwSize-1);
    if not HttpQueryInfoA(fRequest, Info, pointer(result), dwSize, dwIndex) then
      result := '';
  end;
end;

function TWinINet.InternalGetInfo32(Info: DWORD): DWORD;
var dwSize, dwIndex: DWORD;
begin
  dwSize := sizeof(result);
  dwIndex := 0;
  Info := Info or HTTP_QUERY_FLAG_NUMBER;
  if not HttpQueryInfoA(fRequest, Info, @result, dwSize, dwIndex) then
    result := 0;
end;

function TWinINet.InternalReadData(var Data: RawByteString;
  Read: integer): cardinal;
begin
  if not InternetReadFile(fRequest, @PByteArray(Data)[Read], length(Data)-Read, result) then
    raise EWinInetException.Create;
end;

procedure TWinINet.InternalRequest(const method, aURL: RawByteString);
const ALL_ACCEPT: array[0..1] of PAnsiChar = ('*/*',nil);
var Flags: DWORD;
begin
  Flags := INTERNET_FLAG_HYPERLINK or INTERNET_FLAG_PRAGMA_NOCACHE
 //   or INTERNET_FLAG_RESYNCHRONIZE // options for a true RESTful request
    ;
  if fKeepAlive<>0 then
    Flags := Flags or INTERNET_FLAG_KEEP_CONNECTION;
  if fHttps then
    Flags := Flags or INTERNET_FLAG_SECURE;

  if IgnoreInvalidCerts then begin
    Flags:=Flags or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID
                 or INTERNET_FLAG_IGNORE_CERT_CN_INVALID
                 or SECURITY_FLAG_IGNORE_UNKNOWN_CA
                 or SECURITY_FLAG_IGNORE_REVOCATION;
  end;

  FRequest := HttpOpenRequestA(FConnection, Pointer(method), Pointer(aURL), nil,
    nil, nil{@ALL_ACCEPT}, Flags,0);

  if FRequest=nil then
    raise EWinInetException.Create;
end;

class function TWinINet.InternalREST(const url, method, data,
  header: RawByteString): RawByteString;
var URI: TbsURI;
    outHeaders: RawByteString;
begin
  result := '';
  with URI do
  if From(url) then
  try
    with self.Create(Server,Port,Https) do
    try
      Request(Address,method,0,header,data,'',outHeaders,result);
    finally
      Free;
    end;
  except
    result := '';
  end;
end;

procedure TWinINet.InternalSendRequest(const aData: RawByteString);
begin
  if not HttpSendRequestA(fRequest, nil, 0, pointer(aData), length(aData)) then
    raise EWinInetException.Create;
end;

class function TWinINet.Post(const aURI, aData,
  aHeader: RawByteString): RawByteString;
begin
  result := InternalREST(aURI,'POST',aData,aHeader);
end;

class function TWinINet.Put(const aURI, aData,
  aHeader: RawByteString): RawByteString;
begin
  result := InternalREST(aURI,'PUT',aData,aHeader);
end;


const
  // while reading an HTTP response, read it in blocks of this size. 8K for now
  HTTP_RESP_BLOCK_SIZE = 8*1024;

function TWinINet.Request(const url, method: RawByteString; KeepAlive: cardinal;
  const InHeader, InData, InDataType: RawByteString; out OutHeader,
  OutData: RawByteString): integer;
var aData, aDataEncoding, aAcceptEncoding, aURL: RawByteString;
    Bytes, ContentLength, Read,SecurityFlags: DWORD;
    i: integer;
begin
  if (url='') or (url[1]<>'/') then
    aURL := '/'+url else // need valid url according to the HTTP/1.1 RFC
    aURL := url;
  fKeepAlive := KeepAlive;
  InternalRequest(method,aURL); // should raise an exception on error
  try
    // common headers
    InternalAddHeader(InHeader);
    if InDataType<>'' then
      InternalAddHeader(RawByteString('Content-Type: ')+InDataType);
    // handle custom compression
    InternalAddHeader(RawByteString('Accept-Encoding: gzip, deflate'));
    aData := InData;

    if IgnoreInvalidCerts then begin
      SecurityFlags:=0;
      SecurityFlags:=SecurityFlags or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID
                                   or INTERNET_FLAG_IGNORE_CERT_CN_INVALID
                                   or SECURITY_FLAG_IGNORE_UNKNOWN_CA
                                   or SECURITY_FLAG_IGNORE_REVOCATION
                                   or INTERNET_ERROR_MASK_LOGIN_FAILURE_DISPLAY_ENTITY_BODY;
      InternetSetOption(fRequest, INTERNET_OPTION_SECURITY_FLAGS, Pointer(@SecurityFlags), SizeOf(SecurityFlags));
    end;
    // send request to remote server
    InternalSendRequest(aData);
    // retrieve status and headers (HTTP_QUERY* and WINHTTP_QUERY* do match)
    result := InternalGetInfo32(HTTP_QUERY_STATUS_CODE);
    OutHeader := InternalGetInfo(HTTP_QUERY_RAW_HEADERS_CRLF);
    aDataEncoding := InternalGetInfo(HTTP_QUERY_CONTENT_ENCODING);
    aAcceptEncoding := InternalGetInfo(HTTP_QUERY_ACCEPT_ENCODING);
    // retrieve received content (if any)
    Read := 0;
    ContentLength := InternalGetInfo32(HTTP_QUERY_CONTENT_LENGTH);
    if ContentLength<>0 then begin
      SetLength(OutData,ContentLength);
      repeat
        Bytes := InternalReadData(OutData,Read);
        if Bytes=0 then begin
          SetLength(OutData,Read); // truncated content
          break;
        end else
          inc(Read,Bytes);
      until Read=ContentLength;
    end else begin
      // Content-Length not set: read response in blocks of HTTP_RESP_BLOCK_SIZE
      repeat
        SetLength(OutData,Read+HTTP_RESP_BLOCK_SIZE);
        Bytes := InternalReadData(OutData,Read);
        if Bytes=0 then
          break;
        inc(Read,Bytes);
      until false;
      SetLength(OutData,Read);
    end;
    // handle incoming answer compression
  finally
    InternalCloseRequest;
  end;
end;

{ EWinInetException }

const
  ENGLISH_LANGID = $0409;

function SysErrorMessagePerModule(Code: DWORD; ModuleName: PChar): string;
var tmpLen: DWORD;
    err: PChar;
begin
  if Code=NO_ERROR then begin
    result := '';
    exit;
  end;
  tmpLen := FormatMessage(
    FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_ALLOCATE_BUFFER,
    pointer(GetModuleHandle(ModuleName)),Code,ENGLISH_LANGID,@err,0,nil);
  try
    while (tmpLen>0) and (ord(err[tmpLen-1]) in [0..32,ord('.')]) do
      dec(tmpLen);
    SetString(result,err,tmpLen);
  finally
    LocalFree(HLOCAL(err));
  end;
  if result='' then
    result := SysErrorMessage(Code);
end;

procedure RaiseLastModuleError(ModuleName: PChar; ModuleException: ExceptClass);
var LastError: Integer;
    Error: Exception;
begin
  LastError := GetLastError;
  if LastError<>NO_ERROR then
    Error := ModuleException.CreateFmt('%s error %d (%s)',
      [ModuleName,LastError,SysErrorMessagePerModule(LastError,ModuleName)]) else
    Error := ModuleException.CreateFmt('Undefined %s error',[ModuleName]);
  raise Error;
end;


constructor EWinInetException.Create;
var dwError, tmpLen: DWORD;
    msg, tmp: string;
begin // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383884
  FErrorCode := GetLastError;
  FErrorMessage := SysErrorMessagePerModule(ErrorCode,'wininet.dll');
  if FErrorCode=ERROR_INTERNET_EXTENDED_ERROR then begin
    InternetGetLastResponseInfo(dwError,nil,tmpLen);
    if tmpLen > 0 then begin
      SetLength(tmp,tmpLen);
      InternetGetLastResponseInfo(dwError,PChar(tmp),tmpLen);
      FErrorMessage := FErrorMessage+' ['+tmp+']';
    end;
  end;
  inherited CreateFmt('%s (%d)',[FErrorMessage,FErrorCode]);
end;

end.


