(******************************************************)
(*                                                    *)
(*            Besa Software Classes Library           *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// Indy Transporter Library 
/// </summary>
unit bsIndyTransporter;
{$I Besa.inc}

interface
uses
{$IF Defined(IOS) and Defined(CPUARM)}
  IdSSLOpenSSLHeaders_Static,
{$ELSE}
  IdSSLOpenSSLHeaders,
{$IFEND}
  bsTransporter, bsClasses,IdHTTP, System.Classes,
  IdCompressorZLib, System.SysUtils,IdSSLOpenSSL, IdStack;

type

  /// <summary>
  ///   <b>TbsIndyTransporter</b> uses the open source Indy components HTTP
  ///   client to make HTTP Requests.
  /// </summary>
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 {$IFDEF DELPHIXE5_UP}or pidiOSSimulator or pidiOSDevice or pidAndroid{$ENDIF})]
  TbsIndyTransporter=class(TbsHttpTransporter)
  private
    FIdHttp: TIdHTTP;
    FSSLOptions: TIdSSLOptions;
    FEnabledCompression: Boolean;
    procedure Init;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Get(AURL: string; AResponseContent: TStream);
    procedure Post(AURL: string; ASource, AResponseContent: TStream);
    function Execute(Request : TStream):TStream; override;
   published
    property SSLOptions : TIdSSLOptions read FSSLOptions write FSSLOptions;
  end;

  EIndyException=class(ETransporterException)
  public
    constructor Create; override;
  end;

implementation


{ TbsIndyTransporter }


procedure TbsIndyTransporter.Init;
var
  URI: TbsURI;
  FIdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
  //FIdSocksInfo: TIdSocksInfo;
  I: Integer;
  LHeader:string;
begin
  if not URI.From(TargetURL) then Exit;
  FIdHttp:= TIdHTTP.Create(nil);
  //FIdHttp.ReadTimeout:=ReceiveTimeout;

  FIdHttp.HandleRedirects := True;
  FIdHttp.AllowCookies:= True;
  //FIdHttp.Request.CustomHeaders.FoldLines := false;

  if URI.Https then
  begin
    FIdSSLIOHandlerSocketOpenSSL:=TIdSSLIOHandlerSocketOpenSSL.Create(FIdHttp);
    FIdHttp.IOHandler :=FIdSSLIOHandlerSocketOpenSSL;
    FIdSSLIOHandlerSocketOpenSSL.SSLOptions.Assign(FSSLOptions);
  end;
  if EnableCompression then
  begin
    FIdHttp.Compressor:=TIdCompressorZLib.Create(FIdHttp);
  end;


  FIdHttp.Request.Accept := '*/*';

  if Headers.HeaderIsExist('User-Agent') then begin
    FIdHttp.Request.UserAgent := Headers.Header['User-Agent'];
    Headers.Delete(Headers.GetHeaderIndex('User-Agent'));
  end;

  if Headers.HeaderIsExist('Content-Type') then begin
    FIdHttp.Request.ContentType := Headers.Header['Content-Type'];
    Headers.Delete(Headers.GetHeaderIndex('Content-Type'));
  end;

  if Headers.HeaderIsExist('Keep-Alive') then begin
    FIdHttp.Request.Connection := Headers.Header['Keep-Alive'];
    Headers.Delete(Headers.GetHeaderIndex('Keep-Alive'));
  end;

  FIdHttp.Request.CustomHeaders.Clear;

  for I := 0 to Headers.Count-1 do
  begin
    LHeader:=Headers[I];
    FIdHttp.Request.CustomHeaders.AddValue(Headers.GetHeaderName(LHeader),Headers.Header[ Headers.GetHeaderName(LHeader) ]);
  end;

  { Proxy support configuration }
  if Length(ProxyServer)>0 then
    FIdHttp.ProxyParams.ProxyServer := ProxyServer;

  if Length(ProxyPort)>0 then
    FIdHttp.ProxyParams.ProxyPort := StrToInt(ProxyPort);

  if Length(ProxyUsername)>0 then
    FIdHttp.ProxyParams.ProxyUsername := ProxyUsername;

  if Length(ProxyPassword)>0 then
    FIdHttp.ProxyParams.ProxyPassword := ProxyPassword;
end;

procedure TbsIndyTransporter.Post(AURL: string; ASource,
  AResponseContent: TStream);
begin

  try
    TargetURL:=AURL;
    Init;
    FIdHttp.Post(AURL, ASource, AResponseContent);
  finally
    FIdHttp.Free;
  end;
end;

constructor TbsIndyTransporter.Create(AOwner: TComponent);
begin
  inherited;
  FSSLOptions:=TIdSSLOptions.Create;
  FSSLOptions.Method:=sslvSSLv23;
end;

destructor TbsIndyTransporter.Destroy;
begin
  FSSLOptions.Free;
  inherited;
end;

function TbsIndyTransporter.Execute(Request:TStream): TStream;
var
  URI: TbsURI;
begin
  if not URI.From(TargetURL) then Exit;
 //IndyProxyAuthorization
 // ContentType

  try
    Result:=TMemoryStream.Create;
    Post(TargetURL, Request,Result);
  except
    {
    on E: EIdHTTPProtocolException do
    begin
      if E.ErrorCode = 404 then
        exit;
      retryMode := hrmRaise;
      if assigned(OnError) then
        OnError(e.Message, e.ErrorMessage, e.ErrorCode, retryMode);
      if retryMode = hrmRaise then
        raise EHTTPError.Create(e.Message, e.ErrorMessage, e.ErrorCode)
      else if retryMode = hrmRetry then
        Post(AUrl, AContent, AResponse);
    end;
    on E: EIdSocketError do
    begin
      FIdHttp.Disconnect(false);
      retryMode := hrmRaise;
      if assigned(OnConnectionLost) then
        OnConnectionLost(e, retryMode);
      if retryMode = hrmRaise then
        raise
      else if retryMode = hrmRetry then
        Post(AUrl, AContent, AResponse);
    end;
    }
  //end;

        on E : Exception do
          raise Exception.Create(E.Message);

        end;
end;

procedure TbsIndyTransporter.Get(AURL: string; AResponseContent: TStream);
begin
  try
    TargetURL:=AURL;
    Init;
    FIdHttp.Get(AURL, AResponseContent);
  finally
    FIdHttp.Free;
  end;
end;

{ TWinINet }

constructor EIndyException.Create;
var dwError, tmpLen: Cardinal;
    msg, tmp: string;
begin // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383884

  inherited CreateFmt('%s (%d)',[FErrorMessage,FErrorCode]);
end;

end.


