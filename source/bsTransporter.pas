(******************************************************)
(*                                                    *)
(*            Besa Software Classes Library           *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// Transporter Library 
/// </summary>
unit bsTransporter;

interface

uses Classes,SysUtils,bsClasses;


const
  HTTP_DEFAULT_SENDTIMEOUT=60*1000; //Default
  HTTP_DEFAULT_RECEIVETIMEOUT=60*1000; // Default

type
  ETransporterException = class(Exception)
    protected
      FErrorCode: integer;
      FErrorMessage: string;
    public
      constructor Create; overload; virtual;
      constructor Create(const AErrorMessage: string; const AErrorCode: integer); overload;
      property ErrorMessage: string read FErrorMessage;
      property ErrorCode: integer read FErrorCode;
  end;


  IBSTransporter= interface
  ['{62F3E580-B583-4D89-94A0-4FDC164C7CA4}']
    function Execute(Request : TStream):TStream;
    function GetObject:TObject;
  end;

  IBSHTTPTransporter= interface(IBSTransporter)
  ['{62F3E580-B583-4D89-94A0-4FDC164C7CA4}']

    procedure SetHeaders(Headers : TbsHeaderList);
    function GetHeaders:TbsHeaderList;
    function GetTargetURL : string;
    procedure SetTargetURL(const Value : string);
    function GetSendTimeout: Cardinal;
    procedure SetSendTimeout(const Value : Cardinal);
    function GetReceiveTimeout : Cardinal;
    procedure SetReceiveTimeout(const Value : Cardinal);
    function GetUserAgent:String;
    procedure SetUserAgent(const Value:String);
    function GetIgnoreInvalidCerts:Boolean;
    procedure SetIgnoreInvalidCerts(const Value:Boolean);

    function GetUsername: string;
    procedure SetUsername(const Value: string);
    function GetPassword:string;
    procedure SetPassword(const Value: string);

    function GetProxyServer : string;
    procedure SetProxyServer(const Value:string);
    function GetProxyPort:string;
    procedure SetProxyPort(const Value: string);
    function GetProxyUsername: string;
    procedure SetProxyUsername(const Value: string);
    function GetProxyPassword:string;
    procedure SetProxyPassword(const Value: string);

    property TargetURL : string read GetTargetURL write SetTargetURL;
    property Username :string read GetUsername write SetUsername; //username if requires authentication
    property Password :string read GetPassword write SetPassword; //password if requires authentication
    property ProxyServer : string read GetProxyServer write SetProxyServer; //the DNS name of the proxy server or its IP address
    property ProxyPort : string read GetProxyPort write SetProxyPort; //port the HTTP the HTTP proxy listens on
    property ProxyUsername :string read GetProxyUsername write SetProxyUsername; //username if the proxy requires authentication
    property ProxyPassword :string read GetProxyPassword write SetProxyPassword; //password if the proxy requires authentication

    property SendTimeout : Cardinal read GetSendTimeout write SetSendTimeout;
    property ReceiveTimeout : Cardinal read GetReceiveTimeout write SetReceiveTimeout;
    property IgnoreInvalidCerts : Boolean read GetIgnoreInvalidCerts write SetIgnoreInvalidCerts;
    property UserAgent : String read GetUserAgent write SetUserAgent;
  end;

  TbsTransporter=class(TComponent, IBSTransporter)
    procedure BeforeExecute(SOAPAction, Request: TStream); virtual;abstract;
    procedure AfterExecute(SOAPAction, Response: TStream); virtual;abstract;
    function Execute(Request: TStream):TStream;virtual;abstract;
    function GetObject:TObject;
  end;

 TbsHttpTransporter=class(TbsTransporter ,IBSHTTPTransporter)
  private
    FHeaders : TbsHeaderList;
    FTargetURL : String;
    FSendTimeout : Cardinal;
    FReceiveTimeout: Cardinal;
    FUserAgent : String;
    FIgnoreInvalidCerts:Boolean;
    FUsername: string;
    FPassword:string;
    FProxyServer : string;
    FProxyPort:string;
    FProxyUsername: string;
    FProxyPassword:string;
    FEnableCompression: Boolean;
    //FOptions : TbsTranporterOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(Request : TStream):TStream; override;
    procedure SetHeaders(aHeaders : TbsHeaderList);virtual;
    function GetHeaders:TbsHeaderList;virtual;
    function GetTargetURL : string; virtual;
    procedure SetTargetURL(const Value : string);virtual;
    function GetSendTimeout: Cardinal; virtual;
    procedure SetSendTimeout(const Value : Cardinal); virtual;
    function GetReceiveTimeout : Cardinal; virtual;
    procedure SetReceiveTimeout(const Value : Cardinal); virtual;
    function GetUserAgent:String; virtual;
    procedure SetUserAgent(const Value:String); virtual;
    function GetIgnoreInvalidCerts:Boolean; virtual;
    procedure SetIgnoreInvalidCerts(const Value:Boolean); virtual;
    function GetUsername: string; virtual;
    procedure SetUsername(const Value: string); virtual;
    function GetPassword:string; virtual;
    procedure SetPassword(const Value: string);virtual;
    function GetProxyServer : string;virtual;
    procedure SetProxyServer(const Value:string);virtual;
    function GetProxyPort:string;virtual;
    procedure SetProxyPort(const Value: string);virtual;
    function GetProxyUsername: string;virtual;
    procedure SetProxyUsername(const Value: string);virtual;
    function GetProxyPassword:string;virtual;
    procedure SetProxyPassword(const Value: string);virtual;
    function GetEnableCompression: Boolean;
    procedure SetEnableCompression(const Value: Boolean);

  published
    property TargetURL : string read GetTargetURL write SetTargetURL;
    property Username :string read GetUsername write SetUsername; //username if the proxy requires authentication
    property Password :string read GetPassword write SetPassword; //password if the proxy requires authentication
    property ProxyServer : string read GetProxyServer write SetProxyServer; //the DNS name of the proxy server or its IP address
    property ProxyPort : string read GetProxyPort write SetProxyPort; //port the HTTP the HTTP proxy listens on
    property ProxyUsername :string read GetProxyUsername write SetProxyUsername; //username if the proxy requires authentication
    property ProxyPassword :string read GetProxyPassword write SetProxyPassword; //password if the proxy requires authentication
    property SendTimeout : Cardinal read GetSendTimeout write SetSendTimeout;
    property ReceiveTimeout : Cardinal read GetReceiveTimeout write SetReceiveTimeout;
    property Headers :TbsHeaderList read GetHeaders write SetHeaders;
    property IgnoreInvalidCerts:Boolean read GetIgnoreInvalidCerts write SetIgnoreInvalidCerts;
    property EnableCompression: Boolean read GetEnableCompression write SetEnableCompression;
  end;

implementation

{ TbsTransporter }

function TbsTransporter.GetObject: TObject;
begin
  Result:=Self;
end;


{ ETranporterException }

constructor ETransporterException.Create;
begin
  FErrorCode := GetLastError;
  inherited CreateFmt('%s (%d)', [SysErrorMessage(FErrorCode), FErrorCode]);
end;

constructor ETransporterException.Create(const AErrorMessage: string;
  const AErrorCode: integer);
begin
  inherited Create(AErrorMessage);
  FErrorMessage := AErrorMessage;
  FErrorCode := AErrorCode;
end;


{ TbsHttpTransporter }

constructor TbsHttpTransporter.Create(AOwner: TComponent);
begin
  inherited;
  FSendTimeout:=HTTP_DEFAULT_SENDTIMEOUT;
  FReceiveTimeout:=HTTP_DEFAULT_RECEIVETIMEOUT;
  FIgnoreInvalidCerts:=False;
  FEnableCompression:=False;
  FHeaders:=TbsHeaderList.Create;
end;

destructor TbsHttpTransporter.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

function TbsHttpTransporter.Execute(Request: TStream): TStream;
begin
end;

function TbsHttpTransporter.GetEnableCompression: Boolean;
begin
  Result:=FEnableCompression;
end;

function TbsHttpTransporter.GetHeaders: TbsHeaderList;
begin
  Result:=FHeaders;
end;

function TbsHttpTransporter.GetIgnoreInvalidCerts: Boolean;
begin
  Result:=FIgnoreInvalidCerts;
end;

function TbsHttpTransporter.GetPassword: string;
begin
  Result:=FPassword;
end;

function TbsHttpTransporter.GetProxyPassword: string;
begin
  Result:=FProxyPassword;
end;

function TbsHttpTransporter.GetProxyPort: string;
begin
  Result:=FProxyPort;
end;

function TbsHttpTransporter.GetProxyServer: string;
begin
  Result:=FProxyServer;
end;

function TbsHttpTransporter.GetProxyUsername: string;
begin
  Result:=FProxyUsername;
end;

function TbsHttpTransporter.GetReceiveTimeout: Cardinal;
begin
  Result:=FReceiveTimeout;
end;

function TbsHttpTransporter.GetSendTimeout: Cardinal;
begin
  Result:=FSendTimeout;
end;

function TbsHttpTransporter.GetTargetURL: string;
begin
  Result:=FTargetURL;
end;

function TbsHttpTransporter.GetUserAgent: String;
begin
  Result:=FUserAgent;
end;

function TbsHttpTransporter.GetUsername: string;
begin
  Result:=FUsername;
end;

procedure TbsHttpTransporter.SetEnableCompression(const Value: Boolean);
begin
  FEnableCompression:=Value;
end;

procedure TbsHttpTransporter.SetHeaders(aHeaders: TbsHeaderList);
begin
  FHeaders.Clear;
  FHeaders.AddStrings(aHeaders);
end;

procedure TbsHttpTransporter.SetIgnoreInvalidCerts(const Value: Boolean);
begin
  FIgnoreInvalidCerts:=Value;
end;

procedure TbsHttpTransporter.SetPassword(const Value: string);
begin
  FPassword:=Value;
end;

procedure TbsHttpTransporter.SetProxyPassword(const Value: string);
begin
  FProxyPassword:=Value;
end;

procedure TbsHttpTransporter.SetProxyPort(const Value: string);
begin
  FProxyPort:=Value;
end;

procedure TbsHttpTransporter.SetProxyServer(const Value: string);
begin
  FProxyServer:=Value;
end;

procedure TbsHttpTransporter.SetProxyUsername(const Value: string);
begin
  FProxyUsername:=Value;
end;

procedure TbsHttpTransporter.SetReceiveTimeout(const Value: Cardinal);
begin
  FReceiveTimeout:=Value;
end;

procedure TbsHttpTransporter.SetSendTimeout(const Value: Cardinal);
begin
  FSendTimeout:=Value;
end;

procedure TbsHttpTransporter.SetTargetURL(const Value: string);
begin
  FTargetURL:=Value;
end;

procedure TbsHttpTransporter.SetUserAgent(const Value: String);
begin
  FUserAgent:=Value;
end;

procedure TbsHttpTransporter.SetUsername(const Value: string);
begin
  FUsername:=Value;
end;

end.
