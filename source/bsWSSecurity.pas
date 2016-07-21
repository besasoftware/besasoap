(******************************************************)
(*                                                    *)
(*            Besa Software Classes Library           *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// WS-Security Library
/// </summary>
unit bsWSSecurity;

interface
uses Classes, SysUtils, XMLIntf;

type
  TbsWSSEPasswordType=(ptText, ptDigest);

  TbsWSSEBase=class
    procedure GenerateNode( aHeaderNode : IXMLNode);virtual;abstract;
  end;

  TbsWSSEUsername = class(TbsWSSEBase)
  private
    FUserName:String;
    FPassword : String;
    FAddNonce : Boolean;
    FAddCreated :Boolean;
    FPasswordType : TbsWSSEPasswordType;
    FNonce :String;
    FCreatedDateTime:TDateTime;
    FCreated:String;
    FPasswordDigest:String;
  public
    procedure GenerateNode( aHeaderNode : IXMLNode); override;
    property UserName:String read FUserName write FUserName;
    property Password:String read FPassword write FPassword;
    property AddNonce : Boolean read FAddNonce write FAddNonce;
    property AddCreated :Boolean read FAddCreated write FAddCreated;
    property PasswordType : TbsWSSEPasswordType read FPasswordType write FPasswordType;
  end;

  TbsWSSEConfig=class(TList)
  private
    FMustUndertand:Boolean;
    FActor :String;
  public
    destructor Destroy;override;
    procedure GenerateNode( aHeaderNode : IXMLNode);
    function AddUsername(AUserName,APassword:String; AnAddNonce:Boolean=True;
       AnAddCreated: Boolean=True; APasswordType: TbsWSSEPasswordType= ptText)
       : TbsWSSEUsername;
  end;

implementation
uses bsUtil,bsClasses;

{ TbsWSSEConfiguration }

function TbsWSSEConfig.AddUsername(AUserName, APassword: String;
  AnAddNonce, AnAddCreated: Boolean; APasswordType: TbsWSSEPasswordType): TbsWSSEUsername;
begin
  Result:=TbsWSSEUsername.Create;
  Result.UserName:=AUserName;
  Result.Password:=APassword;
  Result.PasswordType:=APasswordType;
  Result.AddCreated:=AnAddCreated;
  Result.AddNonce:=AnAddNonce;
  Add(Result);
end;

destructor TbsWSSEConfig.Destroy;
var
  I:Integer;
begin
  for I := 0 to Count-1 do
  begin
    TbsWSSEBase(Items[I]).Free;
  end;

  inherited;
end;

procedure TbsWSSEConfig.GenerateNode(aHeaderNode: IXMLNode);
var
  I:Integer;
begin
  for I := 0 to Count-1 do
  begin
    TbsWSSEBase(Items[I]).GenerateNode(aHeaderNode);
  end;
end;

function CreateNonce1: string;
begin
  Result:='';//FormatDateTime('yyyymmddhhnnsszzz',Now);
end;

{ TbsWSSEUsername }

procedure TbsWSSEUsername.GenerateNode(aHeaderNode: IXMLNode);
const
  soapenv        = 'http://schemas.xmlsoap.org/soap/envelope/';
  wsse           = 'http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd';
  wsu            = 'http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd';
  PassTextType   = 'http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText';
  PassDigestType = 'http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordDigest';
  EncodingType   = 'http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary';

var
  aNode,aUsernameTokenNode,aUsernameNode,
  aPassNode,aNonceNode,aCreatedNode : IXMLNode;
  Guid: TGuid;
  aGuid : String;
  AUsername,APassword,
  AUsernameTokenIDAttr:string;

begin
  inherited;

  AUsername:=Username;
  APassword:=Password;

  aHeaderNode.DeclareNamespace('wsse',wsse);
  aNode:=aHeaderNode.AddChild('Security',wsse);

  aNode.SetAttributeNS('mustUnderstand',soapenv,'1');

  aNode.DeclareNamespace('wsu',wsu);

  aUsernameTokenNode:=aNode.AddChild('UsernameToken');
  CreateGUID(Guid);
  aGuid:=GUIDToString(Guid);
  AUsernameTokenIDAttr:='SecurityToken-'+aGuid;

  FNonce:=CreateNonce1;
  FCreatedDateTime:=Now;
  FCreated:= DelphiDateTimeToISO8601Date(FCreatedDateTime);
  FNonce:=Base64Encode(SHA1(FCreated+FNonce));

  if FPasswordType = ptDigest then begin
    // Password_Digest = Base64 ( SHA-1 ( nonce + created + password ) )
    FPasswordDigest:=Base64Encode(SHA1(FNonce+FCreated+FPassword));
    APassword:=FPasswordDigest;
  end;

  if FPasswordType=ptText then
    Password:=APassword
  else
    FPasswordDigest:=APassword;


  aUsernameTokenNode.SetAttributeNS('Id',wsu,AUsernameTokenIDAttr);

  aUsernameNode:=aUsernameTokenNode.AddChild('Username');
  aUsernameNode.NodeValue:=AUsername;

  aPassNode:=aUsernameTokenNode.AddChild('Password');

  if FPasswordType=ptText then
  begin
    aPassNode.Attributes['Type']:=PassTextType;
    aPassNode.NodeValue:=APassword;
  end else
  begin
    {
     <wsse:UsernameToken wsu:Id="uuid_faf0159a-6b13-4139-a6da-cb7b4100c10c">
        <wsse:Username>Alice</wsse:Username>
        <wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordDigest">6S3P2EWNP3lQf+9VC3emNoT57oQ=</wsse:Password>
        <wsse:Nonce EncodingType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary">YF6j8V/CAqi+1nRsGLRbuZhi</wsse:Nonce>
        <wsu:Created>2008-04-28T10:02:11Z</wsu:Created>
     </wsse:UsernameToken>
    }
    aPassNode.Attributes['Type']:=PassDigestType;
    aPassNode.NodeValue:=APassword;
  end;

  if FAddNonce then
  begin
    aNonceNode:=aUsernameTokenNode.AddChild('Nonce');
    aNonceNode.NodeValue:=FNonce;
    aNonceNode.Attributes['EncodingType']:=EncodingType;
  end;

  if FAddCreated then
  begin
    aCreatedNode :=aUsernameTokenNode.AddChild('Created');
    aCreatedNode.NodeValue:=FCreated;
  end;

//  if Assigned(FOnAfterGenerateUsernameToken) then
//    FOnAfterGenerateUsernameToken(aNode);
{
         <wsu:Timestamp wsu:Id="TS-0839C3AAE9BE35562314056718819263">

            <wsu:Created>2014-07-18T08:24:41.926Z</wsu:Created>
            <wsu:Expires>2014-07-18T08:25:41.926Z</wsu:Expires>
         </wsu:Timestamp>
}

end;

end.
