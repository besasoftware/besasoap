(******************************************************)
(*                                                    *)
(*            Besa Software Classes Library           *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// UserName Authentication Library 
/// </summary>
unit bsUserNameAuthentication;

{$I Besa.inc}
interface
uses
  Classes, bsClasses,bsAuthentication,XMLIntf;

type
  TbsAuthPasswordType=(ptText,ptDigest);

  TOnGenerateUsernameToken = procedure(var Username,Password,UsernameTokenIDAttr,
    Nonce,Created: string) of object;

  TOnAfterGenerateUsernameToken = procedure(var SecurityNode: IXMLNode) of object;

  ///  <summary>
  ///   TbsAuthUsernameToken implements UsernameToken authenticaton. Adds:
  ///   Authorization information in http headers, Security node to Soap XML's header.
  ///  </summary>
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 {$IFDEF DELPHIXE5_UP}or pidiOSSimulator or pidiOSDevice or pidAndroid{$ENDIF})]
  TbsAuthUsernameToken=class(TbsCustomAuthenticate)
  private
    FPasswordType : TbsAuthPasswordType;
    FOnGenerateUsernameToken : TOnGenerateUsernameToken;
    FOnAfterGenerateUsernameToken: TOnAfterGenerateUsernameToken;
    FNonce : string;
    FCreatedDateTime:TDateTime;
    FCreated:string;
    FPasswordDigest:string;
    FmustUnderstand:String;
    FShowNonceNode :Boolean;
    FShowCreatedNode:Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GenerateNode( aHeaderNode : IXMLNode); override;
    procedure SetHeaders(Headers : TbsHeaderList);override;
    property Nonce : string read FNonce write FNonce;
    property Created : string read FCreated write FCreated;
    property mustUnderstand : string read FmustUnderstand write FmustUnderstand;
    property PasswordDigest: string read FPasswordDigest write FPasswordDigest;
  published
    ///<summary>
    /// When set True, Generated "Nonce" Node.
    /// </summary>
    property GenerateNonceNode :Boolean read FShowNonceNode write FShowNonceNode;
    ///<summary>
    /// When set True, Generated "Created" Node.
    /// </summary>
    property GenerateCreatedNode:Boolean read FShowCreatedNode write FShowCreatedNode;
    property PasswordType : TbsAuthPasswordType read FPasswordType write FPasswordType;
    property OnGenerateUsernameToken: TOnGenerateUsernameToken read FOnGenerateUsernameToken write FOnGenerateUsernameToken;
    property OnAfterGenerateUsernameToken: TOnAfterGenerateUsernameToken read FOnAfterGenerateUsernameToken write FOnAfterGenerateUsernameToken;
  end;

implementation
uses bsUtil,SysUtils;

{
 <wsse:Security xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd" xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
   <wsse:UsernameToken wsu:Id="SecurityToken-04ce24bd-9c7c-4ca9-9764-92c53b0662c5">
     <wsse:Username>xxxx</wsse:Username>
     <wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText">xxxx</wsse:Password>
   </wsse:UsernameToken>
 </wssEleme:Security>
}

{ TbsAuthUsernameToken }
constructor TbsAuthUsernameToken.Create(AOwner: TComponent);
begin
  inherited;
  FPasswordType:=ptText;
  FShowNonceNode :=True;
  FShowCreatedNode:=True;

end;

function CreateNonce1: string;
begin
  Result:=FormatDateTime('yyyymmddhhnnsszzz',Now);
end;

procedure TbsAuthUsernameToken.GenerateNode(aHeaderNode: IXMLNode);
const
  soapenv='http://schemas.xmlsoap.org/soap/envelope/';
  wsse='http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd';
  wsu ='http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd';
  PassTextType='http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText';
  PassDigestType='http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordDigest';
  EncodingType='http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary';
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

  Nonce:=CreateNonce1;
  FCreatedDateTime:=Now;
  Created:= DelphiDateTimeToISO8601Date(FCreatedDateTime);
  Nonce:=Base64Encode(SHA1(Created+Nonce));

  if FPasswordType=TbsAuthPasswordType.ptDigest then begin
    // Password_Digest = Base64 ( SHA-1 ( nonce + created + password ) )
    PasswordDigest:=Base64Encode(SHA1(Nonce+Created+Password));
    APassword:=PasswordDigest;
  end;

  if Assigned(FOnGenerateUsernameToken) then
    FOnGenerateUsernameToken(AUsername,APassword,AUsernameTokenIDAttr,
      FNonce,FCreated);

  if FPasswordType=TbsAuthPasswordType.ptText then
    Password:=APassword
  else
    PasswordDigest:=APassword;


  aUsernameTokenNode.SetAttributeNS('Id',wsu,AUsernameTokenIDAttr);

  aUsernameNode:=aUsernameTokenNode.AddChild('Username');
  aUsernameNode.NodeValue:=AUsername;

  aPassNode:=aUsernameTokenNode.AddChild('Password');

  if FPasswordType=TbsAuthPasswordType.ptText then
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

  if FShowNonceNode then
  begin
    aNonceNode:=aUsernameTokenNode.AddChild('Nonce');
    aNonceNode.NodeValue:=Nonce;
    aNonceNode.Attributes['EncodingType']:=EncodingType;
  end;

  if FShowCreatedNode then
  begin
    aCreatedNode :=aUsernameTokenNode.AddChild('Created');
    aCreatedNode.NodeValue:=Created;
  end;

  if Assigned(FOnAfterGenerateUsernameToken) then
    FOnAfterGenerateUsernameToken(aNode);
{
         <wsu:Timestamp wsu:Id="TS-0839C3AAE9BE35562314056718819263">

            <wsu:Created>2014-07-18T08:24:41.926Z</wsu:Created>
            <wsu:Expires>2014-07-18T08:25:41.926Z</wsu:Expires>
         </wsu:Timestamp>}

end;

procedure TbsAuthUsernameToken.SetHeaders(Headers : TbsHeaderList);
begin
  Headers.Header['Authorization']:='Basic '+Base64Encode(Username+':'+Password);
  if Assigned(OnGetHeader) then
    OnGetHeader(Headers);
end;

end.
