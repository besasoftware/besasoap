unit bsSOAPClient;

{$I Besa.inc}

interface

uses
  Classes,SysUtils,TypInfo, Rtti,Generics.Collections,System.Generics.Collections,
  bsSerializer, bsConst, bsTransporter,XMLIntf,XMLDoc,bsAttribute,
  bsClasses, bsAuthentication;

type

  TbsOption = (
    ///	<summary>
    ///	  When the server uses SSL but has an invalid certificate, treat it as if
    ///	  the certificate had been valid.
    ///	</summary>
    oIgnoreInvalidCerts,

    ///	<summary>
    ///	  Specified, utf-8 encoding in header.
    ///	</summary>
    oUseUTF8InHeader,

    ///	<summary>
    ///	  Generates SOAP 1.1 compitable XML
    ///	</summary>
    oUseSOAP11,

    ///	<summary>
    ///	  Generates SOAP 1.2 compitable XML
    ///	</summary>
    oUseSOAP12
  );

  TbsOptions= set of TbsOption;


  TBeforeExecuteEvent = procedure(const MethodName: string; SOAPRequest: TStream) of object;
  TAfterExecuteEvent  = procedure(const MethodName: string; SOAPResponse: TStream) of object;


  TbsService=class;

  /// <exclude />
  TbsRIO=class(TVirtualInterface)
  private
     FService : TbsService;
     function GetHeader : TbsHeaderList;
     procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>;
      out Result: TValue);
  public
    constructor Create(PIID: PTypeInfo);
    destructor Destroy; override;
  end;


  ///	<summary>
  ///	  <para>
  ///	    <b>TbsService</b> uses HTTP messages to call remote interfaced
  ///	    objects using SOAP.
  ///	  </para>
  ///	  <para>
  ///	    Use <b>TbsService</b> to generate statically-linked calls to
  ///	    invokable interfaces on a remote Web Service application. When an
  ///	    application casts <b>TbsService</b> to a registered invokable
  ///	    interface, it dynamically generates an in-memory method table,
  ///	    providing an implementation to that invokable interface.
  ///	  </para>
  ///	  <para>
  ///	    <b>TbsService</b> executes the methods in this method table by
  ///	    encoding the method call as a SOAP request and sending an HTTP
  ///	    request message to the Web Service application. It unpacks the
  ///	    resulting HTTP response message to obtain the return value and any
  ///	    output parameters, or to raise an exception if the request generated
  ///	    an exception on the server.
  ///	  </para>
  ///	  <para>
  ///	    Use the published properties of <b>TbsService</b> to indicate how to
  ///	    connect to the Web Service application.
  ///	  </para>
  ///	</summary>
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 {$IFDEF DELPHIXE5_UP}or pidiOSSimulator or pidiOSDevice or pidAndroid{$ENDIF})]
  TbsService=class(TComponent)
  private
    FURL: string;
    FUsername: string;
    FPassword: string;
    FAgent: string;
    FContentType: string;
    FURLHost: string;
    FURLSite: string;
    FURLPort: Integer;
    FURLScheme: Integer;
    FProxyServer : string;
    FProxyPort :string;
    FProxyUsername :string;
    FProxyPassword :string;
    FNamespaceURI : string;
    FRIO: TbsRIO;
    FSOAPAction : TStringList;
    FTransporter :TbsTransporter;
    FHeaderList : TbsHeaderList;
    FAuthentication : TbsCustomAuthenticate;
    FServiceTypeInfo: PTypeInfo;
    FOptions: TbsOptions;
    FOperation: string;
    FElementForm:TSchemaForm;
    FOnBeforeExecuteEvent :TBeforeExecuteEvent;
    FOnAfterExecuteEvent: TAfterExecuteEvent;
    procedure SetURL(const Value: string);
    function GetTransporter: TbsTransporter;
    procedure SetTransporter(const Value: TbsTransporter);
    function GetAuthentication: TbsCustomAuthenticate;
    procedure SetAuthentication(const Value: TbsCustomAuthenticate);
    procedure SetServiceTypeInfo(const Value: PTypeInfo);
    procedure ReadFault(AFaultNode : IXMLNode);
    function GetSoapAction(OperationName:String=''):String;
    function GetContentType:string;
    procedure SetOptions(const Value: TbsOptions);
  protected
    property RIO : TbsRIO read FRIO write FRIO;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    procedure RegisterInterface(AInterface: PTypeInfo; ANamespace: string='');
    procedure RegisterSoapAction(AOperation, ASoapAction:String);
    property ServiceTypeInfo : PTypeInfo read FServiceTypeInfo write SetServiceTypeInfo;
    property Headers : TbsHeaderList read FHeaderList write FHeaderList;
    property Operation: string read FOperation write FOperation;
    property ContentType: string read GetContentType;// write FContentType;
    property SoapAction: TStringList read FSOAPAction write FSOAPAction;
    property ElementForm:TSchemaForm read FElementForm write FElementForm;
 published
    property NamespaceURI : string read FNamespaceURI write FNamespaceURI;
    property URL: string read FURL write SetURL;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Agent: string read FAgent write FAgent;

    property ProxyServer : string read FProxyServer write FProxyServer; //the DNS name of the proxy server or its IP address
    property ProxyPort : string read FProxyPort write FProxyPort; //port the HTTP the HTTP proxy listens on
    property ProxyUsername :string read FProxyUsername write FProxyUsername; //username if the proxy requires authentication
    property ProxyPassword :string read FProxyPassword write FProxyPassword; //password if the proxy requires authentication

    property Authentication : TbsCustomAuthenticate read GetAuthentication write SetAuthentication;
    property Transporter : TbsTransporter read GetTransporter write SetTransporter;
    property Options : TbsOptions read FOptions write SetOptions;
    property OnBeforeExecute :TBeforeExecuteEvent read FOnBeforeExecuteEvent write FOnBeforeExecuteEvent;
    property OnAfterExecute: TAfterExecuteEvent read FOnAfterExecuteEvent write FOnAfterExecuteEvent;

  end;


  EBSRemotableException = class(Exception)
  private
    FFaultActor: String;
    FFaultCode: String;
    FFaultDetail: String;
    FFaultReasonLang: String;
    FFaultNode: String;
    FFaultRole: String;
    procedure SetFaultReason(const Value: String);
    function GetFaultReason: String;
  public
    constructor Create; overload;  virtual;
    constructor Create(const Msg: string;
                       const AFaultCode: String = '';
                       const AFaultActor: String = '';
                       const AFaultReasonLang: String = '';
                       const AFaultNode: String = '';
                       const AFaultRole: String = ''); overload; virtual;
    property FaultActor:      String read FFaultActor write FFaultActor;
    property FaultCode:       String read FFaultCode  write FFaultCode;
    property FaultDetail:     String read FFaultDetail write FFaultDetail;
    property FaultReason:     String read GetFaultReason write SetFaultReason;
    property FaultReasonLang: String read FFaultReasonLang write FFaultReasonLang;
    property FaultNode:       String read FFaultNode write FFaultNode;
    property FaultRole:       String read FFaultRole write FFaultRole;
  end;


implementation

{ TblRIO }
constructor TbsRIO.Create(PIID: PTypeInfo);
begin
  inherited Create(PIID,DoInvoke);
end;

destructor TbsRIO.Destroy;
begin
  inherited;
end;

procedure TbsRIO.DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>;
  out Result: TValue);
var
  LParam : TRttiParameter;
  Arg: TValue;
  ArgType, ArgName: string;
  TempKind: TTypeKind;
  LReqDoc : IXMLDocument;
  LRespDoc : IXMLDocument;

  LEnvelop,LHeader,LBody : IXMLNode;
  LNode,LResponseNode,LFaultNode : IXMLNode;
  c: TRttiContext;
  attr :TArray<TCustomAttribute>;
  aData : String;
  LRequest,LResponse: TStream;
  I:Integer;
  LConverter : TbsXMLSerializer;
  aValue:TValue;
  Transporter : IBSTransporter;
  HTTPTransporter : IBSHTTPTransporter;
  aAttribute : TCustomAttribute;
  HasHolderAttribute:Boolean;
  IsMessageHeader:Boolean;
begin
  if FService.Transporter=nil then
    raise Exception.Create('Please select a Transporter.');

  LReqDoc:=TXMLDocument.Create(NIL);
  LReqDoc.Active:=true;
  LRespDoc := TXMLDocument.Create(NIL);
  LRespDoc.Active:=true;
  LConverter:=TbsXMLSerializer.Create;
  LConverter.ElementForm:=FService.ElementForm;
  LConverter.Standalone:=False;

  LEnvelop:=LReqDoc.AddChild(SSoapNameSpacePre + ':' + SSoapEnvelope, SoapEnvelopeNamespaces[oUseSOAP12 in FService.Options]);
  LEnvelop.DeclareNamespace(SXMLSchemaNameSpacePre, XMLSchemaNameSpace);
  LEnvelop.DeclareNamespace(SXMLSchemaInstNameSpace99Pre, XMLSchemaInstNameSpace);
  //if not (soDocument in Options) then  'SOAP-ENC'
  LEnvelop.DeclareNamespace(SSoapEncodingPre, SoapEncodingNamespaces[oUseSOAP12 in FService.Options]);

  if FService<>nil then
  begin
    LEnvelop.DeclareNamespace('ns', FService.NamespaceURI);
    LConverter.targetNamespace:=FService.NamespaceURI;
  end;

   LHeader :=LEnvelop.AddChild(SSoapHeader);
   if FService.Authentication<>nil then
   begin
    FService.Authentication.Username:=FService.Username;
    FService.Authentication.Password:=FService.Password;

    FService.Authentication.GenerateNode(LHeader);
   end;



   LBody :=LEnvelop.AddChild(SSoapBody);
   FService.Operation:=Method.Name;
   LNode:=LBody.AddChild(FService.Operation,FService.NamespaceURI,true);
   //Result.DeclareNamespace(APrefix, LNamespace);
   I:=0;

   for Arg in Args do
   begin
     TempKind := Arg.Kind;
     if TempKind = tkInterface then
     begin
       c:=TRttiContext.Create;
       attr:=c.GetType(Arg.TypeInfo).GetAttributes;
     end
     else if TempKind=tkClass then
     begin
       LParam:=Method.GetParameters[I];

       IsMessageHeader:=False;
       for aAttribute in LParam.GetAttributes do
         if (aAttribute is MessageHeaderAttribute) then IsMessageHeader:=True;

       if not IsMessageHeader then
         IsMessageHeader:=TbsAttributeUtils.HasAttribute(Arg.TypeInfo,MessageHeaderAttribute);

       if IsMessageHeader
       then
        LConverter.SerializeWithNode(LParam.Name, Arg.AsObject,LHeader)
       else
        LConverter.SerializeWithNode(LParam.Name, Arg.AsObject,LNode);
       Inc(I);
     end else
     begin
        LParam:=Method.GetParameters[I];
        LConverter.SerializeWithNode(LParam.Name, Arg,LNode);
        Inc(I);
     end;
   end;

   try
     try
       LRequest:=TStringStream.Create;
       LReqDoc.Version:='1.0';
       LReqDoc.Encoding:='utf-8';
       LReqDoc.SaveToStream(LRequest);

       if Assigned(FService.OnBeforeExecute) then
       begin
          LRequest.Position:=0;
          FService.OnBeforeExecute(FService.Operation, LRequest);
          LRequest.Position := 0;
       end;

       if Supports( FService.Transporter,IBSHTTPTransporter,HTTPTransporter ) then
       begin
         HTTPTransporter.SetHeaders(GetHeader);
         HTTPTransporter.TargetURL:=FService.FURL;
         HTTPTransporter.UserAgent:=FService.Agent;
         HTTPTransporter.ProxyServer:=FService.ProxyServer;
         HTTPTransporter.ProxyPort:=FService.ProxyPort;
         HTTPTransporter.ProxyUsername:=FService.ProxyUsername;
         HTTPTransporter.ProxyPassword:=FService.ProxyPassword;
         HTTPTransporter.IgnoreInvalidCerts:=(oIgnoreInvalidCerts in FService.Options);

         LResponse:=HTTPTransporter.Execute(LRequest);
       end else
         LResponse:=FService.Transporter.Execute(LRequest);

       if LResponse=nil then raise Exception.Create('Response value is null');

       if Assigned(FService.OnAfterExecute) then
       begin
         LResponse.Position:=0;
         FService.OnAfterExecute(FService.Operation, LResponse);
         LResponse.Position := 0;
       end;

       LRespDoc.LoadFromStream(LResponse);

       LBody:=LRespDoc.DocumentElement.ChildNodes.FindNode(SSoapBody);

        //if aBody=nil then...

        // Check Fault node
       if (LBody.ChildNodes.First.LocalName=SSoapFault) then
       begin
          LFaultNode:=LBody.ChildNodes.First;
          FService.ReadFault(LFaultNode)
       end else
       begin

         {
          WS-I's Basic Profile dictates that in the RPC/literalresponse message,
          the name of the child of soap:body is "... the corresponding
          wsdl:operationname suffixed with the string 'Response'." Surprise!
         }

         LResponseNode:=LBody.ChildNodes.First;


         if LResponseNode=nil then raise Exception.Create('Can not any Response Node');

         if LResponseNode.LocalName<>FService.Operation+SSoapResponseSuff then
         raise Exception.Create('Can not find Response Node');


         HasHolderAttribute:=False;
         if Method.ReturnType<>nil then
         begin
           if Method.ReturnType.IsInstance then
           begin
             Result:=Method.ReturnType.AsInstance.MetaclassType.Create;
             for aAttribute in Method.ReturnType.GetAttributes do
               if (aAttribute is XmlHolderAttribute) then HasHolderAttribute:=True;
           end
           else
             aValue.TryCast(Method.ReturnType.Handle,Result);

           if HasHolderAttribute then
             LNode:=LResponseNode
           else
             LNode:=LResponseNode.ChildNodes.First; //FindNode(ReturnName)

           LConverter.DeSerializeWithNode(LNode.LocalName,Result, LNode);
         end;

       end;

     except
        on E : Exception do
          raise Exception.Create(E.ClassName+' error raised, with message : '+E.Message);

     end;

   finally
      LReqDoc:=NIL;
      LRespDoc:=NIL;
      FreeAndNil(LConverter);
      FreeAndNil(LRequest);
      FreeAndNil(LResponse);
   end;
end;


function TbsRIO.GetHeader: TbsHeaderList;
var
  aString : String;
  aNamespace : String;
  aHeaderList : TbsHeaderList;
begin
  aNamespace:=FService.FNamespaceURI;
  if Length(aNamespace)=0 then Exit;
  if aNamespace[Length(aNamespace)]<>'/' then
    aNamespace:=aNamespace+'/';


  FService.Headers.Header[SContentType]:=FService.ContentType;
  FService.Headers.Header[SHTTPSoapAction]:=Format('"%s"',[ FService.GetSoapAction]);
  FService.Headers.Header['Host']:=Format('%s:%s',[FService.FURLHost,IntToStr(FService.FURLPort)]);
  FService.Headers.Header[SConnection]:=SKeepAlive;
  FService.Headers.Header[SUserAgent]:=FService.FAgent;

  if FService<>nil then
    if FService.Authentication<>nil then
    begin
      FService.Authentication.Username:=FService.Username;
      FService.Authentication.Password:=FService.Password;
      FService.Authentication.SetHeaders(FService.Headers);
    end;

  Result:=FService.Headers;

end;


{ TblService }

constructor TbsService.Create(AOwner: TComponent);
begin
  inherited;
  FAgent:=SAgent;
  FOptions:=[oUseSOAP11,oUseUTF8InHeader];
  FSOAPAction:=TStringList.Create;
  FSOAPAction.Delimiter:= '|';
  FHeaderList := TbsHeaderList.Create;
end;

function TbsService.GetAuthentication: TbsCustomAuthenticate;
begin
  Result:= FAuthentication;
end;

function TbsService.GetContentType: string;
begin
  Result := '';
  if not (oUseSOAP12 in Options) then
  begin
    if (oUseUTF8InHeader in Options) then
      Result := ContentTypeUTF8
    else
      Result := ContentTypeNoUTF8;
  end
  else
  begin
    if (oUseUTF8InHeader in Options) then
      Result := Format(ContentTypeWithActionNoLabelFmt, [ContentType12UTF8, GetSOAPAction])
    else
      Result := Format(ContentTypeWithActionNoLabelFmt, [ContentType12NoUTF8, GetSOAPAction]);
  end;


end;

function TbsService.GetSoapAction(OperationName: String=''): String;
var
  iName: Integer;
  Value: string;
begin
  if OperationName='' then OperationName:=FOperation;

  Value:=OperationName;
  iName:=SoapAction.IndexOfName(OperationName);

  if iName=-1 then
    iName:=SoapAction.IndexOfName(SOperationName);

  if iName>-1 then
    Value:=SoapAction.ValueFromIndex[iName];

  Value:=StringReplace(Value,SOperationName,OperationName,[rfReplaceAll]);

  Result:=Value;
end;

procedure TbsService.SetAuthentication(const Value: TbsCustomAuthenticate);
begin
  FAuthentication:=Value;
end;

procedure TbsService.SetOptions(const Value: TbsOptions);
begin
  if  (oUseSOAP11 in Value) and (oUseSOAP12 in Value) then
    raise Exception.Create('Can not set oUseSOAP11 and oUseSOAP12 same time.');

  FOptions := Value;
end;

procedure TbsService.SetServiceTypeInfo(const Value: PTypeInfo);
begin
  if FServiceTypeInfo=Value then Exit;
  if FRIO<>nil then FRIO._Release;

  FServiceTypeInfo := Value;
  FRIO:=TbsRIO.Create(FServiceTypeInfo);
  FRIO.FService:=Self;
end;

procedure TbsService.ReadFault(AFaultNode: IXMLNode);
  {
  function GetNodeURIAndName(const Node: IXMLNode; var TypeURI, ElemName: InvString): boolean;
  var
    Pre: InvString;
  begin
    ElemName := Node.NodeName;
    if IsPrefixed(ElemName) then
    begin
      Pre := ExtractPrefix(ElemName);
      ElemName := ExtractLocalName(ElemName);
      TypeURI := Node.FindNamespaceURI(Pre);
    end
    else
      TypeURI := Node.NamespaceURI;
    Result := True;
  end;
  }
var
  CustNode, CurrNode: IXMLNode;
  FaultActorNode, FaultCodeNode, FaultDetailNode, FaultStringNode: IXMLNode;
  FaultReasonNode, FaultNodeNode, FaultRoleNode: IXMLNode;
  I: Integer;
  AMessage: String;
  AClass: TClass;
  URI, TypeName, LocalName, ReasonLang: String;
  Ex: EBSRemotableException;
begin
  FaultActorNode := nil;
  FaultCodeNode := nil;
  FaultDetailNode := nil;
  FaultStringNode := nil;
  FaultReasonNode := nil;
  FaultNodeNode := nil;
  FaultRoleNode := nil;
  Ex := nil;



  if (oUseSOAP12 in Options) then
  begin
    for I := 0 to AFaultNode.ChildNodes.Count - 1 do
    begin
      CurrNode := AFaultNode.ChildNodes[I];
      LocalName := CurrNode.LocalName;

      if SameText(LocalName, SSOAP12FaultCode) then
        FaultCodeNode := CurrNode
      else if SameText(LocalName, SSOAP12FaultReason) then
      begin
        CurrNode := CurrNode.ChildNodes.FindNode(SSOAP12FaultText);
        if CurrNode <> nil then
          FaultReasonNode := CurrNode;
      end
      else if SameText(LocalName, SSOAP12FaultNode) then
        FaultNodeNode := CurrNode
      else if SameText(LocalName, SSOAP12FaultRole) then
        FaultRoleNode := CurrNode
      else if SameText(LocalName, SSOAP12FaultDetail) then
        FaultDetailNode := CurrNode;
    end;

    if FaultReasonNode <> nil then
    begin
      AMessage := FaultReasonNode.Text;
      for I := 0 to FaultReasonNode.AttributeNodes.Count - 1 do
      begin
        CurrNode := FaultReasonNode.AttributeNodes[i];
        if SameText(CurrNode.LocalName, SSOAP12FaultReasonLang) then
          ReasonLang := CurrNode.Text;
      end;
    end;
  end
  else
  begin
    for I := 0 to AFaultNode.ChildNodes.Count - 1 do
    begin
      CurrNode := AFaultNode.ChildNodes[I];
      LocalName := CurrNode.LocalName;

      if SameText(LocalName, SSoapFaultCode) then
        FaultCodeNode := CurrNode
      else if SameText(LocalName, SSoapFaultString) then
        FaultStringNode := CurrNode
      else if SameText(LocalName, SSoapFaultDetails) then
        FaultDetailNode := CurrNode
      else if SameText(LocalName, SSoapFaultActor) then
        FaultActorNode := CurrNode;
    end;

    { Retrieve message from FaultString node }
    if FaultStringNode <> nil then
      AMessage := FaultStringNode.Text;
  end;

  { Fallback on detail node if exception fault is empty, then fallback on whole xml node }
  if (AMessage = '') then
  begin
  { ToDo : XML
    if Assigned(FaultDetailNode) then
      AMessage := FaultDetailNode.XML
    else
      AMessage := FaultNode.XML;
   }
  end;

  { If there's a <detail> node, try to map it to a registered type }
  if FaultDetailNode <> nil then
  begin

    CustNode := nil;

  end;

  { Create default SOAP invocation exception if no suitable class was found }
  if Ex = nil then
    Ex := EBSRemotableException.Create(AMessage);

  { Fill all found exception info }
  if FaultActorNode <> nil then
    Ex.FaultActor := FaultActorNode.Text;
  if FaultCodeNode <> nil then
  begin
   {
    if svSOAP12 =  SoapVersion then
      Ex.FaultCode := FaultCodeNode.XML
    else
      Ex.FaultCode := FaultCodeNode.Text;
   }
  end;
  {if FaultDetailNode <> nil then
    Ex.FaultDetail := FaultDetailNode.XML; }
  if FaultNodeNode <> nil then
    Ex.FaultNode := FaultNodeNode.Text;
  if FaultRoleNode <> nil then
    Ex.FaultRole := FaultRoleNode.Text;
  if ReasonLang <> '' then
    Ex.FaultReasonLang := ReasonLang;

  raise Ex;
end;

procedure TbsService.RegisterInterface(AInterface: PTypeInfo;
  ANamespace: string);
begin
  SetServiceTypeInfo(AInterface);
  FNamespaceURI:=ANameSpace;
end;

procedure TbsService.RegisterSoapAction(AOperation, ASoapAction: String);
begin
  FSoapAction.Values[AOperation]:=ASoapAction;
end;

function TbsService.GetTransporter: TbsTransporter;
begin
  Result := FTransporter;
end;

procedure TbsService.SetTransporter(const Value: TbsTransporter);
begin
  FTransporter:=Value;
end;

destructor TbsService.Destroy;
begin
  if FRIO <>nil then
    if FRIO.RefCount>0  then FRIO._Release;
  FHeaderList.Free;
  FSOAPAction.Free;
  inherited;
end;

function TbsService.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if(csDesigning in ComponentState)then
  begin
    Result:=S_FALSE;
    Exit;
  end;
  if FRIO.RefCount=0 then FRIO._AddRef;
  Result:=FRIO.QueryInterface(IID,Obj);

end;


procedure TbsService.SetURL(const Value: string);
var
  URI: TbsURI;
begin
  FURL := Value;
  URI.From(FURL);
  FURLHost:= URI.Server;
  FURLPort:= StrToIntDef(URI.Port, 80);
end;




{ EBSRemotableException }

constructor EBSRemotableException.Create;
begin
  inherited Create('');
end;

constructor EBSRemotableException.Create(const Msg, AFaultCode, AFaultActor,
  AFaultReasonLang, AFaultNode, AFaultRole: String);
begin
  Message:=Msg;
  FFaultActor:=AFaultActor;
  FFaultCode:=AFaultCode;
  FFaultReasonLang:=AFaultReasonLang;
  FFaultNode:=AFaultNode;
  FFaultRole:=AFaultRole
end;

function EBSRemotableException.GetFaultReason: String;
begin
  Result:=Message;
end;

procedure EBSRemotableException.SetFaultReason(const Value: String);
begin
  Message:=Value;
end;


end.
