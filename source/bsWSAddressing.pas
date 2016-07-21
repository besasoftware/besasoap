(******************************************************)
(*                                                    *)
(*            Besa Software Classes Library           *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// WS-Addressing
/// </summary>
unit bsWSAddressing;

interface
uses bsConst,SysUtils, XMLIntf;

type
  TbsWSAddressingMustUnderstand = (wsmNONE,wsmTRUE,wsmFALSE);
  TbsWSAddressingVersion = (wsv200508,wsv200408);

  TbsWSAddressing = class
  private
    FEnabled:Boolean;
    FMustUnderstand: Boolean;
    FWSAddressingVersion: TbsWSAddressingVersion;
    FAction: String;
    FAddDefaultAction: Boolean;
    FTo: String;
    FAddDefaultTo: Boolean;
    FReplyTo: String;
    FMessageId: String;
    FGenerateMessageId: Boolean;
    FFrom: String;
    FFaultTo: string;
  public
    constructor Create;
    procedure GenerateNode( aHeaderNode : IXMLNode);
  published
    property Enabled : Boolean read FEnabled write FEnabled;
    property MustUnderstand :Boolean read FMustUnderstand write FMustUnderstand;
    property Version : TbsWSAddressingVersion read FWSAddressingVersion write FWSAddressingVersion;
    property AddDefaultAction :Boolean read FAddDefaultAction write FAddDefaultAction;
    property Action : String read FAction write FAction;
    property AddDefaultTo :Boolean read FAddDefaultTo write FAddDefaultTo;
    property To_ : String read FTo write FTo;
    property ReplyTo : String read FReplyTo write FReplyTo;
    property GenerateMessageId : Boolean read FGenerateMessageId write FGenerateMessageId;
    property MessageId: String read FMessageId write FMessageId;
    property From_ :String read FFrom write FFrom;
    property FaultTo: string read FFaultTo write FFaultTo;
  end;

implementation

{ TbsWSAddressing }


procedure TbsWSAddressing.GenerateNode(aHeaderNode: IXMLNode);
var
  LNode :IXMLNode;
  LAddrNode :IXMLNode;
  LNamespaceURI:String;
  LGuid:TGuid;
begin

  case Version of
    wsv200508: LNamespaceURI:=SWSA_2005_08;
    wsv200408: LNamespaceURI:=SWSA_2004_08;
  end;
  {<a:Action s:mustUnderstand="1">Service/Method</a:Action>}
  if FAction<>''
  then
    begin
      LNode:=aHeaderNode.AddChild('Action', LNamespaceURI, True);
      LNode.NodeValue:=FAction;

      if FMustUnderstand
      then
        LNode.Attributes[SSoapNameSpacePre+':mustUnderstand']:='1';
    end;

  {<a:MessageID>urn:uuid:Guid</a:MessageID>}
  if FGenerateMessageId then
  begin
    CreateGUID(LGuid);
    FMessageId:='urn:uuid:'+GUIDToString(LGuid);
  end;


  if FMessageId<>''
  then
    begin
      LNode:=aHeaderNode.AddChild('MessageID', LNamespaceURI, True);
      LNode.NodeValue:=FMessageId;
    end;

   {<a:ReplyTo>
			<a:Address>http://www.w3.org/2005/08/addressing/anonymous</a:Address>
		</a:ReplyTo>}
  if FReplyTo<>''
  then
    begin
      LNode:=aHeaderNode.AddChild('ReplyTo', LNamespaceURI, True);
      LAddrNode:=LNode.AddChild('Address', LNamespaceURI, True);
      LAddrNode.NodeValue:=FReplyTo;
    end;

  {<a:To s:mustUnderstand="1">Service Adress</a:To>}
  if FTo<>''
  then
    begin
      LNode:=aHeaderNode.AddChild('To',LNamespaceURI,True);
      LNode.NodeValue:=FTo;

      if FMustUnderstand
      then
        LNode.Attributes[SSoapNameSpacePre+':mustUnderstand']:='1';
    end;

end;

{ TbsWSAddressing }

constructor TbsWSAddressing.Create;
begin
   FEnabled             := False;
   FMustUnderstand      := True;
   FWSAddressingVersion := wsv200508;
   FAction              := '';
   FAddDefaultAction    := True;
   FTo                  := '';
   FAddDefaultTo        := True;
   FReplyTo             := 'http://www.w3.org/2005/08/addressing/anonymous';
   FMessageId           := '';
   FGenerateMessageId   := True;
   FFrom                := '';
   FFaultTo             := '';
end;

end.
