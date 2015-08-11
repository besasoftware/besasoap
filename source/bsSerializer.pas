unit bsSerializer;

interface

uses
  SysUtils, Classes, RTTI, TypInfo, XMLIntf, XMLDoc,
  bsConst, bsNullable, bsAttribute;

type
  TbsSerializer = class
    procedure Serialize(Stream:TStream;Obj:TValue); virtual;abstract;
    procedure Deserialize(Stream:TStream;var Obj: TValue); virtual; abstract;
  end;

  TbsXMLSerializer = class(TbsSerializer)
  private
    FDefNSAdded:Boolean;
    FContext: TRttiContext;
    FType:PTypeInfo;
    FElementForm:TSchemaForm;
    FtargetNamespace : string;
    FStandalone:Boolean;
    procedure SetNodeNil(ANode:IXMLNode);
    function NodeIsNil(ANode:IXMLNode):Boolean;
    function AddChild(AParentNode:IXMLNode;ANodeName:string;
    AForm:TSchemaForm; ANamespace:string='@';APrefix:string=''):IXMLNode;
    function FindElementNode(AParent:IXMLNode;ALocalName:String):IXMLNode;
  public
    constructor Create;
    destructor Destroy;override;
    class function StringToNative(AType:TRttiType; AValue:string): TValue;
    class function NativeToString(AType:TRttiType; AValue:TValue): String;
    procedure SerializeWithNode(ANodeName: String; aObj: TValue; ParentNode: IXMLNode);
    procedure DeSerializeWithNode(ANodeName: String; var aObj: TValue; ParentNode: IXMLNode);
    procedure SerializeToStream(Stream:TStream;Obj:TValue);
    function SerializeToString(Obj:TValue):string;
    function DeserializeFromString(AValue: string): TValue;
    procedure Serialize(Stream:TStream;Obj:TValue); override;
    procedure Deserialize(Stream:TStream;var Obj: TValue); override;
    procedure SetType(AType:PTypeInfo);
    property ElementForm:TSchemaForm read FElementForm write FElementForm;
    property targetNamespace : string read FtargetNamespace write FtargetNamespace;
    property Standalone:Boolean read FStandalone write FStandalone;
  end;

implementation

uses Types, bsUtil, bsClasses,Variants;


function String2DateTime(AValue: String): TDateTime;
var
  fs : TFormatSettings;
begin
  FillChar(fs, SizeOf(fs), 0);
  fs.DateSeparator := '-';
  fs.TimeSeparator := ':';
  fs.ShortDateFormat := 'yyyy/mm/dd';
  fs.LongDateFormat := 'yyyy/mm/dd';
  fs.ShortTimeFormat := 'HH:MM:ss';
  fs.LongTimeFormat := 'HH:MM:ss';
  result := StrToDateTimeDef(AValue, EncodeDate(1899, 12, 30), fs);
end;


const
  DEFAULT_DECIMALSEPARATOR  = '.'; // don't change!
  DEFAULT_TRUE              = '1'; // don't change!
  DEFAULT_TRUE_STR          = 'true'; // don't change!
  DEFAULT_FALSE             = '0'; // don't change!
  DEFAULT_FALSE_STR         = 'false'; // don't change!
  DEFAULT_DATETIMESEPARATOR = 'T'; // don't change!
  DEFAULT_DATESEPARATOR     = '-'; // don't change!
  DEFAULT_TIMESEPARATOR     = ':'; // don't change!
  DEFAULT_MSSEPARATOR       = '.'; // don't change!

function DecimalSeparator: char;
begin
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF RTLVersion >= 22.0}  // Delphi XE
    Result := FormatSettings.DecimalSeparator;
    {$ELSE}
    Result := SysUtils.DecimalSeparator;  // Delphi 2010 and below
    {$IFEND}
  {$ELSE}
  Result := SysUtils.DecimalSeparator;
  {$ENDIF}  // CONDITIONALEXPRESSIONS
end; { DecimalSeparator }

class function TbsXMLSerializer.StringToNative(AType:TRttiType;AValue:string) : TValue;
var
  ADateTime :TDateTime;
begin

  case AType.TypeKind of
    tkInteger: Result := StrToIntDef(AValue, 0);
    tkChar: if(AValue.Length>0) then Result:=AValue.Chars[0];
    tkEnumeration:
      begin
        if AType.Handle=TypeInfo(Boolean)then
        begin
          Result:=SameText(AValue,'true');
        end;
      end;
    tkFloat:
      begin
        if AType.Handle=TypeInfo(TDate)then
        begin
          ISO8601DateToDelphiDateTime(AValue,ADateTime);
          Result:=TDate(ADateTime);
        end
        else if AType.Handle=TypeInfo(TTime)then
        begin
          ISO8601DateToDelphiDateTime(AValue,ADateTime);
          Result:=TTime(ADateTime);
        end
        else if AType.Handle=TypeInfo(TDateTime)then
        begin
          ISO8601DateToDelphiDateTime(AValue,ADateTime);
          Result:=ADateTime;
        end
        else
          Result:=StrToFloatDef( StringReplace(AValue, DEFAULT_DECIMALSEPARATOR, DecimalSeparator, [rfReplaceAll]) , 0) ;
      end;
    tkString: Result:=AValue;
    tkWChar: if(AValue.Length>0) then Result:=WideChar(AValue.Chars[0]);
    tkLString: Result:=AValue;
    tkWString: Result:=WideString(AValue);
    tkInt64: Result := StrToInt64Def(AValue, 0);
    tkUString: Result:=UnicodeString(AValue);
    else
      Result:=AValue;
  end;

end;

procedure TbsXMLSerializer.Deserialize(Stream:TStream;var Obj: TValue);
var
  xmldoc: IXMLDocument;
  name,ns :string;
begin
  xmldoc:=TXMLDocument.Create(nil);
  xmldoc.Active:=TRUE;
  try
    name:='';
    Stream.Position:=0;
    xmldoc.LoadFromStream(Stream);
    Obj:=GetTypeData(PTypeInfo(FType))^.ClassType.Create;
    TbsAttributeUtils.GetXMLRootAttribute(FContext,FContext.GetType(Obj.TypeInfo),name,ns);
    DeserializeWithNode(name,Obj,xmldoc.DocumentElement);
  finally
    XMLDoc:=NIL;
  end;
end;

destructor TbsXMLSerializer.Destroy;
begin
  FContext.Free;;
  inherited;
end;

function TbsXMLSerializer.FindElementNode(AParent: IXMLNode;
  ALocalName: String): IXMLNode;
var
  I: Integer;
begin
  Result:=NIL;
  for I := 0 to AParent.ChildNodes.Count-1 do
  if AParent.ChildNodes[I].LocalName=ALocalName then
  begin
    Result:=AParent.ChildNodes[I];
    Break;
  end;
end;

class function TbsXMLSerializer.NativeToString(AType:TRttiType; AValue:TValue): String;
var
  ADateTime :TDateTime;
begin

  case AType.TypeKind of
    tkEnumeration:
      begin
        if AType.Handle=TypeInfo(Boolean)then
        begin
          Result:=LowerCase(BoolToStr(AValue.AsBoolean,True));
        end;
      end;
    tkFloat:
      begin
        if AType.Handle=TypeInfo(TDate)then
          Result:=DelphiDateTimeToISO8601Date( AValue.AsType<TDateTime>, TbsDateTimeFormat.dfDate)
        else if AType.Handle=TypeInfo(TTime)then
          Result:=DelphiDateTimeToISO8601Date( AValue.AsType<TDateTime>, TbsDateTimeFormat.dfTime)
        else if AType.Handle=TypeInfo(TDateTime)then
          Result:=DelphiDateTimeToISO8601Date( AValue.AsType<TDateTime>, TbsDateTimeFormat.dfDateTime)
        else
          Result:=StringReplace(AValue.ToString, DecimalSeparator, DEFAULT_DECIMALSEPARATOR, [rfReplaceAll])
      end;
    else
      Result:=AValue.ToString;
  end;

end;

function TbsXMLSerializer.NodeIsNil(ANode: IXMLNode): Boolean;
begin
  Result:=False;
  if ANode=nil then Exit;
  Result:=VarToStr(ANode.GetAttributeNS('nil',XMLSchemaInstNameSpace))='true';
end;

procedure TbsXMLSerializer.Serialize(Stream: TStream; Obj: TValue);
begin
  SerializeToStream(Stream,Obj);
end;

procedure TbsXMLSerializer.SerializeToStream(Stream: TStream; Obj: TValue);
var
  xmldoc: IXMLDocument;
  name,ns :string;
begin
  xmldoc:=TXMLDocument.Create(NIL);
  xmldoc.Active:=TRUE;
  try
    name:='';
    TbsAttributeUtils.GetXMLRootAttribute(FContext,FContext.GetType(Obj.TypeInfo),name,ns);
    if (targetNamespace='')
    then targetNamespace:=ns;
    SerializeWithNode(name,Obj,xmldoc.GetDocumentNode);
    xmldoc.SaveToStream(Stream);
    Stream.Position:=0;
  finally
    xmldoc:=NIL;
  end;
end;

function TbsXMLSerializer.SerializeToString(Obj: TValue): string;
var
  AStream: TStringStream;
begin
  AStream:=TStringStream.Create('');
  SerializeToStream(AStream,Obj);
  Result:=AStream.DataString;
  FreeAndNil(AStream);
end;

function TbsXMLSerializer.DeserializeFromString(AValue: string): TValue;
var
  LStream: TStringStream;
begin
  LStream:=TStringStream.Create(AValue);
  Deserialize(LStream,Result);
  LStream.Free;
end;

procedure TbsXMLSerializer.SetNodeNil(ANode: IXMLNode);
begin
  ANode.SetAttributeNS('nil',XMLSchemaInstNameSpace,'true');
end;

procedure TbsXMLSerializer.SetType(AType: PTypeInfo);
begin
  FType:=AType;
end;

procedure TbsXMLSerializer.SerializeWithNode(ANodeName: String; aObj: TValue;
  ParentNode: IXMLNode );
var
  aType: TRttiType;
  aField: TRttiField;
  aRecord: TRttiRecordType;
  aValue: TValue;
  I: Integer;
  NodeName,NamespaceURI: String;
  AForm : TSchemaForm;
  pNode, node: IXMLNode;
  NodeType: TNodeType;
  AsElement: Boolean;
  LNamespaceAdded:Boolean;
  aNodeValue:String;
  sPrefix{,sNamespace}: string;


  function ReadNullableRecord(anObj: TValue): TValue;
  begin
    TryGetUnderlyingValue(anObj, Result);
  end;

begin
  aType := FContext.GetType(aObj.TypeInfo);

  AForm:=FElementForm;//sfUnqualified;

  NodeName := aType.Name;
  NamespaceURI:=targetNamespace;

  if Length(ANodeName) > 0 then
    NodeName := ANodeName;

  TbsAttributeUtils.GetXMLElementAttribute(FContext,aType,NodeName,AForm,NamespaceURI);
  TbsAttributeUtils.GetXMLFormAttribute(FContext,aType,AForm);

  if aObj.IsObject then  begin
    if (aObj.AsObject =nil) then begin
      pNode:= AddChild(ParentNode,NodeName,AForm);
      SetNodeNil(pNode);
      Exit;
    end;
  end;

  if aType.IsInstance then // Class
  begin
    if TbsAttributeUtils.GetXmlNamespaceAttribute(FContext,aType,sPrefix,NamespaceURI,AForm)
    then
      begin
        sPrefix:=ParentNode.OwnerDocument.GeneratePrefix(ParentNode);

        if (Length(sPrefix) = 0) or (AForm=sfQualified) then
        begin
          pNode.DeclareNamespace(sPrefix, NamespaceURI);
        end;

        if Length(NamespaceURI) > 0 then
        begin
          pNode.DeclareNamespace(sPrefix, NamespaceURI);
          LNamespaceAdded:=True;
        end;
      end;

    pNode:=AddChild(ParentNode,NodeName,AForm);

  end
  else if aType.TypeKind = tkRecord then // Record
  begin

    if TbsAttributeUtils.GetXmlNamespaceAttribute(FContext,aType,sPrefix,NamespaceURI,AForm)then
    begin
      if Length(NamespaceURI) > 0 then
        begin
          pNode:=AddChild(ParentNode,NodeName,AForm, NamespaceURI,sPrefix);
        end;
    end else
        pNode:=ParentNode.AddChild(NodeName);

    aValue := ReadNullableRecord(aObj);
    aRecord:= aType.AsRecord;
    pNode.NodeValue :=  NativeToString(aRecord.GetField('fValue').FieldType,aValue);
    Exit;
  end else if aType.TypeKind in [tkInteger, tkInt64, tkChar, tkEnumeration, tkFloat, tkString, tkWChar,
        tkLString, tkWString, tkVariant, tkUString, tkSet] then begin
    pNode:=AddChild(ParentNode,NodeName,AForm);
    pNode.NodeValue:= NativeToString(aType,aObj);
    Exit;
  end
  else
    pNode := ParentNode;


    if TbsAttributeUtils.GetXmlNamespaceAttribute(FContext,aType,sPrefix,NamespaceURI,AForm)then
    begin
      if Length(NamespaceURI) > 0 then
        pNode.DeclareNamespace(sPrefix, NamespaceURI);
    end;


  for aField in aType.GetFields do begin
    // Only Public & Published
    if not(aField.Visibility in [mvPublic, mvPublished]) then
      Continue;

    case aField.FieldType.TypeKind of
      tkUnknown:
        begin

        end;
      tkInteger, tkInt64, tkChar, tkEnumeration, tkFloat, tkString, tkWChar,
        tkLString, tkWString, tkVariant, tkUString, tkSet:
        begin
          NodeName := aField.Name;
          NamespaceURI:=PNode.NamespaceURI;
          // Default
          NodeType := TNodeType.ntElement;

          if TbsAttributeUtils.GetXmlAttributeAttribute(FContext,aField,NodeName,NamespaceURI) then
            NodeType := ntAttribute;
          TbsAttributeUtils.GetXMLElementAttribute(FContext,aField,NodeName,AForm,NamespaceURI);
          TbsAttributeUtils.GetXMLFormAttribute(FContext,aField,AForm);

          if TbsAttributeUtils.HasAttribute(FContext,aField,XmlTextAttribute) then
              NodeType := ntText;


          aNodeValue:= NativeToString(aField.FieldType,aField.GetValue(aObj.AsObject));

          if NodeType = ntElement then
          begin

            node := AddChild(pNode,NodeName, AForm, NamespaceURI);
            node.NodeValue := aNodeValue;
          end
          else if NodeType = ntAttribute then
          begin
            if NamespaceURI<>targetNamespace then pNode.SetAttributeNS(NodeName,NamespaceURI,aNodeValue)
            else pNode.Attributes[NodeName] := aNodeValue;
          end
          else if NodeType = ntText then
          begin
            pNode.NodeValue := aNodeValue;
          end;
        end;

      tkClass:
        begin
           if aField.FieldType.IsInstance then begin
           NodeName := aField.Name;

           if (aField.GetValue(aObj.AsObject).AsObject =nil) then
           begin

              SetNodeNil(AddChild(pNode,NodeName,AForm));


           end else
           begin
              SerializeWithNode(NodeName,aField.GetValue(aObj.AsObject).AsObject,pNode);
           end;
           end;
        end;
      // tkMethod: ;
      tkArray:
        begin

        end;
      tkDynArray:
        begin
          NodeName := aField.Name;

          AsElement := False;

          if TbsAttributeUtils.GetXMLElementAttribute(FContext,aField,NodeName,AForm,NamespaceURI) then
            AsElement := True;

          TbsAttributeUtils.GetXMLFormAttribute(FContext,aField,AForm);

          if not AsElement then
          begin
            TbsAttributeUtils.GetXMLArrayAttribute(FContext,aField,NodeName);
            node := AddChild(pNode,NodeName,AForm);
            // ArrayItem
            NodeName := 'item';
            TbsAttributeUtils.GetXMLArrayItemAttribute(FContext,aField,NodeName);
          end
          else
          begin
            node := pNode;
          end;

          aValue := aField.GetValue(aObj.AsObject);
          // LTypeKind := TRttiDynamicArrayType( aField.FieldType ).ElementType.TypeKind;
          for I := 0 to aValue.GetArrayLength - 1 do
          begin
            SerializeWithNode(NodeName, aValue.GetArrayElement(I), node);
          end;

        end;

      tkRecord:
        begin
          NodeName := aField.Name;
          // Default
          NodeType := TNodeType.ntElement;

          TbsAttributeUtils.GetXmlAttributeAttribute(FContext,aField,NodeName,NamespaceURI);
          TbsAttributeUtils.GetXMLElementAttribute(FContext,aField,NodeName,AForm,NamespaceURI);
          TbsAttributeUtils.GetXMLFormAttribute(FContext,aField,AForm);

          aValue := ReadNullableRecord(aField.GetValue(aObj.AsObject));
          aRecord:= aField.FieldType.AsRecord;
          aNodeValue := NativeToString(aRecord.GetField('fValue').FieldType,aValue);

          if (NodeType = ntElement) then
          begin
            node:=AddChild(pNode,NodeName,aForm);
            if (aValue.IsEmpty) then
              node.NodeValue:=''
            else
              node.NodeValue := aNodeValue;
          end
          else if NodeType = ntAttribute then
          begin
            if (aValue.IsEmpty) then
              pNode.Attributes[NodeName]:=''
            else
              pNode.Attributes[NodeName] := aNodeValue;
          end;

          if (aValue.IsEmpty) and (NodeType = ntElement)then
          SetNodeNil(node);


        end;
      // tkInterface: ;

      // tkClassRef: ;
      // tkPointer: ;
      // tkProcedure: ;
    end;
  end;

end;




function TbsXMLSerializer.AddChild(AParentNode: IXMLNode;
  ANodeName: string; AForm:TSchemaForm; ANamespace:string='@'; APrefix:string=''): IXMLNode;
var
  LPrefix:string;
  LGenPrefix: Boolean;
  LNameSpace:String;
begin
  LNameSpace:=ANamespace;
  if ANameSpace='@' then LNamespace:=targetNamespace;

  if not FDefNSAdded
  then
    begin
      Result:=AParentNode.AddChild(ANodeName,targetNamespace,True);
      if FStandalone then
      Result.DeclareNamespace(SXMLSchemaInstNameSpace99Pre, XMLSchemaInstNameSpace);

      FDefNSAdded:=True;
    end
  else if (AForm=sfQualified)
  then
    begin
      if Length(APrefix)>0
      then
        begin
          Result:=AParentNode.AddChild(ANodeName,LNamespace);
          Result.DeclareNamespace(APrefix, LNamespace);
        end
      else
        begin
          Result:=AParentNode.AddChild(ANodeName,LNamespace,True);
        end;
    end
  else if (AForm=sfUnqualified) then
    begin
      if (targetNamespace<>LNamespace) then
        Result:=AParentNode.AddChild(ANodeName,LNamespace)
      else
        begin
          LNamespace:='';
          Result:=AParentNode.AddChild(ANodeName,LNamespace)
        end;
    end;

end;

constructor TbsXMLSerializer.Create;
begin
  FContext:= TRttiContext.Create;
  FElementForm:=sfUnqualified;
  FStandalone:=True;
end;

procedure TbsXMLSerializer.DeSerializeWithNode(ANodeName: String; var aObj: TValue;
  ParentNode: IXMLNode);
var
  aContext: TRttiContext;
  aAttribute : TCustomAttribute;
  aType: TRttiType;
  aField : TRttiField;
  aRecord : TRttiRecordType;
  NodeName : String;
  NamespaceURI : String;
  NodeType : TNodeType;
  aNode,pNode : IXMLNode;
  aValue         : TValue;
  AsElement: Boolean;
  Form : TSchemaForm;

  function XML2ObjNative(NativeNode:IXMLNode; NativeType:TRttiType;NativeNodeName:String):TValue;
  var
    NativeNodeType : TNodeType;
    NativeNS :String;
    NativeChildNode:IXmlNode;
  begin
    NamespaceURI:='';
    // Default
    NativeNodeType := TNodeType.ntElement;

    TbsAttributeUtils.GetXmlAttributeAttribute(aContext,NativeType,NativeNodeName,NativeNS);
    TbsAttributeUtils.GetXMLElementAttribute(aContext,NativeType,NativeNodeName,Form,NativeNS);
    TbsAttributeUtils.GetXMLFormAttribute(aContext,NativeType,Form);
    if TbsAttributeUtils.HasAttribute(aContext,NativeType,XmlTextAttribute) then
      NodeType := ntText;

      if NativeNode = nil then Exit;

      if NativeNodeType = ntElement then begin
        if NodeIsNil(NativeNode) then aValue:=nil;
        Result:= StringToNative(NativeType,NativeNode.NodeValue);
      end
      else if NativeNodeType = ntAttribute then begin
        NativeChildNode :=  NativeNode.AttributeNodes.FindNode(NativeNodeName);
        //if aNode.IsNil then aValue:=nil;
        //aValue:= NativeChildNode.NodeValue;
        Result:= StringToNative(NativeType,NativeChildNode.NodeValue);
      end
      else if NativeNodeType = ntText then begin
        //if aNode.IsNil then aValue:=nil;
        //aValue:= NativeNode.NodeValue;
        Result:= StringToNative(NativeType,NativeNode.NodeValue);
      end;
  end;


  function XML2ObjDynArray(ArrayNode:IXMLNode; ArrayType:TRttiType; ElementArray:Boolean=False):TValue;
  var
    ArrayLen : Integer;
    ArrayValue : array of TValue;
    ArrayItem : TValue;
    Arr : Integer;
    S:String;
    TB:TBytes;
    ArrayList :TInterfaceList;
    Tmpnode : IXMLNode;
    I:Integer;
  begin
    //SelectNodes...
    if ElementArray then begin
      ArrayList :=TInterfaceList.Create;
      TmpNode:=ArrayNode;
      for I := 0 to ArrayNode.ParentNode.ChildNodes.Count-1 do begin
        TmpNode:=ArrayNode.ParentNode.ChildNodes[I];
        if (ArrayNode.LocalName=TmpNode.LocalName) then
          ArrayList.Add(TmpNode)
        else
          Break;
      end;

      ArrayLen:=ArrayList.Count;
    end else
       ArrayLen:=ArrayNode.ChildNodes.Count;

    // TByteDynArray ...
    if (TRttiDynamicArrayType(ArrayType).ElementType.Handle = TypeInfo(Byte))
        and (ArrayLen=0) then begin
        S:=  ArrayNode.NodeValue;

        S:=Base64Decode(ArrayNode.NodeValue);
        TB:=TEncoding.UTF8{ANSI}.GetBytes(S);
        TValue.Make(@TB,TRttiDynamicArrayType(aType).Handle,Result);
    end else begin

    SetLength(ArrayValue,ArrayLen);
    for Arr := 0 to ArrayLen-1 do begin
      TValue.Make(nil,TRttiDynamicArrayType(ArrayType).ElementType.Handle,ArrayItem);
      if ElementArray
      then
        begin
          TmpNode:=(ArrayList[Arr] as IXMLNode);
          DeSerializeWithNode( TmpNode.LocalName, ArrayItem, TmpNode)
        end
      else
        DeSerializeWithNode(ArrayNode.ChildNodes[Arr].LocalName, ArrayItem, ArrayNode.ChildNodes[Arr]);

      ArrayValue[Arr]:= ArrayItem;
    end;

     Result:= TValue.FromArray(ArrayType.Handle,ArrayValue);
    end;
    ArrayList.Free;
  end;



begin
  aType :=aContext.GetType(aObj.TypeInfo);


  if not aObj.IsObject then begin
    case aType.TypeKind of
      tkInteger, tkChar, tkFloat, tkString, tkWChar, tkLString, tkWString,
      tkUString, tkInt64,tkVariant,tkEnumeration:

      aObj:= XML2ObjNative(ParentNode,aType,ANodeName);
      tkDynArray:
        begin
          AsElement:=False;

          if TbsAttributeUtils.GetXMLElementAttribute(aContext,aType,NodeName,Form,NamespaceURI) then
            AsElement := True;
          TbsAttributeUtils.GetXMLFormAttribute(aContext,aType,Form);


         if not AsElement then
          begin
            TbsAttributeUtils.GetXMLArrayAttribute(aContext,aType,NodeName);
            // ArrayItem
            NodeName := 'item';
            TbsAttributeUtils.GetXMLArrayItemAttribute(aContext,aType,NodeName);
          end;


          aObj:=XML2ObjDynArray(ParentNode, aType,AsElement);

        end;
      //tkSet: ;
      //tkClass: ;
      //tkArray: ;
      //tkRecord: ;
    end;

  end else

    if (aObj.AsObject =nil) then begin
      aObj:=aType.AsInstance.MetaclassType.Create;
    end;

  for aAttribute in aType.GetAttributes do
    if aAttribute is XmlReturnNameAttribute then
    begin
      if Length(XmlReturnNameAttribute(aAttribute).ReturnName) > 0 then
        NodeName := XmlReturnNameAttribute(aAttribute).ReturnName;
        //NamespaceURI:=XmlElementAttribute(aAttribute).NamespaceURI;
    end;

    pNode:=ParentNode;

  for aField in aType.GetFields do begin
    if not(aField.Visibility in [mvPublic, mvPublished]) then
      Continue;

    case aField.FieldType.TypeKind of
      //tkUnknown: ;
      tkInteger, tkInt64, tkChar, tkEnumeration, tkFloat, tkString, tkWChar,
        tkLString, tkWString, tkVariant, tkUString, tkSet:
        begin
          NodeName := aField.Name;
          NamespaceURI:='';
          NodeType := TNodeType.ntElement;

          if TbsAttributeUtils.GetXmlAttributeAttribute(aContext,aField,NodeName,NamespaceURI) then
          NodeType := ntAttribute;

          if TbsAttributeUtils.GetXmlElementAttribute(aContext,aField,NodeName,Form,NamespaceURI) then
          NodeType := ntElement;

          if TbsAttributeUtils.HasAttribute(aContext,aField,XmlTextAttribute) then
          NodeType := ntText;

          if NodeType = ntElement then
          begin
            aNode := FindElementNode(pNode,NodeName);
            if aNode=nil then aValue:=nil
            else begin
              aValue:= StringToNative(aField.FieldType, aNode.NodeValue);
              if NodeIsNil(aNode) then aValue:=nil;
            end;

            aField.SetValue(aObj.AsObject,aValue);
          end
          else if NodeType = ntAttribute then
          begin
            aNode := pNode.AttributeNodes.FindNode(NodeName);
            if aNode=nil then aValue:=nil
            else
            aValue:= StringToNative(aField.FieldType,aNode.NodeValue);
            aField.SetValue(aObj.AsObject,aValue);
          end
          else if NodeType = ntText then
          begin
            aValue:= StringToNative(aField.FieldType,aNode.NodeValue);
            aField.SetValue(aObj.AsObject,aValue);
          end;
        end;


      //tkSet: ;
      tkClass:
        begin
          NodeName := aField.Name;

          TbsAttributeUtils.GetXMLElementAttribute(aContext,aField,NodeName,Form,NamespaceURI);

          aNode := FindElementNode(pNode,NodeName);
          if aNode=nil then
            aValue:=nil
          else if NodeIsNil(aNode) then
            aValue:=nil
          else
          begin
            aValue:= aField.GetValue(aObj.AsObject);
            DeSerializeWithNode(NodeName, aValue, aNode);
            aField.SetValue(aObj.AsObject, aValue);
          end;

        end;
      //tkMethod: ;
      //tkArray:

      tkDynArray:
        begin
          NodeName := aField.Name;
          AsElement := False;

          if TbsAttributeUtils.GetXMLElementAttribute(aContext,aField,NodeName,Form,NamespaceURI) then
              AsElement := True;

          TbsAttributeUtils.GetXMLFormAttribute(aContext,aField,Form);

         if not AsElement then
          begin
            TbsAttributeUtils.GetXMLArrayAttribute(aContext,aField,NodeName);
            NodeName := 'item';
            TbsAttributeUtils.GetXMLArrayItemAttribute(aContext,aField,NodeName);
          end;

          aNode := FindElementNode( pNode,NodeName);
          if aNode=nil then Continue;
          aValue:=XML2ObjDynArray(aNode, aField.FieldType,AsElement);
        end;
      tkRecord:
        begin
          NodeName := aField.Name;
          NodeType := TNodeType.ntElement;

          TbsAttributeUtils.GetXmlAttributeAttribute(aContext,aField,NodeName,NamespaceURI);
          TbsAttributeUtils.GetXmlElementAttribute(aContext,aField,NodeName,Form,NamespaceURI);

          // Type Casting
          //TValue.Make(nil, aField.FieldType.Handle,aValue);
          aValue:=aField.GetValue(aObj.AsObject);
          aRecord:=aField.FieldType.AsRecord;

          if (NodeType = ntElement) then
          begin
            aNode := FindElementNode(pNode,NodeName);
            if   NodeIsNil( aNode) then aValue:=nil
            else
            TrySetUnderlyingValue(aValue, StringToNative(aRecord.GetField('fValue').FieldType,aNode.NodeValue));
            aField.SetValue(aObj.AsObject,aValue);
          end
          else if NodeType = ntAttribute then
          begin
            aNode := pNode.AttributeNodes.FindNode(NodeName);
            TrySetUnderlyingValue(aValue, StringToNative(aRecord.GetField('fValue').FieldType,aNode.NodeValue));
          end;

        end;
    end;
  end;
end;



end.
