(******************************************************)
(*                                                    *)
(*            Besa Software Classes Library           *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// Serializer Library 
/// </summary>
unit bsSerializer;

interface

uses
  SysUtils, Classes, RTTI, TypInfo, XMLIntf, XMLDoc,
  bsConst, bsNullable, bsAttribute;

type
  TbsSerializer = class
    procedure Serialize(AStream:TStream; AObj:TValue); virtual;abstract;
    procedure Deserialize(AStream:TStream; var AObj: TValue); virtual; abstract;
  end;

  TbsXMLSerializer = class(TbsSerializer)
  private
    FDefNSAdded:Boolean;
    FContext: TRttiContext;
    FType:PTypeInfo;
    FElementForm:TSchemaForm;
    FNamespace : string;
    FStandalone:Boolean;
    FXSIChecked:Boolean;
    procedure SetNodeNil(ANode:IXMLNode);
    function NodeIsNil(ANode:IXMLNode):Boolean;
    function AddChild(AParentNode:IXMLNode;ANodeName:string;
    AForm:TSchemaForm; ANamespace:string='@';APrefix:string=''):IXMLNode;
    function FindElementNode(AParent:IXMLNode;ALocalName:String):IXMLNode;
  public
    constructor Create;
    destructor Destroy;override;
    class function StringToValue(AType:TRttiType; AValue:string): TValue;
    class function ValueToString(AType:TRttiType; AValue:TValue): String;
    procedure SerializeWithNode(ANodeName: String; AObj: TValue; AParentNode: IXMLNode);
    procedure DeSerializeWithNode(ANodeName: String; var aObj: TValue; AParentNode: IXMLNode);
    procedure SerializeToStream(AStream:TStream; AObj:TValue);
    function SerializeToString(AObj:TValue):string;
    function DeserializeFromString(AValue: string; ATypeInfo: PTypeInfo=NIL): TValue;
    procedure Serialize(AStream:TStream; AObj:TValue); override;
    procedure Deserialize(AStream:TStream;var AObj: TValue); override;
    procedure SetType(AType:PTypeInfo);
    property ElementForm:TSchemaForm read FElementForm write FElementForm;
    property Namespace : string read FNamespace write FNamespace;
    property Standalone:Boolean read FStandalone write FStandalone;
  end;

implementation

uses Types, bsUtil, bsClasses,Variants, StrUtils;


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
    {$ENDIF}
  {$ELSE}
  Result := SysUtils.DecimalSeparator;
  {$ENDIF}  // CONDITIONALEXPRESSIONS
end; { DecimalSeparator }

class function TbsXMLSerializer.StringToValue(AType: TRttiType; AValue: string) : TValue;
var
  ADateTime :TDateTime;
begin

  case AType.TypeKind of
    tkInteger:
      Result := StrToIntDef(AValue, 0);
    tkChar:
      if(AValue.Length>0) then Result:=AValue.Chars[0];
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
    tkString:
      Result:=AValue;
    tkWChar:
      if(AValue.Length>0) then
        Result:={$IFNDEF NEXTGEN}WideChar{$ELSE}Char{$ENDIF}(AValue.Chars[0]);

    tkLString:
      Result:=AValue;
    tkWString:
      Result:={$IFNDEF NEXTGEN}WideString{$ELSE}String{$ENDIF}(AValue);
    tkInt64:
      Result := StrToInt64Def(AValue, 0);
    tkUString:
      Result:=UnicodeString(AValue);
    else
      Result:=AValue;
  end;

end;

procedure TbsXMLSerializer.Deserialize(AStream:TStream;var AObj: TValue);
var
  LXmlDoc: IXMLDocument;
  LNodeName,ns :string;
begin
  LXmlDoc:=TXMLDocument.Create(nil);
  LXmlDoc.Active:=TRUE;
  try
    LNodeName:='';
    AStream.Position:=0;
    LXmlDoc.LoadFromStream(AStream);
    if FType=NIL
    then
      raise Exception.Create('You must define value type');
    AObj:=GetTypeData(PTypeInfo(FType))^.ClassType.Create;
    TbsAttributeUtils.GetXMLRootAttribute(FContext,FContext.GetType(AObj.TypeInfo), LNodeName, ns);
    DeserializeWithNode(LNodeName, AObj, LXmlDoc.DocumentElement);
  finally
    LXmlDoc:=NIL;
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

class function TbsXMLSerializer.ValueToString(AType:TRttiType; AValue:TValue): String;
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

procedure TbsXMLSerializer.Serialize(AStream: TStream; AObj: TValue);
begin
  SerializeToStream(AStream, AObj);
end;

procedure TbsXMLSerializer.SerializeToStream(AStream: TStream; AObj: TValue);
var
  LXmlDoc: IXMLDocument;
  LNodeName,ns :string;
  LType: TRttiType;
begin
  LXmlDoc:=TXMLDocument.Create(NIL);
  LXmlDoc.Options := LXmlDoc.Options + [doNodeAutoIndent];
  LXmlDoc.Active:=TRUE;
  try
    LNodeName:='';
    LType:=FContext.GetType(AObj.TypeInfo);
    TbsAttributeUtils.GetXMLRootAttribute(FContext,LType, LNodeName, ns);
    if (FNamespace='') then FNamespace:=ns;
    SerializeWithNode(LNodeName, AObj, LXmlDoc.GetDocumentNode);
    LXmlDoc.SaveToStream(AStream);
    AStream.Position:=0;
  finally
    LXmlDoc:= NIL;
  end;
end;

function TbsXMLSerializer.SerializeToString(AObj: TValue): string;
var
  LStream: TStringStream;
begin
  Result:='';
  try
    LStream:=TStringStream.Create('');
    SerializeToStream(LStream, AObj);
    Result:=LStream.DataString;
  finally
    FreeAndNil(LStream);
  end;
end;

function TbsXMLSerializer.DeserializeFromString(AValue: string; ATypeInfo: PTypeInfo=NIL): TValue;
var
  LStream: TStringStream;
begin
  if ATypeInfo<>NIL
  then
    SetType(ATypeInfo);
  try
    LStream:=TStringStream.Create(AValue);
    Deserialize(LStream, Result);
  finally
    LStream.Free;
  end;

end;

procedure TbsXMLSerializer.SetNodeNil(ANode: IXMLNode);
begin
  if not FXSIChecked then
  begin
    if ANode.OwnerDocument.DocumentElement.FindNamespaceDecl(XMLSchemaInstNameSpace)=NIL
    then
      ANode.OwnerDocument.DocumentElement.DeclareNamespace(SXMLSchemaInstNameSpace99Pre,XMLSchemaInstNameSpace);
    FXSIChecked:=True;
  end;

  ANode.SetAttributeNS('nil',XMLSchemaInstNameSpace,'true');
end;

procedure TbsXMLSerializer.SetType(AType: PTypeInfo);
begin
  FType:=AType;
end;

procedure TbsXMLSerializer.SerializeWithNode(ANodeName: String; AObj: TValue;
  AParentNode: IXMLNode);
var
  LType         : TRttiType;
  LField        : TRttiField;
  aRecord       : TRttiRecordType;
  LValue        : TValue;
  I             : Integer;
  LNodeName,
  LNamespaceURI : String;
  LSchemaForm   : TSchemaForm;
  LChildNode,
  LFieldNode    : IXMLNode;
  LNodeType     : TNodeType;
  AsElement     : Boolean;
  LNodeValue    : string;
  LNSPrefix     : string;
  LAttrUseType  : TXmlAttributeAttributeUseType;

  function ReadNullableRecord(anObj: TValue): TValue;
  begin
    TryGetUnderlyingValue(anObj, Result);
  end;

begin
  LType        := FContext.GetType(aObj.TypeInfo);
  LNodeName    := LType.Name;
  LSchemaForm  := FElementForm;//sfUnqualified;
  LNamespaceURI:= FNamespace;

  if Length(ANodeName) > 0
  then
    LNodeName := ANodeName;

  TbsAttributeUtils.GetXMLElementAttribute(FContext, LType, LNodeName, LSchemaForm, LNamespaceURI);
  TbsAttributeUtils.GetXMLFormAttribute(FContext, LType, LSchemaForm);
  TbsAttributeUtils.GetXMLTypeAttribute(FContext, LType, LNodeName, LNamespaceURI);

  if (AObj.IsObject)
  then
    begin
      if (AObj.AsObject = NIL)
      then
        begin
          LChildNode:= AddChild(AParentNode, LNodeName, LSchemaForm);
          SetNodeNil(LChildNode);
          Exit;
        end;
    end;

  if LType.IsInstance then // Class
  begin
    // Check Namespace ...
    if TbsAttributeUtils.GetXmlNamespaceAttribute(FContext, LType, LNSPrefix, LNamespaceURI, LSchemaForm)
    then
      begin
        LNSPrefix:= AParentNode.OwnerDocument.GeneratePrefix(AParentNode);

        if (Length(LNSPrefix) = 0) or (LSchemaForm=sfQualified)
        then
          LChildNode.DeclareNamespace(LNSPrefix, LNamespaceURI);

        if Length(LNamespaceURI) > 0
        then
          LChildNode.DeclareNamespace(LNSPrefix, LNamespaceURI);
      end;

    LChildNode:=AddChild(AParentNode, LNodeName, LSchemaForm);
  end
  else if LType.TypeKind = tkRecord then // Record
  begin

    if TbsAttributeUtils.GetXmlNamespaceAttribute(FContext, LType, LNSPrefix, LNamespaceURI,LSchemaForm)then
    begin
      if Length(LNamespaceURI) > 0
      then
        LChildNode:= AddChild(AParentNode, LNodeName, LSchemaForm, LNamespaceURI, LNSPrefix);
    end
    else
      LChildNode:= AParentNode.AddChild(LNodeName);

    LValue := ReadNullableRecord(aObj);
    aRecord:= LType.AsRecord;
    LChildNode.NodeValue :=  ValueToString(aRecord.GetField('fValue').FieldType, LValue);
    Exit;
  end
  else if LType.TypeKind in [tkInteger, tkInt64, tkChar, tkEnumeration,
                             tkFloat, tkString, tkWChar, tkLString,
                             tkWString, tkVariant, tkUString, tkSet]
  then
    begin
      LChildNode:= AddChild(AParentNode, LNodeName, LSchemaForm);
      LChildNode.NodeValue:= ValueToString(LType, AObj);
      Exit;
    end
  else
    LChildNode := AParentNode;

  if TbsAttributeUtils.GetXmlNamespaceAttribute(FContext, LType, LNSPrefix, LNamespaceURI, LSchemaForm)
  then
    begin
      if Length(LNamespaceURI) > 0
      then
        LChildNode.DeclareNamespace(LNSPrefix, LNamespaceURI);
    end;


  // Class Fields...
  for LField in LType.GetFields do
  begin
    // Only Public & Published
    if not(LField.Visibility in [mvPublic, mvPublished])
    then
      Continue;

    Assert(LField.FieldType<>NIL,'aField.FieldType is NIL, reason may be Nullable(Type) like generic records.');
    LNodeValue    := '';

    case LField.FieldType.TypeKind of
      tkUnknown:
        begin

        end;
      tkInteger,
      tkInt64,
      tkChar,
      tkEnumeration,
      tkFloat,
      tkString,
      tkWChar,
      tkLString,
      tkWString,
      tkVariant,
      tkUString,
      tkSet:
        begin
          LNodeName    := LField.Name;
          LNamespaceURI:= LChildNode.NamespaceURI;
          LNodeType    := TNodeType.ntElement; // Default

          if TbsAttributeUtils.GetXmlAttributeAttribute(FContext, LField, LNodeName, LNamespaceURI,LAttrUseType)
          then
            LNodeType := ntAttribute;

          TbsAttributeUtils.GetXMLElementAttribute(FContext, LField, LNodeName, LSchemaForm, LNamespaceURI);
          TbsAttributeUtils.GetXMLFormAttribute(FContext, LField, LSchemaForm);

          if TbsAttributeUtils.HasAttribute(FContext, LField, XmlTextAttribute)
          then
            LNodeType := ntText;

          LNodeValue:= ValueToString(LField.FieldType, LField.GetValue(AObj.AsObject));

          if LNodeType = ntElement then
          begin
            LFieldNode := AddChild(LChildNode, LNodeName, LSchemaForm, LNamespaceURI);
            LFieldNode.NodeValue := LNodeValue;
          end
          else if LNodeType = ntAttribute then
          begin
            if ((LAttrUseType=autOptional) and  (LNodeValue<>'')) or (LAttrUseType=autRequired)
            then
              begin
                if LNamespaceURI<>FNamespace
                then
                  LChildNode.SetAttributeNS(LNodeName, LNamespaceURI, LNodeValue)
                else
                  LChildNode.Attributes[LNodeName]:= LNodeValue;
              end;
          end
          else if LNodeType = ntText then
          begin
            LChildNode.NodeValue:= LNodeValue;
          end;
        end;

      tkClass:
        begin
           if LField.FieldType.IsInstance then
           begin
             LNodeName:= LField.Name;

             if (LField.GetValue(aObj.AsObject).AsObject = NIL)
             then
                SetNodeNil( AddChild(LChildNode, LNodeName, LSchemaForm) )
             else
                SerializeWithNode(LNodeName, LField.GetValue(AObj.AsObject).AsObject, LChildNode);
           end;
        end;

      tkArray:
        begin

        end;
      tkDynArray:
        begin
          LNodeName := LField.Name;
          AsElement := False;

          if TbsAttributeUtils.GetXMLElementAttribute(FContext, LField, LNodeName, LSchemaForm, LNamespaceURI)
          then
            AsElement := True;

          TbsAttributeUtils.GetXMLFormAttribute(FContext, LField, LSchemaForm);

          if not AsElement then
          begin
            TbsAttributeUtils.GetXMLArrayAttribute(FContext, LField, LNodeName);
            LFieldNode:= AddChild(LChildNode, LNodeName, LSchemaForm);
            LNodeName := 'item'; // ArrayItem
            TbsAttributeUtils.GetXMLArrayItemAttribute(FContext, LField, LNodeName);
          end
          else
            LFieldNode := LChildNode;

          LValue := LField.GetValue(AObj.AsObject);

          for I := 0 to LValue.GetArrayLength - 1 do
          begin
            SerializeWithNode(LNodeName, LValue.GetArrayElement(I), LFieldNode);
          end;

        end;

      tkRecord:
        begin
          LNodeName := LField.Name;
          LNodeType := TNodeType.ntElement; // Default

          TbsAttributeUtils.GetXmlAttributeAttribute(FContext, LField, LNodeName, LNamespaceURI, LAttrUseType);
          TbsAttributeUtils.GetXMLElementAttribute(FContext, LField, LNodeName, LSchemaForm, LNamespaceURI);
          TbsAttributeUtils.GetXMLFormAttribute(FContext, LField, LSchemaForm);
          //Nullable type...
          if AnsiStartsText('Nullable<',LField.GetValue(aObj.AsObject).TypeInfo^.Name)
            then
              begin
                LValue     := ReadNullableRecord( LField.GetValue(aObj.AsObject) );
                aRecord    := LField.FieldType.AsRecord;
                LNodeValue := ValueToString(aRecord.GetField('fValue').FieldType, LValue);

                if (LNodeType = ntElement) then
                begin
                  LFieldNode:= AddChild(LChildNode, LNodeName, LSchemaForm, LNamespaceURI);
                  if (LValue.IsEmpty) then
                    LFieldNode.NodeValue := ''
                  else
                    LFieldNode.NodeValue := LNodeValue;
                end
                else if LNodeType = ntAttribute then
                begin
                  if (LValue.IsEmpty) then
                    LChildNode.Attributes[LNodeName] := ''
                  else
                    LChildNode.Attributes[LNodeName] := LNodeValue;
                end;

                if (LValue.IsEmpty) and (LNodeType = ntElement)
                then
                  SetNodeNil(LFieldNode);
              end
            else
              begin
                {
                if (LNodeType = ntElement) then
                begin
                  LFieldNode:= AddChild(LChildNode, LNodeName, LSchemaForm, LNamespaceURI);
                  if (LValue.IsEmpty) then
                    LFieldNode.NodeValue := ''
                  else
                    LFieldNode.NodeValue := LNodeValue;
                end
                else if LNodeType = ntAttribute then
                begin
                  if (LValue.IsEmpty) then
                    LChildNode.Attributes[LNodeName] := ''
                  else
                    LChildNode.Attributes[LNodeName] := LNodeValue;
                end;
                }
              end;



        end;
      // tkInterface: ;
      // tkMethod: ;
      // tkClassRef: ;
      // tkPointer: ;
      // tkProcedure: ;
    end;
  end;

end;

function TbsXMLSerializer.AddChild(AParentNode: IXMLNode;
  ANodeName: string; AForm:TSchemaForm; ANamespace:string='@'; APrefix:string=''): IXMLNode;
var
  LPrefix    : string;
  LGenPrefix : Boolean;
  LNameSpace : String;
begin
  LNameSpace:= ANamespace;
  if ANameSpace='@' then LNamespace:= FNamespace;

  {if not FDefNSAdded
  then
    begin
      Result:=AParentNode.AddChild(ANodeName,targetNamespace,True);
      if FStandalone then
      Result.DeclareNamespace(SXMLSchemaInstNameSpace99Pre, XMLSchemaInstNameSpace);

      FDefNSAdded:=True;
    end
  else} if (AForm in [sfNone, sfQualified])
  then
    begin
      if Length(APrefix)>0
      then
        begin
          Result:=AParentNode.AddChild(ANodeName, LNamespace);
          Result.DeclareNamespace(APrefix, LNamespace);
        end
      else
        begin
          Result:=AParentNode.AddChild(ANodeName, LNamespace, True);
        end;
    end
  else if (AForm=sfUnqualified)
  then
    begin
      if (FNamespace<>LNamespace)
      then
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
  FContext    := TRttiContext.Create;
  FElementForm:= sfUnqualified;
  FStandalone := True;
  FType       := NIL;
  FXSIChecked := False;
end;

procedure TbsXMLSerializer.DeSerializeWithNode(ANodeName: String; var AObj: TValue;
  AParentNode: IXMLNode);
var
  LAttribute    : TCustomAttribute;
  LType         : TRttiType;
  LField        : TRttiField;
  LRecord       : TRttiRecordType;
  LNodeName     : String;
  LNamespaceURI : String;
  LNodeType     : TNodeType;
  LChildNode,
  pNode         : IXMLNode;
  LValue        : TValue;
  AsElement     : Boolean;
  LSchemaForm   : TSchemaForm;
  LAttrUseType  : TXmlAttributeAttributeUseType;

  function XML2ObjNative(NativeNode:IXMLNode; NativeType:TRttiType; NativeNodeName:String): TValue;
  var
    NativeNodeType : TNodeType;
    NativeNS :String;
    NativeChildNode:IXmlNode;
  begin
    LNamespaceURI:='';
    // Default
    NativeNodeType := TNodeType.ntElement;

    TbsAttributeUtils.GetXmlAttributeAttribute(FContext,NativeType,NativeNodeName,NativeNS,LAttrUseType);
    TbsAttributeUtils.GetXMLElementAttribute(FContext,NativeType,NativeNodeName,LSchemaForm,NativeNS);
    TbsAttributeUtils.GetXMLFormAttribute(FContext,NativeType,LSchemaForm);
    if TbsAttributeUtils.HasAttribute(FContext,NativeType,XmlTextAttribute) then
      LNodeType := ntText;

      if NativeNode = nil then Exit;

      if NativeNodeType = ntElement then
      begin
        if NodeIsNil(NativeNode) then LValue:=nil;
        Result:= StringToValue(NativeType,NativeNode.NodeValue);
      end
      else if NativeNodeType = ntAttribute then
      begin
        NativeChildNode :=  NativeNode.AttributeNodes.FindNode(NativeNodeName);
        //if aNode.IsNil then aValue:=nil;
        //aValue:= NativeChildNode.NodeValue;
        Result:= StringToValue(NativeType,NativeChildNode.NodeValue);
      end
      else if NativeNodeType = ntText then
      begin
        //if aNode.IsNil then aValue:=nil;
        //aValue:= NativeNode.NodeValue;
        Result:= StringToValue(NativeType,NativeNode.NodeValue);
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
    if ElementArray then
    begin
      ArrayList :=TInterfaceList.Create;
      TmpNode:=ArrayNode;
      for I := 0 to ArrayNode.ParentNode.ChildNodes.Count-1 do begin
        TmpNode:=ArrayNode.ParentNode.ChildNodes[I];
        if (ArrayNode.LocalName=TmpNode.LocalName) then
          ArrayList.Add(TmpNode)
        //else
        //  Break;
      end;

      ArrayLen:=ArrayList.Count;
    end else
       ArrayLen:=ArrayNode.ChildNodes.Count;

    // TByteDynArray ...
    if (TRttiDynamicArrayType(ArrayType).ElementType.Handle = TypeInfo(Byte))
        and (ArrayLen=0) then
    begin
        S:=  ArrayNode.NodeValue;

        S:=Base64Decode(ArrayNode.NodeValue);
        TB:=TEncoding.UTF8{ANSI}.GetBytes(S);
        TValue.Make(@TB,TRttiDynamicArrayType(LType).Handle,Result);
    end else
    begin
      SetLength(ArrayValue,ArrayLen);
      for Arr := 0 to ArrayLen-1 do
      begin
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

     Result:= TValue.FromArray(ArrayType.Handle, ArrayValue);
    end;
    ArrayList.Free;
  end;

begin
  LType:= FContext.GetType(AObj.TypeInfo);

  if AObj.IsObject then
  begin
    if (aObj.AsObject =nil) then
    begin
      aObj:=LType.AsInstance.MetaclassType.Create;
    end;
  end
  else
  begin
    case LType.TypeKind of
      //tkSet: ;
      //tkClass: ;
      //tkArray: ;
      tkInteger,
      tkChar,
      tkFloat,
      tkString,
      tkWChar,
      tkLString,
      tkWString,
      tkUString,
      tkInt64,
      tkVariant,
      tkEnumeration :
        AObj:= XML2ObjNative(AParentNode, LType, ANodeName);

      tkDynArray:
        begin
          AsElement:=False;

          if TbsAttributeUtils.GetXMLElementAttribute(FContext, LType, LNodeName, LSchemaForm, LNamespaceURI)
          then
            AsElement := True;
          TbsAttributeUtils.GetXMLFormAttribute(FContext,LType,LSchemaForm);

          if not AsElement then
          begin
            TbsAttributeUtils.GetXMLArrayAttribute(FContext,LType,LNodeName);
            // ArrayItem
            LNodeName := 'item';
            TbsAttributeUtils.GetXMLArrayItemAttribute(FContext,LType,LNodeName);
          end;

          AObj:=XML2ObjDynArray(AParentNode, LType,AsElement);

        end;

      tkRecord:

        begin
          LNodeName := ANodeName;
          LNodeType := TNodeType.ntElement;

          TbsAttributeUtils.GetXmlAttributeAttribute(FContext, LType, LNodeName, LNamespaceURI,LAttrUseType);
          TbsAttributeUtils.GetXmlElementAttribute(FContext, LType, LNodeName, LSchemaForm, LNamespaceURI);

          LValue := AObj;
          LRecord:= LType.AsRecord;

          if (LNodeType = ntElement) then
          begin
            LChildNode := AParentNode;
            if   NodeIsNil( LChildNode)
            then
              LValue:=nil
            else
              TrySetUnderlyingValue(LValue, StringToValue(LRecord.GetField('fValue').FieldType,LChildNode.NodeValue));
          end
          else if LNodeType = ntAttribute then
          begin
            LChildNode := pNode.AttributeNodes.FindNode(LNodeName);
            TrySetUnderlyingValue(LValue, StringToValue(LRecord.GetField('fValue').FieldType,LChildNode.NodeValue));
          end;

        end;

    end;

  end;



  for LAttribute in LType.GetAttributes do
    if LAttribute is XmlReturnNameAttribute then
    begin
      if Length(XmlReturnNameAttribute(LAttribute).ReturnName) > 0
      then
        LNodeName:= XmlReturnNameAttribute(LAttribute).ReturnName;
    end;

    pNode:=AParentNode;

  for LField in LType.GetFields do
  begin
    if not(LField.Visibility in [mvPublic, mvPublished]) then
      Continue;

    case LField.FieldType.TypeKind of
      //tkUnknown: ;
      //tkSet: ;
      tkInteger,
      tkInt64,
      tkChar,
      tkEnumeration,
      tkFloat,
      tkString,
      tkWChar,
      tkLString,
      tkWString,
      tkVariant,
      tkUString,
      tkSet:
        begin
          LNodeName    := LField.Name;
          LNamespaceURI:= '';
          LNodeType    := TNodeType.ntElement;

          if TbsAttributeUtils.GetXmlAttributeAttribute(FContext,LField,LNodeName,LNamespaceURI,LAttrUseType)
          then
            LNodeType := ntAttribute;

          if TbsAttributeUtils.GetXmlElementAttribute(FContext, LField, LNodeName, LSchemaForm, LNamespaceURI)
          then
            LNodeType := ntElement;

          if TbsAttributeUtils.HasAttribute(FContext,LField,XmlTextAttribute)
          then
            LNodeType := ntText;

          if LNodeType = ntElement then
          begin
            LChildNode := FindElementNode(pNode,LNodeName);
            if LChildNode=NIL
            then
              LValue:=NIL
            else
              begin
                LValue:= StringToValue(LField.FieldType, LChildNode.NodeValue);
                if NodeIsNil(LChildNode) then LValue:=nil;
              end;

            LField.SetValue(aObj.AsObject,LValue);
          end
          else if LNodeType = ntAttribute then
          begin
            LChildNode := pNode.AttributeNodes.FindNode(LNodeName);
            if LChildNode=nil then LValue:=nil
            else
            LValue:= StringToValue(LField.FieldType, LChildNode.NodeValue);
            LField.SetValue(aObj.AsObject,LValue);
          end
          else if LNodeType = ntText then
          begin
            LValue:= StringToValue(LField.FieldType, LChildNode.NodeValue);
            LField.SetValue(aObj.AsObject,LValue);
          end;
        end;

      tkClass:
        begin
          LNodeName := LField.Name;

          TbsAttributeUtils.GetXMLElementAttribute(FContext,LField,LNodeName,LSchemaForm,LNamespaceURI);

          LChildNode := FindElementNode(pNode,LNodeName);
          if LChildNode=nil then
            LValue:=nil
          else if NodeIsNil(LChildNode) then
            LValue:=nil
          else
          begin
            LValue:= LField.GetValue(aObj.AsObject);
            DeSerializeWithNode(LNodeName, LValue, LChildNode);
            LField.SetValue(aObj.AsObject, LValue);
          end;

        end;
      //tkMethod: ;
      //tkArray:

      tkDynArray:
        begin
          LNodeName := LField.Name;
          AsElement := False;

          if TbsAttributeUtils.GetXMLElementAttribute(FContext,LField,LNodeName,LSchemaForm,LNamespaceURI) then
              AsElement := True;

          TbsAttributeUtils.GetXMLFormAttribute(FContext,LField,LSchemaForm);

         if not AsElement then
          begin
            TbsAttributeUtils.GetXMLArrayAttribute(FContext, LField, LNodeName);
            LNodeName := 'item';
            TbsAttributeUtils.GetXMLArrayItemAttribute(FContext, LField, LNodeName);
          end;

          LChildNode := FindElementNode( pNode,LNodeName);
          if LChildNode=nil then Continue;
          LValue:=XML2ObjDynArray(LChildNode, LField.FieldType, AsElement);
          LField.SetValue(aObj.AsObject,LValue);
        end;

      tkRecord:
        begin
          LNodeName := LField.Name;
          LNodeType := TNodeType.ntElement;

          TbsAttributeUtils.GetXmlAttributeAttribute(FContext, LField,LNodeName, LNamespaceURI, LAttrUseType);
          TbsAttributeUtils.GetXmlElementAttribute(FContext, LField, LNodeName, LSchemaForm, LNamespaceURI);

          // Type Casting
          //TValue.Make(nil, aField.FieldType.Handle,aValue);
          LValue := LField.GetValue(aObj.AsObject);
          LRecord:= LField.FieldType.AsRecord;

          if (LNodeType = ntElement) then
          begin
            LChildNode := FindElementNode(pNode,LNodeName);
            if NodeIsNil(LChildNode)
            then
              LValue:=nil
            else
              TrySetUnderlyingValue(LValue, StringToValue(LRecord.GetField('fValue').FieldType, LChildNode.NodeValue));
            LField.SetValue(aObj.AsObject,LValue);
          end
          else if LNodeType = ntAttribute then
          begin
            LChildNode:= pNode.AttributeNodes.FindNode(LNodeName);
            TrySetUnderlyingValue(LValue, StringToValue(LRecord.GetField('fValue').FieldType,LChildNode.NodeValue));
          end;

        end;
    end;
  end;
end;



end.
