(******************************************************)
(*                                                    *)
(*                   Besa Software                    *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// Attribute Library 
/// </summary>
unit bsAttribute;

interface
uses RTTI, TypInfo;

type
  TSchemaForm=(sfNone,sfUnqualified,sfQualified);
  /// <summary>
  ///   Base attribute class
  /// </summary>
  TCustomXmlAttribute = class(TCustomAttribute)
  end;



  /// <summary>
  ///   Instructs the Serialize method of the Serializer not to serialize
  ///   the public field or public read/write property value.
  /// </summary>
  /// <exclude />
  XmlIgnoreAttribute = class(TCustomXmlAttribute)
  end;

  /// <exclude />
  XmlNullableAttribute = class(TCustomXmlAttribute)
  private
    FNullable:Boolean;
  public
    constructor Create(const Nullable:Boolean=False);
    property Nullable: Boolean read FNullable write FNullable;
  end;

  /// <summary>
  ///   Indicates to the Serializer that the member must be treated as
  ///   XML text when the class that contains it is serialized or deserialized.
  /// </summary>
  XmlTextAttribute = class(TCustomXmlAttribute)
  end;

  // [XmlNamespace]
  XmlNamespaceAttribute = class(TCustomXmlAttribute)
  private
    FNamespace: string;
    FPrefix :string;
    FForm : TSchemaForm;
  public
    constructor Create(const Namespace:string; const Prefix:string=''; Form: TSchemaForm=sfUnqualified);
    property Namespace: string read FNamespace write FNamespace;
    property Prefix: string read FPrefix write FPrefix;
  end;


  /// <summary>
  ///   <para>
  ///     Indicates that a public field or property represents an XML element
  ///     when the serializer serializes or deserializes the object that
  ///     contains it.
  ///   </para>
  ///   <para>
  ///     In contrast if an XmlElementAttribute is not applied to such a
  ///     field or property, the items in the array are encoded as a sequence
  ///     of elements, nested under an element named after the field or
  ///     property. (Use the <see cref="bsAttribute|XmlArrayAttribute" /> and
  ///     <see cref="bsAttribute|XmlArrayItemAttribute" /> attributes to
  ///     control how an array is serialized.)
  ///   </para>
  /// </summary>
  XmlElementAttribute = class(TCustomXmlAttribute)
  private
    FElementName: string;
    FForm : TSchemaForm;
    FIsNullable : Boolean;
    FNamespace: string;
  public
    constructor Create(const ElementName: string=''; const IsNullable: Boolean=False;
    const Form : TSchemaForm=sfUnqualified; const Namespace: string='');
    /// <summary>
    ///   <span>Gets or sets the name of the generated XML element.</span>
    /// </summary>
    property ElementName: string read FElementName write FElementName;

    /// <summary>
    ///   Gets or sets a value that indicates whether the element is
    ///   qualified.
    /// </summary>
    property Form : TSchemaForm read FForm write FForm;

    /// <summary>
    ///   Gets or sets a value that indicates whether the serializer must
    ///   serialize a member that is set to <span><span class="input"><span>
    ///         null</span></span></span> as an empty tag with the <span><span class="input">
    ///       xsi:nil</span></span> attribute set to <span><span class="input">
    ///       true</span></span>.
    /// </summary>
    property IsNullable:Boolean read FIsNullable write FIsNullable;

    /// <summary>
    ///   Gets or sets the namespace assigned to the XML element that
    ///   results when the class is serialized.
    /// </summary>
    property Namespace: string read FNamespace write FNamespace;
  end;

  /// <summary>
  ///  Specifies that the serializer must serialize the class as a Holder
  ///  class.
  /// </summary>
  XmlHolderAttribute = class(TCustomXmlAttribute)
  end;

  TXmlAttributeAttributeUseType=(autOptional, autRequired, autProhibited);

  /// <summary>
  ///   Specifies that the serializer must serialize the class member as an XML
  ///   attribute.
  /// </summary>
  XmlAttributeAttribute = class(TCustomXmlAttribute)
  private
    FAttributeName: string;
    FNamespace: string;
    FUse : TXmlAttributeAttributeUseType;
  public
    constructor Create(const AName: string=''; const ANamespace: string='';
      const AUse: TXmlAttributeAttributeUseType=autOptional);
    /// <summary>
    ///   Gets or sets the name of the XML attribute.
    /// </summary>
    property AttributeName: string read FAttributeName write FAttributeName;
    /// <summary>
    ///   Gets or sets the XML namespace of the XML attribute.
    /// </summary>
    property Namespace: string read FNamespace write FNamespace;
    /// <summary>
    ///   Gets or sets the use type of the XML attribute.
    /// </summary>
    property Use : TXmlAttributeAttributeUseType read FUse write FUse;
  end;

  /// <summary>
  ///   Specifies that the serializer must serialize a particular class member
  ///   as an array of XML elements.
  /// </summary>
  XmlArrayAttribute = class(TCustomXmlAttribute)
  private
    FName: string;
    FIsNullable: Boolean;
    FNamespace: string;
  public
    constructor Create(const Name: string=''; const IsNullable:Boolean=False; const Namespace: string='' );

    /// <summary>
    ///   Gets or sets the XML element name given to the serialized array.
    /// </summary>
    property ElementName: string read FName write FName;
    property IsNullable:Boolean read FIsNullable write FIsNullable;
    property Namespace: string read FNamespace write FNamespace;
  end;


  /// <summary>
  ///   Represents an attribute that specifies the derived types that the
  ///   Serializer can place in a serialized array.
  /// </summary>
  XmlArrayItemAttribute = class(TCustomXmlAttribute)
  private
    FArrayItemName: string;
    FIsNullable: Boolean;
    FNamespace: string;
  public
    constructor Create(const ArrayItemName: string=''; const IsNullable:Boolean=False; const Namespace: string='');

    /// <summary>
    ///   Gets or sets the name of the generated XML element.
    /// </summary>
    property ElementName: string read FArrayItemName write FArrayItemName;

    /// <summary>
    ///   <span>Gets or sets a value that indicates whether the <span>
    ///     Serializer</span> must serialize a member as an empty XML tag with
    ///   the <span><span class="input">xsi:nil</span></span> attribute set to <span><span class="input">
    ///       true</span></span>.</span>
    /// </summary>
    property IsNullable:Boolean read FIsNullable write FIsNullable;

    /// <summary>
    ///   <span>Gets or sets the namespace of the generated XML element.</span>
    /// </summary>
    property Namespace: string read FNamespace write FNamespace;
  end;

  /// <exclude />
  XmlChoiceIdentifierAttribute = class(TCustomXmlAttribute)
  private
    FName: string;
  public
    constructor Create(const Name: string='');
    property ElementName: string read FName write FName;
  end;

  /// <exclude />
  XmlRootAttribute = class(TCustomXmlAttribute)
  private
    FElementName: string;
    FIsNullable : Boolean;
    FNamespace: string;
  public
    constructor create(const name: string;Namespace: string='');
    property ElementName: string read FElementName write FElementName;
    property Namespace: string read FNamespace write FNamespace;
  end;

  /// <exclude />
  XmlFormAttribute = class(TCustomXmlAttribute)
  private
    FForm : TSchemaForm;
  public
    constructor Create(const Form: TSchemaForm=sfNone);
    property Form: TSchemaForm read FForm write FForm;
  end;

  /// <exclude />
  XmlReturnNameAttribute = class(TCustomXmlAttribute)
  private
    FReturnName : String;
  public
    constructor Create(const ReturnName: string);
    property ReturnName: string read FReturnName write FReturnName;
  end;


  MessageHeaderAttribute = class(TCustomXmlAttribute)
  end;

  //EXmlSerializationError = class(Exception);

  //TMemberNodeType = (ntNone, ntElement, ntAttribute);
  TCustomAttributeClass = class of TCustomAttribute;
  /// <exclude />
  TbsAttributeUtils = class(TObject)
   public
     class function HasAttribute(aType : pTypeinfo; aClass : TCustomAttributeClass) : Boolean; overload;
     class function HasAttribute(aType : pTypeinfo;aClass : TCustomAttributeClass;var Attr : TCustomAttribute) : Boolean; overload;
     class function HasAttribute(aContext : TRttiContext; aType : TRttiObject;aClass : TCustomAttributeClass) : Boolean; overload;
     class function HasAttribute(aContext : TRttiContext; aType : TRttiObject;aClass : TCustomAttributeClass;var Attr : TCustomAttribute) : Boolean; overload;
     class function HasAttributes(aType : pTypeinfo;aClass : TCustomAttributeClass;var Attrs : TArray<TCustomAttribute>) : Boolean; overload;
     class function HasAttributes(aContext : TRttiContext; aType : TRttiObject;aClass : TCustomAttributeClass;var Attrs : TArray<TCustomAttribute>) : Boolean; overload;

     class function GetXMLRootAttribute(aContext : TRttiContext; aType : TRttiObject;var NodeName:String; var NamespaceURI:String):Boolean;
     class function GetXMLElementAttribute(aContext : TRttiContext; aType : TRttiObject;var NodeName:String; var Form :TSchemaForm; var NamespaceURI:String):Boolean;
     class function GetXMLFormAttribute(aContext : TRttiContext; aType : TRttiObject;var Form :TSchemaForm):Boolean;
     class function GetXMLNamespaceAttribute(aContext : TRttiContext; aType : TRttiObject;var Prefix: String; var NamespaceURI: string; var Form: TSchemaForm):Boolean;
     class function GetXMLArrayAttribute(aContext : TRttiContext; aType : TRttiObject;var NodeName: String):Boolean;
     class function GetXMLArrayItemAttribute(aContext : TRttiContext; aType : TRttiObject;var NodeName: String):Boolean;
     class function GetXmlAttributeAttribute(aContext : TRttiContext; aType : TRttiObject;var AttributeName:String; var NamespaceURI:String; var AUseType:TXmlAttributeAttributeUseType):Boolean;


     class function GetAttribute(aType : pTypeinfo;aClass : TCustomAttributeClass) :  TCustomAttribute; overload;
     class function GetAttribute(aContext : TRttiContext; aType : TRttiObject;aClass : TCustomAttributeClass): TCustomAttribute ; overload;

     class function GetAttributes(aType : pTypeinfo;aClass : TCustomAttributeClass) :  TArray<TCustomAttribute>; overload;
     class function GetAttributes(aContext : TRttiContext; aType : TRttiObject;aClass : TCustomAttributeClass): TArray<TCustomAttribute> ; overload;
  end;

implementation

{ XmlElementAttribute }
constructor XmlElementAttribute.Create(const ElementName: string='';
  const IsNullable: Boolean=False; const Form : TSchemaForm=sfUnqualified;
  const Namespace: string='');
begin
  FElementName  := ElementName;
  FIsNullable := IsNullable;
  FForm:= Form;
  FNamespace := Namespace;
end;

{ XmlAttributeAttribute }
constructor XmlAttributeAttribute.Create(const AName: string;
  const ANamespace: string; const AUse: TXmlAttributeAttributeUseType);
begin
  FAttributeName:= AName;
  FNamespace    := ANamespace;
  FUse          := AUse;
end;

{ XmlRootAttribute }
constructor XmlRootAttribute.create(const name: string;Namespace: string='');
begin
  FElementName := name;
  FNamespace   := Namespace;
end;
{ XmlArrayAttribute }

constructor XmlArrayAttribute.Create(const Name: string=''; const IsNullable:Boolean=False; const Namespace: string='');
begin
  FName := name;
  FIsNullable:=IsNullable;
  FNamespace:=Namespace;
end;

{ XmlArrayItemAttribute }

constructor XmlArrayItemAttribute.Create(const ArrayItemName: string=''; const IsNullable:Boolean=False; const Namespace: string='');
begin
  FArrayItemName:= ArrayItemName;
  FIsNullable:=IsNullable;
  FNamespace:=Namespace;
end;

{ XmlChoiceIdentifierAttribute }

constructor XmlChoiceIdentifierAttribute.Create(const Name: string);
begin
  FName:=Name;
end;

{ XmlNamespaceAttribute }


constructor XmlNamespaceAttribute.Create(const Namespace:string; const Prefix:string='';
  Form: TSchemaForm=sfUnqualified);
begin
  FNamespace:=Namespace;
  FPrefix:=Prefix;
  FForm:=Form;
end;

{ XmlReturnNameAttribute }

constructor XmlReturnNameAttribute.Create(const ReturnName: string);
begin
  FReturnName:= ReturnName;
end;

{ TAttrUtils }

class function TbsAttributeUtils.GetAttribute(aType: pTypeinfo;
  aClass: TCustomAttributeClass): TCustomAttribute;
var
 c : TRttiContext;
begin
 c := TRttiContext.Create;
 try
   result := GetAttribute(c, c.GetType(aType),aClass);
 finally
   c.Free;
 end;
end;

class function TbsAttributeUtils.GetAttribute(aContext: TRttiContext; aType: TRttiObject;
  aClass: TCustomAttributeClass): TCustomAttribute;
var
 lAttr : TCustomAttribute;
begin
  Assert(Assigned(aType));
  for lAttr in aType.GetAttributes do
  begin
    if lAttr is aClass then begin
      exit(lAttr);
    end;
  end;
  result := nil;
end;

class function TbsAttributeUtils.GetAttributes(aContext: TRttiContext;
  aType: TRttiObject; aClass: TCustomAttributeClass): TArray<TCustomAttribute>;
var
  Attrs : TArray<TCustomAttribute>;
  lp,idx : Integer;
begin
  Assert(Assigned(aType));
  Attrs := aType.GetAttributes;
  SetLength(result,Length(Attrs));
  idx := 0;
  for lp := 0 to Length(Attrs) - 1 do begin
    if Attrs[lp] is aClass then begin
      result[idx] := Attrs[lp];
      inc(idx);
    end;
  end;
  SetLength(result,idx);
end;

class function TbsAttributeUtils.GetAttributes(aType: pTypeinfo;
  aClass: TCustomAttributeClass): TArray<TCustomAttribute>;
var
 c : TRttiContext;
begin
 c := TRttiContext.Create;
 try
   result := GetAttributes(c, c.GetType(aType),aClass);
 finally
   c.Free;
 end;
end;

class function TbsAttributeUtils.HasAttribute(aType : pTypeinfo; aClass : TCustomAttributeClass) : Boolean;
var
 c : TRttiContext;
 lAttr : TCustomAttribute;
begin
 Result:=False;
 c := TRttiContext.Create;
 try
   for lAttr in c.GetType(Atype).GetAttributes do
   begin
     if (lAttr is aClass) then begin
       exit(True);
     end;
   end;

 finally
   c.Free;
 end;

end;

class function TbsAttributeUtils.HasAttribute(aType: pTypeinfo;
  aClass: TCustomAttributeClass; var Attr: TCustomAttribute): Boolean;
var
 c : TRttiContext;
begin
 Result:=False;
 c := TRttiContext.Create;
 try
   result := HasAttribute(c, c.GetType(aType),aClass,Attr);
 finally
   c.Free;
 end;
end;

class function TbsAttributeUtils.HasAttribute(aContext: TRttiContext; aType: TRttiObject;
  aClass: TCustomAttributeClass; var Attr: TCustomAttribute): Boolean;
begin
  Result:=False;
  Attr := GetAttribute(aContext,aType,aClass);
  result := Assigned(Attr);
end;


class function TbsAttributeUtils.HasAttribute(aContext: TRttiContext;
  aType: TRttiObject; aClass: TCustomAttributeClass): Boolean;
var
  aAttribute : TCustomAttribute;
begin
  Result:=HasAttribute(aContext,aType,aClass,aAttribute);
end;

class function TbsAttributeUtils.HasAttributes(aContext: TRttiContext;
  aType: TRttiObject; aClass: TCustomAttributeClass;
  var Attrs: TArray<TCustomAttribute>): Boolean;
begin
  Attrs := GetAttributes(aContext,aType,aClass);
  result := Length(Attrs) > 0;
end;

class function TbsAttributeUtils.GetXMLArrayAttribute(aContext: TRttiContext;
  aType: TRttiObject; var NodeName: String):Boolean;
var
  aAttribute : TCustomAttribute;
begin
  Result:=False;
  if TbsAttributeUtils.HasAttribute(aContext,aType,XmlArrayAttribute,aAttribute) then
  begin
    Result:=True;
    if Length(XMLArrayAttribute(aAttribute).ElementName) > 0 then
      NodeName := XMLArrayAttribute(aAttribute).ElementName;
  end;
end;


class function TbsAttributeUtils.GetXMLArrayItemAttribute(
  aContext: TRttiContext; aType: TRttiObject; var NodeName: String):Boolean;
var
  aAttribute : TCustomAttribute;
begin
  Result:=False;
  if TbsAttributeUtils.HasAttribute(aContext,aType,XMLArrayItemAttribute,aAttribute) then
  begin
    Result:=True;
    if Length(XMLArrayItemAttribute(aAttribute).ElementName) > 0 then
      NodeName := XMLArrayItemAttribute(aAttribute).ElementName;
  end;
end;

class function TbsAttributeUtils.GetXmlAttributeAttribute(
  aContext: TRttiContext; aType: TRttiObject; var AttributeName: String;
  var NamespaceURI: String; var AUseType:TXmlAttributeAttributeUseType):Boolean;
var
  aAttribute : TCustomAttribute;
begin
  Result:=False;
  if TbsAttributeUtils.HasAttribute(aContext,aType,XmlAttributeAttribute,aAttribute) then
  begin
    Result:=True;
    if Length(XmlAttributeAttribute(aAttribute).AttributeName) > 0 then
      AttributeName := XmlAttributeAttribute(aAttribute).AttributeName;

    if Length(XmlAttributeAttribute(aAttribute).Namespace) > 0 then
      NamespaceURI := XmlAttributeAttribute(aAttribute).Namespace;

    AUseType:=XmlAttributeAttribute(aAttribute).Use;
  end;
end;

class function TbsAttributeUtils.GetXMLElementAttribute(aContext: TRttiContext;
  aType: TRttiObject; var NodeName: String; var Form: TSchemaForm;
  var NamespaceURI: String):Boolean;
var
  aAttribute : TCustomAttribute;
begin
  Result:=False;
  if TbsAttributeUtils.HasAttribute(aContext,aType,XmlElementAttribute,aAttribute) then
  begin
    Result:=True;
    if Length(XmlElementAttribute(aAttribute).ElementName) > 0 then
      NodeName := XmlElementAttribute(aAttribute).ElementName;

    Form:=XmlElementAttribute(aAttribute).Form;

    if Length(XmlElementAttribute(aAttribute).Namespace) > 0 then
      NamespaceURI:=XmlElementAttribute(aAttribute).Namespace;
  end;
end;

class function TbsAttributeUtils.GetXMLFormAttribute(aContext: TRttiContext;
  aType: TRttiObject; var Form: TSchemaForm):Boolean;
var
  aAttribute : TCustomAttribute;
begin
  Result:=False;
  if TbsAttributeUtils.HasAttribute(aContext,aType,XmlFormAttribute,aAttribute) then
  begin
    Result:=True;
    Form:=XmlFormAttribute(aAttribute).Form;
  end;
end;

class function TbsAttributeUtils.GetXMLNamespaceAttribute(
  aContext: TRttiContext; aType: TRttiObject; var Prefix, NamespaceURI: string;
  var Form: TSchemaForm):boolean;
var
  aAttribute : TCustomAttribute;
begin
  Result:=False;
  if TbsAttributeUtils.HasAttribute(aContext,aType,XmlNamespaceAttribute,aAttribute) then
  begin
    Result:=True;
    if Length(XmlNamespaceAttribute(aAttribute).Prefix) > 0 then
      Prefix := XmlNamespaceAttribute(aAttribute).Prefix;

    if Length(XmlNamespaceAttribute(aAttribute).Namespace) > 0 then
      NamespaceURI:=XmlNamespaceAttribute(aAttribute).Namespace;

    Form:=XmlElementAttribute(aAttribute).Form;
  end;
end;

class function TbsAttributeUtils.GetXMLRootAttribute(aContext: TRttiContext;
  aType: TRttiObject; var NodeName: String;
  var NamespaceURI: String): Boolean;
var
  aAttribute : TCustomAttribute;
begin
  Result:=False;
  if TbsAttributeUtils.HasAttribute(aContext,aType,XmlRootAttribute,aAttribute) then
  begin
    Result:=True;
    if Length(XmlRootAttribute(aAttribute).ElementName) > 0 then
      NodeName := XmlRootAttribute(aAttribute).ElementName;

    if Length(XmlRootAttribute(aAttribute).Namespace) > 0 then
      NamespaceURI:=XmlRootAttribute(aAttribute).Namespace;

  end;
end;

class function TbsAttributeUtils.HasAttributes(aType: pTypeinfo;
  aClass: TCustomAttributeClass; var Attrs: TArray<TCustomAttribute>): Boolean;
var
 c : TRttiContext;
begin
 c := TRttiContext.Create;
 try
   result := HasAttributes(c, c.GetType(aType),aClass,Attrs);
 finally
   c.Free;
 end;
end;


{ XmlFormAttribute }

constructor XmlFormAttribute.Create(const Form: TSchemaForm);
begin
  FForm:=Form;
end;

{ XmlNullableAttribute }

constructor XmlNullableAttribute.Create(const Nullable: Boolean=False);
begin
  FNullable:=Nullable;
end;

end.
