# BesaSoap
BesaSoap is written using new features of the Delphi language, represents C# or Java like native class support, nullable data types and custom attributes. The BesaSoap library is designed to help programmers develop faster and more native web service client applications. 

BesaSoap is running Delphi XE2 and Up.

##Installation
Open your releated delphi package then compile and install.

##

###Nullable Types

  *  NullableString
  *  NullableWideString
  *  NullableAnsiString
  *  NullableBoolean
  *  NullableByte
  *  NullableCardinal
  *  NullableChar
  *  NullableCurrency
  *  NullableDateTime
  *  NullableDouble
  *  NullableExtended
  *  NullableGuid
  *  NullableInt64
  *  NullableInteger
  *  NullableNativeInt
  *  NullableShortInt
  *  NullableSingle
  *  NullableWord
  
Sample usage:
```pascal
var
  Str:string;
  StrN:NullableString;
begin
  Str:='test';
  StrN:='Nullable test';
  Str:=StrN+' append';
  StrN:=nil;
end;
```
###Serialization & Deserialization

With bsXMLSerializer you can serialize & deserialize your object.

```pascal
type
  ArrayOfString=array of String;

  [XmlRootAttribute('native_test','http://www.besasoftware.com')]
  TNativeTest=class
   [XmlElementAttribute('ShortInt')]
   ShortInt_:ShortInt;
   [XmlAttributeAttribute('SmallInt')]
   SmallInt_:SmallInt;
   LongInt_:LongInt;
   Integer_:Integer;
   Int64_:Int64;
   Byte_:Byte;
   Word_:Word;
   LongWord_:LongWord;
   Cardinal_:Cardinal;
   UInt64_:UInt64;
   //
   Single_:Single;
   Double_:Double;
   Real_:Real;
   Extended_:Extended;
   Comp_:Comp;
   Currency_:Currency;
   //
   Char_ : Char;
   WideChar_ : WideChar;
   AnsiChar_ : AnsiChar;
   ShortString_ : ShortString;
   String_ : String;
   //
   Date_:TDate;
   Time_:TTime;
   TDateTime_:TDateTime;
   //
   NlbString:NullableString;
   NullString:NullableString;
   NlbInteger:NullableInteger;
   NullInteger:NullableInteger;
   //
   [XmlArrayAttribute('ArrayString')]
   ArrayString_: array of string;
   ArrayOfString_:ArrayOfString;
end;
```
####Serialization
```pascal
procedure TForm1.bSerializeClick(Sender: TObject);
var
  serializer : TbsXMLSerializer;
  clazz: TNativeTest;
  xml:string;
begin
  serializer:=TbsXMLSerializer.Create;
  clazz:=TNativeTest.Create;

   clazz.ShortInt_:=1;
   clazz.SmallInt_:=2;
   clazz.LongInt_:=3;
   clazz.Integer_:=4;
   clazz.Int64_:=5;
   clazz.Byte_:=6;
   clazz.Word_:=7;
   clazz.LongWord_:=8;
   clazz.Cardinal_:=9;
   clazz.UInt64_:=10;

   clazz.Single_:=1.0001;
   clazz.Double_:=1.0002;
   clazz.Real_:=1.0003;
   clazz.Extended_:=1.0004;
   clazz.Comp_:=1.05;
   clazz.Currency_:=1.0006;
   //
   clazz.Char_ :='A';
   clazz.WideChar_ :='B';
   clazz.AnsiChar_ :='C';
   clazz.ShortString_ :='ABCD';
   clazz.String_ :='defgh';
   //
   clazz.Date_:=Date;
   clazz.Time_:=Time;
   clazz.TDateTime_:=Now;
   //
   clazz.NlbString:='NullableString';
   clazz.NullString:=nil;
   clazz.NlbInteger:=2015;
   clazz.NullInteger:=nil;
   //

   SetLength(clazz.ArrayString_,3);
   clazz.ArrayString_[0]:='One';
   clazz.ArrayString_[1]:='Two';
   clazz.ArrayString_[2]:='Three';
   SetLength(clazz.ArrayOfString_,3);
   clazz.ArrayOfString_[0]:='OneOf';
   clazz.ArrayOfString_[1]:='TwoOf';
   clazz.ArrayOfString_[2]:='ThreeOf';


  xml:=serializer.SerializeToString(clazz);
  Memo1.Lines.Text:=xml;
  clazz.Free;
  serializer.Free;
end;
```
Generated xml:
```xml
<NS1:native_test xmlns:NS1="http://www.besasoftware.com" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" SmallInt="2">
	<ShortInt>1</ShortInt>
	<LongInt_>3</LongInt_>
	<Integer_>4</Integer_>
	<Int64_>5</Int64_>
	<Byte_>6</Byte_>
	<Word_>7</Word_>
	<LongWord_>8</LongWord_>
	<Cardinal_>9</Cardinal_>
	<UInt64_>10</UInt64_>
	<Single_>1.00010001659393</Single_>
	<Double_>1.0002</Double_>
	<Real_>1.0003</Real_>
	<Extended_>1.0004</Extended_>
	<Comp_>1</Comp_>
	<Currency_>1.0006</Currency_>
	<Char_>A</Char_>
	<WideChar_>B</WideChar_>
	<AnsiChar_>C</AnsiChar_>
	<ShortString_>ABCD</ShortString_>
	<String_>defgh</String_>
	<Date_>2015-08-09T00:00:00Z</Date_>
	<Time_>1899-12-30T10:21:29.58Z</Time_>
	<TDateTime_>2015-08-09T10:21:29.58Z</TDateTime_>
	<NlbString>NullableString</NlbString>
	<NullString xsi:nil="true"/>
	<NlbInteger>2015</NlbInteger>
	<NullInteger xsi:nil="true"/>
	<ArrayString>
		<item>One</item>
		<item>Two</item>
		<item>Three</item>
	</ArrayString>
	<ArrayOfString_>
		<item>OneOf</item>
		<item>TwoOf</item>
		<item>ThreeOf</item>
	</ArrayOfString_>
</NS1:native_test>
```
####Deserialization
```pascal
var
  serializer : TbsXMLSerializer;
  clazz: TNativeTest;
  xml:string;
begin
  serializer:=TbsXMLSerializer.Create;
  serializer.SetType(TypeInfo(TNativeTest));
  clazz:=serializer.DeserializeFromString(Memo1.Lines.Text).AsType<TNativeTest>;
  Memo1.Lines.Append('clazz.NlbString:'+ clazz.NlbString);
  Memo1.Lines.Append('clazz.NullString.HasValue:'+ BoolToStr(clazz.NullString.HasValue,true));
  clazz.Free;
  serializer.Free;
end;
```
###Attribute Types

Id|Attribute|Description
-|---------|-----------
1|XmlElementAttribute|Indicates that a public field or property represents an XML element when the serializer serializes or deserializes the object that contains it. In contrast if an XmlElementAttribute is not applied to such a field or property, the items in the array are encoded as a sequence of elements, nested under an element named after the field or property.
2|XmlAttributeAttribute|Specifies that the serializer must serialize the class member as an XML,attribute.
3|XmlTextAttribute|Indicates to the Serializer that the member must be treated as,XML,text when the class that contains it is serialized or deserialized.
4|XmlArrayAttribute|Specifies that the serializer must serialize a particular class member,as an array of XML elements.
5|XmlArrayItemAttribute|Represents an attribute that specifies the derived types that the Serializer can place in a serialized array.
6|XmlHolderAttribute|Specifies that the serializer must serialize the class as a Holder class. |
 
###SOAP 
####Auhentication Types
BesaSoap, supports basic authentication and UsernameToken authentication. 

####Generate WSDL proxy code

For importing wsdl you need the BesaSoap Importer Tool. You can download trial from  http://www.besasoftware.com

Sample Generated Unit
```pascal
// ************************************************************************ //
// Generated with BesaSoap WSDL Importer - http://www.besasoftware.com
// WSDL Address :http://www.webservicex.net/CurrencyConvertor.asmx?WSDL
// Date Time :8.8.2015 20:08:14
// Importer Version  :1.01.113
// ************************************************************************ //
unit CurrencyConvertor;

interface
uses
  bsSOAPClient, bsNullable, bsAttribute;

type

  // ************************************************************************ //
  // Namespace : http://www.webserviceX.NET/
  // binding   : CurrencyConvertorSoap
  // service   : CurrencyConvertor
  // port      : CurrencyConvertorSoap
  // URL       : http://www.webservicex.net/CurrencyConvertor.asmx
  // ************************************************************************ //
  CurrencyConvertorSoap = interface(IInvokable)
  ['{A927F398-6AD1-4362-9A25-5D4775730267}']
    function ConversionRate(FromCurrency: string; ToCurrency: string): Double; stdcall;
  end;

function GetCurrencyConvertorSoap(Addr: string; BSSERVICE: TbsService): CurrencyConvertorSoap;


implementation

uses SysUtils;

function GetCurrencyConvertorSoap(Addr: string; BSSERVICE: TbsService): CurrencyConvertorSoap;
const
  defURL  = 'http://www.webservicex.net/CurrencyConvertor.asmx';
  defSvc  = 'CurrencyConvertor';
  defPrt  = 'CurrencyConvertorSoap';
  defNS   = 'http://www.webserviceX.NET/';
var
  Service : TbsService;
begin
  Result := nil;
  if (Addr = '') then Addr := defURL;
  if BSSERVICE = nil then
    Service:= TbsService.Create(nil)
  else
    Service := BSSERVICE;

  Service.URL:= defURL;
  Service.ElementForm:=sfQualified;
  Service.RegisterInterface( TypeInfo(CurrencyConvertorSoap), defNS);
  Service.RegisterSoapAction('%OperationName%','http://www.webserviceX.NET/%OperationName%');
  Result := (Service as CurrencyConvertorSoap);
end;


end.
```
Using soap methods:
```pascal
procedure TForm1.btnConversionRateClick(Sender: TObject);
var
  Service : CurrencyConvertorSoap;
  res: double;
begin
  Service := GetCurrencyConvertorSoap('', bsCurrency);
  res:=Service.ConversionRate(cbFromCurrency.Text,cbToCurrency.Text);
  lblResult.Caption:='Rate :'+FloatToStr(res)
end;
```
