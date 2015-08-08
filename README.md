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
You can serialize your object.

```pascal
type
TNativeTest=class
   ShortInt_:ShortInt;
   DateTime:TDateTime;
   Str2:NullableString;
   ArrayString_: array of string;
end;

procedure TForm1.bSerialize(Sender: TObject);
var
  xmldoc: IXMLDocument;
  serializer : TbsXMLSerializer;
  clazz: TNativeTest;
  xml:string;
begin
  xmldoc:=TXMLDocument.Create(NIL);
  xmldoc.Active:=TRUE;
  serializer:=TbsXMLSerializer.Create;

  clazz:=TNativeTest.Create;

   clazz.ShortInt_:=1;
   clazz.DateTime:=Now;
   clazz.Str2:='NullableStr';

   SetLength(clazz.ArrayString_,3);
   clazz.ArrayString_[0]:='One';
   clazz.ArrayString_[1]:='Two';
   clazz.ArrayString_[2]:='Three';

  xmldoc.AddChild('Test');
  xml:=serializer.SerializeToString(clazz);
  clazz.Free;
  serializer.Free;
  xmldoc:=NIL;
end;
```
Generated xml:
```xml
<NS1:TNativeTest>
	<ShortInt_>1</ShortInt_>
	<DateTime>2015-08-08T22:34:34.676Z</DateTime>
	<Str2>NullableStr</Str2>
	<ArrayString_>
		<item>One</item>
		<item>Two</item>
		<item>Three</item>
	</ArrayString_>
</NS1:TNativeTest>
```

###SOAP 
Sample usage for soap sample. You can find full project in demos folder.

For importing wsdl you need *buy* the BesaSoap Tool from  http://www.besasoftware.com

Generated Unit
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
