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
