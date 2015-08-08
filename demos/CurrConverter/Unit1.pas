unit Unit1;
//
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,CurrencyConvertor, Vcl.StdCtrls,
  bsTransporter, bsSOAPClient, bsIndyTransporter;

type
  TForm1 = class(TForm)
    cbFromCurrency: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    cbToCurrency: TComboBox;
    lblResult: TLabel;
    btnConversionRate: TButton;
    bsCurrency: TbsService;
    bsIndyTransporter1: TbsIndyTransporter;
    procedure btnConversionRateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnConversionRateClick(Sender: TObject);
var
  Service : CurrencyConvertorSoap;
  res: double;
begin
  Service := GetCurrencyConvertorSoap('', bsCurrency);
  res:=Service.ConversionRate(cbFromCurrency.Text,cbToCurrency.Text);
  lblResult.Caption:='Rate :'+FloatToStr(res)
end;

end.
