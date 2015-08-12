unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, bsSOAPClient,
  bsTransporter, bsIndyTransporter, FMX.StdCtrls, FMX.ListBox,
  FMX.Controls.Presentation, CurrencyConvertor;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    cbFromCurrency: TComboBox;
    cbToCurrency: TComboBox;
    btnConversionRate: TButton;
    lblResult: TLabel;
    bsIndyTransporter1: TbsIndyTransporter;
    bsCurrency: TbsService;
    procedure btnConversionRateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnConversionRateClick(Sender: TObject);
var
  Service : CurrencyConvertorSoap;
  res: double;
begin
  Service := GetCurrencyConvertorSoap('', bsCurrency);
  res:=Service.ConversionRate(cbFromCurrency.Selected.Text,cbToCurrency.Selected.Text);
  lblResult.Text:='Rate :'+FloatToStr(res)
end;


end.
