unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,XMLDoc,XMLIntf,
  bsSerializer,bsAttribute,bsNullable;

type
  TForm1 = class(TForm)
    bSerialize: TButton;
    bDeserialize: TButton;
    Memo1: TMemo;
    procedure bSerializeClick(Sender: TObject);
    procedure bDeserializeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

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
   NlbString:Nullable<String>;
   NullString:Nullable<String>;
   NlbInteger:Nullable<Integer>;
   NullInteger:Nullable<Integer>;
   //
   [XmlArrayAttribute('ArrayString')]
   ArrayString_: array of string;
   ArrayOfString_:ArrayOfString;
end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.bDeserializeClick(Sender: TObject);
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

end.
