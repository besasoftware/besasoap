object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 191
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object bSerialize: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Serialize'
    TabOrder = 0
    OnClick = bSerializeClick
  end
  object bDeserialize: TButton
    Left = 24
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Deserialize'
    TabOrder = 1
    OnClick = bDeserializeClick
  end
  object Memo1: TMemo
    Left = 120
    Top = 26
    Width = 308
    Height = 157
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
