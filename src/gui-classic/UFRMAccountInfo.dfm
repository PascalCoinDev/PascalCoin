object FRMAccountInfo: TFRMAccountInfo
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Account Information'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 25
    Top = 26
    Width = 39
    Height = 13
    Caption = 'Account'
  end
  object lblAccount: TLabel
    Left = 77
    Top = 21
    Width = 97
    Height = 19
    Caption = '0000000-00'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 24
    Top = 56
    Width = 47
    Height = 13
    Caption = 'Public key'
  end
  object lblBalance: TLabel
    Left = 252
    Top = 21
    Width = 97
    Height = 19
    Caption = '0000000-00'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 200
    Top = 26
    Width = 37
    Height = 13
    Caption = 'Balance'
  end
  object Label2: TLabel
    Left = 25
    Top = 91
    Width = 26
    Height = 13
    Caption = 'State'
  end
  object Label4: TLabel
    Left = 77
    Top = 86
    Width = 200
    Height = 19
    Caption = 'Normal/For sale/Private'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ebPublicKey: TEdit
    Left = 77
    Top = 51
    Width = 533
    Height = 25
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    Text = 'ebPublicKey'
  end
end
