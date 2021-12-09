object FRMChangeLanguage: TFRMChangeLanguage
  Left = 2170
  Top = 250
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Language Selection'
  ClientHeight = 451
  ClientWidth = 374
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbxAvailableLanguages: TListBox
    Left = 0
    Top = 41
    Width = 374
    Height = 357
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    ExplicitLeft = 15
    ExplicitTop = 56
    ExplicitWidth = 337
    ExplicitHeight = 328
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 374
    Height = 41
    Align = alTop
    Caption = 'Select your preferred language'
    TabOrder = 1
    ExplicitLeft = 144
    ExplicitTop = 16
    ExplicitWidth = 185
  end
  object Panel2: TPanel
    Left = 0
    Top = 398
    Width = 374
    Height = 53
    Align = alBottom
    Caption = ' '
    TabOrder = 2
    ExplicitLeft = 8
    ExplicitTop = 390
    ExplicitWidth = 358
    object bbOk: TBitBtn
      Left = 15
      Top = 13
      Width = 75
      Height = 30
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
    end
    object bbCancel: TBitBtn
      Left = 106
      Top = 13
      Width = 75
      Height = 30
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
    end
  end
end
