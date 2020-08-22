object FRMChangeLanguage: TFRMChangeLanguage
  Left = 2170
  Height = 451
  Top = 250
  Width = 374
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Language Selection'
  ClientHeight = 451
  ClientWidth = 374
  OnCreate = FormCreate
  Position = poOwnerFormCenter
    object bbOk: TBitBtn
    Left = 184
    Height = 30
    Top = 400
    Width = 75
    Caption = 'OK'
    Kind = bkOK
    ModalResult = 1
    TabOrder = 0
  end
  object bbCancel: TBitBtn
    Left = 274
    Height = 30
    Top = 400
    Width = 75
    Caption = 'Cancel'
    Kind = bkCancel
    ModalResult = 2
    TabOrder = 1
  end
  object lbxAvailableLanguages: TListBox
    Left = 15
    Height = 328
    Top = 56
    Width = 337
    ItemHeight = 0
    TabOrder = 2
  end
  object Label1: TLabel
    Left = 16
    Height = 15
    Top = 24
    Width = 161
    Caption = 'Select your preferred language'
    ParentColor = False
  end
end
