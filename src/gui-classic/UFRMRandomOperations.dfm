object FRMRandomOperations: TFRMRandomOperations
  Left = 876
  Top = 391
  Caption = 'Random Operations'
  ClientHeight = 229
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 439
    Height = 66
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 0
    object lblTopCaption: TLabel
      Left = 10
      Top = 10
      Width = 419
      Height = 46
      Align = alClient
      AutoSize = False
      Caption = 
        'WARNING! This form will generate RANDOM operations using your pr' +
        'ivate keys!!!'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
    end
  end
  object pnlClient: TPanel
    Left = 0
    Top = 66
    Width = 439
    Height = 163
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlTop1: TPanel
      Left = 0
      Top = 0
      Width = 439
      Height = 42
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object bbRandomOperations: TButton
        Left = 16
        Top = 8
        Width = 195
        Height = 25
        Caption = 'Random Operations'
        TabOrder = 0
        OnClick = bbRandomOperationsClick
      end
    end
    object mLogs: TMemo
      Left = 0
      Top = 42
      Width = 439
      Height = 121
      Align = alClient
      Lines.Strings = (
        'mLogs')
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
    end
  end
end
