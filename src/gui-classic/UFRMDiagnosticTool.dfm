object FRMDiagnosticTool: TFRMDiagnosticTool
  Left = 0
  Top = 0
  Caption = 'FRMDiagnosticTool'
  ClientHeight = 324
  ClientWidth = 666
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    666
    324)
  PixelsPerInch = 96
  TextHeight = 13
  object btnRH: TButton
    Left = 8
    Top = 8
    Width = 130
    Height = 25
    Caption = 'Start Random Hash'
    TabOrder = 0
    OnClick = btnRHClick
  end
  object btnRH2: TButton
    Left = 336
    Top = 8
    Width = 130
    Height = 25
    Caption = 'Start Random Hash 2'
    TabOrder = 1
    OnClick = btnRH2Click
  end
  object txtLog: TMemo
    Left = 8
    Top = 80
    Width = 650
    Height = 236
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
  object btnRH2C: TButton
    Left = 472
    Top = 8
    Width = 186
    Height = 25
    Caption = 'Start Random Hash 2 (Cached)'
    TabOrder = 3
    OnClick = btnRH2CClick
  end
  object btnEntropy: TButton
    Left = 327
    Top = 39
    Width = 130
    Height = 25
    Caption = 'Entropy Tests'
    TabOrder = 4
    OnClick = btnEntropyClick
  end
  object btnRH2NonceScan: TButton
    Left = 8
    Top = 39
    Width = 186
    Height = 25
    Caption = 'Start Random Hash 2 (Nonce Scan)'
    TabOrder = 5
    OnClick = btnRH2NonceScanClick
  end
  object txtScanLevel: TEdit
    Left = 200
    Top = 39
    Width = 121
    Height = 21
    TabOrder = 6
    TextHint = 'Enter Scan Level (2-4)'
  end
  object btnRHC: TButton
    Left = 144
    Top = 8
    Width = 177
    Height = 25
    Caption = 'Start Random Hash (Cached)'
    TabOrder = 7
    OnClick = btnRHCClick
  end
end
