object FRMDiagnosticTool: TFRMDiagnosticTool
  Left = 0
  Top = 0
  Caption = 'FRMDiagnosticTool'
  ClientHeight = 324
  ClientWidth = 735
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    735
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
    Left = 144
    Top = 8
    Width = 130
    Height = 25
    Caption = 'Start Random Hash 2'
    TabOrder = 1
    OnClick = btnRH2Click
  end
  object txtLog: TMemo
    Left = 8
    Top = 39
    Width = 719
    Height = 277
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    ExplicitWidth = 554
  end
  object btnRHC: TButton
    Left = 280
    Top = 8
    Width = 186
    Height = 25
    Caption = 'Start Random Hash 2 (Cached)'
    TabOrder = 3
    OnClick = btnRHCClick
  end
  object btnEntropy: TButton
    Left = 472
    Top = 8
    Width = 186
    Height = 25
    Caption = 'Entropy Tests'
    TabOrder = 4
    OnClick = btnEntropyClick
  end
end
