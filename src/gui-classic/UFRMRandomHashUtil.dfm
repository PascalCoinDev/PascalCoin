object FRMDiagnosticTool: TFRMDiagnosticTool
  Left = 0
  Top = 0
  Caption = 'FRMDiagnosticTool'
  ClientHeight = 324
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    570
    324)
  PixelsPerInch = 96
  TextHeight = 13
  object txtLog: TEdit
    Left = 8
    Top = 39
    Width = 554
    Height = 21
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    Text = 'txtLog'
  end
  object btnRH: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Random Hash'
    TabOrder = 1
  end
  object btnRH2: TButton
    Left = 119
    Top = 8
    Width = 98
    Height = 25
    Caption = 'Random Hash 2'
    TabOrder = 2
  end
end
