object FRMMemoText: TFRMMemoText
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Information'
  ClientHeight = 428
  ClientWidth = 745
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 373
    Width = 745
    Height = 55
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 245
    ExplicitWidth = 619
    DesignSize = (
      745
      55)
    object bbCancel: TBitBtn
      Left = 620
      Top = 14
      Width = 116
      Height = 31
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkCancel
      ParentDoubleBuffered = False
      TabOrder = 0
      ExplicitLeft = 494
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 745
    Height = 373
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'Memo')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    ExplicitWidth = 619
    ExplicitHeight = 245
  end
end
