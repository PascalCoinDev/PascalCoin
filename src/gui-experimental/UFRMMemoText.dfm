object FRMMemoText: TFRMMemoText
  Left = 410
  Height = 428
  Top = 234
  Width = 745
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Information'
  ClientHeight = 428
  ClientWidth = 745
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  OnResize = FormResize
  Position = poOwnerFormCenter
  object pnlPrompt: TPanel
    Left = 0
    Top = 0
    Width = 745
    Height = 110
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object lblPrompt: TLabel
      Left = 12
      Top = 12
      Width = 721
      Height = 86
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = ''
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Height = 55
    Top = 373
    Width = 745
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object bbCancel: TBitBtn
      Left = 620
      Height = 31
      Top = 14
      Width = 116
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      Kind = bkCancel
      ModalResult = 2
      TabOrder = 0
    end
  end
  object Memo: TMemo
    Left = 0
    Height = 373
    Top = 0
    Width = 745
    Align = alClient
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Lines.Strings = (
      'Memo'
    )
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
end
