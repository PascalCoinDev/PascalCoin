object FRMNodesIp: TFRMNodesIp
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Nodes IP'
  ClientHeight = 411
  ClientWidth = 389
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
  object memoNodesIp: TMemo
    Left = 0
    Top = 23
    Width = 389
    Height = 326
    Align = alClient
    Lines.Strings = (
      'memoNodesIp')
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitTop = 29
    ExplicitWidth = 352
    ExplicitHeight = 261
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 389
    Height = 23
    Align = alTop
    Caption = 'Available Nodes IP to connect:'
    TabOrder = 1
    ExplicitWidth = 352
  end
  object Panel2: TPanel
    Left = 0
    Top = 349
    Width = 389
    Height = 62
    Align = alBottom
    Caption = ' '
    TabOrder = 2
    ExplicitTop = 290
    ExplicitWidth = 352
    object bbOk: TBitBtn
      Left = 9
      Top = 29
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
      TabOrder = 0
      OnClick = bbOkClick
    end
    object bbCancel: TBitBtn
      Left = 90
      Top = 29
      Width = 75
      Height = 25
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
    end
    object cbTryOnlyWithThisServers: TCheckBox
      Left = 9
      Top = 7
      Width = 271
      Height = 17
      Caption = 'Try to connect ONLY with this servers'
      TabOrder = 2
      OnClick = cbTryOnlyWithThisServersClick
    end
  end
end
