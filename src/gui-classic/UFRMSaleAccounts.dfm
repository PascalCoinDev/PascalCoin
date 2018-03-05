object FRMSaleAccounts: TFRMSaleAccounts
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Sale accounts'
  ClientHeight = 312
  ClientWidth = 604
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblAccountCaption: TLabel
    Left = 25
    Top = 20
    Width = 43
    Height = 13
    Caption = 'Account:'
  end
  object Label1: TLabel
    Left = 25
    Top = 95
    Width = 50
    Height = 13
    Caption = 'Sale price:'
  end
  object Label2: TLabel
    Left = 202
    Top = 95
    Width = 87
    Height = 13
    Caption = 'Locked until block:'
  end
  object Label3: TLabel
    Left = 25
    Top = 119
    Width = 142
    Height = 13
    Caption = 'Seller account (where to pay)'
  end
  object lblAccountBalance: TLabel
    Left = 432
    Top = 15
    Width = 149
    Height = 19
    Caption = 'lblAccountBalance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblAccountsCount: TLabel
    Left = 387
    Top = 58
    Width = 18
    Height = 13
    Caption = 'XXX'
  end
  object lblBalanceCaption: TLabel
    Left = 387
    Top = 20
    Width = 41
    Height = 13
    Caption = 'Balance:'
  end
  object memoAccounts: TMemo
    Left = 74
    Top = 12
    Width = 307
    Height = 59
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Lines.Strings = (
      '123456-11 (0.0000); 123456-11 (0.0000); '
      '123456-11 (0.0000); 123456-11 (0.0000); ')
    ParentCtl3D = False
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ebSenderAccount: TEdit
    Left = 74
    Top = 12
    Width = 97
    Height = 27
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Text = 'ebSenderAccount'
  end
  object ebSalePrice: TEdit
    Left = 81
    Top = 92
    Width = 86
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object ebLockedUntilBlock: TEdit
    Left = 295
    Top = 92
    Width = 86
    Height = 21
    TabOrder = 3
    Text = 'ebLockedUntilBlock'
  end
  object gbSellOptions: TGroupBox
    Left = 25
    Top = 150
    Width = 516
    Height = 101
    Caption = ' Sell options: '
    TabOrder = 4
    object lblNewOwnerPublicKey: TLabel
      Left = 25
      Top = 66
      Width = 111
      Height = 13
      Caption = 'New owner'#39's public key'
    end
    object rbEveryoneCanBuy: TRadioButton
      Left = 10
      Top = 20
      Width = 113
      Height = 17
      Caption = 'Everyone can buy'
      TabOrder = 0
    end
    object rbReservedForAPublickKey: TRadioButton
      Left = 10
      Top = 43
      Width = 301
      Height = 17
      Caption = 'Reserved for only one public key (private)'
      TabOrder = 1
    end
    object ebNewOwnerPublicKey: TEdit
      Left = 148
      Top = 63
      Width = 331
      Height = 21
      TabOrder = 2
      Text = 'ebDestAccount'
    end
  end
  object ebSellerAccount: TEdit
    Left = 181
    Top = 116
    Width = 86
    Height = 21
    TabOrder = 5
    Text = 'Edit1'
  end
  object bbExecute: TBitBtn
    Left = 277
    Top = 261
    Width = 130
    Height = 31
    Action = actExecute
    Caption = 'Execute (F12)'
    DoubleBuffered = True
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
    ParentDoubleBuffered = False
    TabOrder = 6
  end
  object bbCancel: TBitBtn
    Left = 425
    Top = 261
    Width = 116
    Height = 31
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 7
  end
  object ActionList: TActionList
    Left = 140
    Top = 350
    object actExecute: TAction
      Caption = 'Execute (F12)'
      ShortCut = 123
      OnExecute = actExecuteExecute
    end
  end
end
