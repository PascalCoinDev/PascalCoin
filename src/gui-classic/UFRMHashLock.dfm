object FRMHashLock: TFRMHashLock
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Atomic Swap Hash-Lock '
  ClientHeight = 270
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    548
    270)
  PixelsPerInch = 96
  TextHeight = 13
  object lblError: TLabel
    Left = 19
    Top = 239
    Width = 282
    Height = 13
    AutoSize = False
    Caption = 'Errors detected'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object btnCancel: TBitBtn
    Left = 424
    Top = 231
    Width = 116
    Height = 31
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object btnSet: TBitBtn
    Left = 307
    Top = 231
    Width = 111
    Height = 31
    Anchors = [akRight, akBottom]
    Caption = 'Set Hash-Lock'
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnSetClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 14
    Width = 532
    Height = 83
    Caption = 'Mode'
    TabOrder = 2
    object rbHashLock: TRadioButton
      Left = 11
      Top = 25
      Width = 446
      Height = 17
      Caption = 
        'Hash-Lock - the 32 byte hash-lock provided to you by the counter' +
        'party'
      TabOrder = 0
    end
    object rbHashLockKey: TRadioButton
      Left = 11
      Top = 48
      Width = 478
      Height = 17
      Caption = 
        'Hash-Lock Key - a secret that will be hashed to create the 32 by' +
        'te hash-lock'
      TabOrder = 1
      WordWrap = True
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 103
    Width = 532
    Height = 122
    Caption = 'Data'
    TabOrder = 3
    DesignSize = (
      532
      122)
    object Label2: TLabel
      Left = 11
      Top = 21
      Width = 266
      Height = 13
      Caption = 'Please enter the hash lock data as a hexadecimal string'
    end
    object meHashLockData: TMemo
      Left = 11
      Top = 40
      Width = 505
      Height = 67
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      WantReturns = False
    end
  end
end
