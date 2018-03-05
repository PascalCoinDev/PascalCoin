object FRMPayloadDecoder: TFRMPayloadDecoder
  Left = 0
  Top = 0
  ActiveControl = ebOphash
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Operation Information/Decoder'
  ClientHeight = 406
  ClientWidth = 674
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
  object Label1: TLabel
    Left = 20
    Top = 56
    Width = 46
    Height = 13
    Caption = 'Block/Op:'
  end
  object lblBlock: TLabel
    Left = 72
    Top = 51
    Width = 109
    Height = 19
    Caption = '000000/0000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblDateTime: TLabel
    Left = 255
    Top = 51
    Width = 30
    Height = 19
    Caption = '000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 195
    Top = 56
    Width = 52
    Height = 13
    Caption = 'Date Time:'
  end
  object Label2: TLabel
    Left = 20
    Top = 106
    Width = 52
    Height = 13
    Caption = 'Operation:'
  end
  object lblOperationTxt: TLabel
    Left = 90
    Top = 101
    Width = 561
    Height = 22
    AutoSize = False
    Caption = '000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object lblAmountCaption: TLabel
    Left = 220
    Top = 81
    Width = 41
    Height = 13
    Caption = 'Amount:'
  end
  object lblAmount: TLabel
    Left = 270
    Top = 76
    Width = 30
    Height = 19
    Caption = '000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblFeeCaption: TLabel
    Left = 517
    Top = 56
    Width = 22
    Height = 13
    Caption = 'Fee:'
  end
  object lblFee: TLabel
    Left = 547
    Top = 51
    Width = 30
    Height = 19
    Caption = '000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 20
    Top = 24
    Width = 38
    Height = 13
    Caption = 'OpHash'
  end
  object lblSenderCaption: TLabel
    Left = 20
    Top = 81
    Width = 38
    Height = 13
    Caption = 'Sender:'
  end
  object lblSender: TLabel
    Left = 64
    Top = 76
    Width = 97
    Height = 19
    Caption = '9999999-99'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblReceiverCaption: TLabel
    Left = 411
    Top = 81
    Width = 46
    Height = 13
    Caption = 'Receiver:'
  end
  object lblReceiver: TLabel
    Left = 463
    Top = 76
    Width = 30
    Height = 19
    Caption = '000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblReceiverInfo: TLabel
    Left = 481
    Top = 50
    Width = 121
    Height = 19
    Caption = '(Receiver info)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object PageControl: TPageControl
    Left = 20
    Top = 140
    Width = 631
    Height = 206
    ActivePage = tsDecoded
    TabOrder = 2
    OnChanging = PageControlChanging
    object tsDecoded: TTabSheet
      Caption = 'Payload'
      object Label7: TLabel
        Left = 15
        Top = 94
        Width = 87
        Height = 13
        Caption = 'Decoded Payload:'
      end
      object lblDecodedMethod: TLabel
        Left = 562
        Top = 94
        Width = 44
        Height = 13
        Alignment = taRightJustify
        Caption = '(Method)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label3: TLabel
        Left = 15
        Top = 9
        Width = 155
        Height = 13
        Caption = 'Original Payload in Hexadecimal:'
      end
      object memoDecoded: TMemo
        Left = 15
        Top = 111
        Width = 591
        Height = 55
        TabStop = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Lines.Strings = (
          'memoDecoded')
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
        OnKeyDown = memoDecodedKeyDown
      end
      object memoOriginalPayloadInHexa: TMemo
        Left = 15
        Top = 28
        Width = 591
        Height = 55
        TabStop = False
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Lines.Strings = (
          'memoDecoded')
        ParentFont = False
        ReadOnly = True
        TabOrder = 1
        OnKeyDown = memoDecodedKeyDown
      end
      object cbShowAsHexadecimal: TCheckBox
        Left = 115
        Top = 93
        Width = 136
        Height = 17
        Caption = 'Show as hexadecimal'
        TabOrder = 2
        OnClick = cbShowAsHexadecimalClick
      end
    end
    object tsDecodeMethods: TTabSheet
      Caption = 'Decode methods'
      ImageIndex = 1
      object lblPasswordsInfo: TLabel
        Left = 235
        Top = 162
        Width = 81
        Height = 13
        Caption = 'lblPasswordsInfo'
      end
      object cbMethodPublicPayload: TCheckBox
        Left = 16
        Top = 15
        Width = 225
        Height = 17
        Caption = 'Not encrypted (Public payload)'
        TabOrder = 0
        OnClick = cbMethodPublicPayloadClick
      end
      object cbUsingPrivateKeys: TCheckBox
        Left = 16
        Top = 38
        Width = 225
        Height = 17
        Caption = 'Using Private keys of my Wallet'
        TabOrder = 1
        OnClick = cbMethodPublicPayloadClick
      end
      object cbUsingPasswords: TCheckBox
        Left = 16
        Top = 61
        Width = 225
        Height = 17
        Caption = 'Using Passwords of the list'
        TabOrder = 2
        OnClick = cbMethodPublicPayloadClick
      end
      object memoPasswords: TMemo
        Left = 235
        Top = 10
        Width = 371
        Height = 146
        ScrollBars = ssBoth
        TabOrder = 3
        OnChange = cbMethodPublicPayloadClick
      end
      object bbSaveMethods: TBitBtn
        Left = 60
        Top = 110
        Width = 75
        Height = 25
        Caption = 'Save'
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
        TabOrder = 4
        OnClick = bbSaveMethodsClick
      end
    end
  end
  object bbClose: TBitBtn
    Left = 572
    Top = 362
    Width = 79
    Height = 25
    Cancel = True
    Caption = 'Close'
    DoubleBuffered = True
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      555555555555555555555555555555555555555555FF55555555555559055555
      55555555577FF5555555555599905555555555557777F5555555555599905555
      555555557777FF5555555559999905555555555777777F555555559999990555
      5555557777777FF5555557990599905555555777757777F55555790555599055
      55557775555777FF5555555555599905555555555557777F5555555555559905
      555555555555777FF5555555555559905555555555555777FF55555555555579
      05555555555555777FF5555555555557905555555555555777FF555555555555
      5990555555555555577755555555555555555555555555555555}
    ModalResult = 2
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object bbFind: TBitBtn
    Left = 599
    Top = 19
    Width = 52
    Height = 25
    Caption = '&Find'
    DoubleBuffered = True
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
      BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
      2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
      00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
      B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
      EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
      FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
      C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
      FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
      E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
      C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
      C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
      DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
      86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
    ParentDoubleBuffered = False
    TabOrder = 1
    TabStop = False
    OnClick = bbFindClick
  end
  object ebOphash: TEdit
    Left = 64
    Top = 20
    Width = 529
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Text = '12345678901234567890123456789012345678901234567890123456789012'
    OnExit = ebOphashExit
    OnKeyPress = ebOphashKeyPress
  end
end
