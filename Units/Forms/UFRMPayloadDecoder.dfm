object FRMPayloadDecoder: TFRMPayloadDecoder
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Payload Decoder'
  ClientHeight = 360
  ClientWidth = 569
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
    Top = 20
    Width = 28
    Height = 13
    Caption = 'Block:'
  end
  object lblBlock: TLabel
    Left = 54
    Top = 15
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
  object lblDateTime: TLabel
    Left = 169
    Top = 15
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
    Left = 111
    Top = 20
    Width = 52
    Height = 13
    Caption = 'Date Time:'
  end
  object Label2: TLabel
    Left = 20
    Top = 45
    Width = 52
    Height = 13
    Caption = 'Operation:'
  end
  object lblOperationTxt: TLabel
    Left = 90
    Top = 40
    Width = 457
    Height = 39
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
  object PageControl: TPageControl
    Left = 20
    Top = 90
    Width = 521
    Height = 206
    ActivePage = tsDecoded
    TabOrder = 0
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
        Left = 457
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
        Width = 486
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
        Width = 486
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
    end
    object tsDecodeMethods: TTabSheet
      Caption = 'Decode methods'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        Width = 261
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
  object BitBtn1: TBitBtn
    Left = 462
    Top = 310
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
    TabOrder = 1
  end
end
