object FRMRPCCalls: TFRMRPCCalls
  Left = 515
  Top = 250
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'RPC Calls'
  ClientHeight = 433
  ClientWidth = 760
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 424
    Top = 50
    Width = 5
    Height = 366
    Align = alRight
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 760
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 0
    object lblTopCaption: TLabel
      Left = 5
      Top = 5
      Width = 750
      Height = 40
      Align = alClient
      AutoSize = False
      Caption = 
        'WARNING! Execute only calls you understand. Execute unknown call' +
        's provided by third person can cause that you lost your keys/bal' +
        'ance!'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 416
    Width = 760
    Height = 17
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlRight: TPanel
    Left = 429
    Top = 50
    Width = 331
    Height = 366
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      331
      366)
    object Label1: TLabel
      Left = 16
      Top = 43
      Width = 36
      Height = 13
      Caption = 'Method'
      Color = clBtnFace
      ParentColor = False
    end
    object Label2: TLabel
      Left = 16
      Top = 13
      Width = 32
      Height = 13
      Caption = 'Server'
      Color = clBtnFace
      ParentColor = False
    end
    object ebMethod: TEdit
      Left = 70
      Top = 40
      Width = 184
      Height = 21
      TabOrder = 1
      Text = 'ebMethod'
    end
    object pageControl: TPageControl
      Left = 16
      Top = 75
      Width = 289
      Height = 222
      ActivePage = tsKeyNames
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 2
      object tsKeyNames: TTabSheet
        Caption = 'Key/Value Params'
        object vlKeyParams: TValueListEditor
          Left = 0
          Top = 0
          Width = 281
          Height = 194
          Align = alClient
          KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
          Strings.Strings = (
            '')
          TabOrder = 0
          ColWidths = (
            64
            211)
        end
      end
      object tsJSONParams: TTabSheet
        Caption = 'Params as JSON'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object mJSONParams: TMemo
          Left = 0
          Top = 0
          Width = 238
          Height = 172
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
    object bbSendCommand: TBitBtn
      Left = 72
      Top = 305
      Width = 168
      Height = 46
      Anchors = [akLeft, akBottom]
      Caption = '&Send'
      Default = True
      DoubleBuffered = True
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
        00000000000000000000FFFFFF0046986FFF379064FF0B774240FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
        000000000000FFFFFF0054A07AFF78BCA1FF7DB99EFF368F63FF0B774240FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
        FF00FFFFFF0054A07AFF7BBEA4FF39A681FF55B191FF7AB698FF399166FF0B77
        4240FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0054A0
        7A0054A07AFF7CC0A6FF3CA985FF76BDA1FF7FBBA0FF4FB592FF7BBEA2FF54A0
        7AFF0B77424054A07A0054A07A00FFFFFF00FFFFFF00FFFFFF0054A07A0054A0
        7AFF7DC1A7FF4DB391FF7FBB9FFF4D9C75FF3E946AFF7BC5A9FF40B18BFF7BBE
        A2FF54A07AFF0B77424054A07A0054A07A00FFFFFF00FFFFFF0054A07AFF7FC3
        A8FF5FBB9BFF7FBBA0FF3A9166FF0B7742600B7742404D9C75FF7BC5A9FF4EBC
        97FF7BBEA2FF54A07AFF0B7742400B77421054A07A00FFFFFF000B7742303A91
        66FC7FBBA0FF3A9166FC0B77423000000000000000000B77426054A07AFF7BC5
        A9FF4EBE99FF5CC2A0FF54A07AFF0B77423F0B77421054A07A00000000000B77
        42203A9166F60B774220000000000000000000000000FFFFFF000B77424054A0
        7AFF7CCAAEFF52C39DFF81BDA2FF54A07AFF0B77423F54A07A00000000000000
        0000000000000000000000000000000000000000000000000000000000000B77
        424054A07AFF6DC9A8FF57C7A2FF7BCCAFFF54A07AFF0B774239000000000000
        000000000000000000000000000000000000000000000000000000000000FFFF
        FF000B77424C54A07AFF7BCCAFFF81CBAFFF54A07AFF0B77423F000000000000
        000000000000000000000000000000000000000000000000000000000000FFFF
        FF00FFFFFF000B77423D54A07AFF54A07AFF0B774247FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF000B77423F0B77423FFFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
      ParentDoubleBuffered = False
      TabOrder = 3
      OnClick = bbSendCommandClick
    end
    object ebServerURL: TEdit
      Left = 70
      Top = 8
      Width = 184
      Height = 21
      TabStop = False
      TabOrder = 0
      Text = 'ebServerURL'
    end
  end
  object pnlInfo: TPanel
    Left = 0
    Top = 50
    Width = 424
    Height = 366
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object Splitter2: TSplitter
      Left = 0
      Top = 192
      Width = 424
      Height = 5
      Cursor = crVSplit
      Align = alBottom
    end
    object pnlInfoBottom: TPanel
      Left = 0
      Top = 197
      Width = 424
      Height = 169
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object mLastResult: TMemo
        Left = 0
        Top = 24
        Width = 424
        Height = 145
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object pnlLastCall: TPanel
        Left = 0
        Top = 0
        Width = 424
        Height = 24
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label3: TLabel
          Left = 8
          Top = 3
          Width = 72
          Height = 13
          Caption = 'Last call result:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblLastCallResultState: TLabel
          Left = 112
          Top = 3
          Width = 31
          Height = 13
          Caption = 'STATE'
          Color = clBtnFace
          ParentColor = False
        end
      end
    end
    object mCalls: TMemo
      Left = 0
      Top = 0
      Width = 424
      Height = 192
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
end
