object FrameMessages: TFrameMessages
  Left = 0
  Top = 0
  Width = 764
  Height = 466
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 233
    Width = 764
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
    ExplicitTop = 0
    ExplicitWidth = 304
  end
  object memoMessages: TMemo
    Left = 0
    Top = 256
    Width = 764
    Height = 210
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'dsfa '#195#177'ldsaf '#195#177'lk dasf'
      'dsfklda'#195#177'fs '#195#177'l')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 236
    Width = 764
    Height = 20
    Align = alBottom
    Caption = 'Messages:'
    TabOrder = 1
    ExplicitTop = 238
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 764
    Height = 233
    Align = alClient
    Caption = ' '
    TabOrder = 2
    ExplicitLeft = 256
    ExplicitTop = 192
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Splitter2: TSplitter
      Left = 289
      Top = 1
      Height = 231
      ResizeStyle = rsUpdate
      ExplicitLeft = 440
      ExplicitTop = 144
      ExplicitHeight = 100
    end
    object PanelLeft: TPanel
      Left = 1
      Top = 1
      Width = 288
      Height = 231
      Align = alLeft
      Caption = ' '
      TabOrder = 0
      ExplicitHeight = 299
      object lbNetConnections: TListBox
        Left = 1
        Top = 21
        Width = 286
        Height = 209
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        ScrollWidth = 273
        TabOrder = 0
        ExplicitLeft = 0
        ExplicitTop = 19
        ExplicitHeight = 213
      end
      object Panel5: TPanel
        Left = 1
        Top = 1
        Width = 286
        Height = 20
        Align = alTop
        Caption = 'Available Connections:'
        TabOrder = 1
      end
    end
    object PanelRight: TPanel
      Left = 292
      Top = 1
      Width = 471
      Height = 231
      Align = alClient
      Caption = ' '
      TabOrder = 1
      ExplicitLeft = 504
      ExplicitTop = 152
      ExplicitWidth = 185
      ExplicitHeight = 41
      object memoMessageToSend: TMemo
        Left = 1
        Top = 21
        Width = 469
        Height = 143
        Align = alClient
        Lines.Strings = (
          'memoMessageToSend')
        TabOrder = 0
        WantReturns = False
        ExplicitLeft = 64
        ExplicitTop = 103
        ExplicitWidth = 371
        ExplicitHeight = 61
      end
      object Panel6: TPanel
        Left = 1
        Top = 1
        Width = 469
        Height = 20
        Align = alTop
        Caption = 'Message to send:'
        TabOrder = 1
      end
      object Panel7: TPanel
        Left = 1
        Top = 164
        Width = 469
        Height = 66
        Align = alBottom
        Caption = ' '
        TabOrder = 2
        ExplicitTop = 232
        ExplicitWidth = 522
        DesignSize = (
          469
          66)
        object Label14: TLabel
          Left = 14
          Top = 11
          Width = 361
          Height = 13
          Caption = 
            '(Messages will be encrypted, so only dest connection will be abl' +
            'e to read it)'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object bbSendAMessage: TButton
          Left = 14
          Top = 30
          Width = 446
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Send a Message'
          TabOrder = 0
          OnClick = bbSendAMessageClick
          ExplicitWidth = 499
        end
      end
    end
  end
end
