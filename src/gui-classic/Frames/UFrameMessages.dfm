object FrameMessages: TFrameMessages
  Left = 0
  Top = 0
  Width = 764
  Height = 466
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 213
    Width = 764
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
    ExplicitTop = 0
    ExplicitWidth = 304
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 216
    Width = 764
    Height = 250
    Align = alBottom
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 0
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 764
      Height = 20
      Align = alTop
      Caption = 'Messages:'
      TabOrder = 0
    end
    object memoMessages: TMemo
      Left = 0
      Top = 20
      Width = 764
      Height = 230
      Align = alClient
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
      TabOrder = 1
    end
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 764
    Height = 213
    Align = alClient
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 288
      Top = 0
      Height = 213
      ResizeStyle = rsUpdate
      ExplicitLeft = 440
      ExplicitTop = 144
      ExplicitHeight = 100
    end
    object PanelLeft: TPanel
      Left = 0
      Top = 0
      Width = 288
      Height = 213
      Align = alLeft
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 0
      object lbNetConnections: TListBox
        Left = 0
        Top = 20
        Width = 288
        Height = 193
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        ScrollWidth = 273
        TabOrder = 0
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 20
        Align = alTop
        Caption = 'Available Connections:'
        TabOrder = 1
      end
    end
    object PanelRight: TPanel
      Left = 291
      Top = 0
      Width = 473
      Height = 213
      Align = alClient
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 1
      object memoMessageToSend: TMemo
        Left = 0
        Top = 20
        Width = 473
        Height = 127
        Align = alClient
        Lines.Strings = (
          'memoMessageToSend')
        TabOrder = 0
        WantReturns = False
      end
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 473
        Height = 20
        Align = alTop
        Caption = 'Message to send:'
        TabOrder = 1
      end
      object Panel7: TPanel
        Left = 0
        Top = 147
        Width = 473
        Height = 66
        Align = alBottom
        Caption = ' '
        TabOrder = 2
        DesignSize = (
          473
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
          Width = 450
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Send a Message'
          TabOrder = 0
          OnClick = bbSendAMessageClick
        end
      end
    end
  end
end
