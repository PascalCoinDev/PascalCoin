object FrameMessages: TFrameMessages
  Left = 0
  Top = 0
  Width = 817
  Height = 622
  TabOrder = 0
  DesignSize = (
    817
    622)
  object Label11: TLabel
    Left = 15
    Top = 151
    Width = 51
    Height = 13
    Caption = 'Messages:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label12: TLabel
    Left = 315
    Top = 11
    Width = 85
    Height = 13
    Caption = 'Message to send:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label13: TLabel
    Left = 15
    Top = 11
    Width = 109
    Height = 13
    Caption = 'Available Connections:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label14: TLabel
    Left = 410
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
  object lbNetConnections: TListBox
    Left = 15
    Top = 30
    Width = 275
    Height = 96
    ItemHeight = 13
    MultiSelect = True
    ScrollWidth = 273
    TabOrder = 0
  end
  object bbSendAMessage: TButton
    Left = 292
    Top = 101
    Width = 525
    Height = 25
    Caption = 'Send a Message'
    TabOrder = 1
  end
  object memoMessages: TMemo
    Left = -4
    Top = 170
    Width = 821
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    TabOrder = 2
  end
  object memoMessageToSend: TMemo
    Left = 292
    Top = 30
    Width = 525
    Height = 61
    Lines.Strings = (
      'memoMessageToSend')
    TabOrder = 3
    WantReturns = False
  end
end
