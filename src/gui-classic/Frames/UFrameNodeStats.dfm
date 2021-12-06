object FrameNodeStats: TFrameNodeStats
  Left = 0
  Top = 0
  Width = 711
  Height = 458
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 153
    Width = 711
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
    ExplicitTop = 233
    ExplicitWidth = 399
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 301
    Width = 711
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
    ExplicitLeft = 3
    ExplicitTop = 482
    ExplicitWidth = 909
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 711
    Height = 153
    Align = alTop
    Caption = ' '
    TabOrder = 0
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 709
      Height = 20
      Align = alTop
      Caption = 'Active Connections:'
      TabOrder = 0
    end
    object memoNetConnections: TMemo
      Left = 1
      Top = 21
      Width = 709
      Height = 131
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
      ExplicitHeight = 148
    end
  end
  object PanelMiddle: TPanel
    Left = 0
    Top = 156
    Width = 711
    Height = 145
    Align = alClient
    Caption = ' '
    TabOrder = 1
    ExplicitHeight = 97
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 709
      Height = 20
      Align = alTop
      Caption = 'Blacklisted Nodes:'
      TabOrder = 0
    end
    object memoNetBlackLists: TMemo
      Left = 1
      Top = 21
      Width = 709
      Height = 123
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
      ExplicitLeft = 2
      ExplicitTop = 22
      ExplicitHeight = 135
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 304
    Width = 711
    Height = 154
    Align = alBottom
    Anchors = [akLeft, akTop, akRight]
    Caption = ' '
    TabOrder = 2
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 709
      Height = 20
      Align = alTop
      Caption = 'Known Node Servers:'
      TabOrder = 0
    end
    object memoNetServers: TMemo
      Left = 1
      Top = 21
      Width = 709
      Height = 132
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
      ExplicitLeft = 0
      ExplicitTop = 384
      ExplicitWidth = 711
      ExplicitHeight = 74
    end
  end
end
