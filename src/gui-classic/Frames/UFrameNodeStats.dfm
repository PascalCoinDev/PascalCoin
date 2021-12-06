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
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 0
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 711
      Height = 20
      Align = alTop
      Caption = 'Active Connections:'
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 709
    end
    object memoNetConnections: TMemo
      Left = 0
      Top = 20
      Width = 711
      Height = 133
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
      ExplicitLeft = 1
      ExplicitTop = 21
      ExplicitWidth = 709
      ExplicitHeight = 131
    end
  end
  object PanelMiddle: TPanel
    Left = 0
    Top = 156
    Width = 711
    Height = 145
    Align = alClient
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 711
      Height = 20
      Align = alTop
      Caption = 'Blacklisted Nodes:'
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 709
    end
    object memoNetBlackLists: TMemo
      Left = 0
      Top = 20
      Width = 711
      Height = 125
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
      ExplicitLeft = 1
      ExplicitTop = 21
      ExplicitWidth = 709
      ExplicitHeight = 123
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 304
    Width = 711
    Height = 154
    Align = alBottom
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 2
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 711
      Height = 20
      Align = alTop
      Caption = 'Known Node Servers:'
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 709
    end
    object memoNetServers: TMemo
      Left = 0
      Top = 20
      Width = 711
      Height = 134
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
      ExplicitLeft = 1
      ExplicitTop = 21
      ExplicitWidth = 709
      ExplicitHeight = 132
    end
  end
end
