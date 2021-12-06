object FrameNodeStats: TFrameNodeStats
  Left = 0
  Top = 0
  Width = 711
  Height = 458
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 137
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
    Top = 292
    Width = 711
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
    ExplicitLeft = 3
    ExplicitTop = 482
    ExplicitWidth = 909
  end
  object memoNetConnections: TMemo
    Left = 0
    Top = 20
    Width = 711
    Height = 117
    Align = alTop
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object memoNetServers: TMemo
    Left = 0
    Top = 315
    Width = 711
    Height = 143
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object memoNetBlackLists: TMemo
    Left = 0
    Top = 160
    Width = 711
    Height = 132
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 711
    Height = 20
    Align = alTop
    Caption = 'Active Connections:'
    TabOrder = 3
  end
  object Panel2: TPanel
    Left = 0
    Top = 140
    Width = 711
    Height = 20
    Align = alTop
    Caption = 'Blacklisted Nodes:'
    TabOrder = 4
  end
  object Panel3: TPanel
    Left = 0
    Top = 295
    Width = 711
    Height = 20
    Align = alBottom
    Caption = 'Known Node Servers:'
    TabOrder = 5
    ExplicitTop = 293
  end
end
