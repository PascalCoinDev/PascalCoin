object FrameNodeStats: TFrameNodeStats
  Left = 0
  Top = 0
  Width = 711
  Height = 458
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 139
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
    Top = 293
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
    Top = 22
    Width = 711
    Height = 117
    Align = alTop
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    ExplicitTop = 28
    ExplicitWidth = 763
  end
  object memoNetServers: TMemo
    Left = 0
    Top = 315
    Width = 711
    Height = 143
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object memoNetBlackLists: TMemo
    Left = 0
    Top = 165
    Width = 711
    Height = 128
    Align = alClient
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
    ExplicitHeight = 108
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 711
    Height = 22
    Align = alTop
    Caption = 'Active Connections:'
    TabOrder = 3
  end
  object Panel2: TPanel
    Left = 0
    Top = 142
    Width = 711
    Height = 23
    Align = alTop
    Caption = 'Blacklisted Nodes:'
    TabOrder = 4
  end
  object Panel3: TPanel
    Left = 0
    Top = 296
    Width = 711
    Height = 19
    Align = alBottom
    Caption = 'Known Node Servers:'
    TabOrder = 5
    ExplicitTop = 303
  end
end
