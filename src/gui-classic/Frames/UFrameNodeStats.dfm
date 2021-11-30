object FrameNodeStats: TFrameNodeStats
  Left = 0
  Top = 0
  Width = 691
  Height = 572
  TabOrder = 0
  DesignSize = (
    691
    572)
  object Label3: TLabel
    Left = 15
    Top = 15
    Width = 96
    Height = 13
    Caption = 'Active Connections:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label6: TLabel
    Left = 15
    Top = 291
    Width = 104
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Known Node Servers:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label7: TLabel
    Left = 15
    Top = 187
    Width = 82
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Blacklisted Nodes'
    Color = clBtnFace
    ParentColor = False
  end
  object memoNetConnections: TMemo
    Left = -139
    Top = 34
    Width = 830
    Height = 145
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object memoNetServers: TMemo
    Left = -139
    Top = 310
    Width = 830
    Height = 116
    Anchors = [akLeft, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object memoNetBlackLists: TMemo
    Left = -138
    Top = 206
    Width = 829
    Height = 79
    Anchors = [akLeft, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
end
