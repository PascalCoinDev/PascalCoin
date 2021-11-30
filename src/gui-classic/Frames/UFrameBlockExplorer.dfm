object FrameBlockChainExplorer: TFrameBlockChainExplorer
  Left = 0
  Top = 0
  Width = 782
  Height = 489
  TabOrder = 0
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 782
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label9: TLabel
      Left = 11
      Top = 10
      Width = 102
      Height = 13
      Caption = 'Filter by blocks range'
      Color = clBtnFace
      ParentColor = False
    end
    object lblHashRateBackBlocks: TLabel
      Left = 280
      Top = 10
      Width = 104
      Height = 13
      Caption = 'HashRate back blocks'
      Color = clBtnFace
      ParentColor = False
    end
    object lblHashRateBackBlocks1: TLabel
      Left = 463
      Top = 10
      Width = 24
      Height = 13
      Caption = 'Units'
      Color = clBtnFace
      ParentColor = False
    end
    object ebBlockChainBlockStart: TEdit
      Left = 125
      Top = 7
      Width = 57
      Height = 21
      TabOrder = 0
      OnExit = ebBlockChainBlockStartExit
      OnKeyPress = ebBlockChainBlockStartKeyPress
    end
    object ebBlockChainBlockEnd: TEdit
      Left = 185
      Top = 7
      Width = 57
      Height = 21
      TabOrder = 1
    end
    object ebHashRateBackBlocks: TEdit
      Left = 392
      Top = 7
      Width = 56
      Height = 21
      TabOrder = 2
      OnKeyPress = ebHashRateBackBlocksKeyPress
    end
    object cbHashRateUnits: TComboBox
      Left = 496
      Top = 7
      Width = 56
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 3
      Text = 'Mh/s'
      OnClick = cbHashRateUnitsClick
      Items.Strings = (
        'Kh/s'
        'Mh/s'
        'Gh/s'
        'Th/s')
    end
  end
  object dgBlockChainExplorer: TDrawGrid
    Left = 0
    Top = 41
    Width = 782
    Height = 448
    Align = alClient
    TabOrder = 1
  end
end
