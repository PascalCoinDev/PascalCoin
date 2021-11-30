object FrameOperationsExplorer: TFrameOperationsExplorer
  Left = 0
  Top = 0
  Width = 817
  Height = 441
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 817
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label2: TLabel
      Left = 11
      Top = 10
      Width = 102
      Height = 13
      Caption = 'Filter by blocks range'
      Color = clBtnFace
      ParentColor = False
    end
    object ebFilterOperationsStartBlock: TEdit
      Left = 125
      Top = 7
      Width = 57
      Height = 21
      TabOrder = 0
    end
    object ebFilterOperationsEndBlock: TEdit
      Left = 185
      Top = 7
      Width = 57
      Height = 21
      TabOrder = 1
    end
  end
  object dgOperationsExplorer: TDrawGrid
    Left = 0
    Top = 41
    Width = 817
    Height = 400
    Align = alClient
    TabOrder = 1
  end
end
