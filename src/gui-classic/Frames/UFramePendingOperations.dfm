object FramePendingOperations: TFramePendingOperations
  Left = 0
  Top = 0
  Width = 826
  Height = 522
  TabOrder = 0
  object dgPendingOperations: TDrawGrid
    Left = 0
    Top = 86
    Width = 826
    Height = 436
    Align = alClient
    TabOrder = 0
    ExplicitLeft = -537
    ExplicitTop = -112
    ExplicitWidth = 857
    ExplicitHeight = 352
  end
  object pnlPendingOperations: TPanel
    Left = 0
    Top = 0
    Width = 826
    Height = 86
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 1
    ExplicitLeft = -537
    ExplicitWidth = 857
    object Label10: TLabel
      Left = 10
      Top = 10
      Width = 806
      Height = 66
      Align = alClient
      AutoSize = False
      Caption = 
        'Here you can see Operations transmited/received from other nodes' +
        ' that will be included in next block. There is no guarantee that' +
        ' other nodes will include them when mining, so it'#39's important th' +
        'at you mine too to help include Operations to the main BlockChai' +
        'n'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
      ExplicitWidth = 837
    end
  end
end
