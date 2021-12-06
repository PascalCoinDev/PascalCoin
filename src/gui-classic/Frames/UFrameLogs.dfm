object FrameLogs: TFrameLogs
  Left = 0
  Top = 0
  Width = 821
  Height = 531
  TabOrder = 0
  object memoLogs: TMemo
    Left = 0
    Top = 33
    Width = 821
    Height = 498
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    ExplicitTop = 32
  end
  object pnlTopLogs: TPanel
    Left = 0
    Top = 0
    Width = 821
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      821
      33)
    object cbShowDebugLogs: TCheckBox
      Left = 13
      Top = 7
      Width = 102
      Height = 19
      Caption = 'Show Debug Logs'
      TabOrder = 0
    end
    object Button1: TButton
      Left = 680
      Top = 4
      Width = 133
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Copy log to clipboard'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
end
