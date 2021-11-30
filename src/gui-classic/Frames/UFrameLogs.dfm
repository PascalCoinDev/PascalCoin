object FrameLogs: TFrameLogs
  Left = 0
  Top = 0
  Width = 821
  Height = 531
  TabOrder = 0
  object memoLogs: TMemo
    Left = 0
    Top = 41
    Width = 821
    Height = 490
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    ExplicitLeft = -537
    ExplicitTop = -157
    ExplicitWidth = 857
    ExplicitHeight = 397
  end
  object pnlTopLogs: TPanel
    Left = 0
    Top = 0
    Width = 821
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = -537
    ExplicitWidth = 857
    object cbShowDebugLogs: TCheckBox
      Left = 15
      Top = 10
      Width = 102
      Height = 19
      Caption = 'Show Debug Logs'
      TabOrder = 0
    end
  end
end
