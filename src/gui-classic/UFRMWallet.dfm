object FRMWallet: TFRMWallet
  Left = 389
  Top = 201
  Caption = 'Pascal full node Wallet (Classic GUI)'
  ClientHeight = 580
  ClientWidth = 865
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 865
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 865
    Height = 104
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    inline FrameInfo: TFrameInfo
      Left = 0
      Top = 0
      Width = 865
      Height = 104
      Align = alTop
      TabOrder = 0
      ExplicitWidth = 865
      inherited Panel1: TPanel
        ExplicitHeight = 104
        inherited Image1: TImage
          Width = 75
          Height = 104
        end
      end
      inherited Panel2: TPanel
        ExplicitTop = 0
      end
      inherited Panel3: TPanel
        ExplicitLeft = 315
        ExplicitTop = 0
        ExplicitHeight = 104
      end
      inherited Panel4: TPanel
        Width = 310
        ExplicitLeft = 555
        ExplicitTop = 0
        ExplicitWidth = 310
        ExplicitHeight = 104
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 557
    Width = 865
    Height = 23
    Panels = <
      item
        Alignment = taCenter
        Text = 'Server Active'
        Width = 130
      end
      item
        Text = 'Connection status'
        Width = 430
      end
      item
        Text = 'Blocks'
        Width = 50
      end>
  end
  object PageControl: TPageControl
    Left = 0
    Top = 104
    Width = 865
    Height = 453
    ActivePage = tsMyAccounts
    Align = alClient
    TabOrder = 2
    OnChange = PageControlChange
    object tsMyAccounts: TTabSheet
      Caption = 'Account Explorer'
      inline FrameAccountExplorer: TFrameAccountExplorer
        Left = 0
        Top = 0
        Width = 857
        Height = 425
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 857
        ExplicitHeight = 425
        inherited Splitter1: TSplitter
          Height = 359
          ExplicitHeight = 359
        end
        inherited pnlMyAccountsTop: TPanel
          Width = 857
          ExplicitWidth = 857
        end
        inherited pnlAccounts: TPanel
          Height = 359
          ExplicitHeight = 359
          inherited dgAccounts: TDrawGrid
            Height = 325
            ExplicitHeight = 325
          end
          inherited pnlAccountsInfo: TPanel
            Top = 325
            ExplicitTop = 325
          end
        end
        inherited pcAccountsOptions: TPageControl
          Width = 452
          Height = 359
          ExplicitWidth = 452
          ExplicitHeight = 359
          inherited tsMultiSelectAccounts: TTabSheet
            ExplicitWidth = 444
            ExplicitHeight = 331
            inherited dgSelectedAccounts: TDrawGrid
              Height = 274
              ExplicitHeight = 274
            end
            inherited pnlSelectedAccountsTop: TPanel
              Width = 444
              ExplicitWidth = 444
            end
            inherited pnlSelectedAccountsBottom: TPanel
              Top = 305
              Width = 444
              ExplicitTop = 305
              ExplicitWidth = 444
            end
            inherited pnlSelectedAccountsLeft: TPanel
              Height = 274
              ExplicitHeight = 274
            end
          end
        end
      end
    end
    object tsPendingOperations: TTabSheet
      Caption = 'Pending Operations'
      ImageIndex = 5
      inline FramePendingOperations: TFramePendingOperations
        Left = 0
        Top = 0
        Width = 857
        Height = 425
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 857
        ExplicitHeight = 438
        inherited dgPendingOperations: TDrawGrid
          Width = 857
          Height = 339
          ExplicitLeft = 0
          ExplicitTop = 86
          ExplicitHeight = 339
        end
        inherited pnlPendingOperations: TPanel
          Width = 857
          ExplicitLeft = 0
          inherited Label10: TLabel
            Width = 837
          end
        end
      end
    end
    object tsBlockChain: TTabSheet
      Caption = 'Block Explorer'
      ImageIndex = 1
      inline FrameBlockChainExplorer: TFrameBlockChainExplorer
        Left = 0
        Top = 0
        Width = 857
        Height = 425
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 296
        ExplicitHeight = 46
        inherited Panel2: TPanel
          Width = 857
          ExplicitWidth = 296
        end
        inherited dgBlockChainExplorer: TDrawGrid
          Width = 857
          Height = 384
          ExplicitWidth = 857
          ExplicitHeight = 384
        end
      end
    end
    object tsOperations: TTabSheet
      Caption = 'Operations Explorer'
      ImageIndex = 1
      inline FrameOperationsExplorer: TFrameOperationsExplorer
        Left = 0
        Top = 0
        Width = 857
        Height = 425
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 857
        ExplicitHeight = 438
        inherited Panel1: TPanel
          Width = 857
          ExplicitWidth = 857
        end
        inherited dgOperationsExplorer: TDrawGrid
          Width = 857
          Height = 384
          ExplicitWidth = 857
          ExplicitHeight = 384
        end
      end
    end
    object tsLogs: TTabSheet
      Caption = 'Logs'
      ImageIndex = 2
      inline FrameLogs: TFrameLogs
        Left = 0
        Top = 0
        Width = 857
        Height = 425
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 857
        ExplicitHeight = 438
        inherited memoLogs: TMemo
          Width = 857
          Height = 392
          ExplicitTop = 33
          ExplicitWidth = 857
          ExplicitHeight = 405
        end
        inherited pnlTopLogs: TPanel
          Width = 857
          ExplicitWidth = 857
          inherited Button1: TButton
            Left = 716
            ExplicitLeft = 716
          end
        end
      end
    end
    object tsNodeStats: TTabSheet
      Caption = 'Node Stats'
      ImageIndex = 3
      inline FrameNodeStats: TFrameNodeStats
        Left = 0
        Top = 0
        Width = 857
        Height = 425
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 857
        ExplicitHeight = 438
        inherited Splitter1: TSplitter
          Width = 857
          ExplicitWidth = 857
        end
        inherited Splitter2: TSplitter
          Top = 259
          Width = 857
          ExplicitTop = 273
          ExplicitWidth = 857
        end
        inherited memoNetConnections: TMemo
          Width = 857
          ExplicitWidth = 857
        end
        inherited memoNetServers: TMemo
          Top = 282
          Width = 857
          ExplicitTop = 295
          ExplicitWidth = 857
        end
        inherited memoNetBlackLists: TMemo
          Width = 857
          Height = 99
          ExplicitWidth = 857
          ExplicitHeight = 112
        end
        inherited Panel1: TPanel
          Width = 857
          ExplicitWidth = 857
        end
        inherited Panel2: TPanel
          Width = 857
          ExplicitWidth = 857
        end
        inherited Panel3: TPanel
          Top = 262
          Width = 857
          ExplicitTop = 275
          ExplicitWidth = 857
        end
      end
    end
    object tsMessages: TTabSheet
      Caption = 'Messages'
      ImageIndex = 6
      inline FrameMessages: TFrameMessages
        Left = 0
        Top = 0
        Width = 857
        Height = 425
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 296
        ExplicitHeight = 46
        inherited Splitter1: TSplitter
          Top = 192
          Width = 857
          ExplicitTop = 205
          ExplicitWidth = 857
        end
        inherited memoMessages: TMemo
          Top = 215
          Width = 857
          ExplicitTop = 215
          ExplicitWidth = 857
        end
        inherited PanelBottom: TPanel
          Top = 195
          Width = 857
          ExplicitTop = 208
          ExplicitWidth = 857
        end
        inherited PanelTop: TPanel
          Width = 857
          Height = 192
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 296
          ExplicitHeight = 192
          inherited Splitter2: TSplitter
            Height = 190
            ExplicitHeight = 203
          end
          inherited PanelLeft: TPanel
            Height = 190
            ExplicitHeight = 190
            inherited lbNetConnections: TListBox
              Height = 168
              ExplicitLeft = 1
              ExplicitTop = 21
              ExplicitHeight = 168
            end
          end
          inherited PanelRight: TPanel
            Width = 564
            Height = 190
            ExplicitLeft = 292
            ExplicitTop = 1
            ExplicitWidth = 3
            ExplicitHeight = 190
            inherited memoMessageToSend: TMemo
              Width = 562
              Height = 102
              ExplicitLeft = 1
              ExplicitTop = 21
              ExplicitWidth = 562
              ExplicitHeight = 102
            end
            inherited Panel6: TPanel
              Width = 562
              ExplicitWidth = 562
            end
            inherited Panel7: TPanel
              Top = 123
              Width = 562
              ExplicitTop = 136
              ExplicitWidth = 562
              inherited bbSendAMessage: TButton
                Width = 539
                ExplicitWidth = 539
              end
            end
          end
        end
      end
    end
  end
  object TimerUpdateStatus: TTimer
    OnTimer = TimerUpdateStatusTimer
    Left = 801
    Top = 13
  end
  object MainMenu: TMainMenu
    Left = 165
    Top = 160
    object miProject: TMenuItem
      Caption = 'Project'
      object miPrivatekeys: TMenuItem
        Caption = 'Private Keys'
        ShortCut = 16464
        OnClick = miPrivatekeysClick
      end
      object miN1: TMenuItem
        Caption = '-'
      end
      object miOptions: TMenuItem
        Caption = 'Options'
        ShortCut = 16463
        OnClick = miOptionsClick
      end
      object IPnodes1: TMenuItem
        Caption = 'Available Node List'
        OnClick = IPnodes1Click
      end
      object MiRPCCalls: TMenuItem
        Caption = 'RPC Calls'
        OnClick = MiRPCCallsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiClose: TMenuItem
        Caption = 'Close'
        OnClick = MiCloseClick
      end
    end
    object MiOperations: TMenuItem
      Caption = 'Operations'
      object miNewOperation: TMenuItem
        Caption = 'New single Operation'
        ShortCut = 120
        OnClick = miNewOperationClick
      end
      object MiFindOperationbyOpHash: TMenuItem
        Caption = 'Find Operation by OpHash'
        ShortCut = 116
        OnClick = MiFindOperationbyOpHashClick
      end
      object MiDecodePayload: TMenuItem
        Caption = 'Decode Payload'
        ShortCut = 113
        OnClick = MiDecodePayloadClick
      end
      object MiFindaccount: TMenuItem
        Caption = 'Find account'
        ShortCut = 16454
        OnClick = MiFindaccountClick
      end
      object MiAccountInformation: TMenuItem
        Caption = 'Account Information'
        ShortCut = 112
        OnClick = MiAccountInformationClick
      end
      object MiOperationsExplorer: TMenuItem
        Caption = 'Operations Explorer'
        ShortCut = 16453
        OnClick = MiOperationsExplorerClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MiAddaccounttoSelected: TMenuItem
        Caption = 'Add account to selected'
        ShortCut = 117
        OnClick = MiAddaccounttoSelectedClick
      end
      object MiRemoveaccountfromselected: TMenuItem
        Caption = 'Remove account from selected'
        ShortCut = 118
        OnClick = MiRemoveaccountfromselectedClick
      end
      object MiMultiaccountoperation: TMenuItem
        Caption = 'Multi account operation'
        OnClick = MiMultiaccountoperationClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MiFindpreviousaccountwithhighbalance: TMenuItem
        Caption = 'Find previous account with high balance'
        ShortCut = 16498
        OnClick = MiFindpreviousaccountwithhighbalanceClick
      end
      object MiFindnextaccountwithhighbalance: TMenuItem
        Caption = 'Find next account with high balance'
        ShortCut = 114
        OnClick = MiFindnextaccountwithhighbalanceClick
      end
    end
    object miAbout: TMenuItem
      Caption = 'About'
      object miAboutPascalCoin: TMenuItem
        Caption = 'About Pascal Coin...'
        OnClick = miAboutPascalCoinClick
      end
    end
  end
end
