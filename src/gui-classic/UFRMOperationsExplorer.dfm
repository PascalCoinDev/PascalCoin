object FRMOperationsExplorer: TFRMOperationsExplorer
  Left = 471
  Top = 231
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Operations Explorer Tool'
  ClientHeight = 406
  ClientWidth = 786
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mMenu
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object splitter: TSplitter
    Left = 427
    Top = 56
    Width = 5
    Height = 320
  end
  object lbOperations: TListBox
    Left = 0
    Top = 56
    Width = 427
    Height = 320
    Align = alLeft
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
    OnClick = lbOperationsClick
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 786
    Height = 56
    Align = alTop
    BorderWidth = 5
    TabOrder = 1
    object lblTopCaption: TLabel
      Left = 6
      Top = 6
      Width = 774
      Height = 44
      Align = alClient
      AutoSize = False
      Caption = 
        'You can use this tool to explore operations, also you can copy/p' +
        'aste operations as a RAW (Use Ctrl+I to Import). This tool allow' +
        's to generate a Multioperation, send to third party the RAW, add' +
        ' more txs... and then sign. Once Everybody has signed, you will ' +
        'be able to EXECUTE OPERATION to be added to network nodes. '
      Color = clBtnFace
      ParentColor = False
      WordWrap = True
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 376
    Width = 786
    Height = 30
    Align = alBottom
    TabOrder = 2
  end
  object pnlOpInfo: TPanel
    Left = 432
    Top = 56
    Width = 354
    Height = 320
    Align = alClient
    TabOrder = 3
    object pnlExport: TPanel
      Left = 1
      Top = 216
      Width = 352
      Height = 103
      Align = alClient
      Caption = 'pnlExport'
      TabOrder = 0
      object Label1: TLabel
        Left = 1
        Top = 1
        Width = 350
        Height = 25
        Align = alTop
        AutoSize = False
        Caption = 'Operation as RAW:'
        Color = clBtnFace
        ParentColor = False
        Layout = tlCenter
      end
      object mOperationExport: TMemo
        Left = 1
        Top = 26
        Width = 350
        Height = 76
        Align = alClient
        ReadOnly = True
        TabOrder = 0
      end
    end
    object pnlInfo: TPanel
      Left = 1
      Top = 1
      Width = 352
      Height = 215
      Align = alTop
      Caption = 'pnlInfo'
      TabOrder = 1
      object mOperationInfo: TMemo
        Left = 1
        Top = 87
        Width = 350
        Height = 127
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object pnlInfoOptions: TPanel
        Left = 1
        Top = 1
        Width = 350
        Height = 86
        Align = alTop
        TabOrder = 1
        object Label2: TLabel
          Left = 10
          Top = 39
          Width = 109
          Height = 13
          Caption = 'Multioperation actions:'
          Color = clBtnFace
          ParentColor = False
        end
        object bbAddTx: TButton
          Left = 8
          Top = 56
          Width = 67
          Height = 25
          Action = ActAddTx
          TabOrder = 0
        end
        object bbAddOpChange: TButton
          Left = 80
          Top = 56
          Width = 104
          Height = 25
          Action = ActAddOpChange
          TabOrder = 1
        end
        object bbSign: TButton
          Left = 256
          Top = 56
          Width = 80
          Height = 25
          Action = ActSign
          TabOrder = 2
        end
        object bbExecuteOperation: TButton
          Left = 8
          Top = 8
          Width = 176
          Height = 25
          Action = ActExecuteOperation
          TabOrder = 3
        end
        object bbRemove: TButton
          Left = 188
          Top = 57
          Width = 60
          Height = 25
          Action = ActRemove
          TabOrder = 4
        end
        object bbRandom: TButton
          Left = 190
          Top = 8
          Width = 146
          Height = 25
          Caption = 'RANDOM OPERATIONS'
          TabOrder = 5
          OnClick = bbRandomClick
        end
      end
    end
  end
  object mMenu: TMainMenu
    Left = 112
    Top = 30
    object MiTools: TMenuItem
      Caption = 'Tools'
      object MiImportOperationsFromTxt: TMenuItem
        Caption = 'Import Operations from RAW'
        ShortCut = 16457
        OnClick = MiImportOperationsFromTxtClick
      end
      object MiDivider1: TMenuItem
        Caption = '-'
      end
      object miLoadFromBlockchain: TMenuItem
        Caption = 'Load Operations from Blockchain'
        ShortCut = 16460
        OnClick = miLoadFromBlockchainClick
      end
      object MiLoadOperationsFromBlockchainFile: TMenuItem
        Caption = 'Load Operations from Blockchain file'
        OnClick = MiLoadOperationsFromBlockchainFileClick
      end
    end
    object MiActions: TMenuItem
      Caption = 'Actions'
      object MenuItem5: TMenuItem
        Action = ActExecuteOperation
      end
      object MenuItem6: TMenuItem
        Caption = '-'
      end
      object MenuItem1: TMenuItem
        Action = AddOpChangeToMultioperation
      end
      object MenuItem2: TMenuItem
        Action = ActAddTxToMultioperation
      end
      object MenuItem3: TMenuItem
        Action = ActRemoveAccountFromMultioperation
      end
      object MenuItem4: TMenuItem
        Action = ActSignMultioperation
      end
    end
  end
  object ActionList: TActionList
    Left = 299
    Top = 34
    object ActExecuteOperation: TAction
      Caption = 'Execute Operation'
      OnExecute = ActExecuteOperationExecute
      OnUpdate = ActExecuteOperationUpdate
    end
    object ActAddTxToMultioperation: TAction
      Caption = 'Add Tx to Multioperation'
      OnExecute = ActAddTxToMultioperationExecute
      OnUpdate = ActAddTxToMultioperationUpdate
    end
    object AddOpChangeToMultioperation: TAction
      Caption = 'Add OpChange to Multioperation'
      OnExecute = AddOpChangeToMultioperationExecute
      OnUpdate = AddOpChangeToMultioperationUpdate
    end
    object ActRemoveAccountFromMultioperation: TAction
      Caption = 'Remove Account from Multioperation'
      OnExecute = ActRemoveAccountFromMultioperationExecute
      OnUpdate = ActRemoveAccountFromMultioperationUpdate
    end
    object ActSignMultioperation: TAction
      Caption = 'Sign Multioperation'
      OnExecute = ActSignMultioperationExecute
      OnUpdate = ActSignMultioperationUpdate
    end
    object ActAddTx: TAction
      Caption = 'Add Tx'
      OnExecute = ActAddTxToMultioperationExecute
      OnUpdate = ActAddTxToMultioperationUpdate
    end
    object ActAddOpChange: TAction
      Caption = 'Add OpChange'
      OnExecute = AddOpChangeToMultioperationExecute
      OnUpdate = AddOpChangeToMultioperationUpdate
    end
    object ActRemove: TAction
      Caption = 'Remove'
      OnExecute = ActRemoveAccountFromMultioperationExecute
      OnUpdate = ActRemoveAccountFromMultioperationUpdate
    end
    object ActSign: TAction
      Caption = 'Sign'
      OnExecute = ActSignMultioperationExecute
      OnUpdate = ActExecuteOperationUpdate
    end
  end
end
