object FRMPascalCoinWalletConfig: TFRMPascalCoinWalletConfig
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 460
  ClientWidth = 374
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 30
    Top = 335
    Width = 120
    Height = 13
    Caption = 'Default fee for operation'
  end
  object Label2: TLabel
    Left = 30
    Top = 116
    Width = 98
    Height = 13
    Caption = 'Internet Server Port'
  end
  object lblDefaultInternetServerPort: TLabel
    Left = 248
    Top = 116
    Width = 70
    Height = 13
    Caption = '(Default XXXX)'
  end
  object Label3: TLabel
    Left = 15
    Top = 67
    Width = 60
    Height = 13
    Caption = 'Miner Name:'
  end
  object Label4: TLabel
    Left = 90
    Top = 90
    Width = 259
    Height = 13
    Caption = 'This name will be included in each new block you mine!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 30
    Top = 176
    Width = 58
    Height = 13
    Caption = 'CPUs to use'
  end
  object lblMaxCPUS: TLabel
    Left = 248
    Top = 176
    Width = 70
    Height = 13
    Caption = '(Default XXXX)'
  end
  object cbAutomaticMiningWhenConnectedToNodes: TCheckBox
    Left = 15
    Top = 150
    Width = 261
    Height = 17
    Caption = 'Allow automatic mining when connected to nodes'
    TabOrder = 4
  end
  object ebDefaultFee: TEdit
    Left = 170
    Top = 332
    Width = 56
    Height = 21
    Alignment = taRightJustify
    TabOrder = 7
    Text = '0'
  end
  object cbSaveLogFiles: TCheckBox
    Left = 15
    Top = 359
    Width = 97
    Height = 17
    Caption = 'Save log file'
    TabOrder = 8
    OnClick = cbSaveLogFilesClick
  end
  object cbShowLogs: TCheckBox
    Left = 15
    Top = 397
    Width = 97
    Height = 17
    Caption = 'Show logs'
    TabOrder = 10
  end
  object bbOk: TBitBtn
    Left = 171
    Top = 410
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 11
    OnClick = bbOkClick
  end
  object bbCancel: TBitBtn
    Left = 261
    Top = 410
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 12
  end
  object udInternetServerPort: TUpDown
    Left = 226
    Top = 113
    Width = 16
    Height = 21
    Associate = ebInternetServerPort
    Min = 1
    Max = 40000
    Position = 4004
    TabOrder = 3
    Thousands = False
  end
  object ebInternetServerPort: TEdit
    Left = 170
    Top = 113
    Width = 56
    Height = 21
    Alignment = taRightJustify
    TabOrder = 2
    Text = '4004'
  end
  object bbUpdatePassword: TBitBtn
    Left = 15
    Top = 14
    Width = 336
    Height = 38
    Caption = 'Wallet Password'
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Glyph.Data = {
      76060000424D7606000000000000360400002800000018000000180000000100
      0800000000004002000000000000000000000001000000010000000000000101
      0100020202000303030004040400050505000606060007070700080808000909
      09000A0A0A000B0B0B000C0C0C000D0D0D000E0E0E000F0F0F00101010001111
      1100121212001313130014141400151515001616160017171700181818001919
      19001A1A1A001B1B1B001C1C1C001D1D1D001E1E1E001F1F1F00202020002121
      2100222222002323230024242400252525002626260027272700282828002929
      29002A2A2A002B2B2B002C2C2C002D2D2D002E2E2E002F2F2F00303030003131
      3100323232003333330034343400353535003636360037373700383838003939
      39003A3A3A003B3B3B003C3C3C003D3D3D003E3E3E003F3F3F00404040004141
      4100424242004343430044444400454545004646460047474700484848004949
      49004A4A4A004B4B4B004C4C4C004D4D4D004E4E4E004F4F4F00505050005151
      5100525252005353530054545400555555005656560057575700585858005959
      59005A5A5A005B5B5B005C5C5C005D5D5D005E5E5E00685968007C4F7C009441
      9400B72CB700DD15DD00F506F500FD01FD00FE00FE00FE00FE00FE00FE00FE00
      FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
      FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
      FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
      FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
      FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
      FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
      FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
      FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
      FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
      FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
      FE00FE00FE00FE00FE00FE00FE00F008F900D517F000BD24E800A830E1007B4B
      D0005065C1003774B7002B7DB100237FAC001F82AD001C83AD001785AE001687
      AF001489B100128AB300108CB6000F8EB9000D91BB000C94BF000A97C3000A99
      C5000A9AC7000B9BC8000B9CCA000C9ECC000C9FCD000DA0CE000DA1CE000EA2
      CF0011A3CF0015A4CF0018A5CF001CA7D10021A9D1002AAAD00035AACD0035AD
      D00035AFD30035B1D60037B5D8003AB8DB003EB9DC0040BBDD0045BEDE004CC1
      E00055C6E3005BC8E40061CAE40066CBE3006BCCE30072D3E60078D8EB0080D8
      EE0083D9F10085DCF40088DBF5008BDBF6008EDAF70091DAF70095DCF70097E0
      F7009EE3F800A4E7F900A9E9F900AEE9F900B5EBF900B2EBF800919191919191
      919191919191919191919191919191919191919191919191D5E7E8E7E3DED591
      919191919191919191919191919191D6DDF7F8F8F7F6F4EEE6D5919191919191
      919191919191D7DADEF8F8F6F7F6EDF1F8F1E69191919191919191919191DADE
      DFFAFAF8F7F7E9EDF7F7F8EB91919191919191919191DDE5E3FCFCFAFAEEE0E9
      F3F7F7F7DC919191919191919191DEE7E4FEFEFCFCEDC5E0EFF7F6F6DC919191
      919191919191DEEAE7FEFEFEFDFBC4C4F5F7F6F6DC919191919191919191E3EC
      E9FEFEFEFEFEE0C3F9F7F6F6DC919191919191919191E3E9E3F8F8F1F1F8FBFB
      FCFAF7F6DC919191919191919191DCEAEAE0E1E9E5E3DEE4EBF2FAF9DC919191
      919191919191DBF3EEC5C5EFF3EBE5DDD9D6E6F5DC91919191919191919191D5
      E8EDF1FAF6F0EAE4DAD1D2DBDC919191919191919191919191DBE8F0F3F3F3E4
      D0CFE4D8DC919191919191919191919191919191D5DEE4DED2CFD4D491919191
      91919191919191919191919191919191D2CE9191919191919191919191919191
      91C8C89191919191D2CE919191919191919191919191919191D9D09191919191
      D2CE919191919191919191919191919191E6E39191919191D2CE919191919191
      919191919191919191E0F3CE91919191D2CE9191919191919191919191919191
      9191EFF6E1C9C8CDD9CC91919191919191919191919191919191C9EDF9F3F3EB
      DC91919191919191919191919191919191919191CBE0E1CF9191919191919191
      9191919191919191919191919191919191919191919191919191}
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 0
    OnClick = bbUpdatePasswordClick
  end
  object ebMinerName: TEdit
    Left = 90
    Top = 64
    Width = 261
    Height = 21
    TabOrder = 1
    Text = 'ebMinerName'
  end
  object cbShowModalMessages: TCheckBox
    Left = 170
    Top = 359
    Width = 171
    Height = 17
    Caption = 'Show modal messages'
    TabOrder = 13
  end
  object udCPUs: TUpDown
    Left = 226
    Top = 173
    Width = 16
    Height = 21
    Associate = ebCPUs
    Min = 1
    Max = 6
    Position = 1
    TabOrder = 6
    Thousands = False
  end
  object ebCPUs: TEdit
    Left = 170
    Top = 173
    Width = 56
    Height = 21
    Alignment = taRightJustify
    TabOrder = 5
    Text = '1'
  end
  object gbMinerPrivateKey: TGroupBox
    Left = 8
    Top = 200
    Width = 334
    Height = 121
    Caption = ' Miner Private Key: '
    TabOrder = 14
    object rbGenerateANewPrivateKeyEachBlock: TRadioButton
      Left = 20
      Top = 20
      Width = 301
      Height = 17
      Caption = 'Generate a new private key for each generated block'
      TabOrder = 0
    end
    object rbUseARandomKey: TRadioButton
      Left = 20
      Top = 40
      Width = 216
      Height = 17
      Caption = 'Use a random existing key'
      TabOrder = 1
    end
    object rbMineAllwaysWithThisKey: TRadioButton
      Left = 20
      Top = 61
      Width = 216
      Height = 17
      Caption = 'Always mine with this key:'
      TabOrder = 2
    end
    object cbPrivateKeyToMine: TComboBox
      Left = 45
      Top = 85
      Width = 266
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
  end
  object cbSaveDebugLogs: TCheckBox
    Left = 31
    Top = 377
    Width = 97
    Height = 17
    Caption = 'Save debug logs too'
    TabOrder = 9
  end
end
