object FRMOperation: TFRMOperation
  Left = 60
  Top = 0
  ActiveControl = ebDestAccount
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'New Operation'
  ClientHeight = 443
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 20
    Top = 20
    Width = 43
    Height = 13
    Caption = 'Account:'
  end
  object lblAccountBalance: TLabel
    Left = 225
    Top = 15
    Width = 149
    Height = 19
    Caption = 'lblAccountBalance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 180
    Top = 20
    Width = 41
    Height = 13
    Caption = 'Balance:'
  end
  object bbExecute: TBitBtn
    Left = 295
    Top = 386
    Width = 130
    Height = 31
    Caption = 'Execute (F12)'
    DoubleBuffered = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
    OnClick = actExecuteExecute
  end
  object bbCancel: TBitBtn
    Left = 460
    Top = 386
    Width = 116
    Height = 31
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object ebSenderAccount: TEdit
    Left = 69
    Top = 12
    Width = 97
    Height = 27
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Text = 'ebSenderAccount'
    OnExit = ebSenderAccountExit
    OnKeyPress = ebSenderAccountKeyPress
  end
  object PageControl: TPageControl
    Left = 20
    Top = 50
    Width = 556
    Height = 321
    ActivePage = tsOperation
    TabOrder = 1
    object tsOperation: TTabSheet
      TabVisible = False
      object gbOperation: TGroupBox
        Left = 13
        Top = 8
        Width = 521
        Height = 167
        Caption = ' Operation: '
        TabOrder = 0
        object lblDestAccount: TLabel
          Left = 96
          Top = 42
          Width = 68
          Height = 13
          Caption = 'Dest. Account'
        end
        object lblAmount: TLabel
          Left = 255
          Top = 42
          Width = 37
          Height = 13
          Caption = 'Amount'
        end
        object lblNewPrivateKey: TLabel
          Left = 59
          Top = 85
          Width = 83
          Height = 13
          Caption = 'New Private Key:'
        end
        object lblTransactionErrors: TLabel
          Left = 170
          Top = 21
          Width = 331
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Errors detected'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblChangeKeyErrors: TLabel
          Left = 170
          Top = 63
          Width = 331
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Errors detected'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblNewOwnerPublicKey: TLabel
          Left = 59
          Top = 138
          Width = 105
          Height = 13
          Caption = 'New owner Public Key'
        end
        object lblNewOwnerErrors: TLabel
          Left = 199
          Top = 113
          Width = 302
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Errors detected'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblFee: TLabel
          Left = 407
          Top = 42
          Width = 18
          Height = 13
          Caption = 'Fee'
        end
        object rbTransaction: TRadioButton
          Left = 15
          Top = 19
          Width = 220
          Height = 17
          Caption = 'Transaction (tx)'
          TabOrder = 0
          OnClick = rbTransactionClick
        end
        object rbChangeKey: TRadioButton
          Left = 15
          Top = 62
          Width = 220
          Height = 17
          Caption = 'Change Private key'
          TabOrder = 4
          OnClick = rbTransactionClick
        end
        object ebDestAccount: TEdit
          Left = 170
          Top = 39
          Width = 76
          Height = 21
          TabOrder = 1
          Text = 'ebDestAccount'
          OnChange = ebDestAccountChange
          OnClick = rbTransactionClick
          OnExit = ebDestAccountExit
        end
        object ebAmount: TEdit
          Left = 298
          Top = 39
          Width = 86
          Height = 21
          TabOrder = 2
          Text = 'Edit1'
          OnChange = ebDestAccountChange
          OnClick = rbTransactionClick
          OnExit = ebFeeExit
        end
        object cbNewPrivateKey: TComboBox
          Left = 170
          Top = 82
          Width = 294
          Height = 21
          Style = csDropDownList
          Sorted = True
          TabOrder = 5
          OnChange = cbNewPrivateKeyChange
          OnClick = rbTransactionClick
        end
        object rbTransferToANewOwner: TRadioButton
          Left = 15
          Top = 112
          Width = 220
          Height = 17
          Caption = 'Transfer account to a new owner'
          TabOrder = 7
          OnClick = rbTransactionClick
        end
        object ebNewPublicKey: TEdit
          Left = 170
          Top = 135
          Width = 331
          Height = 21
          TabOrder = 8
          Text = 'ebDestAccount'
          OnChange = ebNewPublicKeyChange
          OnClick = rbTransactionClick
        end
        object ebFee: TEdit
          Left = 431
          Top = 39
          Width = 70
          Height = 21
          TabOrder = 3
          Text = 'ebFee'
          OnChange = ebDestAccountChange
          OnClick = rbTransactionClick
          OnExit = ebFeeExit
        end
        object bbKeys: TBitBtn
          Left = 470
          Top = 81
          Width = 31
          Height = 22
          DoubleBuffered = True
          Glyph.Data = {
            36050000424D3605000000000000360400002800000010000000100000000100
            0800000000000001000000000000000000000001000000010000FF00FF008C6B
            6C0087707000AE877C000D7FA9006F7C88006D7C94001F7ECE000E80AA001180
            A7001081AB00048CB900078DBC000B8DBA000C8DBA00088EBC001285B0007882
            95006092BD005EA8BE000A91C1000F9DCE002087DE0011A7D10030BCDC001F89
            E00059A9DC0044BADD004ABFE00057AEF4004DB1F90049B2FF004EB7FF0057B1
            F60050B6FE0022D7FC0024D7FF0024D8FD0039D7FB0035D8FD004BC6DC0046C6
            E40048D5EE0075D3E90058E9FD006FE6FF0070E6FF0071F9FE007BFFFF008683
            88008B8697008F989B00969594009C919000AD858000AD868500AB939500A49E
            9900B1979400B5A09F008AA5AD00CAA08C00CDAC9300C2A69A00F3BE8000C6AE
            A000CFB7A100D3BBA200F4C88E00F5CB9A00F5D09C00F8D09800DAC5B700E4CC
            A900EFD2A900ECD1AC00F6DAAB00F5DEB500F5E1B600F9E1B100FEEAB900FFF2
            BA00A1C6C8008DE6FA0081F8FE008CFAFD008DFCFE0097FCFD009BFBFD00B8ED
            F600A7FFFF00AAFFFE00ADFFFE00B6F6FF00B1FCFD00B4FFFF00ECDDCC00E8DD
            D600FFF7C600FCF5CD00FCF7D100FAF6D600FFFBD500FEFED600F7F2D900FEFF
            D900FFFEDD00C6F5FF00C6FEFF00D2FFFF00FEF7E000FBFCE100FDFFE100FFFF
            E400E3FEFF00F9F6F200FFFFF400F1FBFC00F5FFFE00FBFFFF00000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00000000000000000605000004080408080A000000000011191A000B2A23272D
            531B080000001116201D0B552C23272E531C1509003207201D000F552C23272B
            3A3F41030112202000000F552C252938606771684236200000000B5F5D6B3B61
            74676A67513D000000000B59181735716A676A63474B360000000F282C23396A
            6A6A6A4C404D360000000B552C2534656A654F455049360000000B552C251343
            6247446E7336000000000B552C25263C3E4B4E483636000000000F55542F3057
            523331020000000000000B77766D5F5C5C5C2F08000000000000001476726C5C
            5A58100000000000000000000F0F0B0F0F0F0000000000000000}
          ParentDoubleBuffered = False
          TabOrder = 6
          OnClick = bbKeysClick
        end
      end
      object gbPayload: TGroupBox
        Left = 13
        Top = 181
        Width = 521
        Height = 119
        Caption = ' Payload: '
        TabOrder = 1
        object lblEncryptPassword: TLabel
          Left = 38
          Top = 76
          Width = 50
          Height = 13
          Caption = 'Password:'
        end
        object Label4: TLabel
          Left = 255
          Top = 15
          Width = 67
          Height = 13
          Caption = 'Payload data:'
        end
        object lblEncryptionErrors: TLabel
          Left = 255
          Top = 96
          Width = 246
          Height = 13
          AutoSize = False
          Caption = 'Errors detected'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblPayloadLength: TLabel
          Left = 483
          Top = 15
          Width = 18
          Height = 13
          Alignment = taRightJustify
          Caption = '000'
        end
        object rbEncryptedWithEC: TRadioButton
          Left = 15
          Top = 35
          Width = 231
          Height = 17
          Caption = 'Encrypted with dest account public key'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnClick = memoPayloadClick
        end
        object rbEncrptedWithPassword: TRadioButton
          Left = 15
          Top = 53
          Width = 231
          Height = 17
          Caption = 'Encrypted with Password'
          TabOrder = 2
          OnClick = memoPayloadClick
        end
        object rbNotEncrypted: TRadioButton
          Left = 15
          Top = 93
          Width = 231
          Height = 17
          Caption = 'Don'#39't encrypt (Public payload)'
          TabOrder = 4
          OnClick = memoPayloadClick
        end
        object ebEncryptPassword: TEdit
          Left = 94
          Top = 73
          Width = 122
          Height = 21
          TabOrder = 3
          Text = 'ebDestAccount'
          OnChange = ebEncryptPasswordChange
          OnClick = memoPayloadClick
        end
        object memoPayload: TMemo
          Left = 255
          Top = 37
          Width = 246
          Height = 57
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          WantReturns = False
          OnChange = memoPayloadClick
          OnClick = memoPayloadClick
        end
        object rbEncryptedWithOldEC: TRadioButton
          Left = 15
          Top = 16
          Width = 231
          Height = 17
          Caption = 'Encrypted with old public key'
          TabOrder = 0
          OnClick = memoPayloadClick
        end
      end
    end
    object tsGlobalError: TTabSheet
      ImageIndex = 1
      TabVisible = False
      object lblGlobalErrors: TLabel
        Left = 40
        Top = 50
        Width = 456
        Height = 156
        Alignment = taCenter
        AutoSize = False
        Caption = '????'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object bbPassword: TBitBtn
        Left = 191
        Top = 243
        Width = 159
        Height = 38
        Caption = 'Password'
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
        OnClick = bbPasswordClick
      end
    end
  end
  object ActionList1: TActionList
    Left = 135
    Top = 325
    object actExecute: TAction
      Caption = 'Execute (F12)'
      ShortCut = 123
      OnExecute = actExecuteExecute
    end
  end
end
