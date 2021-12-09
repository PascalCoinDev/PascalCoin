object FrameAccountExplorer: TFrameAccountExplorer
  Left = 0
  Top = 0
  Width = 820
  Height = 582
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 400
    Top = 66
    Width = 5
    Height = 516
    ExplicitLeft = 315
    ExplicitTop = -132
    ExplicitHeight = 372
  end
  object pnlMyAccountsTop: TPanel
    Left = 0
    Top = 0
    Width = 820
    Height = 66
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label18: TLabel
      Left = 11
      Top = 35
      Width = 61
      Height = 13
      Caption = 'Find account'
      Color = clBtnFace
      ParentColor = False
    end
    object sbSearchAccount: TSpeedButton
      Left = 176
      Top = 32
      Width = 23
      Height = 22
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
        BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
        2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
        00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
        B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
        EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
        FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
        C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
        FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
        E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
        C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
        C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
        DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
        86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
      OnClick = sbSearchAccountClick
    end
    object cbMyPrivateKeys: TComboBox
      Left = 260
      Top = 7
      Width = 411
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbMyPrivateKeysChange
    end
    object cbExploreMyAccounts: TCheckBox
      Left = 11
      Top = 10
      Width = 235
      Height = 19
      Caption = 'Explore accounts with one of my Wallet Keys'
      TabOrder = 1
      OnClick = cbExploreMyAccountsClick
    end
    object ebFindAccountNumber: TEdit
      Left = 87
      Top = 33
      Width = 83
      Height = 21
      TabOrder = 3
      OnChange = ebFindAccountNumberChange
      OnExit = ebFindAccountNumberExit
    end
    object bbChangeKeyName: TBitBtn
      Left = 685
      Top = 5
      Width = 126
      Height = 25
      Caption = 'Change Key name'
      TabOrder = 2
      OnClick = bbChangeKeyNameClick
    end
    object cbFilterAccounts: TCheckBox
      Left = 260
      Top = 35
      Width = 145
      Height = 19
      Caption = 'Filter accounts by balance'
      TabOrder = 4
      OnClick = cbFilterAccountsClick
    end
    object ebFilterAccountByBalanceMin: TEdit
      Left = 412
      Top = 33
      Width = 83
      Height = 21
      Hint = 'Min balance'
      TabOrder = 5
      OnExit = ebFilterAccountByBalanceMinExit
      OnKeyPress = ebFilterAccountByBalanceMinKeyPress
    end
    object ebFilterAccountByBalanceMax: TEdit
      Left = 503
      Top = 33
      Width = 83
      Height = 21
      Hint = 'Max balance'
      TabOrder = 6
      OnExit = ebFilterAccountByBalanceMaxExit
      OnKeyPress = ebFilterAccountByBalanceMaxKeyPress
    end
  end
  object pnlAccounts: TPanel
    Left = 0
    Top = 66
    Width = 400
    Height = 516
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object dgAccounts: TDrawGrid
      Left = 0
      Top = 0
      Width = 400
      Height = 482
      Align = alClient
      TabOrder = 0
      OnClick = dgAccountsClick
    end
    object pnlAccountsInfo: TPanel
      Left = 0
      Top = 482
      Width = 400
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        400
        34)
      object Label17: TLabel
        Left = 5
        Top = 10
        Width = 48
        Height = 13
        Caption = 'Accounts:'
        Color = clBtnFace
        ParentColor = False
      end
      object Label19: TLabel
        Left = 136
        Top = 10
        Width = 41
        Height = 13
        Caption = 'Balance:'
        Color = clBtnFace
        ParentColor = False
      end
      object lblAccountsCount: TLabel
        Left = 60
        Top = 10
        Width = 18
        Height = 13
        Caption = '000'
        Color = clBtnFace
        ParentColor = False
      end
      object lblAccountsBalance: TLabel
        Left = 200
        Top = 10
        Width = 18
        Height = 13
        Caption = '000'
        Color = clBtnFace
        ParentColor = False
      end
      object bbAccountsRefresh: TBitBtn
        Left = 322
        Top = 6
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Refresh'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6
          A4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFCFBFEFCFBFEFCFBFEFCFBFE
          FCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBC2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FEFCFBFEFCFBFEFCFBFEFCFBD8EBD6018A02018A02D8EBD6FEFCFBFEFC
          FBC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFBF7FEFBF7018A02D8EAD201
          8A02D8EAD2D8EAD2018A02FEFBF7FEFBF7C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FEF9F4FEF9F4018A02018A02D8E8D0FEF9F4FEF9F4D8E8D0FEF9F4FEF9
          F4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF7F0FEF7F0018A02018A0201
          8A02FEF7F0FEF7F0FEF7F0FEF7F0FEF7F0C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FEF5ECFEF5ECFEF5ECFEF5ECFEF5EC018A02018A02018A02FEF5ECFEF5
          ECC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF3E9FEF3E9D8E3C7FEF3E9FE
          F3E9D8E3C7018A02018A02FEF3E9FEF3E9C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FFF1E5FFF1E5018A02D9E2C3D9E2C3018A02D9E2C3018A02FFF1E5FFF1
          E5C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FFF0E2FFF0E2D9E1C1018A0201
          8A02D9E1C1DDCFC2DDCFC2DDCFC2DDCFC2C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FFEEDEFFEEDEFFEEDEFFEEDEFFEEDEFFEEDEC5B5A9C3B4A8C2B3A7C1B2
          A6C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FFECDAFFECDAFFECDAFFECDAFF
          ECDAFFECDAB0A296B0A296B0A296B0A296C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FFEAD7FFEAD7FFEAD7FFEAD7FFEAD7C9B9ACFBF8F4FBF8F4E6DAD9C2A6
          A4FF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4FFE8D3FFE8D3FFE8D3FFE8D3FF
          E8D3C9B9ACFBF8F4DFCEC7C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          C2A6A4FFE6D0FFE6D0FFE6D0FFE6D0FFE6D0C9B9ACDFCEC7C2A6A4FF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2
          A6A4C2A6A4C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        TabOrder = 0
        OnClick = bbAccountsRefreshClick
      end
    end
  end
  object pcAccountsOptions: TPageControl
    Left = 405
    Top = 66
    Width = 415
    Height = 516
    ActivePage = tsMultiSelectAccounts
    Align = alClient
    TabOrder = 2
    object tsAccountOperations: TTabSheet
      Caption = 'Account Operations'
      object dgAccountOperations: TDrawGrid
        Left = 0
        Top = 0
        Width = 407
        Height = 488
        Align = alClient
        TabOrder = 0
      end
    end
    object tsMultiSelectAccounts: TTabSheet
      Caption = 'Selected Accounts For Batch Operation'
      ImageIndex = 1
      object dgSelectedAccounts: TDrawGrid
        Left = 41
        Top = 31
        Width = 320
        Height = 431
        Align = alLeft
        TabOrder = 0
      end
      object pnlSelectedAccountsTop: TPanel
        Left = 0
        Top = 0
        Width = 407
        Height = 31
        Align = alTop
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object Label15: TLabel
          Left = 41
          Top = 4
          Width = 361
          Height = 16
          Caption = 'Select multiple accounts to execute massive operations'
          Color = clBtnFace
          ParentColor = False
        end
      end
      object pnlSelectedAccountsBottom: TPanel
        Left = 0
        Top = 462
        Width = 407
        Height = 26
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object Label20: TLabel
          Left = 41
          Top = 6
          Width = 48
          Height = 13
          Caption = 'Accounts:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblSelectedAccountsCount: TLabel
          Left = 96
          Top = 6
          Width = 18
          Height = 13
          Caption = '000'
          Color = clBtnFace
          ParentColor = False
        end
        object Label22: TLabel
          Left = 156
          Top = 6
          Width = 88
          Height = 13
          Caption = 'Accounts Balance:'
          Color = clBtnFace
          ParentColor = False
        end
        object lblSelectedAccountsBalance: TLabel
          Left = 250
          Top = 6
          Width = 18
          Height = 13
          Caption = '000'
          Color = clBtnFace
          ParentColor = False
        end
      end
      object pnlSelectedAccountsLeft: TPanel
        Left = 0
        Top = 31
        Width = 41
        Height = 431
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 3
        object sbSelectedAccountsAdd: TSpeedButton
          Left = 2
          Top = 0
          Width = 33
          Height = 31
          Caption = '>'
          OnClick = sbSelectedAccountsAddClick
        end
        object sbSelectedAccountsAddAll: TSpeedButton
          Left = 2
          Top = 37
          Width = 33
          Height = 31
          Caption = '>>'
          OnClick = sbSelectedAccountsAddAllClick
        end
        object sbSelectedAccountsDel: TSpeedButton
          Left = 2
          Top = 74
          Width = 33
          Height = 31
          Caption = '<'
          OnClick = sbSelectedAccountsDelClick
        end
        object sbSelectedAccountsDelAll: TSpeedButton
          Left = 2
          Top = 111
          Width = 33
          Height = 31
          Caption = '<<'
          OnClick = sbSelectedAccountsDelAllClick
        end
      end
      object bbSelectedAccountsOperation: TBitBtn
        Left = 367
        Top = 31
        Width = 75
        Height = 61
        Caption = 'Operations'
        Glyph.Data = {
          F6060000424DF606000000000000360000002800000018000000180000000100
          180000000000C0060000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FF019ACF019ACF019ACF019ACFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0C8518FF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FF0D9FD18BD4EE6BD3F845C0ED28B0E0019ACF01
          9ACF019ACF019ACFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0C85180C8518
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF069CD076C8E5A9E9FE6DD8
          FF75DBFF77DCFF77DBFF63D1F930B3E3029BD0019ACF019ACF019ACF019ACFFF
          00FF0C85181399220C8518FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF019ACF
          34AFD9BCE9F86ED8FF6FD8FE70D8FE70D8FE71D8FF0C85180C85180C85180C85
          180C85180C85180C85180C85181DAC31139A220C8518FF00FFFF00FFFF00FFFF
          00FFFF00FF019ACF1FA9D68FD3EB97E4FF6FD9FE71D9FE71D9FE71D9FE0C8518
          57E38851DD7E4AD77443D0693BC95E34C1522BBA4725B33C1EAE33149B230C85
          18FF00FFFF00FFFF00FFFF00FF019ACF31B1DC49B7DEBDEEFB71DDFE77DEFE77
          DEFE77DEFE0C85185EE89059E48953DE804CD87645D16C3DCA6035C2542DBB49
          26B53F1FAF35149B250C8518FF00FFFF00FFFF00FF019ACF52C2E71DA7D5ADE2
          F38FE8FF7CE2FE7CE3FE7CE3FE0C851861EB955FE9925AE58B54DF824DD97846
          D26D3ECB6237C4562FBD4C27B64021B037159B250C8518FF00FFFF00FF019ACF
          60CAEF1FA8D85EC1E1C2E6ED8ACEE08FCFE18ECFE10C851861EB9561EB955FEA
          935CE58D56E0844FDB7A48D47040CD6538C65931BF4D1DA3320C8518FF00FFFF
          00FFFF00FF019ACF65CFF53EB7E52CA9D4C5EFF8ACF3FEA5F2FFA5F2FF0C8518
          61EB9561EB9561EB9561EB945CE68E57E18650DC7C49D57242CE6727AD410C85
          18FF00FFFF00FFFF00FFFF00FF019ACF69D1F855C4F32A9CC673CBE7D6FEFDB1
          FBFDB2FBFD0C85180C85180C85180C85180C85180C85180C85180C851852DD7F
          32B6500C851898FAFF019ACFFF00FFFF00FFFF00FF019ACF77D5FC5CC8FB748E
          A224A8D5B9E7F3D5F5F9D5F6F9D6F6FADCFAFBCDFDFCB9FCFCAFFAFCB0FAFCB1
          FAFC0C85183ABE5C0C85189FFCFFA4FFFF43C1E2019ACFFF00FFFF00FF019ACF
          8BDBFF5FCDFFB7898973C3DD18A2D218A2D216A2D215A1D21AA4D391D7EBEBFE
          FDDBFDFCC5FBFBC2FBFB0C85180C851883E4F3B6FDFFBAFFFFB5FCFD019ACFFF
          00FFFF00FF019ACF99E2FF67D3FFB88989FEF5ECFDF3EBF0EFEAE5EBE8D6E5E6
          A4D2E025A6D34DB9DDE5F8FBF5FDFCEBFCFB0C8518C4FBFF9CE4F2DAFEFFD9FE
          FFE3FFFFADE9F5019ACFFF00FF019ACF9FE9FF70DCFFB88989FEF3E9FFF2E6FE
          F3E9FEF3E9FEF3E9FEF3E9D4E4E439ADD422A5D49DD8ECF1F9FBEEEFEFE9FDFF
          CEEEF7F8FFFFF7FFFFFEFFFFE9F9FD019ACFFF00FF019ACFA7EFFF76E5FFB889
          89FFF2E5FFF0E2FFF2E5FFF2E5FFF2E5FFF2E5FFF2E5EAEBE38EC9DA44B0D501
          9ACF019ACF019ACF019ACF019ACF019ACF019ACF019ACF019ACFFF00FF019ACF
          ABF6FF7EEDFFB88989FFF0E2FFEFDFFFF0E2FFF0E2FFF0E2FFF0E2FFF0E2FEEE
          E0FBECDEFAEBDEF6E6D9B8898993F7FF019ACFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FF019ACFC7FFFF82F5FFB88989FFEEDFFFECDBFFEEDFFFEEDFFFEEDF
          FFEEDFF9E8D9DECCC1D9CABDCFBDB4C8B3ACB88989B5FFFF019ACFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FF019ACFA4E0F0A0FDFFB88989FFECDBFFEBD8FF
          ECDBFFECDBFFECDBFFECDBF5E2D2C4ABA7C2A8A5BBA39FC2AFA9B88989019ACF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF019ACFECFFFFB889
          89FFEBD8FFEAD5FFEBD8FFEBD8FFEBD8FFEBD8FFEBD8D9C8C5FEFEFDFEF6EFDE
          C9C0B88989FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FF019ACFB88989FFE9D5FFE8D3FFE9D5FFE9D5FFE9D5FFE9D5FFE9D5C6AD
          A9FEF8F2E8D4CACD9999FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFB88989FFE7D1FFE7D0FFE7D1FFE7D1FFE7D1
          FFE7D1E7CEBFD3BFB9E8D5CCCD9999FF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB88989FFE6CFFFE6CFFF
          E6CFFFE6CFFFE6CFFFE6CFD5BBB2E0CCC5CD9999FF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB889
          89B88989B88989B88989B88989B88989B88989B88989B88989FF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        Layout = blGlyphTop
        TabOrder = 4
        OnClick = bbSelectedAccountsOperationClick
      end
    end
  end
end
