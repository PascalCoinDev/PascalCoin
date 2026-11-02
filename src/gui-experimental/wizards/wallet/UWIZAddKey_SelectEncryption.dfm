object WIZAddKey_SelectEncryption: TWIZAddKey_SelectEncryption
  Left = -1207
  Height = 240
  Top = 90
  Width = 320
  Caption = 'WIZAddKey_SelectEncryption'
  ClientHeight = 240
  ClientWidth = 320
  OnCreate = FormCreate
  object rgKeyType: TRadioGroup
    Left = 24
    Height = 209
    Top = 16
    Width = 278
    Anchors = [akTop, akLeft, akRight, akBottom]
    Caption = 'Select the type of encryption:'
    Items.Strings = (
      'asdf'
    )
    TabOrder = 0
  end
end
