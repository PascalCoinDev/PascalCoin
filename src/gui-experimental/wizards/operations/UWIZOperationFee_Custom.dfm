object WIZOperationFee_Custom: TWIZOperationFee_Custom
  Left = 0
  Height = 253
  Top = 0
  Width = 429
  ActiveControl = fseFee
  Caption = 'WIZOperationFee_Custom'
  ClientHeight = 253
  ClientWidth = 429
  Visible = False
  OnResize = FormResize
  object gbTransactionFee: TGroupBox
    Left = 0
    Height = 248
    Top = 0
    Width = 422
    Anchors = [akTop, akLeft, akRight, akBottom]
    Caption = 'Operation Fee'
    TabOrder = 0
    object fseFee: TNumberBox
      Mode = nbmFloat
      Left = 11
      Height = 23
      Top = 35
      Width = 125
      Decimal = 4
      SmallStep = 0.0001
      MaxValue = 1
      MinValue = 0
      OnChange = fseFeeChange
      TabOrder = 0
      Value = 0
    end
    object lblNote1: TLabel
      Left = 11
      Height = 15
      Top = 8
      Width = 179
      Caption = 'Please enter the fee per operation.'
      ParentColor = False
    end
    object lblPASC: TLabel
      Left = 144
      Height = 15
      Top = 40
      Width = 28
      Caption = 'PASC'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object lblestimatedfee: TLabel
      Left = 246
      Height = 15
      Top = 40
      Width = 53
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Total Fee:'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object lblTotalFeeValue: TLabel
      Left = 310
      Height = 15
      Top = 40
      Width = 112
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'xxxx'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object cbOverrideDefaultFee: TCheckBox
      Left = 11
      Height = 19
      Top = 72
      Width = 262
      Caption = 'Make this my default fee for future operations'
      TabOrder = 1
    end
    object chkSingleFeePayer: TCheckBox
      Left = 11
      Top = 112
      Width = 350
      Height = 23
      Caption = 'Select a single fee payer'
      TabOrder = 2
      Visible = False
      OnClick = SingleFeePayerChange
    end
    object cbFeePayer: TComboBox
      Left = 11
      Top = 144
      Width = 300
      Height = 23
      Style = csDropDownList
      TabOrder = 3
      Visible = False
      OnChange = FeePayerChange
    end
    object lblFeePayerBalance: TLabel
      Left = 11
      Top = 176
      Width = 350
      Height = 20
      Caption = 'Each updated account pays its own fee.'
      Visible = False
    end
  end
end
