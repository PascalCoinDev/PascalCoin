unit UWIZModels;


{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses Classes, SysUtils, UWizard, UAccounts, UBlockChain, UWallet,
  UBaseTypes, Generics.Defaults;

type

  { TWIZAddKeyAction }

  TWIZAddKeyAction = (akaGenerateKey, akaImportPrivateKey, akaImportPublicKey);

  { TFRMAddKeyModel }

  TWIZAddKeyModel = class(TComponent)
  public
    Name: string;
    KeyText: string;
    Password: string;
    EncryptionTypeNID: word;
    Action: TWIZAddKeyAction;
  end;

  { TWIZOperationsModel }

  TWIZOperationsModel = class(TComponent)
  public
    type

    { TModelType }

    TModelType = (omtAccount, omtSendPasc, omtChangeKey, omtTransferAccount, omtChangeAccountPrivateKey, omtAddKey, omtEnlistAccountForSale);

    { TPayloadEncryptionMode }

    TPayloadEncryptionMode = (akaEncryptWithSender, akaEncryptWithReceiver, akaEncryptWithPassword, akaNotEncrypt);

    { TOperationSigningMode }

    TOperationSigningMode = (akaPrimary, akaSecondary);

    { TChangeKeyMode }

    TChangeKeyMode = (akaTransferAccountOwnership, akaChangeAccountPrivateKey);

    { TSendPASCMode }

    TSendPASCMode = (akaAllBalance, akaSpecifiedAmount);

    { TAccountSaleMode }
    TAccountSaleMode = (akaPublicSale, akaPrivateSale);


    { TAccountModel }

    TAccountModel = class(TComponent)
    public
      SelectedAccounts: TArray<TAccount>;
    end;

    { TSendPASCModel }

    TSendPASCModel = class(TComponent)
    public
      SingleAmountToSend: int64;
      DestinationAccount: TAccount;
      SendPASCMode: TSendPASCMode;
    end;

    { TChangeKeyModel }

    TChangeKeyModel = class(TComponent)
    public
      ChangeKeyMode: TChangeKeyMode;
    end;

    { TTransferAccountModel }

    TTransferAccountModel = class(TComponent)
    public
      AccountKey: TAccountKey;
    end;

    { TChangeAccountPrivateKeyModel }

    TChangeAccountPrivateKeyModel = class(TComponent)
    public
      SelectedIndex: integer;
      NewWalletKey: TWalletKey;
    end;

    { TWIZEnlistAccountForSaleModel }
    TEnlistAccountForSaleModel = class(TComponent)
    public
      SalePrice: int64;
      NewOwnerPublicKey: TAccountKey;
      LockedUntilBlock: cardinal;
      SellerAccount: TAccount;
      AccountSaleMode: TAccountSaleMode;
    end;

    { TFeeModel }

    TFeeModel = class(TComponent)
    public
      DefaultFee, SingleOperationFee: int64;
    end;

    { TSignerModel }

    TSignerModel = class(TComponent)
    public
      OperationSigningMode: TOperationSigningMode;
      SignerAccount: TAccount;
      SignerCandidates: TArray<TAccount>;
      SelectedIndex: integer;
    end;

    { TPayloadModel }

    TPayloadModel = class(TComponent)
    public
      HasPayload: boolean;
      Content, Password: string;
      Mode: TPayloadEncryptionMode;
      EncodedBytes: TRawBytes;
    end;

  private
    FModelType: TModelType;
    FAccount: TAccountModel;
    FSendPASC: TSendPASCModel;
    FChangeKey: TChangeKeyModel;
    FTransferAccount: TTransferAccountModel;
    FChangeAccountPrivateKey: TChangeAccountPrivateKeyModel;
    FEnlistAccountForSale: TEnlistAccountForSaleModel;
    FFee: TFeeModel;
    FSigner: TSignerModel;
    FPayload: TPayloadModel;
  public
    constructor Create(AOwner: TComponent; AType: TModelType); overload;
    property ModelType: TModelType read FModelType;
    property Account: TAccountModel read FAccount;
    property SendPASC: TSendPASCModel read FSendPASC;
    property ChangeKey: TChangeKeyModel read FChangeKey;
    property TransferAccount: TTransferAccountModel read FTransferAccount;
    property ChangeAccountPrivateKey: TChangeAccountPrivateKeyModel read FChangeAccountPrivateKey;
    property EnlistAccountForSale: TEnlistAccountForSaleModel read FEnlistAccountForSale;
    property Fee: TFeeModel read FFee;
    property Signer: TSignerModel read FSigner;
    property Payload: TPayloadModel read FPayload;
  end;

implementation

constructor TWIZOperationsModel.Create(AOwner: TComponent; AType: TWIZOperationsModel.TModelType);
begin
  inherited Create(AOwner);
  FModelType := AType;
  FAccount := TAccountModel.Create(Self);
  FSendPASC := TSendPASCModel.Create(Self);
  FChangeKey := TChangeKeyModel.Create(Self);
  FTransferAccount := TTransferAccountModel.Create(Self);
  FChangeAccountPrivateKey := TChangeAccountPrivateKeyModel.Create(Self);
  FEnlistAccountForSale := TEnlistAccountForSaleModel.Create(Self);
  FFee := TFeeModel.Create(Self);
  FSigner := TSignerModel.Create(Self);
  FPayload := TPayloadModel.Create(Self);
end;

end.
