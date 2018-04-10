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
  public type

    { TModelType }

    TModelType = (omtSendPasc, omtTransferAccount, omtChangeAccountPrivateKey, omtAddKey);

    { TPayloadEncryptionMode }

    TPayloadEncryptionMode = (akaEncryptWithSender, akaEncryptWithReceiver, akaEncryptWithPassword, akaNotEncrypt);

    { TOperationSigningMode }

    TOperationSigningMode = (akaPrimary, akaSecondary);

    { TSendPASCModel }

    TSendPASCModel = class(TComponent)
    public
      SelectedIndex: integer;
      SingleAmountToSend: int64;
      DestinationAccount: TAccount;
      SelectedAccounts: TArray<TAccount>;
    end;

    { TTransferAccountModel }

    TTransferAccountModel = class(TComponent)
    public
      NewPublicKey : string;
      SelectedIndex: integer;
      AccountKey: TAccountKey;
      SelectedAccounts: TArray<TAccount>;
    end;

    { TChangeAccountPrivateKeyModel }

    TChangeAccountPrivateKeyModel = class(TComponent)
    public
      NewPublicKey : string;
      SelectedIndex, PrivateKeySelectedIndex: integer;
      NewWalletKey: TWalletKey;
      EncodedPayload: TRawBytes;
      SelectedAccounts: TArray<TAccount>;
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
    end;

    { TPayloadModel }

    TPayloadModel = class(TComponent)
    public
      HasPayload: boolean;
      Content, Password : string;
      Mode: TPayloadEncryptionMode;
      EncodedBytes: TRawBytes;
    end;

  private
    FModelType: TModelType;
    FSendPASC : TSendPASCModel;
    FTransferAccount : TTransferAccountModel;
    FChangeAccountPrivateKey : TChangeAccountPrivateKeyModel;
    FFee : TFeeModel;
    FSigner : TSignerModel;
    FPayload : TPayloadModel;
  public
    constructor Create(AOwner: TComponent; AType: TModelType); overload;
    property ModelType: TModelType read FModelType;
    property SendPASC: TSendPASCModel read FSendPASC;
    property TransferAccount: TTransferAccountModel read FTransferAccount;
    property ChangeAccountPrivateKey : TChangeAccountPrivateKeyModel read FChangeAccountPrivateKey;
    property Fee : TFeeModel read FFee;
    property Signer : TSignerModel read FSigner;
    property Payload : TPayloadModel read FPayload;
  end;

implementation

constructor TWIZOperationsModel.Create(AOwner: TComponent; AType: TWIZOperationsModel.TModelType);
begin
  inherited Create(AOwner);
  FModelType := AType;
  FSendPASC := TSendPASCModel.Create(Self);
  FTransferAccount := TTransferAccountModel.Create(Self);
  FChangeAccountPrivateKey := TChangeAccountPrivateKeyModel.Create(Self);
  FChangeAccountPrivateKey := TChangeAccountPrivateKeyModel.Create(Self);
  FFee := TFeeModel.Create(Self);
  FSigner := TSignerModel.Create(Self);
  FPayload := TPayloadModel.Create(Self);
end;

end.
