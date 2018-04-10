unit UWIZModels;


{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses Classes, SysUtils, UWizard, UAccounts, UBlockChain, UWallet, UBaseTypes, Generics.Defaults;

type

  { TWIZAddKeyAction }

  TWIZAddKeyAction = (akaGenerateKey, akaImportPrivateKey, akaImportPublicKey);

  { TFRMAddKeyModel }

  TWIZAddKeyModel = class(TComponent)
    public
      Name : String;
      KeyText : String;
      Password : String;
      EncryptionTypeNID : word;
      Action : TWIZAddKeyAction;
  end;

  { TWIZOperationsModel }

  TWIZOperationsModel = class (TComponent)
    public type

       { TModelType }

       TModelType = (omtSendPasc, omtTransferAccount, omtChangeAccountPrivateKey, omtAddKey);

       { TPayloadEncryptionMode }

       TPayloadEncryptionMode = (akaEncryptWithOldEC, akaEncryptWithEC, akaEncryptWithPassword, akaNotEncrypt);

       { TSendPASCModel }

       TSendPASCModel = class(TComponent)
       public
         SelectedIndex: integer;
         AmountToSend : string;
         DestinationAccount: TAccount;
         SelectedAccounts: TArray<TAccount>;
       end;

       { TTransferAccountModel }

       TTransferAccountModel = class(TComponent)
       public
         NewPublicKey: string;
         SelectedIndex: integer;
         AccountKey: TAccountKey;
         SelectedAccounts: TArray<TAccount>;
       end;

       { TChangeAccountPrivateKeyModel }

       TChangeAccountPrivateKeyModel = class(TComponent)
       public
         NewPublicKey: string;
         SelectedIndex, PrivateKeySelectedIndex: integer;
         NewWalletKey: TWalletKey;
         SelectedAccounts: TArray<TAccount>;
       end;

       { TFeeModel }

       TFeeModel = class(TComponent)
       public
         DefaultFee: int64;
       end;

       { TSignerModel }

       TSignerModel = class(TComponent)
       public
         SignerAccount: TAccount;
       end;

       { TPayloadModel }

       TPayloadModel = class(TComponent)
       public
         Payload, EncryptionPassword : string;
         PayloadEncryptionMode: TPayloadEncryptionMode;
         EncodedPayload: TRawBytes;
       end;

    private
      FModelType : TModelType;
      FSendModel : TSendPASCModel;
      FTransferAccountModel : TTransferAccountModel;
      FChangeAccountPrivateKeyModel : TChangeAccountPrivateKeyModel;
      FFeeModel : TFeeModel;
      FSignerModel : TSignerModel;
      FPayloadModel : TPayloadModel;
    public
      constructor Create(AOwner : TComponent; AType : TModelType);  overload;
      property ModelType : TModelType read FModelType;
      property SendPASCModel : TSendPASCModel read FSendModel;
      property TransferAccountModel : TTransferAccountModel read FTransferAccountModel;
      property ChangeAccountPrivateKeyModel : TChangeAccountPrivateKeyModel read FChangeAccountPrivateKeyModel;
      property FeeModel : TFeeModel read FFeeModel;
      property SignerModel : TSignerModel read FSignerModel;
      property PayloadModel : TPayloadModel read FPayloadModel;
  end;

implementation

constructor TWIZOperationsModel.Create(AOwner : TComponent; AType : TWIZOperationsModel.TModelType);
begin
  inherited Create(AOwner);
  FModelType := AType;
  FSendModel := TSendPASCModel.Create(Self);
  FTransferAccountModel := TTransferAccountModel.Create(Self);
  FChangeAccountPrivateKeyModel := TChangeAccountPrivateKeyModel.Create(Self);
  FFeeModel := TFeeModel.Create(Self);
  FSignerModel := TSignerModel.Create(Self);
end;

end.

