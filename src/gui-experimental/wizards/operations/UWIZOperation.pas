unit UWIZOperation;

{ Copyright (c) 2018 by Sphere 10 Software <http://www.sphere10.com/>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Ugochukwu Mmaduekwe - main developer
  - Herman Schoenfeld - designer

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$mode delphi}

interface

uses
  Classes, SysUtils, UAccounts, UWallet, UBaseTypes, UCoreObjects;

type

  { TWIZOperationsModel }

  TWIZOperationsModel = class(TComponent)
    public type

      { TOperationExecuteResultHandler }

      TOperationExecuteResultHandler =
      procedure(const ASourceAccount: TAccount; AOpType: TExecuteOperationType; const AOpText: ansistring; Result: boolean; const Message: ansistring) of object;


      { TAccountModel }

      TAccountModel = class(TComponent)
      public
        SelectedAccounts: TArray<TAccount>;
        Count: Integer;
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
        PayloadEncryptionMode: TPayloadEncryptionMode;
        EncodedBytes: TRawBytes;
      end;

      { TChangeAccountInfoModel }

      TChangeAccountInfoModel = class(TComponent)
      public
        NewName: TRawBytes;
        NewType: Word;
      end;

      { TBuyAccountModel }

      TBuyAccountModel = class(TComponent)
      public
        AccountToBuy: TAccount;
        Amount: Int64;
        NewOwnerPublicKey: TAccountKey;
        SelectedIndex: integer;
      end;

    private
      FExecuteOperationType: TExecuteOperationType;
      FRelockOnFinish: Boolean; static;
      FAccount: TAccountModel;
      FSendPASC: TSendPASCModel;
      FChangeKey: TChangeKeyModel;
      FTransferAccount: TTransferAccountModel;
      FChangeAccountPrivateKey: TChangeAccountPrivateKeyModel;
      FEnlistAccountForSale: TEnlistAccountForSaleModel;
      FFee: TFeeModel;
      FSigner: TSignerModel;
      FPayload: TPayloadModel;
      FChangeAccountInfo: TChangeAccountInfoModel;
      FBuyAccount: TBuyAccountModel;
    public
      constructor Create(AOwner: TComponent; AType: TExecuteOperationType); overload;
      property ExecuteOperationType: TExecuteOperationType read FExecuteOperationType;
      property Account: TAccountModel read FAccount;
      property SendPASC: TSendPASCModel read FSendPASC;
      property ChangeKey: TChangeKeyModel read FChangeKey;
      property TransferAccount: TTransferAccountModel read FTransferAccount;
      property ChangeAccountPrivateKey: TChangeAccountPrivateKeyModel read FChangeAccountPrivateKey;
      property EnlistAccountForSale: TEnlistAccountForSaleModel read FEnlistAccountForSale;
      property Fee: TFeeModel read FFee;
      property Signer: TSignerModel read FSigner;
      property Payload: TPayloadModel read FPayload;
      property ChangeAccountInfo: TChangeAccountInfoModel read FChangeAccountInfo;
      property BuyAccount: TBuyAccountModel read FBuyAccount;
      class property RelockOnFinish: Boolean read FRelockOnFinish write FRelockOnFinish;
  end;

implementation

{ TWIZOperationsModel }

constructor TWIZOperationsModel.Create(AOwner: TComponent; AType: TExecuteOperationType);
begin
  inherited Create(AOwner);
  FExecuteOperationType := AType;
  FRelockOnFinish := False;
  FAccount := TAccountModel.Create(Self);
  FSendPASC := TSendPASCModel.Create(Self);
  FChangeKey := TChangeKeyModel.Create(Self);
  FTransferAccount := TTransferAccountModel.Create(Self);
  FChangeAccountPrivateKey := TChangeAccountPrivateKeyModel.Create(Self);
  FEnlistAccountForSale := TEnlistAccountForSaleModel.Create(Self);
  FFee := TFeeModel.Create(Self);
  FSigner := TSignerModel.Create(Self);
  FPayload := TPayloadModel.Create(Self);
  FChangeAccountInfo := TChangeAccountInfoModel.Create(Self);
  FBuyAccount := TBuyAccountModel.Create(Self);
end;

end.
