unit UGrids;

{$mode delphi}

interface

uses
  Classes, SysUtils, UAccounts, UVisualGrid, UCommonUI, UCommon;

type

  TMyAccountDataSource = class(TComponent, IDataSource)
    public
      function FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
      function GetSearchCapabilities: TSearchCapabilities;
  end;


implementation

uses UWallet,  UUserInterface;

function TMyAccountDataSource.FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
var
  key : Integer;
  i : Integer;
  accountsByKeys : TOrderedAccountKeysList;
begin
  ADataTable.Columns := TTableColumns.Create('Account', 'Key', 'Name', 'Balance');

  accountsByKeys := TOrderedAccountKeysList.Create(TUserInterface.Node.Bank.SafeBox,false);

  for key := 0 to TWallet.Keys.Count - 1 do begin
//    accountsByKeys.AddAccountKey(TAccountKey(TWallet.Keys.Key[i]));
  end;

end;

function TMyAccountDataSource.GetSearchCapabilities: TSearchCapabilities;
begin
   Result :=  TSearchCapabilities.Create(
     TSearchCapability.From('Account', SORTABLE_NUMERIC_FILTER),
     TSearchCapability.From('Balance', SORTABLE_NUMERIC_FILTER)
   );
end;


end.

