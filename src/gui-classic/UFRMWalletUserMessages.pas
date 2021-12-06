unit UFRMWalletUserMessages;

interface

uses
{$IFnDEF FPC}
  Messages
{$ELSE}
  LMessages
{$ENDIF};

Const
  CM_PC_WalletKeysChanged = WM_USER + 1;
  CM_PC_NetConnectionUpdated = WM_USER + 2;

implementation

end.
