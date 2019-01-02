unit UGUIUtils;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Dialogs;

function InputQueryPassword(ACaption, APrompt : String; var defValue : String) : Boolean;


implementation

function InputQueryPassword(ACaption, APrompt : String; var defValue : String) : Boolean;
begin
  {$IFDEF FPC}
  Result := InputQuery(ACaption,APrompt,True,defValue);
  {$ELSE}
  Result := InputQuery(ACaption,#31+APrompt,defValue);
  {$ENDIF}
end;

end.

