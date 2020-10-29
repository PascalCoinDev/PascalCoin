unit UFRMSelectLanguage;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  gnugettext,Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls;

type

   { TFRMChangeLanguage }

   TFRMChangeLanguage = class(TForm)
      bbCancel: TBitBtn;
      bbOk: TBitBtn;
      Label1: TLabel;
      lbxAvailableLanguages: TListBox;
      procedure FormCreate(Sender: TObject);
   private
   public
   end;

   function SelectUILanguage(LanguageCode:String):string;

implementation

  {$R *.dfm}
var
  SelectedLanguageCode : string;

function SelectUILanguage(LanguageCode:String):string;
begin
  result := LanguageCode;
  SelectedLanguageCode := LanguageCode;
  with TFRMChangeLanguage.Create(nil) do
  try
    if (ShowModal=MrOk) and (lbxAvailableLanguages.ItemIndex<>-1) then
       result := NativeLanguageToCode(lbxAvailableLanguages.Items[lbxAvailableLanguages.ItemIndex]);
  finally
    free;
  end;
end;

{ TFRMChangeLanguage }

procedure TFRMChangeLanguage.FormCreate(Sender: TObject);
var
  lst : TStringlist;
  s :string;
  i,y :integer;
begin
   UseLanguage(SelectedLanguageCode);
   TranslateComponent(self);
   // fill listbox with available languages
   lst := TStringlist.Create;
   lbxAvailableLanguages.clear;
   DefaultInstance.GetListOfLanguages('default',lst);
   For i:=0 to lst.count-1 do
   begin
      y := lbxAvailableLanguages.Items.add(CodeToNativeLanguage(lst[i]));
      if lst[i]=SelectedLanguageCode then lbxAvailableLanguages.ItemIndex:=y;
   end;
   if (lbxAvailableLanguages.ItemIndex=-1) and (lbxAvailableLanguages.Items.Count>0) then lbxAvailableLanguages.ItemIndex :=0;
   lst.free;
   //
end;

end.

