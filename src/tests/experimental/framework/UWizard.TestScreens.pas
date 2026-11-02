unit UWizard.TestScreens;

{$mode delphi}
{$H+}

interface

uses
  Classes, SysUtils, Variants, UWizard;

type
  { Keep reference-model screens in another unit: FPC emits their generic
    specialization separately from the wizard manager's specialization. }
  TReferenceWizardScreen = class(TWizardForm<TGenericWizardBag>)
    public
      InitializedName : String;
      InitializeCount : Integer;
      constructor Create(AOwner : TComponent); override;
      procedure Initialize; override;
  end;

  TReferenceWizardFirstScreen = class(TReferenceWizardScreen);
  TReferenceWizardSecondScreen = class(TReferenceWizardScreen);

implementation

constructor TReferenceWizardScreen.Create(AOwner : TComponent);
begin
  inherited CreateNew(AOwner);
end;

procedure TReferenceWizardScreen.Initialize;
begin
  InitializedName := VarToStr(Model['Name']);
  inc(InitializeCount);
end;

end.
