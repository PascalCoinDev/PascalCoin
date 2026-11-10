unit UGridLoadingProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>

  Delphi/VCL manual grid loading regressions with generated rows and owned forms.
  Distributed under the MIT software license; see the accompanying LICENSE. }


interface
uses Classes,Forms;
procedure InitializeGridLoadingProbe(AForm:TForm;const ALog:String);
function GridLoadingProbePassed:Boolean;
implementation
uses SysUtils,Windows,Controls,ExtCtrls,StdCtrls,Graphics,SyncObjs,Generics.Collections,Variants,
 UCommon.Data,UVisualGrid,UCommon.UI;
type
 TSource=class(TCustomDataSource<Integer>)
 public FetchCount:LongInt;BlockRead,FailRead:Boolean;Entered,ReleaseRead:TEvent;
  constructor Create(AOwner:TComponent);override;destructor Destroy;override;
  function GetColumns:TDataColumns;override;
  function GetItemField(const AItem:Integer;const AName:String):Variant;override;
  procedure FetchAll(const AContainer:TList<Integer>);override;
 end;
 TGridAccess=class(TVisualGrid)
 public function RegionsEnabled:Boolean;
 end;
 TProbe=class(TComponent)
 private FForm:TForm;FGrid:TVisualGrid;FSource:TSource;FTimer:TTimer;
  FPhase:Integer;FInTick:Boolean;FDeadline,FAt:UInt64;FCount:Integer;
  procedure Require(C:Boolean;const S:String);procedure Tick(Sender:TObject);
  function Card:TPanel;function Title:TLabel;function Subtitle:TLabel;
  procedure CheckGraphic;procedure CheckUnparentedFocus;
 public constructor Create(AForm:TForm);reintroduce;destructor Destroy;override;
 end;
var LogPath:String;Passed:Boolean;
procedure Log(const S:String);
var F:TFileStream;B:TBytes;
begin
 if FileExists(LogPath)then F:=TFileStream.Create(LogPath,fmOpenReadWrite or fmShareDenyNone)else F:=TFileStream.Create(LogPath,fmCreate);
 try F.Position:=F.Size;B:=TEncoding.UTF8.GetBytes(UIntToStr(GetTickCount64)+' '+S+#13#10);F.WriteBuffer(B[0],Length(B));finally F.Free;end;
end;
constructor TSource.Create(AOwner:TComponent);
begin
  inherited;
  Entered:=TEvent.Create(nil,True,False,'');
  ReleaseRead:=TEvent.Create(nil,True,False,'');
end;
destructor TSource.Destroy;
begin
  Entered.Free;
  ReleaseRead.Free;
  inherited;
end;
function TSource.GetColumns:TDataColumns;
begin
  Result:=TDataColumns.Create(TDataColumn.From('Id',True));
end;
function TSource.GetItemField(const AItem:Integer;const AName:String):Variant;
begin
  Result:=AItem;
end;
procedure TSource.FetchAll(const AContainer:TList<Integer>);
var I:Integer;
begin
 InterlockedIncrement(FetchCount);Entered.SetEvent;
 if BlockRead and(ReleaseRead.WaitFor(5000)<>wrSignaled)then raise Exception.Create('Native source gate timeout');
 if FailRead then raise Exception.Create('Native controlled fetch failure');
 for I:=0 to 99 do AContainer.Add(I);
end;
function TGridAccess.RegionsEnabled:Boolean;
begin
  Result:=FTopPanel.Enabled and FTopPanelMultiSearch.Enabled and FBottomPanel.Enabled;
end;
constructor TProbe.Create(AForm:TForm);
begin
 inherited Create(AForm);FForm:=AForm;FDeadline:=GetTickCount64+12000;
 FSource:=TSource.Create(AForm);FGrid:=TVisualGrid.Create(AForm);
 FGrid.ShowLoading('Before parenting','Held before native handles');
 Require(FGrid.LoadingVisible and not FGrid.HandleAllocated,'Unparented manual Show created a native grid handle');
 FGrid.Parent:=AForm;FGrid.Align:=alClient;FGrid.FetchDataInThread:=True;
 FGrid.AddColumn('Id').Binding:='Id';FGrid.DataSource:=FSource;
 FTimer:=TTimer.Create(Self);FTimer.Interval:=30;FTimer.OnTimer:=Tick;
end;
destructor TProbe.Destroy;
begin
 if Assigned(FTimer) then FTimer.Enabled:=False;
 if Assigned(FSource) then FSource.ReleaseRead.SetEvent;
 inherited;
end;
procedure TProbe.Require(C:Boolean;const S:String);
begin
  if not C then raise Exception.Create(S);
end;
function TProbe.Card:TPanel;
begin
  Result:=FGrid.FindComponent('LoadingOverlay')as TPanel;
end;
function TProbe.Title:TLabel;
begin
  Result:=FGrid.FindComponent('LoadingStatus')as TLabel;
end;
function TProbe.Subtitle:TLabel;
begin
  Result:=FGrid.FindComponent('LoadingSubtitle')as TLabel;
end;
procedure TProbe.CheckGraphic;
var B:TBitmap;I:TImage;
begin
 FCount:=FSource.FetchCount;B:=TBitmap.Create;
 try B.SetSize(8,8);B.Canvas.Brush.Color:=clRed;B.Canvas.FillRect(Rect(0,0,8,8));
  FGrid.ShowLoading('Custom title','Custom subtitle',B);
  B.Canvas.Brush.Color:=clBlue;B.Canvas.FillRect(Rect(0,0,8,8));
 finally B.Free;end;
 I:=FGrid.FindComponent('LoadingImage')as TImage;
 Require(FGrid.LoadingVisible and Card.Visible,'Manual card missing');
 Require((Title.Caption='Custom title')and(Subtitle.Caption='Custom subtitle'),'Manual labels missing');
 Require((fsBold in Title.Font.Style)and not(fsBold in Subtitle.Font.Style),'Manual font styles wrong');
 Require(I.Visible and(I.Picture.Width=8)and(ColorToRGB(I.Picture.Bitmap.Canvas.Pixels[1,1])=ColorToRGB(clRed)),'Graphic not copied');
 Require(not TLoadingCircle(FGrid.FindComponent('LoadingCircle')).Visible,'Custom image did not replace circle');
 Require(FGrid.Enabled and not FGrid.InternalDrawGrid.Enabled and not TGridAccess(FGrid).RegionsEnabled,'Manual hold did not gate complete input regions');
 Require(FSource.FetchCount=FCount,'Show initiated a query');
 B:=FForm.GetFormImage;try B.SaveToFile(ChangeFileExt(LogPath,'-custom.bmp'));finally B.Free;end;
 FGrid.Enabled:=False;FGrid.HideLoading;Require(not FGrid.Enabled,'Hide enabled caller-disabled grid');FGrid.Enabled:=True;
 FGrid.ShowLoading('Manual hold','A worker may complete');
 Log('PASS custom labels/imagecopy/input regions/callerdisabled/noquery');
end;
procedure TProbe.CheckUnparentedFocus;
var D:TForm;E:TEdit;G:TVisualGrid;Active,Focus:HWND;
begin
 D:=TForm.CreateNew(FForm);G:=nil;
 try
  E:=TEdit.Create(D);E.Parent:=D;D.SetBounds(200,180,300,150);D.Show;D.BringToFront;E.SetFocus;
  Active:=GetActiveWindow;Focus:=GetFocus;
  Require((Active=D.Handle)and(Focus=E.Handle),'Owned modeless dialog did not acquire focus');
  G:=TVisualGrid.Create(FForm);G.ShowLoading('Unparented again','Do not activate the host');
  Require(not G.HandleAllocated,'Unparented Show allocated HWND');
  G.SetBounds(0,0,320,180);G.Parent:=FForm;
  Require((GetActiveWindow=Active)and(GetFocus=Focus),'Parenting held overlay activated background main form');
  FGrid.ShowLoading('Background changed','Preserve modeless keyboard focus');
  Require((GetActiveWindow=Active)and(GetFocus=Focus),'Showing manual card activated background main form');
  G.HideLoading;G.Free;G:=nil;
  Require((GetActiveWindow=Active)and(GetFocus=Focus),'Hide changed modeless activation');
  Log('PASS unparentedstate/attach/modelessactivation/keyboardfocus');
 finally G.Free;D.Free;end;
end;
procedure TProbe.Tick(Sender:TObject);
var B:TBitmap;
begin
 if FInTick then Exit;FInTick:=True;
 try
  try
   Require(GetTickCount64<FDeadline,'Native manual-loading fixture timeout');
   case FPhase of
    0:begin
     if FGrid.IsUpdating then Exit;
     Require(FGrid.LoadingVisible and(Title.Caption='Before parenting'),'Initial fetch cleared unparented manual state');
     Require(not FGrid.IsUpdating,'Manual hold changed worker IsUpdating');CheckGraphic;
     FSource.BlockRead:=True;FSource.Entered.ResetEvent;FGrid.RefreshGrid;FPhase:=1;
    end;
    1:begin
     if FSource.Entered.WaitFor(0)<>wrSignaled then Exit;
     Require(FGrid.IsUpdating and FGrid.LoadingVisible,'Held manual worker state missing');
     FGrid.HideLoading;Require(FGrid.IsUpdating and not FGrid.LoadingVisible and not FGrid.InternalDrawGrid.Enabled,'Hide overrode worker/input state');
     FAt:=GetTickCount64+350;FPhase:=2;
    end;
    2:begin
     if GetTickCount64<FAt then Exit;
     Require(Card.Visible and TLoadingCircle(FGrid.FindComponent('LoadingCircle')).Active,'Delayed automatic card not restored');
     FSource.ReleaseRead.SetEvent;FPhase:=3;
    end;
    3:begin
     if FGrid.IsUpdating then Exit;
     Require(not Card.Visible and FGrid.InternalDrawGrid.Enabled,'Automatic completion did not restore controls');
     FGrid.ShowLoading('Held across error','Independent subtitle');FSource.FailRead:=True;FGrid.RefreshGrid;FPhase:=4;
    end;
    4:begin
     if FGrid.IsUpdating then Exit;
     Require(FGrid.LoadingVisible and(Title.Caption='Held across error'),'Fetch error overwrote manual text');
     FGrid.HideLoading;Require(Card.Visible and(Title.Caption='Native controlled fetch failure'),'Hide did not expose fetch error');
     FSource.FailRead:=False;FGrid.ShowLoading('Nonblocking','',nil,False);FGrid.RefreshGrid;FPhase:=5;
    end;
    5:begin
     if FGrid.IsUpdating then Exit;
     Require(FGrid.LoadingVisible and FGrid.InternalDrawGrid.Enabled and TGridAccess(FGrid).RegionsEnabled,'Nonblocking overlay changed idle input state');
     CheckUnparentedFocus;FGrid.HideLoading;Require(not Card.Visible,'Final hide retained idle card');
     B:=FForm.GetFormImage;try B.SaveToFile(ChangeFileExt(LogPath,'.bmp'));finally B.Free;end;
     Passed:=True;Log('PASS manual/automatic/error layering; sourcefetches='+IntToStr(FSource.FetchCount));FTimer.Enabled:=False;Application.Terminate;
    end;
   end;
  except on E:Exception do begin
    Log('FAIL '+E.ClassName+': '+E.Message);
    Passed:=False;
    FTimer.Enabled:=False;
    FSource.ReleaseRead.SetEvent;
    Application.Terminate;
  end;end;
 finally FInTick:=False;end;
end;
procedure InitializeGridLoadingProbe(AForm:TForm;const ALog:String);
var F:TFileStream;
begin
  LogPath:=ALog;
  F:=TFileStream.Create(LogPath,fmCreate);
  F.Free;
  TProbe.Create(AForm);
end;
function GridLoadingProbePassed:Boolean;
begin
  Result:=Passed;
end;
end.