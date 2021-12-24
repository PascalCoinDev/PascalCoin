unit UPCTemporalAbstractMem;

{ Copyright (c) 2016-2021 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I ./../config.inc}

uses
  Classes, {$IFnDEF FPC}Windows,{$ENDIF} SysUtils,
  UPCTemporalFileStream,
  UAbstractMem, UFileMem;

Type
  { TPCTemporalAbstractMem }

  TPCTemporalAbstractMem = Class({$IFDEF USE_ABSTRACTMEM}TFileMem{$ELSE}TMem{$ENDIF})
  private
    {$IFDEF USE_ABSTRACTMEM}
    FTemporalFileName : String;
    {$ENDIF}
  protected
  public
    Constructor Create; reintroduce;
    Destructor Destroy; override;
  End;

implementation

Uses {$IFDEF HIGHLOG}ULog, {$ENDIF} UNode;

{ TPCTemporalFileStream }

constructor TPCTemporalAbstractMem.Create;
begin
  {$IFDEF USE_ABSTRACTMEM}
  FTemporalFileName := TPCTemporalFileStream.GetTemporalFileName('ABSTRACTMEM');
  {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,Format('Creating a new Temporal AbstractMem file: %s',[FTemporalFileName]));{$ENDIF}
  inherited Create(FTemporalFileName,False);
  {$ELSE}
  inherited Create(0,False);
  {$ENDIF}
end;

destructor TPCTemporalAbstractMem.Destroy;
{$IFDEF HIGHLOG}var LSize : Int64;{$ENDIF}
begin
  {$IFDEF HIGHLOG}
  LSize := {$IFDEF USE_ABSTRACTMEM}NextAvailablePos{$ELSE}Size{$ENDIF};
  {$ENDIF}
  inherited Destroy;
  {$IFDEF USE_ABSTRACTMEM}
  if FTemporalFileName<>'' then begin
    {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,Format('Deleting a Temporal AbstractMem file (%d bytes): %s',[LSize, FTemporalFileName]));{$ENDIF}
    DeleteFile(FTemporalFileName);
  end;
  {$ENDIF}
end;

end.
