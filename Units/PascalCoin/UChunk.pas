unit UChunk;

{$mode delphi}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Classes, SysUtils,  ZLib, zStream, UAccounts, ULog, UConst, UCrypto;

type

  { TPCChunk }

  TPCChunk = Class
  private
  public
    class function SaveSafeBoxChunkFromSafeBox(SafeBoxStream, DestStream : TStream; fromBlock, toBlock : Cardinal; var errors : AnsiString) : Boolean;
    class function LoadSafeBoxFromChunk(Chunk, DestStream : TStream; var safeBoxHeader : TPCSafeBoxHeader; var errors : AnsiString) : Boolean;
  end;

implementation

{ TPCChunk }

class function TPCChunk.SaveSafeBoxChunkFromSafeBox(SafeBoxStream, DestStream : TStream; fromBlock, toBlock: Cardinal; var errors : AnsiString) : Boolean;
Var
  c: Cardinal;
  cs : Tcompressionstream;
  auxStream : TStream;
  iPosSize, iAux : Int64;
  initialSbPos : Int64;
  sbHeader : TPCSafeBoxHeader;
begin
  Result := false; errors := '';
  // Chunk struct:
  // - Header:
  //   - Magic value  (fixed AnsiString)
  //   - SafeBox version (2 bytes)
  //   - Uncompressed size (4 bytes)
  //   - Compressed size (4 bytes)
  // - Data:
  //   - Compressed data using ZLib
  initialSbPos :=SafeBoxStream.Position;
  Try
    If Not TPCSafeBox.LoadSafeBoxStreamHeader(SafeBoxStream,sbHeader) then begin
      errors := 'SafeBoxStream is not a valid SafeBox!';
      exit;
    end;
    If (sbHeader.startBlock>fromBlock) Or (sbHeader.endBlock<ToBlock) Or (fromBlock>toBlock) then begin
      errors := Format('Cannot save a chunk from %d to %d on a stream with %d to %d!',[fromBlock,toBlock,sbHeader.startBlock,sbHeader.endBlock]);
      exit;
    end;
    TLog.NewLog(ltDebug,ClassName,Format('Saving safebox chunk from %d to %d (current blockscount: %d)',[FromBlock,ToBlock,sbHeader.blocksCount]));

    // Header:
    TStreamOp.WriteAnsiString(DestStream,CT_SafeBoxChunkIdentificator);
    DestStream.Write(CT_SafeBoxBankVersion,SizeOf(CT_SafeBoxBankVersion));
    //
    auxStream := TMemoryStream.Create;
    try
      SafeBoxStream.Position:=initialSbPos;
      If Not TPCSafeBox.CopySafeBoxStream(SafeBoxStream,auxStream,fromBlock,toBlock,errors) then exit;
      auxStream.Position:=0;
      // Save uncompressed size
      c := auxStream.Size;
      DestStream.Write(c,SizeOf(c));
      // Save compressed size ... later
      iPosSize := DestStream.Position;
      c := $FFFFFFFF;
      DestStream.Write(c,SizeOf(c)); // Save 4 random bytes, latter will be changed
      //
      // Zip it and add to Stream
      cs := Tcompressionstream.create(cldefault,DestStream);
      try
        cs.CopyFrom(auxStream,auxStream.Size); // compressing
      finally
        cs.Free;
      end;
    finally
      auxStream.Free;
    end;
    //
    iAux := DestStream.Position;
    c := DestStream.Position - iPosSize - 4; // Save data size
    DestStream.Position:=iPosSize;
    DestStream.Write(c,SizeOf(c));
    DestStream.Position := iAux; // Back to last position
    Result := True; errors := '';
  finally
    SafeBoxStream.Position:=initialSbPos;
  end;
end;

class function TPCChunk.LoadSafeBoxFromChunk(Chunk, DestStream: TStream; var safeBoxHeader : TPCSafeBoxHeader; var errors: AnsiString): Boolean;
var s : AnsiString;
  w : Word;
  cUncompressed, cCompressed : Cardinal;
  ds : Tdecompressionstream;
  dbuff : Array[1..2048] of byte;
  r : Integer;
  destInitialPos, auxPos : Int64;
begin
  Result := false;
  safeBoxHeader := CT_PCSafeBoxHeader_NUL;
  // Header:
  errors := 'Invalid stream header';
  TStreamOp.ReadAnsiString(Chunk,s);
  If (s<>CT_SafeBoxChunkIdentificator) then begin
    exit;
  end;
  Chunk.Read(w,sizeof(w));
  if (w<>CT_SafeBoxBankVersion) then begin
    errors := errors + ' Invalid version '+IntToStr(w);
    exit;
  end;
  // Size
  Chunk.Read(cUncompressed,SizeOf(cUncompressed)); // Uncompressed size
  Chunk.Read(cCompressed,SizeOf(cCompressed)); // Compressed size
  if (Chunk.Size - Chunk.Position < cCompressed) then begin
    errors := Format('Not enough LZip bytes Stream.size:%d Stream.position:%d (avail %d) LZipSize:%d',[Chunk.Size,Chunk.Position,Chunk.Size - Chunk.Position,cCompressed]);
    exit;
  end;
  //
  destInitialPos:=DestStream.Position;
  ds := Tdecompressionstream.create(Chunk);
  try
    repeat
      r := ds.read(dbuff,SizeOf(dbuff));
      if (r>0) then begin
        DestStream.Write(dbuff,r);
      end;
    until r < SizeOf(dbuff);
    //auxStream.CopyFrom(Stream,cCompressed);
  finally
    ds.Free;
  end;
  If (DestStream.Size-destInitialPos)<>cUncompressed then begin
    errors := Format('Uncompressed size:%d <> saved:%d',[(DestStream.Size-destInitialPos),cUncompressed]);
    exit;
  end;

  auxPos := DestStream.Position;
  DestStream.Position:=destInitialPos;
  If Not TPCSafeBox.LoadSafeBoxStreamHeader(DestStream,safeBoxHeader) then begin
    errors:= 'Invalid extracted stream!';
    exit;
  end;
  DestStream.Position:=auxPos;
  Result := true;
end;

end.

