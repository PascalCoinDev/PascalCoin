unit UPCDataTypes;

{ Copyright (c) 2016-2019 by Albert Molina

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

uses
  Classes, SysUtils, UBaseTypes;

type

  TECPrivateKeyInfo = record
    EC_OpenSSL_NID : Word;
    EC_KEY_Ptr : Pointer;    // Used when compiled with $DEFINE Use_OpenSSL
    RAW_PrivKey : TRawBytes; // Used when compiled with $DEFINE Use_CryptoLib4Pascal
  end;

  { TECDSA_Public is a public key information }
  TECDSA_Public = record
     EC_OpenSSL_NID : Word;
     x: TRawBytes;
     y: TRawBytes;
  end;

  { TECDSA_Public_Raw is a TECDSA_Public stored in a single TRawBytes
    Information will be:
    2 bytes for EC_OpenSSL_NID ++ 2 byte for x length ++ x RAW data ++ 2 byte for y length ++ y RAW data }
  TECDSA_Public_Raw = TRawBytes;

  { TECDSA_Public_Helper }

  TECDSA_Public_Helper = record helper for TECDSA_Public
     function ToRaw(var OECDSA_Public_Raw : TECDSA_Public_Raw) : Boolean;
     function FromRaw(const AECDSA_Public_Raw : TECDSA_Public_Raw) : Boolean;
  end;

  { TECDSA_SIG is a Eliptic Curve signature }
  TECDSA_SIG = record
     r: TRawBytes;
     s: TRawBytes;
  end;
  PECDSA_Public = ^TECDSA_Public; // Pointer to a TECDSA_SIG

implementation

{ TECDSA_Public_Helper }

function TECDSA_Public_Helper.ToRaw(var OECDSA_Public_Raw: TECDSA_Public_Raw): Boolean;
var l_length : Integer;
  l_bs : TBytesStream;
  l_w : Word;
begin
  if (Length(self.x)>65536) or (Length(self.y)>65536) then begin
    Result := False;
    SetLength(OECDSA_Public_Raw,0);
  end else begin
    l_length := 6 + Length(self.x) + Length(self.y);
    SetLength(OECDSA_Public_Raw,l_length);
    l_bs := TBytesStream.Create;
    try
      l_bs.Write(self.EC_OpenSSL_NID,2); // Write 2 bytes little endian with EC_OpenSSL_NID
      l_w := Length(self.x);
      l_bs.Write(l_w,2);                 // Write 2 bytes little endian for x length
      l_bs.WriteBuffer(self.x[Low(self.x)],l_w); // Write l_w bytes from x as RAW
      l_w := Length(self.y);
      l_bs.Write(l_w,2);                 // Write 2 bytes little endian for y length
      l_bs.WriteBuffer(self.y[Low(self.x)],l_w); // Write l_w bytes from y as RAW
      // Save to OECDSA_Public_Raw
      l_bs.Position:=0;
      l_bs.ReadBuffer(OECDSA_Public_Raw[Low(OECDSA_Public_Raw)],l_bs.Size);
    finally
      l_bs.Free;
    end;
    Result := True;
  end;
end;

function TECDSA_Public_Helper.FromRaw(const AECDSA_Public_Raw: TECDSA_Public_Raw): Boolean;
var l_bs : TBytesStream;
  l_w : Word;
begin
  Result := False; // Initial result to False
  if Length(AECDSA_Public_Raw)<6 then Exit; // Not minimum size: 2 + 2 + 0 + 2 + 0 = 6 bytes
  l_bs := TBytesStream.Create;
  try
    l_bs.WriteBuffer(AECDSA_Public_Raw[Low(AECDSA_Public_Raw)],Length(AECDSA_Public_Raw));
    l_bs.Read(self.EC_OpenSSL_NID,2); // Read 2 bytes little endian with EC_OpenSSL_NID
    l_bs.Read(l_w,2);                 // Read 2 bytes little endian with x length
    if (l_bs.Position + l_w) < l_bs.Size then Exit;
    SetLength(self.x,l_w);
    l_bs.ReadBuffer(self.x[Low(self.x)],l_w); // Read x as RAW
    l_bs.Read(l_w,2);                 // Read 2 bytes little endian with y length
    if (l_bs.Position + l_w - 1) <> l_bs.Size then Exit; // size must match
    SetLength(self.x,l_w);
    l_bs.ReadBuffer(self.y[Low(self.y)],l_w); // Read y as RAW
    Result := True;
  finally
    l_bs.Free;
  end;
end;

end.

