{ *********************************************************************************** }
{ *                              CryptoLib Library                                  * }
{ *                Copyright (c) 2018 - 20XX Ugochukwu Mmaduekwe                    * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpBits;

{$I CryptoLib.inc}

interface

type
  TBits = class sealed(TObject)

  public

    class function ReverseBytesInt32(Value: Int32): Int32; static; inline;
    class function ReverseBitsUInt8(Value: UInt8): UInt8; static; inline;
    class function ReverseBytesUInt16(Value: UInt16): UInt16; static; inline;
    class function ReverseBytesUInt32(Value: UInt32): UInt32; static; inline;
    class function ReverseBytesUInt64(Value: UInt64): UInt64; static; inline;

    /// <summary>
    /// Reverse a ByteArray.
    /// </summary>
    /// Implementation was found here <see cref="http://stackoverflow.com/a/12969282" />
    /// <param name="Source">Pointer to Input Array.</param>
    /// <param name="Dest">Pointer to Destination Array.</param>
    /// <param name="Size">Size of the Array to Reverse.</param>

    class procedure ReverseByteArray(Source, Dest: Pointer;
      Size: Int64); static;

    /// <summary>
    /// Calculates Arithmetic shift right.
    /// </summary>
    /// <param name="AValue">Int32 value to compute 'Asr' on.</param>
    /// <param name="AShiftBits">Byte, number of bits to shift value to.</param>
    /// <returns>Shifted value.</returns>
    /// <remarks>
    /// Emulated Implementation was gotten from FreePascal sources
    /// </remarks>

    class function Asr32(AValue: Int32; AShiftBits: Byte): Int32;
      static; inline;

    /// <summary>
    /// Calculates Arithmetic shift right.
    /// </summary>
    /// <param name="AValue">Int64 value to compute 'Asr' on.</param>
    /// <param name="AShiftBits">Byte, number of bits to shift value to.</param>
    /// <returns>Shifted value.</returns>
    /// <remarks>
    /// Emulated Implementation was gotten from FreePascal sources
    /// </remarks>

    class function Asr64(AValue: Int64; AShiftBits: Byte): Int64;
      static; inline;

    /// <summary>
    /// Calculates Negative Left Shift. This was implemented to circumvent a
    /// bug in FPC ARM when performing Shift Left on certain values with a
    /// Negative Shift Bits. For example UInt32(1948415963) shl Int32(-2)
    /// should give "3221225472" but in FPC ARM, It gives "0". In some C
    /// Compilers, this is "Undefined"
    /// </summary>
    /// <param name="Value">
    /// Value to Perform Shift On
    /// </param>
    /// <param name="ShiftBits">
    /// Integer, number of bits to shift value to. This Number <b>Must be
    /// Negative</b>
    /// </param>
    /// <param name="value">
    /// UInt32 value to compute 'NLS' on.
    /// </param>
    /// <returns>
    /// Shifted value.
    /// </returns>

    class function NegativeLeftShift32(Value: UInt32; ShiftBits: Int32): UInt32;
      static; inline;

    /// <summary>
    /// Calculates Negative Right Shift. This was implemented to circumvent a
    /// compiler issue when performing Shift Right on certain values with a
    /// Negative Shift Bits. In some C Compilers, this is "Undefined"
    /// </summary>
    /// <param name="Value">
    /// Value to Perform Shift On
    /// </param>
    /// <param name="ShiftBits">
    /// Integer, number of bits to shift value to. This Number <b>Must be
    /// Negative</b>
    /// </param>
    /// <param name="value">
    /// UInt32 value to compute 'NRS' on.
    /// </param>
    /// <returns>
    /// Shifted value.
    /// </returns>

    class function NegativeRightShift32(Value: UInt32; ShiftBits: Int32)
      : UInt32; static; inline;

    /// <summary>
    /// Calculates Negative Right Shift. This was implemented to circumvent a
    /// compiler issue when performing Shift Right on certain values with a
    /// Negative Shift Bits. In some C Compilers, this is "Undefined"
    /// </summary>
    /// <param name="Value">
    /// Value to Perform Shift On
    /// </param>
    /// <param name="ShiftBits">
    /// Integer, number of bits to shift value to. This Number <b>Must be
    /// Negative</b>
    /// </param>
    /// <param name="value">
    /// UInt64 value to compute 'NRS' on.
    /// </param>
    /// <returns>
    /// Shifted value.
    /// </returns>

    class function NegativeRightShift64(Value: UInt64; ShiftBits: Int32)
      : UInt64; static; inline;

    class function RotateLeft8(a_value: Byte; a_n: Int32): Byte; static; inline;
    class function RotateLeft32(a_value: UInt32; a_n: Int32): UInt32;
      static; inline;
    class function RotateLeft64(a_value: UInt64; a_n: Int32): UInt64;
      static; inline;
    class function RotateRight8(a_value: Byte; a_n: Int32): Byte;
      static; inline;
    class function RotateRight32(a_value: UInt32; a_n: Int32): UInt32;
      static; inline;
    class function RotateRight64(a_value: UInt64; a_n: Int32): UInt64;
      static; inline;

  end;

implementation

{ TBits }

class procedure TBits.ReverseByteArray(Source, Dest: Pointer; Size: Int64);
var
  ptr_src, ptr_dest: PByte;
begin
  ptr_src := PByte(Source);
  ptr_dest := PByte(Dest);
  System.Inc(ptr_dest, Size - 1);
  while Size > 0 do
  begin
    ptr_dest^ := ptr_src^;
    System.Inc(ptr_src);
    System.Dec(ptr_dest);
    System.Dec(Size);
  end;
end;

class function TBits.ReverseBytesInt32(Value: Int32): Int32;
{$IFNDEF FPC}
var
  i1, i2, i3, i4: Int32;
{$ENDIF FPC}
begin
{$IFDEF FPC}
  Result := SwapEndian(Value);
{$ELSE}
  i1 := Value and $FF;
  i2 := TBits.Asr32(Value, 8) and $FF;
  i3 := TBits.Asr32(Value, 16) and $FF;
  i4 := TBits.Asr32(Value, 24) and $FF;

  Result := (i1 shl 24) or (i2 shl 16) or (i3 shl 8) or (i4 shl 0);
{$ENDIF FPC}
end;

class function TBits.ReverseBitsUInt8(Value: UInt8): UInt8;
begin
  Value := ((Value shr 1) and $55) or ((Value shl 1) and $AA);
  Value := ((Value shr 2) and $33) or ((Value shl 2) and $CC);
  Value := ((Value shr 4) and $0F) or ((Value shl 4) and $F0);
  Result := Value;
end;

class function TBits.ReverseBytesUInt16(Value: UInt16): UInt16;
begin
{$IFDEF FPC}
  Result := SwapEndian(Value);
{$ELSE}
  Result := UInt16((Value and UInt32($FF)) shl 8 or
    (Value and UInt32($FF00)) shr 8);
{$ENDIF FPC}
end;

class function TBits.ReverseBytesUInt32(Value: UInt32): UInt32;
begin
{$IFDEF FPC}
  Result := SwapEndian(Value);
{$ELSE}
  Result := (Value and UInt32($000000FF)) shl 24 or (Value and UInt32($0000FF00)
    ) shl 8 or (Value and UInt32($00FF0000)) shr 8 or
    (Value and UInt32($FF000000)) shr 24;
{$ENDIF FPC}
end;

class function TBits.ReverseBytesUInt64(Value: UInt64): UInt64;
begin
{$IFDEF FPC}
  Result := SwapEndian(Value);
{$ELSE}
  Result := (Value and UInt64($00000000000000FF)) shl 56 or
    (Value and UInt64($000000000000FF00)) shl 40 or
    (Value and UInt64($0000000000FF0000)) shl 24 or
    (Value and UInt64($00000000FF000000)) shl 8 or
    (Value and UInt64($000000FF00000000)) shr 8 or
    (Value and UInt64($0000FF0000000000)) shr 24 or
    (Value and UInt64($00FF000000000000)) shr 40 or
    (Value and UInt64($FF00000000000000)) shr 56;
{$ENDIF FPC}
end;

class function TBits.Asr32(AValue: Int32; AShiftBits: Byte): Int32;

begin
{$IFDEF FPC}
  Result := SarLongInt(AValue, AShiftBits);
{$ELSE}
  Result := Int32(UInt32(UInt32(UInt32(AValue) shr (AShiftBits and 31)) or
    (UInt32(Int32(UInt32(0 - UInt32(UInt32(AValue) shr 31)) and
    UInt32(Int32(0 - (Ord((AShiftBits and 31) <> 0) { and 1 } )))))
    shl (32 - (AShiftBits and 31)))));
{$ENDIF FPC}
end;

class function TBits.Asr64(AValue: Int64; AShiftBits: Byte): Int64;
begin
{$IFDEF FPC}
  Result := SarInt64(AValue, AShiftBits);
{$ELSE}
  Result := Int64(UInt64(UInt64(UInt64(AValue) shr (AShiftBits and 63)) or
    (UInt64(Int64(UInt64(0 - UInt64(UInt64(AValue) shr 63)) and
    UInt64(Int64(0 - (Ord((AShiftBits and 63) <> 0) { and 1 } )))))
    shl (64 - (AShiftBits and 63)))));
{$ENDIF FPC}
end;

class function TBits.RotateLeft8(a_value: Byte; a_n: Int32): Byte;
begin
{$IFDEF DEBUG}
  System.Assert(a_n >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RolByte(a_value, a_n);
{$ELSE}
  a_n := a_n and 7;

  Result := (a_value shl a_n) or (a_value shr (8 - a_n));
{$ENDIF FPC}
end;

class function TBits.NegativeLeftShift32(Value: UInt32;
  ShiftBits: Int32): UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(ShiftBits < 0);
{$ENDIF DEBUG}
  Result := Value shl (32 + ShiftBits);
end;

class function TBits.NegativeRightShift32(Value: UInt32;
  ShiftBits: Int32): UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(ShiftBits < 0);
{$ENDIF DEBUG}
  Result := Value shr (32 + ShiftBits);
end;

class function TBits.NegativeRightShift64(Value: UInt64;
  ShiftBits: Int32): UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(ShiftBits < 0);
{$ENDIF DEBUG}
  Result := Value shr (64 + ShiftBits);
end;

class function TBits.RotateLeft32(a_value: UInt32; a_n: Int32): UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(a_n >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RolDWord(a_value, a_n);
{$ELSE}
  a_n := a_n and 31;

  Result := (a_value shl a_n) or (a_value shr (32 - a_n));
{$ENDIF FPC}
end;

class function TBits.RotateLeft64(a_value: UInt64; a_n: Int32): UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(a_n >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RolQWord(a_value, a_n);
{$ELSE}
  a_n := a_n and 63;

  Result := (a_value shl a_n) or (a_value shr (64 - a_n));
{$ENDIF FPC}
end;

class function TBits.RotateRight8(a_value: Byte; a_n: Int32): Byte;
begin
{$IFDEF DEBUG}
  System.Assert(a_n >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RorByte(a_value, a_n);
{$ELSE}
  a_n := a_n and 7;

  Result := (a_value shr a_n) or (a_value shl (8 - a_n));
{$ENDIF FPC}
end;

class function TBits.RotateRight32(a_value: UInt32; a_n: Int32): UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(a_n >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RorDWord(a_value, a_n);
{$ELSE}
  a_n := a_n and 31;

  Result := (a_value shr a_n) or (a_value shl (32 - a_n));
{$ENDIF FPC}
end;

class function TBits.RotateRight64(a_value: UInt64; a_n: Int32): UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(a_n >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RorQWord(a_value, a_n);
{$ELSE}
  a_n := a_n and 63;

  Result := (a_value shr a_n) or (a_value shl (64 - a_n));
{$ENDIF FPC}
end;

end.
