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

unit ClpPaddingModes;

{$I CryptoLib.inc}

interface

uses
  ClpIBlockCipherPadding,
  ClpSecureRandom,
  ClpISecureRandom,
  ClpCryptoLibTypes,
  ClpIPaddingModes;

resourcestring
  SCorruptedPadBlock = 'Pad Block Corrupted';

type

  /// <summary>
  /// A padder that adds ISO10126-2 padding to a block.
  /// </summary>
  TISO10126d2Padding = class sealed(TInterfacedObject, IISO10126d2Padding,
    IBlockCipherPadding)

  strict private

  var
    FRandom: ISecureRandom;
    /// <returns>
    /// return the name of the algorithm the cipher implements.
    /// </returns>
    function GetPaddingName: String; inline;

  public
    /// <summary>
    /// Initialise the padder.
    /// </summary>
    /// <param name="random">
    /// a SecureRandom if available.
    /// </param>
    procedure Init(const random: ISecureRandom);

    /// <summary>
    /// Return the name of the algorithm the cipher implements.
    /// </summary>
    property PaddingName: String read GetPaddingName;

    /// <summary>
    /// add the pad bytes to the passed in block, returning the number of
    /// bytes added.
    /// </summary>
    /// <param name="input">
    /// input block to pad
    /// </param>
    /// <param name="inOff">
    /// offset to start the padding from in the block
    /// </param>
    /// <returns>
    /// returns number of bytes added
    /// </returns>
    function AddPadding(const input: TCryptoLibByteArray; inOff: Int32): Int32;

    /// <summary>
    /// return the number of pad bytes present in the block.
    /// </summary>
    /// <param name="input">
    /// block to count pad bytes in
    /// </param>
    /// <returns>
    /// the number of pad bytes present in the block.
    /// </returns>
    /// <exception cref="EInvalidCipherTextCryptoLibException">
    /// if the padding is badly formed or invalid.
    /// </exception>
    function PadCount(const input: TCryptoLibByteArray): Int32;

  end;

type

  /// <summary>
  /// A padder that adds the padding according to the scheme referenced in
  /// ISO 7814-4 - scheme 2 from ISO 9797-1. The first byte is $80, rest is $00
  /// </summary>
  TISO7816d4Padding = class sealed(TInterfacedObject, IISO7816d4Padding,
    IBlockCipherPadding)

  strict private
    /// <returns>
    /// return the name of the algorithm the cipher implements.
    /// </returns>
    function GetPaddingName: String; inline;

  public
    /// <summary>
    /// Initialise the padder.
    /// </summary>
    /// <param name="random">
    /// a SecureRandom if available.
    /// </param>
    procedure Init(const random: ISecureRandom);

    /// <summary>
    /// Return the name of the algorithm the cipher implements.
    /// </summary>
    property PaddingName: String read GetPaddingName;

    /// <summary>
    /// add the pad bytes to the passed in block, returning the number of
    /// bytes added.
    /// </summary>
    /// <param name="input">
    /// input block to pad
    /// </param>
    /// <param name="inOff">
    /// offset to start the padding from in the block
    /// </param>
    /// <returns>
    /// returns number of bytes added
    /// </returns>
    function AddPadding(const input: TCryptoLibByteArray; inOff: Int32): Int32;

    /// <summary>
    /// return the number of pad bytes present in the block.
    /// </summary>
    /// <param name="input">
    /// block to count pad bytes in
    /// </param>
    /// <returns>
    /// the number of pad bytes present in the block.
    /// </returns>
    /// <exception cref="EInvalidCipherTextCryptoLibException">
    /// if the padding is badly formed or invalid.
    /// </exception>
    function PadCount(const input: TCryptoLibByteArray): Int32;

  end;

type
  TPkcs7Padding = class sealed(TInterfacedObject, IPkcs7Padding,
    IBlockCipherPadding)

  strict private

    /// <returns>
    /// return the name of the algorithm the cipher implements.
    /// </returns>
    function GetPaddingName: String; inline;

  public
    /// <summary>
    /// Initialise the padder.
    /// </summary>
    /// <param name="random">
    /// a SecureRandom if available.
    /// </param>
    procedure Init(const random: ISecureRandom);

    /// <summary>
    /// Return the name of the algorithm the cipher implements.
    /// </summary>
    property PaddingName: String read GetPaddingName;

    /// <summary>
    /// add the pad bytes to the passed in block, returning the number of
    /// bytes added.
    /// </summary>
    /// <param name="input">
    /// input block to pad
    /// </param>
    /// <param name="inOff">
    /// offset to start the padding from in the block
    /// </param>
    /// <returns>
    /// returns number of bytes added
    /// </returns>
    function AddPadding(const input: TCryptoLibByteArray; inOff: Int32): Int32;

    /// <summary>
    /// return the number of pad bytes present in the block.
    /// </summary>
    /// <param name="input">
    /// block to count pad bytes in
    /// </param>
    /// <returns>
    /// the number of pad bytes present in the block.
    /// </returns>
    /// <exception cref="EInvalidCipherTextCryptoLibException">
    /// if the padding is badly formed or invalid.
    /// </exception>
    function PadCount(const input: TCryptoLibByteArray): Int32;

  end;

type

  /// <summary> A padder that adds Trailing-Bit-Compliment padding to a block.
  /// <p>
  /// This padding pads the block out compliment of the last bit
  /// of the plain text.
  /// </p>
  /// </summary>
  TTBCPadding = class sealed(TInterfacedObject, ITBCPadding,
    IBlockCipherPadding)

  strict private
    /// <returns>
    /// return the name of the algorithm the cipher implements.
    /// </returns>
    function GetPaddingName: String; inline;

  public
    /// <summary>
    /// Initialise the padder.
    /// </summary>
    /// <param name="random">
    /// a SecureRandom if available.
    /// </param>
    procedure Init(const random: ISecureRandom);

    /// <summary>
    /// Return the name of the algorithm the cipher implements.
    /// </summary>
    property PaddingName: String read GetPaddingName;

    /// <summary> add the pad bytes to the passed in block, returning the
    /// number of bytes added.
    /// <p>
    /// Note: this assumes that the last block of plain text is always
    /// passed to it inside in. i.e. if inOff is zero, indicating the
    /// entire block is to be overwritten with padding the value of in
    /// should be the same as the last block of plain text.
    /// </p>
    /// </summary>
    function AddPadding(const input: TCryptoLibByteArray; inOff: Int32): Int32;

    /// <summary>
    /// return the number of pad bytes present in the block.
    /// </summary>
    /// <param name="input">
    /// block to count pad bytes in
    /// </param>
    /// <returns>
    /// the number of pad bytes present in the block.
    /// </returns>
    function PadCount(const input: TCryptoLibByteArray): Int32;

  end;

type

  /// <summary>
  /// A padder that adds X9.23 padding to a block - if a SecureRandom is
  /// passed in random padding is assumed, otherwise padding with zeros is
  /// used.
  /// </summary>
  TX923Padding = class sealed(TInterfacedObject, IX923Padding,
    IBlockCipherPadding)

  strict private
  var
    FRandom: ISecureRandom;

    /// <returns>
    /// return the name of the algorithm the cipher implements.
    /// </returns>
    function GetPaddingName: String; inline;

  public
    /// <summary>
    /// Initialise the padder.
    /// </summary>
    /// <param name="random">
    /// a SecureRandom if available.
    /// </param>
    procedure Init(const random: ISecureRandom);

    /// <summary>
    /// Return the name of the algorithm the cipher implements.
    /// </summary>
    property PaddingName: String read GetPaddingName;

    /// <summary>
    /// add the pad bytes to the passed in block, returning the number of
    /// bytes added.
    /// </summary>
    /// <param name="input">
    /// input block to pad
    /// </param>
    /// <param name="inOff">
    /// offset to start the padding from in the block
    /// </param>
    /// <returns>
    /// returns number of bytes added
    /// </returns>
    function AddPadding(const input: TCryptoLibByteArray; inOff: Int32): Int32;

    /// <summary>
    /// return the number of pad bytes present in the block.
    /// </summary>
    /// <param name="input">
    /// block to count pad bytes in
    /// </param>
    /// <returns>
    /// the number of pad bytes present in the block.
    /// </returns>
    /// <exception cref="EInvalidCipherTextCryptoLibException">
    /// if the padding is badly formed or invalid.
    /// </exception>
    function PadCount(const input: TCryptoLibByteArray): Int32;

  end;

type

  /// <summary>
  /// A padder that adds Null byte padding to a block.
  /// </summary>
  TZeroBytePadding = class sealed(TInterfacedObject, IZeroBytePadding,
    IBlockCipherPadding)

  strict private

    /// <returns>
    /// return the name of the algorithm the cipher implements.
    /// </returns>
    function GetPaddingName: String; inline;

  public
    /// <summary>
    /// Initialise the padder.
    /// </summary>
    /// <param name="random">
    /// a SecureRandom if available.
    /// </param>
    procedure Init(const random: ISecureRandom);

    /// <summary>
    /// Return the name of the algorithm the cipher implements.
    /// </summary>
    property PaddingName: String read GetPaddingName;

    /// <summary>
    /// add the pad bytes to the passed in block, returning the number of
    /// bytes added.
    /// </summary>
    /// <param name="input">
    /// input block to pad
    /// </param>
    /// <param name="inOff">
    /// offset to start the padding from in the block
    /// </param>
    /// <returns>
    /// returns number of bytes added
    /// </returns>
    function AddPadding(const input: TCryptoLibByteArray; inOff: Int32): Int32;

    /// <summary>
    /// return the number of pad bytes present in the block.
    /// </summary>
    /// <param name="input">
    /// block to count pad bytes in
    /// </param>
    /// <returns>
    /// the number of pad bytes present in the block.
    /// </returns>
    function PadCount(const input: TCryptoLibByteArray): Int32;

  end;

implementation

{ TISO10126d2Padding }

function TISO10126d2Padding.AddPadding(const input: TCryptoLibByteArray;
  inOff: Int32): Int32;
var
  code: Byte;
begin
  code := Byte(System.Length(input) - inOff);

  while (inOff < (System.Length(input) - 1)) do
  begin
    input[inOff] := Byte(FRandom.NextInt32);
    System.Inc(inOff);
  end;

  input[inOff] := code;

  result := code;
end;

function TISO10126d2Padding.GetPaddingName: String;
begin
  result := 'ISO10126-2';
end;

procedure TISO10126d2Padding.Init(const random: ISecureRandom);
begin
  if random <> Nil then
  begin
    FRandom := random;
  end
  else
  begin
    FRandom := TSecureRandom.Create();
  end;
end;

function TISO10126d2Padding.PadCount(const input: TCryptoLibByteArray): Int32;
var
  count: Int32;
begin

  count := input[System.Length(input) - 1] and $FF;

  if (count > System.Length(input)) then
  begin
    raise EInvalidCipherTextCryptoLibException.CreateRes(@SCorruptedPadBlock);
  end;

  result := count;

end;

{ TISO7816d4Padding }

function TISO7816d4Padding.AddPadding(const input: TCryptoLibByteArray;
  inOff: Int32): Int32;
var
  added: Int32;
begin
  added := (System.Length(input) - inOff);

  input[inOff] := Byte($80);
  System.Inc(inOff);

  while (inOff < System.Length(input)) do
  begin
    input[inOff] := Byte(0);
    System.Inc(inOff);
  end;

  result := added;
end;

function TISO7816d4Padding.GetPaddingName: String;
begin
  result := 'ISO7816-4';
end;

{$IFNDEF _FIXINSIGHT_}

procedure TISO7816d4Padding.Init(const random: ISecureRandom);
begin
  // nothing to do.
end;
{$ENDIF}

function TISO7816d4Padding.PadCount(const input: TCryptoLibByteArray): Int32;
var
  count: Int32;
begin

  count := System.Length(input) - 1;

  while ((count > 0) and (input[count] = 0)) do
  begin
    System.Dec(count);
  end;

  if (input[count] <> Byte($80)) then
  begin
    raise EInvalidCipherTextCryptoLibException.CreateRes(@SCorruptedPadBlock);
  end;

  result := System.Length(input) - count;

end;

{ TPkcs7Padding }

function TPkcs7Padding.AddPadding(const input: TCryptoLibByteArray;
  inOff: Int32): Int32;
var
  code: Byte;
begin
  code := Byte(System.Length(input) - inOff);

  while (inOff < System.Length(input)) do
  begin
    input[inOff] := code;
    System.Inc(inOff);
  end;

  result := code;
end;

function TPkcs7Padding.GetPaddingName: String;
begin
  result := 'PKCS7';
end;

{$IFNDEF _FIXINSIGHT_}

procedure TPkcs7Padding.Init(const random: ISecureRandom);
begin
  // nothing to do.
end;
{$ENDIF}

function TPkcs7Padding.PadCount(const input: TCryptoLibByteArray): Int32;
var
  countAsByte: Byte;
  count, i: Int32;
  failed: Boolean;
begin
  // countAsByte := input[System.Length(input) - 1];
  // count := countAsByte;
  //
  // if ((count < 1) or (count > System.Length(input))) then
  // begin
  // raise EInvalidCipherTextCryptoLibException.CreateRes(@SCorruptedPadBlock);
  // end;
  //
  // for i := 2 to count do
  // begin
  // if (input[System.Length(input) - i] <> countAsByte) then
  // begin
  // raise EInvalidCipherTextCryptoLibException.CreateRes(@SCorruptedPadBlock);
  // end;
  // end;
  //
  // result := count;

  count := input[System.Length(input) - 1] and $FF;
  countAsByte := Byte(count);

  // constant time version
  failed := ((count > System.Length(input)) or (count = 0));

  for i := 0 to System.Pred(System.Length(input)) do
  begin
    failed := failed or ((System.Length(input) - i <= count) and
      (input[i] <> countAsByte));
  end;

  if (failed) then
  begin
    raise EInvalidCipherTextCryptoLibException.CreateRes(@SCorruptedPadBlock);
  end;

  result := count;

end;

{ TTBCPadding }

function TTBCPadding.AddPadding(const input: TCryptoLibByteArray;
  inOff: Int32): Int32;
var
  count: Int32;
  code: Byte;
begin
  count := System.Length(input) - inOff;

  if (inOff > 0) then
  begin
    if (input[inOff - 1] and $01) = 0 then
    begin
      code := Byte($FF)
    end
    else
    begin
      code := Byte($00)
    end;

  end
  else
  begin

    if (input[System.Length(input) - 1] and $01) = 0 then
    begin
      code := Byte($FF)
    end
    else
    begin
      code := Byte($00)
    end;

  end;

  while (inOff < System.Length(input)) do
  begin
    input[inOff] := code;
    System.Inc(inOff);
  end;

  result := count;
end;

function TTBCPadding.GetPaddingName: String;
begin
  result := 'TBC';
end;

{$IFNDEF _FIXINSIGHT_}

procedure TTBCPadding.Init(const random: ISecureRandom);
begin
  // nothing to do.
end;
{$ENDIF}

function TTBCPadding.PadCount(const input: TCryptoLibByteArray): Int32;
var
  code: Byte;
  index: Int32;
begin

  code := input[System.Length(input) - 1];

  index := System.Length(input) - 1;
  while ((index > 0) and (input[index - 1] = code)) do
  begin
    System.Dec(index);
  end;

  result := System.Length(input) - index;

end;

{ TX923Padding }

function TX923Padding.AddPadding(const input: TCryptoLibByteArray;
  inOff: Int32): Int32;
var
  code: Byte;
begin
  code := Byte(System.Length(input) - inOff);

  while (inOff < (System.Length(input) - 1)) do
  begin
    if (FRandom = Nil) then
    begin
      input[inOff] := 0;
    end
    else
    begin
      input[inOff] := Byte(FRandom.NextInt32);
    end;
    System.Inc(inOff);
  end;

  input[inOff] := code;
  result := code;
end;

function TX923Padding.GetPaddingName: String;
begin
  result := 'X9.23';
end;

procedure TX923Padding.Init(const random: ISecureRandom);
begin
  FRandom := random;
end;

function TX923Padding.PadCount(const input: TCryptoLibByteArray): Int32;
var
  count: Int32;
begin

  count := input[System.Length(input) - 1] and $FF;

  if (count > System.Length(input)) then
  begin
    raise EInvalidCipherTextCryptoLibException.CreateRes(@SCorruptedPadBlock);
  end;

  result := count;

end;

{ TZeroBytePadding }

function TZeroBytePadding.AddPadding(const input: TCryptoLibByteArray;
  inOff: Int32): Int32;
var
  added: Int32;
begin
  added := System.Length(input) - inOff;

  while (inOff < System.Length(input)) do
  begin
    input[inOff] := Byte(0);
    System.Inc(inOff);
  end;

  result := added;
end;

function TZeroBytePadding.GetPaddingName: String;
begin
  result := 'ZeroBytePadding';
end;

{$IFNDEF _FIXINSIGHT_}

procedure TZeroBytePadding.Init(const random: ISecureRandom);
begin
  // nothing to do.
end;
{$ENDIF}

function TZeroBytePadding.PadCount(const input: TCryptoLibByteArray): Int32;
var
  count: Int32;
begin
  count := System.Length(input);
  while (count > 0) do
  begin
    if (input[count - 1] <> 0) then
    begin
      break;
    end;

    System.Dec(count);
  end;

  result := System.Length(input) - count;
end;

end.
