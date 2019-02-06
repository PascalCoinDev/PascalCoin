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

unit ClpHMac;

{$I CryptoLib.inc}

interface

uses
  HlpIHashInfo,
  HlpHashFactory,
  ClpIMac,
  ClpIHMac,
  ClpIDigest,
  ClpIKeyParameter,
  ClpICipherParameters,
  ClpCryptoLibTypes;

resourcestring
  SOutputBufferTooShort = 'Output Buffer Too Short';

type

  /// <summary>
  /// <para>
  /// HMAC implementation based on RFC2104 <br />H(K XOR opad, H(K XOR
  /// ipad, text))
  /// </para>
  /// <para>
  /// Note: This is Just a Wrapper for <b>HMAC</b> Implementation in
  /// HashLib4Pascal
  /// </para>
  /// </summary>
  THMac = class sealed(TInterfacedObject, IHMac, IMac)

  strict private
  var
    FDigest: IDigest;
    FHMAC: HlpIHashInfo.IHMac;

    function GetAlgorithmName: string; inline;

  public
    constructor Create(const digest: IDigest);

    function GetUnderlyingDigest: IDigest; inline;
    function GetMacSize: Int32; inline;

    procedure Update(input: Byte);
    procedure BlockUpdate(const input: TCryptoLibByteArray; inOff, len: Int32);
    procedure Init(const parameters: ICipherParameters);
    function DoFinal(const output: TCryptoLibByteArray; outOff: Int32)
      : Int32; overload;
    function DoFinal: TCryptoLibByteArray; overload;

    /// <summary>
    /// Reset the mac generator.
    /// </summary>
    procedure Reset();

    property AlgorithmName: String read GetAlgorithmName;

  end;

implementation

{ THMac }

function THMac.GetMacSize: Int32;
begin
  result := FHMAC.HashSize;
end;

procedure THMac.BlockUpdate(const input: TCryptoLibByteArray;
  inOff, len: Int32);
begin
  FHMAC.TransformBytes(input, inOff, len);
end;

constructor THMac.Create(const digest: IDigest);
begin
  Inherited Create();
  FDigest := digest;
  FHMAC := THashFactory.THMac.CreateHMAC(FDigest.GetUnderlyingIHash);
end;

function THMac.DoFinal(const output: TCryptoLibByteArray; outOff: Int32): Int32;
var
  buf: TCryptoLibByteArray;
begin

  if (System.Length(output) - outOff) < GetMacSize then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooShort);
  end
  else
  begin
    buf := DoFinal();
    System.Move(buf[0], output[outOff], System.Length(buf) *
      System.SizeOf(Byte));
  end;
  result := System.Length(buf);
end;

function THMac.DoFinal: TCryptoLibByteArray;
begin
  result := FHMAC.TransformFinal.GetBytes();
end;

function THMac.GetAlgorithmName: string;
begin
  result := FDigest.AlgorithmName + '/HMAC';
end;

function THMac.GetUnderlyingDigest: IDigest;
begin
  result := FDigest;
end;

procedure THMac.Init(const parameters: ICipherParameters);
begin
  FHMAC.Key := (parameters as IKeyParameter).GetKey();
  FHMAC.Initialize;
end;

procedure THMac.Reset;
begin
  FHMAC.Initialize;
end;

procedure THMac.Update(input: Byte);
begin
  FHMAC.TransformUntyped(input, System.SizeOf(Byte));
end;

end.
