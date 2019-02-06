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

unit ClpDsaSigner;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  Math,
  SysUtils,
  ClpIDsaExt,
  ClpIDsaSigner,
  ClpISecureRandom,
  ClpIDsaParameters,
  ClpIDsaKCalculator,
  ClpICipherParameters,
  ClpIDsaKeyParameters,
  ClpIParametersWithRandom,
  ClpIDsaPublicKeyParameters,
  ClpIDsaPrivateKeyParameters,
  ClpSecureRandom,
  ClpRandomDsaKCalculator,
  ClpBits,
  ClpBigInteger,
  ClpCryptoLibTypes;

resourcestring
  SDSAPrivateKeyNotFound = 'DSA Private Key Required For Signing';
  SDSAPublicKeyNotFound = 'DSA Public Key Required For Verification';

type

  /// <summary>
  /// The Digital Signature Algorithm - as described in "Handbook of Applied <br />
  /// Cryptography", pages 452 - 453.
  /// </summary>
  TDsaSigner = class(TInterfacedObject, IDsaExt, IDsaSigner)

  strict private
    function GetOrder: TBigInteger; virtual;
    function GetAlgorithmName: String; virtual;
  strict protected
  var
    FkCalculator: IDsaKCalculator;
    Fkey: IDsaKeyParameters;
    Frandom: ISecureRandom;

    function CalculateE(const n: TBigInteger;
      const &message: TCryptoLibByteArray): TBigInteger; virtual;

    function InitSecureRandom(needed: Boolean; const provided: ISecureRandom)
      : ISecureRandom; virtual;

  public

    /// <summary>
    /// Default configuration, random K values.
    /// </summary>
    constructor Create(); overload;

    /// <summary>
    /// Configuration with an alternate, possibly deterministic calculator of
    /// K.
    /// </summary>
    /// <param name="kCalculator">
    /// a K value calculator.
    /// </param>
    constructor Create(const kCalculator: IDsaKCalculator); overload;

    procedure Init(forSigning: Boolean; const parameters: ICipherParameters);

    /// <summary>
    /// Generate a signature for the given message using the key we were <br />
    /// initialised with. For conventional DSA the message should be a SHA-1 <br />
    /// hash of the message of interest.
    /// </summary>
    /// <param name="&amp;message">
    /// the message that will be verified later.
    /// </param>
    function GenerateSignature(const &message: TCryptoLibByteArray)
      : TCryptoLibGenericArray<TBigInteger>; virtual;

    /// <summary>
    /// return true if the value r and s represent a DSA signature for <br />
    /// the passed in message for standard DSA the message should be a <br />
    /// SHA-1 hash of the real message to be verified.
    /// </summary>
    function VerifySignature(const &message: TCryptoLibByteArray;
      const r, s: TBigInteger): Boolean; virtual;

    property Order: TBigInteger read GetOrder;
    property AlgorithmName: String read GetAlgorithmName;

  end;

implementation

{ TDsaSigner }

constructor TDsaSigner.Create;
begin
  Inherited Create();
  FkCalculator := TRandomDsaKCalculator.Create();
end;

function TDsaSigner.CalculateE(const n: TBigInteger;
  const &message: TCryptoLibByteArray): TBigInteger;
var
  length: Int32;
begin
  length := Math.Min(System.length(&message), TBits.Asr32(n.BitLength, 3));
  result := TBigInteger.Create(1, &message, 0, length);
end;

constructor TDsaSigner.Create(const kCalculator: IDsaKCalculator);
begin
  Inherited Create();
  FkCalculator := kCalculator;
end;

function TDsaSigner.GenerateSignature(const &message: TCryptoLibByteArray)
  : TCryptoLibGenericArray<TBigInteger>;
var
  parameters: IDsaParameters;
  q, m, x, k, r, s: TBigInteger;
begin
  parameters := Fkey.parameters;
  q := parameters.q;
  m := CalculateE(q, &message);
  x := (Fkey as IDsaPrivateKeyParameters).x;

  if (FkCalculator.IsDeterministic) then
  begin
    FkCalculator.Init(q, x, &message);
  end
  else
  begin
    FkCalculator.Init(q, Frandom);
  end;

  k := FkCalculator.NextK();

  r := parameters.G.ModPow(k, parameters.P).&Mod(q);

  k := k.ModInverse(q).Multiply(m.Add(x.Multiply(r)));

  s := k.&Mod(q);

  result := TCryptoLibGenericArray<TBigInteger>.Create(r, s);
end;

function TDsaSigner.GetOrder: TBigInteger;
begin
  result := Fkey.parameters.q;
end;

function TDsaSigner.GetAlgorithmName: String;
begin
  result := 'DSA';
end;

procedure TDsaSigner.Init(forSigning: Boolean;
  const parameters: ICipherParameters);
var
  providedRandom: ISecureRandom;
  rParam: IParametersWithRandom;
  Lparameters: ICipherParameters;
begin
  providedRandom := Nil;
  Lparameters := parameters;

  if (forSigning) then
  begin
    if (Supports(Lparameters, IParametersWithRandom, rParam)) then
    begin
      providedRandom := rParam.Random;
      Lparameters := rParam.parameters;
    end;

    if (not Supports(Lparameters, IDsaPrivateKeyParameters)) then
    begin
      raise EInvalidKeyCryptoLibException.CreateRes(@SDSAPrivateKeyNotFound);
    end;

    Fkey := Lparameters as IDsaPrivateKeyParameters;
  end
  else
  begin
    if (not Supports(Lparameters, IDsaPublicKeyParameters)) then
    begin
      raise EInvalidKeyCryptoLibException.CreateRes(@SDSAPublicKeyNotFound);
    end;

    Fkey := Lparameters as IDsaPublicKeyParameters;
  end;

  Frandom := InitSecureRandom(forSigning and (not FkCalculator.IsDeterministic),
    providedRandom);
end;

function TDsaSigner.InitSecureRandom(needed: Boolean;
  const provided: ISecureRandom): ISecureRandom;
begin
  if (not needed) then
  begin
    result := Nil;
  end
  else
  begin
    if provided <> Nil then
    begin
      result := provided;
    end
    else
    begin
      result := TSecureRandom.Create();
    end;
  end;

end;

function TDsaSigner.VerifySignature(const &message: TCryptoLibByteArray;
  const r, s: TBigInteger): Boolean;
var
  parameters: IDsaParameters;
  q, m, w, u1, u2, P, v: TBigInteger;
begin
  parameters := Fkey.parameters;
  q := parameters.q;
  m := CalculateE(q, &message);

  if ((r.SignValue <= 0) or (q.CompareTo(r) <= 0)) then
  begin
    result := false;
    Exit;
  end;

  if ((s.SignValue <= 0) or (q.CompareTo(s) <= 0)) then
  begin
    result := false;
    Exit;
  end;

  w := s.ModInverse(q);

  u1 := m.Multiply(w).&Mod(q);
  u2 := r.Multiply(w).&Mod(q);

  P := parameters.P;
  u1 := parameters.G.ModPow(u1, P);
  u2 := (Fkey as IDsaPublicKeyParameters).Y.ModPow(u2, P);

  v := u1.Multiply(u2).&Mod(P).&Mod(q);

  result := v.Equals(r);
end;

end.
