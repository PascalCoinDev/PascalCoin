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

unit ClpRandomDsaKCalculator;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpBigInteger,
  ClpISecureRandom,
  ClpIDsaKCalculator,
  ClpIRandomDsaKCalculator;

{$IFNDEF _FIXINSIGHT_}

resourcestring
  SUnSupportedOperation = 'Operation not Supported';
{$ENDIF}

type
  TRandomDsaKCalculator = class(TInterfacedObject, IDsaKCalculator,
    IRandomDsaKCalculator)

  strict private
    Fq: TBigInteger;
    Frandom: ISecureRandom;

    function GetIsDeterministic: Boolean; virtual;

  public
    property IsDeterministic: Boolean read GetIsDeterministic;
    procedure Init(const n: TBigInteger; const random: ISecureRandom);
      overload; virtual;
    procedure Init(const n, d: TBigInteger;
      const &message: TCryptoLibByteArray); overload; virtual;
    function NextK(): TBigInteger; virtual;
  end;

implementation

{ TRandomDsaKCalculator }

function TRandomDsaKCalculator.GetIsDeterministic: Boolean;
begin
  Result := False;
end;

procedure TRandomDsaKCalculator.Init(const n: TBigInteger;
  const random: ISecureRandom);
begin
  Fq := n;
  Frandom := random;
end;

{$IFNDEF _FIXINSIGHT_}

procedure TRandomDsaKCalculator.Init(const n, d: TBigInteger;
  const &message: TCryptoLibByteArray);
begin
  raise EInvalidOperationCryptoLibException.CreateRes(@SUnSupportedOperation);
end;
{$ENDIF}

function TRandomDsaKCalculator.NextK: TBigInteger;
var
  qBitLength: Int32;
  k: TBigInteger;
begin
  qBitLength := Fq.BitLength;

  repeat
    k := TBigInteger.Create(qBitLength, Frandom);
  until (not((k.SignValue < 1) or (k.CompareTo(Fq) >= 0)));

  Result := k;

end;

end.
