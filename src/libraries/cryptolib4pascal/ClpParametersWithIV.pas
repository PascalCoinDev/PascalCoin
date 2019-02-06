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

unit ClpParametersWithIV;

{$I CryptoLib.inc}

interface

uses
  ClpIParametersWithIV,
  ClpICipherParameters,
  ClpArrayUtils,
  ClpCryptoLibTypes;

resourcestring
  SIVNil = 'IV Cannot be Nil';

type
  TParametersWithIV = class sealed(TInterfacedObject, IParametersWithIV,
    ICipherParameters)

  strict private
  var
    Fparameters: ICipherParameters;
    Fiv: TCryptoLibByteArray;

    function GetParameters: ICipherParameters; inline;

  public
    constructor Create(const parameters: ICipherParameters;
      const iv: TCryptoLibByteArray); overload;
    constructor Create(const parameters: ICipherParameters;
      const iv: TCryptoLibByteArray; ivOff, ivLen: Int32); overload;
    destructor Destroy; override;
    function GetIV(): TCryptoLibByteArray; inline;
    property parameters: ICipherParameters read GetParameters;

  end;

implementation

{ TParametersWithIV }

constructor TParametersWithIV.Create(const parameters: ICipherParameters;
  const iv: TCryptoLibByteArray);
begin
  Inherited Create();
  Create(parameters, iv, 0, System.Length(iv))
end;

constructor TParametersWithIV.Create(const parameters: ICipherParameters;
  const iv: TCryptoLibByteArray; ivOff, ivLen: Int32);
begin
  Inherited Create();
  // NOTE: 'parameters' may be null to imply key re-use
  if (iv = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SIVNil);
  end;

  Fparameters := parameters;
  Fiv := TArrayUtils.CopyOfRange(iv, ivOff, ivOff + ivLen);
end;

destructor TParametersWithIV.Destroy;
begin
  TArrayUtils.Fill(Fiv, 0, System.Length(Fiv), Byte(0));
  inherited Destroy;
end;

function TParametersWithIV.GetIV: TCryptoLibByteArray;
begin
  result := System.Copy(Fiv);
end;

function TParametersWithIV.GetParameters: ICipherParameters;
begin
  result := Fparameters;
end;

end.
