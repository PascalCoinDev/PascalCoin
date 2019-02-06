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

unit ClpHkdfParameters;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpIHkdfParameters,
  ClpIDerivationParameters,
  ClpCryptoLibTypes;

resourcestring
  SIKMNil = '"ikm" is Nil';

type

  /// <summary>
  /// Parameter class for the HkdfBytesGenerator class.
  /// </summary>
  THkdfParameters = class(TInterfacedObject, IDerivationParameters,
    IHkdfParameters)

  strict private
  var
    Fikm, Fsalt, Finfo: TCryptoLibByteArray;
    FskipExpand: Boolean;

    constructor Create(const ikm: TCryptoLibByteArray; skip: Boolean;
      const salt, info: TCryptoLibByteArray); overload;

  strict protected

    /// <summary>
    /// Returns if step 1: extract has to be skipped or not
    /// </summary>
    /// <returns>
    /// true for skipping, false for no skipping of step 1
    /// </returns>
    function GetSkipExtract: Boolean; virtual;

  public

    /// <summary>
    /// Generates parameters for HKDF, specifying both the optional salt and <br />
    /// optional info. Step 1: Extract won't be skipped.
    /// </summary>
    /// <param name="ikm">
    /// the input keying material or seed
    /// </param>
    /// <param name="salt">
    /// the salt to use, may be null for a salt for hashLen zeros
    /// </param>
    /// <param name="info">
    /// the info to use, may be null for an info field of zero bytes
    /// </param>
    constructor Create(const ikm, salt, info: TCryptoLibByteArray); overload;

    /// <summary>
    /// Returns the input keying material or seed.
    /// </summary>
    /// <returns>
    /// the keying material
    /// </returns>
    function GetIkm(): TCryptoLibByteArray; virtual;

    /// <summary>
    /// Returns the salt, or null if the salt should be generated as a byte
    /// array <br />of HashLen zeros.
    /// </summary>
    /// <returns>
    /// the salt, or null
    /// </returns>
    function GetSalt(): TCryptoLibByteArray; virtual;

    /// <summary>
    /// Returns the info field, which may be empty (null is converted to
    /// empty).
    /// </summary>
    /// <returns>
    /// the info field, never null
    /// </returns>
    function GetInfo(): TCryptoLibByteArray; virtual;

    /// <summary>
    /// Returns if step 1: extract has to be skipped or not
    /// </summary>
    /// <returns>
    /// true for skipping, false for no skipping of step 1
    /// </returns>
    property SkipExtract: Boolean read GetSkipExtract;

    /// <summary>
    /// Factory method that makes the HKDF skip the extract part of the key <br />
    /// derivation function.
    /// </summary>
    /// <param name="ikm">
    /// the input keying material or seed, directly used for step 2: Expand
    /// </param>
    /// <param name="info">
    /// the info to use, may be null for an info field of zero bytes
    /// </param>
    /// <returns>
    /// that makes the implementation skip step 1
    /// </returns>
    class function SkipExtractParameters(const ikm, info: TCryptoLibByteArray)
      : IHkdfParameters; static; inline;

    class function DefaultParameters(const ikm: TCryptoLibByteArray)
      : IHkdfParameters; static; inline;

  end;

implementation

{ THkdfParameters }

constructor THkdfParameters.Create(const ikm: TCryptoLibByteArray;
  skip: Boolean; const salt, info: TCryptoLibByteArray);
begin
  Inherited Create();

  if (ikm = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SIKMNil);
  end;

  Fikm := System.Copy(ikm);
  FskipExpand := skip;

  if ((salt = Nil) or (System.Length(salt) = 0)) then
  begin
    Fsalt := Nil;
  end
  else
  begin
    Fsalt := System.Copy(salt);
  end;

  if (info = Nil) then
  begin
    System.SetLength(Finfo, 0);
  end
  else
  begin
    Finfo := System.Copy(info);
  end;
end;

constructor THkdfParameters.Create(const ikm, salt, info: TCryptoLibByteArray);
begin
  Create(ikm, false, salt, info);
end;

class function THkdfParameters.DefaultParameters(const ikm: TCryptoLibByteArray)
  : IHkdfParameters;
begin
  result := THkdfParameters.Create(ikm, false, Nil, Nil);
end;

function THkdfParameters.GetIkm: TCryptoLibByteArray;
begin
  result := System.Copy(Fikm);
end;

function THkdfParameters.GetInfo: TCryptoLibByteArray;
begin
  result := System.Copy(Finfo);
end;

function THkdfParameters.GetSalt: TCryptoLibByteArray;
begin
  result := System.Copy(Fsalt);
end;

function THkdfParameters.GetSkipExtract: Boolean;
begin
  result := FskipExpand;
end;

class function THkdfParameters.SkipExtractParameters(const ikm,
  info: TCryptoLibByteArray): IHkdfParameters;
begin
  result := THkdfParameters.Create(ikm, true, Nil, info);
end;

end.
