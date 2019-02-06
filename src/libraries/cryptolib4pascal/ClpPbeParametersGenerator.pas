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

unit ClpPbeParametersGenerator;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpICipherParameters,
  ClpIPbeParametersGenerator,
  ClpCryptoLibTypes;

resourcestring
  SEmptyPassword = 'Password can''t be empty';
  SEmptySalt = 'Salt can''t be empty';
  SIterationtooSmall = 'Iteration must be greater than zero.';

type

  /// <summary>
  /// base class for all Password Based Encryption (Pbe) parameter generator
  /// classes.
  /// </summary>
  TPbeParametersGenerator = class abstract(TInterfacedObject,
    IPbeParametersGenerator)

  strict protected
  var
    FmPassword, FmSalt: TCryptoLibByteArray;
    FmIterationCount: Int32;

    /// <returns>
    /// the password byte array.
    /// </returns>
    function GetPassword: TCryptoLibByteArray; virtual;

    /// <returns>
    /// the salt byte array.
    /// </returns>
    function GetSalt: TCryptoLibByteArray; virtual;

    /// <returns>
    /// the iteration count.
    /// </returns>
    function GetIterationCount: Int32; virtual;

  public

    procedure Init(const password, salt: TCryptoLibByteArray;
      iterationCount: Int32); virtual;

    /// <value>
    /// the password byte array.
    /// </value>
    property password: TCryptoLibByteArray read GetPassword;

    /// <value>
    /// the salt byte array.
    /// </value>
    property salt: TCryptoLibByteArray read GetSalt;

    /// <value>
    /// the iteration count.
    /// </value>
    property iterationCount: Int32 read GetIterationCount;

    /// <summary>
    /// Generate derived parameters for a key of length keySize.
    /// </summary>
    /// <param name="algorithm">
    /// a parameters object representing a key.
    /// </param>
    /// <param name="keySize">
    /// the length, in bits, of the key required.
    /// </param>
    /// <returns>
    /// a parameters object representing a key.
    /// </returns>
    function GenerateDerivedParameters(const algorithm: String; keySize: Int32)
      : ICipherParameters; overload; virtual; abstract;

    /// <summary>
    /// Generate derived parameters for a key of length keySize and iv
    /// of length ivSize.
    /// </summary>
    /// <param name="algorithm">
    /// a parameters object representing a key.
    /// </param>
    /// <param name="keySize">
    /// the length, in bits, of the key required.
    /// </param>
    /// <param name="ivSize">
    /// the length, in bits, of the iv required.
    /// </param>
    /// <returns>
    /// a parameters object representing a key and an iv.
    /// </returns>
    function GenerateDerivedParameters(const algorithm: String;
      keySize, ivSize: Int32): ICipherParameters; overload; virtual; abstract;

    /// <summary>
    /// Generate derived parameters for a key of length keySize,
    /// specifically <br />for use with a MAC.
    /// </summary>
    /// <param name="keySize">
    /// the length, in bits, of the key required.
    /// </param>
    /// <returns>
    /// a parameters object representing a key.
    /// </returns>
    function GenerateDerivedMacParameters(keySize: Int32): ICipherParameters;
      virtual; abstract;

  end;

implementation

{ TPbeParametersGenerator }

function TPbeParametersGenerator.GetIterationCount: Int32;
begin
  result := FmIterationCount;
end;

function TPbeParametersGenerator.GetPassword: TCryptoLibByteArray;
begin
  result := System.Copy(FmPassword);
end;

function TPbeParametersGenerator.GetSalt: TCryptoLibByteArray;
begin
  result := System.Copy(FmSalt);
end;

procedure TPbeParametersGenerator.Init(const password,
  salt: TCryptoLibByteArray; iterationCount: Int32);
begin

  if (password = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SEmptyPassword);
  end;

  if (salt = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SEmptySalt);
  end;

  if (iterationCount < 1) then
    raise EArgumentCryptoLibException.CreateRes(@SIterationtooSmall);

  FmPassword := System.Copy(password);
  FmSalt := System.Copy(salt);
  FmIterationCount := iterationCount;
end;

end.
