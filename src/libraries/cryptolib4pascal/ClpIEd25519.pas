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

unit ClpIEd25519;

{$I CryptoLib.inc}

interface

uses
  ClpIDigest,
  ClpISecureRandom,
  ClpCryptoLibTypes;

type
  IEd25519 = interface(IInterface)
    ['{2C6CD9DD-8809-44E6-979D-1EBE465CE9E2}']

    function CreatePreHash(): IDigest;

    procedure GeneratePrivateKey(const random: ISecureRandom;
      const k: TCryptoLibByteArray);

    procedure GeneratePublicKey(const sk: TCryptoLibByteArray; skOff: Int32;
      pk: TCryptoLibByteArray; pkOff: Int32);

    procedure Sign(const sk: TCryptoLibByteArray; skOff: Int32;
      const m: TCryptoLibByteArray; mOff, mLen: Int32;
      const sig: TCryptoLibByteArray; sigOff: Int32); overload;

    procedure Sign(const sk: TCryptoLibByteArray; skOff: Int32;
      const pk: TCryptoLibByteArray; pkOff: Int32; const m: TCryptoLibByteArray;
      mOff, mLen: Int32; const sig: TCryptoLibByteArray;
      sigOff: Int32); overload;

    procedure Sign(const sk: TCryptoLibByteArray; skOff: Int32;
      const ctx, m: TCryptoLibByteArray; mOff, mLen: Int32;
      const sig: TCryptoLibByteArray; sigOff: Int32); overload;

    procedure Sign(const sk: TCryptoLibByteArray; skOff: Int32;
      const pk: TCryptoLibByteArray; pkOff: Int32;
      const ctx, m: TCryptoLibByteArray; mOff, mLen: Int32;
      const sig: TCryptoLibByteArray; sigOff: Int32); overload;

    procedure SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
      const ctx, ph: TCryptoLibByteArray; phOff: Int32;
      const sig: TCryptoLibByteArray; sigOff: Int32); overload;

    procedure SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
      const pk: TCryptoLibByteArray; pkOff: Int32;
      const ctx, ph: TCryptoLibByteArray; phOff: Int32;
      const sig: TCryptoLibByteArray; sigOff: Int32); overload;

    procedure SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
      const ctx: TCryptoLibByteArray; const ph: IDigest;
      const sig: TCryptoLibByteArray; sigOff: Int32); overload;

    procedure SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
      const pk: TCryptoLibByteArray; pkOff: Int32;
      const ctx: TCryptoLibByteArray; const ph: IDigest;
      const sig: TCryptoLibByteArray; sigOff: Int32); overload;

    function Verify(const sig: TCryptoLibByteArray; sigOff: Int32;
      const pk: TCryptoLibByteArray; pkOff: Int32; const m: TCryptoLibByteArray;
      mOff, mLen: Int32): Boolean; overload;

    function Verify(const sig: TCryptoLibByteArray; sigOff: Int32;
      const pk: TCryptoLibByteArray; pkOff: Int32;
      const ctx, m: TCryptoLibByteArray; mOff, mLen: Int32): Boolean; overload;

    function VerifyPreHash(const sig: TCryptoLibByteArray; sigOff: Int32;
      const pk: TCryptoLibByteArray; pkOff: Int32;
      const ctx, ph: TCryptoLibByteArray; phOff: Int32): Boolean; overload;

    function VerifyPreHash(const sig: TCryptoLibByteArray; sigOff: Int32;
      const pk: TCryptoLibByteArray; pkOff: Int32;
      const ctx: TCryptoLibByteArray; const ph: IDigest): Boolean; overload;

  end;

implementation

end.
