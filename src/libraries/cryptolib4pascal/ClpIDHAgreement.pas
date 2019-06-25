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

unit ClpIDHAgreement;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIDHPublicKeyParameters,
  ClpICipherParameters;

type

  /// <summary>
  /// <para>
  /// a Diffie-Hellman key exchange engine.
  /// </para>
  /// <para>
  /// note: This uses MTI/A0 key agreement in order to make the key
  /// agreement secure against passive attacks. If you're doing
  /// Diffie-Hellman and both parties have long term public keys you
  /// should look at using this. For further information have a look at
  /// RFC 2631.
  /// </para>
  /// <para>
  /// It's possible to extend this to more than two parties as well, for
  /// the moment that is left as an exercise for the reader.
  /// </para>
  /// </summary>
  IDHAgreement = interface(IInterface)

    ['{2FD6DE87-B4E3-4184-874B-F51E559235D2}']

    /// <summary>
    /// initialise the agreement engine.
    /// </summary>
    procedure Init(const parameters: ICipherParameters);

    /// <summary>
    /// calculate our initial message.
    /// </summary>
    function CalculateMessage(): TBigInteger;

    /// <summary>
    /// given a message from a given party and the corresponding public key
    /// calculate the next message in the agreement sequence. In this case
    /// this will represent the shared secret.
    /// </summary>
    function CalculateAgreement(const pub: IDHPublicKeyParameters;
      const &message: TBigInteger): TBigInteger;

  end;

implementation

end.
