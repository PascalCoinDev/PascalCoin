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

unit ClpSetWeakRef;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes;

type
  TSetWeakRef = class(TObject)

  public
    class procedure SetWeakReference(aInterfaceField: PIInterface;
      const aValue: IInterface); static;
  end;

implementation

{ TSetWeakRef }

class procedure TSetWeakRef.SetWeakReference(aInterfaceField: PIInterface;
  const aValue: IInterface);
begin
  PPointer(aInterfaceField)^ := Pointer(aValue);
end;

end.
