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

unit ClpWNafPreCompInfo;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpIECC,
  ClpIWNafPreCompInfo,
  ClpIPreCompInfo;

type

  /// <summary>
  /// Class holding precomputation data for the WNAF (Window Non-Adjacent
  /// Form) algorithm.
  /// </summary>
  TWNafPreCompInfo = class(TInterfacedObject, IPreCompInfo, IWNafPreCompInfo)

  strict private
    function GetPreComp: TCryptoLibGenericArray<IECPoint>; virtual;
    procedure SetPreComp(const Value
      : TCryptoLibGenericArray<IECPoint>); virtual;
    function GetPreCompNeg: TCryptoLibGenericArray<IECPoint>; virtual;
    procedure SetPreCompNeg(const Value
      : TCryptoLibGenericArray<IECPoint>); virtual;
    function GetTwice: IECPoint; virtual;
    procedure SetTwice(const Value: IECPoint); virtual;
  strict protected
  var
    /// <summary>
    /// Array holding the precomputed <c>ECPoint</c>s used for a Window NAF
    /// multiplication.
    /// </summary>
    Fm_preComp: TCryptoLibGenericArray<IECPoint>;

    /// <summary>
    /// Array holding the negations of the precomputed <c>ECPoint</c>s used
    /// for a Window NAF multiplication.
    /// </summary>
    Fm_preCompNeg: TCryptoLibGenericArray<IECPoint>;

    /// <summary>
    /// Holds an <c>ECPoint</c> representing Twice(this). Used for the Window
    /// NAF multiplication to create or extend the precomputed values.
    /// </summary>
    Fm_twice: IECPoint;

  public

    constructor Create();
    destructor Destroy; override;
    property PreComp: TCryptoLibGenericArray<IECPoint> read GetPreComp
      write SetPreComp;
    property PreCompNeg: TCryptoLibGenericArray<IECPoint> read GetPreCompNeg
      write SetPreCompNeg;
    property Twice: IECPoint read GetTwice write SetTwice;

  end;

implementation

{ TWNafPreCompInfo }

constructor TWNafPreCompInfo.Create;
begin
  inherited Create();
end;

destructor TWNafPreCompInfo.Destroy;
begin
  inherited Destroy;
end;

function TWNafPreCompInfo.GetPreComp: TCryptoLibGenericArray<IECPoint>;
begin
  Result := Fm_preComp;
end;

function TWNafPreCompInfo.GetPreCompNeg: TCryptoLibGenericArray<IECPoint>;
begin
  Result := Fm_preCompNeg;
end;

function TWNafPreCompInfo.GetTwice: IECPoint;
begin
  Result := Fm_twice;
end;

procedure TWNafPreCompInfo.SetPreComp(const Value
  : TCryptoLibGenericArray<IECPoint>);
begin
  Fm_preComp := Value;
end;

procedure TWNafPreCompInfo.SetPreCompNeg(const Value
  : TCryptoLibGenericArray<IECPoint>);
begin
  Fm_preCompNeg := Value;
end;

procedure TWNafPreCompInfo.SetTwice(const Value: IECPoint);
begin
  Fm_twice := Value;
end;

end.
