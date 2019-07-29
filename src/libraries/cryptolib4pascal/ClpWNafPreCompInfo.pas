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
  TWNafPreCompInfo = class sealed(TInterfacedObject, IPreCompInfo,
    IWNafPreCompInfo)

  strict private
  var
    /// <summary>
    /// Array holding the precomputed <c>ECPoint</c>s used for a Window NAF
    /// multiplication.
    /// </summary>
    FPreComp: TCryptoLibGenericArray<IECPoint>;

    /// <summary>
    /// Array holding the negations of the precomputed <c>ECPoint</c>s used
    /// for a Window NAF multiplication.
    /// </summary>
    FPreCompNeg: TCryptoLibGenericArray<IECPoint>;

    /// <summary>
    /// Holds an <c>ECPoint</c> representing Twice(this). Used for the Window
    /// NAF multiplication to create or extend the precomputed values.
    /// </summary>
    FTwice: IECPoint;

    FConfWidth, FWidth: Int32;

{$IFNDEF FPC}
{$IFDEF HAS_VOLATILE}[volatile]
{$ENDIF}
{$ENDIF}
    FPromotionCountdown: Int32;

    function GetPreComp: TCryptoLibGenericArray<IECPoint>; inline;
    procedure SetPreComp(const Value: TCryptoLibGenericArray<IECPoint>); inline;
    function GetPreCompNeg: TCryptoLibGenericArray<IECPoint>; inline;
    procedure SetPreCompNeg(const Value
      : TCryptoLibGenericArray<IECPoint>); inline;
    function GetTwice: IECPoint; inline;
    procedure SetTwice(const Value: IECPoint); inline;

    function GetConfWidth: Int32; inline;
    procedure SetConfWidth(Value: Int32); inline;

    function GetWidth: Int32; inline;
    procedure SetWidth(Value: Int32); inline;

    function GetPromotionCountdown: Int32; inline;
    procedure SetPromotionCountdown(Value: Int32); inline;

    function DecrementPromotionCountdown: Int32; inline;
    function IsPromoted: Boolean; inline;

  public

    constructor Create();
    property PreComp: TCryptoLibGenericArray<IECPoint> read GetPreComp
      write SetPreComp;
    property PreCompNeg: TCryptoLibGenericArray<IECPoint> read GetPreCompNeg
      write SetPreCompNeg;
    property Twice: IECPoint read GetTwice write SetTwice;

    property ConfWidth: Int32 read GetConfWidth write SetConfWidth;
    property Width: Int32 read GetWidth write SetWidth;

    property PromotionCountdown: Int32 read GetPromotionCountdown
      write SetPromotionCountdown;

  end;

implementation

{ TWNafPreCompInfo }

constructor TWNafPreCompInfo.Create;
begin
  inherited Create();
  FConfWidth := -1;
  FWidth := -1;
  FPromotionCountdown := 4;
end;

function TWNafPreCompInfo.DecrementPromotionCountdown: Int32;
var
  t: Int32;
begin
  t := PromotionCountdown;
  if (t > 0) then
  begin
    System.Dec(t);
    PromotionCountdown := t;
  end;
  result := t;
end;

function TWNafPreCompInfo.GetConfWidth: Int32;
begin
  result := FConfWidth;
end;

function TWNafPreCompInfo.GetPreComp: TCryptoLibGenericArray<IECPoint>;
begin
  result := FPreComp;
end;

function TWNafPreCompInfo.GetPreCompNeg: TCryptoLibGenericArray<IECPoint>;
begin
  result := FPreCompNeg;
end;

function TWNafPreCompInfo.GetPromotionCountdown: Int32;
begin
{$IFDEF FPC}
  result := {$IFDEF HAS_VOLATILE}volatile{$ENDIF}(FPromotionCountdown);
{$ELSE}
    result := FPromotionCountdown;
{$ENDIF}
end;

function TWNafPreCompInfo.GetTwice: IECPoint;
begin
  result := FTwice;
end;

function TWNafPreCompInfo.GetWidth: Int32;
begin
  result := FWidth;
end;

function TWNafPreCompInfo.IsPromoted: Boolean;
begin
  result := PromotionCountdown <= 0;
end;

procedure TWNafPreCompInfo.SetConfWidth(Value: Int32);
begin
  FConfWidth := Value;
end;

procedure TWNafPreCompInfo.SetPreComp(const Value
  : TCryptoLibGenericArray<IECPoint>);
begin
  FPreComp := Value;
end;

procedure TWNafPreCompInfo.SetPreCompNeg(const Value
  : TCryptoLibGenericArray<IECPoint>);
begin
  FPreCompNeg := Value;
end;

procedure TWNafPreCompInfo.SetPromotionCountdown(Value: Int32);
begin
{$IFDEF FPC}
  FPromotionCountdown := {$IFDEF HAS_VOLATILE}volatile{$ENDIF}(Value);
{$ELSE}
    FPromotionCountdown := Value;
{$ENDIF}
end;

procedure TWNafPreCompInfo.SetTwice(const Value: IECPoint);
begin
  FTwice := Value;
end;

procedure TWNafPreCompInfo.SetWidth(Value: Int32);
begin
  FWidth := Value;
end;

end.
