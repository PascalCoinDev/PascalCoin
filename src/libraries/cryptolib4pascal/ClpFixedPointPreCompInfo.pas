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

unit ClpFixedPointPreCompInfo;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpIECC,
  ClpIFixedPointPreCompInfo,
  ClpIPreCompInfo;

type

  /// <summary>
  /// Class holding precomputation data for fixed-point multiplications.
  /// </summary>
  TFixedPointPreCompInfo = class(TInterfacedObject, IPreCompInfo,
    IFixedPointPreCompInfo)

  strict private
    function GetWidth: Int32;
    procedure SetWidth(const Value: Int32);

    function GetLookupTable: IECLookupTable;
    procedure SetLookupTable(const Value: IECLookupTable);

    function GetOffset: IECPoint;
    procedure SetOffset(const Value: IECPoint);

  strict protected
  var
    Fm_offset: IECPoint;

    /// <summary>
    /// Array holding the precomputed <c>ECPoint</c>s used for a fixed point
    /// multiplication.
    /// </summary>
    Fm_lookupTable: IECLookupTable;

    /// <summary>
    /// The width used for the precomputation. If a larger width
    /// precomputation is already available this may be larger than was
    /// requested, so calling code should refer to the actual width.
    /// </summary>
    Fm_width: Int32;

  public
    constructor Create();
    property Offset: IECPoint read GetOffset write SetOffset;
    property LookupTable: IECLookupTable read GetLookupTable
      write SetLookupTable;
    property Width: Int32 read GetWidth write SetWidth;

  end;

implementation

{ TFixedPointPreCompInfo }

constructor TFixedPointPreCompInfo.Create;
begin
  inherited Create();
  Fm_width := -1;
end;

function TFixedPointPreCompInfo.GetLookupTable: IECLookupTable;
begin
  Result := Fm_lookupTable;
end;

function TFixedPointPreCompInfo.GetOffset: IECPoint;
begin
  Result := Fm_offset;
end;

function TFixedPointPreCompInfo.GetWidth: Int32;
begin
  Result := Fm_width;
end;

procedure TFixedPointPreCompInfo.SetLookupTable(const Value: IECLookupTable);
begin
  Fm_lookupTable := Value;
end;

procedure TFixedPointPreCompInfo.SetOffset(const Value: IECPoint);
begin
  Fm_offset := Value;
end;

procedure TFixedPointPreCompInfo.SetWidth(const Value: Int32);
begin
  Fm_width := Value;
end;

end.
