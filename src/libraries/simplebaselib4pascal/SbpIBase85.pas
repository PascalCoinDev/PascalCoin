unit SbpIBase85;

{$I ..\Include\SimpleBaseLib.inc}

interface

uses
  SbpSimpleBaseLibTypes;

type
  IBase85 = interface(IInterface)
    ['{9E8C4AD7-9400-4C80-A8E5-5386C699A1FA}']
    /// <summary>
    /// Encode a byte array into a Base85 string
    /// </summary>
    /// <param name="bytes">Buffer to be encoded</param>
    /// <returns>Encoded string</returns>
    function Encode(const bytes: TSimpleBaseLibByteArray): String;
    /// <summary>
    /// Decode a Base85 encoded string into a byte array.
    /// </summary>
    /// <param name="text">Encoded Base85 string</param>
    /// <returns>Decoded byte array</returns>
    function Decode(const text: String): TSimpleBaseLibByteArray;
  end;

implementation

end.
