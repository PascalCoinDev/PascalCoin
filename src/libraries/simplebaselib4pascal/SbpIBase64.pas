unit SbpIBase64;

{$I SimpleBaseLib.inc}

interface

uses
  SbpSimpleBaseLibTypes;

type
  IBase64 = interface(IInterface)
    ['{7EBE8339-7C3B-43E6-BCAC-73B80DC1A91A}']

    /// <summary>
    /// Encode a byte array into a Base64 string
    /// </summary>
    /// <param name="bytes">Buffer to be encoded</param>
    /// <returns>Encoded string</returns>
    function Encode(const bytes: TSimpleBaseLibByteArray): String;
    /// <summary>
    /// Decode a Base64 encoded string into a byte array.
    /// </summary>
    /// <param name="text">Encoded Base64 string</param>
    /// <returns>Decoded byte array</returns>
    function Decode(const text: String): TSimpleBaseLibByteArray;
  end;

implementation

end.
