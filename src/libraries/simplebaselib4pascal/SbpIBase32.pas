unit SbpIBase32;

{$I ..\Include\SimpleBaseLib.inc}

interface

uses
  SbpSimpleBaseLibTypes;

type
  IBase32 = interface(IInterface)
    ['{012275EE-9B81-4E82-A96A-F2CB7EB5EDA2}']
    /// <summary>
    /// Encode a byte array into a Base32 string
    /// </summary>
    /// <param name="bytes">Buffer to be encoded</param>
    /// <param name="padding">Append padding characters in the output</param>
    /// <returns>Encoded string</returns>
    function Encode(const bytes: TSimpleBaseLibByteArray;
      padding: Boolean): String;
    /// <summary>
    /// Decode a Base32 encoded string into a byte array.
    /// </summary>
    /// <param name="text">Encoded Base32 string</param>
    /// <returns>Decoded byte array</returns>
    function Decode(const text: String): TSimpleBaseLibByteArray;
  end;

implementation

end.
