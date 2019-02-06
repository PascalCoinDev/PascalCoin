unit SbpIBase58;

{$I SimpleBaseLib.inc}

interface

uses
  SbpSimpleBaseLibTypes;

type
  IBase58 = interface(IInterface)
    ['{0F089293-C718-4A07-BB42-FB203FC03CEA}']
    /// <summary>
    /// Encode to Base58 representation
    /// </summary>
    /// <param name="bytes">Bytes to encode</param>
    /// <returns>Encoded string</returns>
    function Encode(const bytes: TSimpleBaseLibByteArray): String;
    /// <summary>
    /// Decode a Base58 representation
    /// </summary>
    /// <param name="text">Base58 encoded text</param>
    /// <returns>Array of decoded bytes</returns>
    function Decode(const text: String): TSimpleBaseLibByteArray;
  end;

implementation

end.
