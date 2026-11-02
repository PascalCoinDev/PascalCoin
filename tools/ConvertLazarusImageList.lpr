program ConvertLazarusImageList;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>

  Converts a Lazarus image-list property stream to the native Windows stream
  consumed by VCL. Existing LCL readers and the Windows image-list serializer
  retain the original artwork; this tool does not redraw or rescale it.

  Distributed under the MIT software license, see the accompanying LICENSE.
}

{$mode delphi}{$H+}

uses
  Interfaces, Forms, Controls, Classes, SysUtils, Windows, ActiveX, CommCtrl, ImgList;

function ReadBitmapPixels(ABitmap: HBITMAP; out AWidth, AHeight: Integer): TBytes;
var
  LBitmap: Windows.TBitmap;
  LInfo: Windows.TBitmapInfo;
  LDC: HDC;
begin
  Result := nil;
  if GetObject(ABitmap, SizeOf(LBitmap), @LBitmap) = 0 then RaiseLastOSError;
  AWidth := LBitmap.bmWidth;
  AHeight := Abs(LBitmap.bmHeight);
  FillChar(LInfo, SizeOf(LInfo), 0);
  LInfo.bmiHeader.biSize := SizeOf(LInfo.bmiHeader);
  LInfo.bmiHeader.biWidth := AWidth;
  LInfo.bmiHeader.biHeight := -AHeight;
  LInfo.bmiHeader.biPlanes := 1;
  LInfo.bmiHeader.biBitCount := 32;
  LInfo.bmiHeader.biCompression := BI_RGB;
  SetLength(Result, AWidth * AHeight * 4);
  LDC := GetDC(0);
  try
    if Windows.GetDIBits(LDC, ABitmap, 0, AHeight, @Result[0], LInfo,
      DIB_RGB_COLORS) = 0 then RaiseLastOSError;
  finally
    ReleaseDC(0, LDC);
  end;
end;

procedure VerifyPixels(AOriginal, AConverted: HIMAGELIST; AWidth, AHeight: Integer);
var
  LBefore, LAfter: IMAGEINFO;
  LBeforePixels, LAfterPixels: TBytes;
  LBeforeWidth, LBeforeHeight, LAfterWidth, LAfterHeight: Integer;
  i, y, LPart, LBeforeOffset, LAfterOffset: Integer;
  LBeforeBitmap, LAfterBitmap: HBITMAP;
begin
  if ImageList_GetImageCount(AOriginal) <> ImageList_GetImageCount(AConverted) then
    raise Exception.Create('Image-list conversion changed the image count');
  for i := 0 to ImageList_GetImageCount(AOriginal) - 1 do begin
    if not ImageList_GetImageInfo(AOriginal, i, @LBefore) or
      not ImageList_GetImageInfo(AConverted, i, @LAfter) then RaiseLastOSError;
    for LPart := 0 to 1 do begin
      if LPart = 0 then begin
        LBeforeBitmap := LBefore.hbmImage;
        LAfterBitmap := LAfter.hbmImage;
      end else begin
        LBeforeBitmap := LBefore.hbmMask;
        LAfterBitmap := LAfter.hbmMask;
      end;
      if (LBeforeBitmap = 0) and (LAfterBitmap = 0) then Continue;
      if (LBeforeBitmap = 0) or (LAfterBitmap = 0) then
        raise Exception.Create('Image-list conversion changed the transparency mask');
      LBeforePixels := ReadBitmapPixels(LBeforeBitmap, LBeforeWidth, LBeforeHeight);
      LAfterPixels := ReadBitmapPixels(LAfterBitmap, LAfterWidth, LAfterHeight);
      for y := 0 to AHeight - 1 do begin
        LBeforeOffset := ((LBefore.rcImage.Top + y) * LBeforeWidth + LBefore.rcImage.Left) * 4;
        LAfterOffset := ((LAfter.rcImage.Top + y) * LAfterWidth + LAfter.rcImage.Left) * 4;
        if not CompareMem(@LBeforePixels[LBeforeOffset], @LAfterPixels[LAfterOffset], AWidth * 4) then
          raise Exception.CreateFmt('Image-list conversion changed pixels/mask in image %d', [i]);
      end;
    end;
  end;
end;

var
  LImages: TImageList;
  LResolution: TCustomImageListResolution;
  LInput, LOutput: TMemoryStream;
  LAdapter: IStream;
  LNative, LRoundTrip: HIMAGELIST;
begin
  if ParamCount <> 2 then begin
    WriteLn(ErrOutput, 'Usage: ConvertLazarusImageList input.bin output.bin');
    Halt(64);
  end;
  Application.Initialize;
  LImages := TImageList.Create(nil);
  LInput := TMemoryStream.Create;
  LOutput := TMemoryStream.Create;
  LRoundTrip := 0;
  try
    LInput.LoadFromFile(ParamStr(1));
    LImages.ReadData(LInput);
    LResolution := LImages.ResolutionByIndex[0];
    if LResolution.Count = 0 then raise Exception.Create('An empty image list cannot be converted');
    // The Win32 widgetset already owns this native list, using the exact image
    // bytes loaded by LCL. Keep LImages alive until serialization and comparison
    // finish; its reference handle must not be freed by this tool.
    // ReadData loads the serialized resolution's own dimensions. Access that
    // resolution directly instead of asking the list's default 16-pixel width
    // for an automatically resized copy.
    LNative := LResolution.Reference.Handle;
    LAdapter := TStreamAdapter.Create(LOutput, soReference);
    if not ImageList_Write(LNative, LAdapter) then
      raise Exception.Create('Windows could not serialize the image list');
    LOutput.Position := 0;
    LRoundTrip := ImageList_Read(LAdapter);
    if LRoundTrip = 0 then raise Exception.Create('Windows could not read its converted image list');
    VerifyPixels(LNative, LRoundTrip, LResolution.Width, LResolution.Height);
    LOutput.SaveToFile(ParamStr(2));
    WriteLn(Format('Converted %d images, %dx%d; native pixels/masks round-trip verified',
      [LResolution.Count, LResolution.Width, LResolution.Height]));
  finally
    if LRoundTrip <> 0 then ImageList_Destroy(LRoundTrip);
    LAdapter := nil;
    LOutput.Free;
    LInput.Free;
    LImages.Free;
  end;
end.
