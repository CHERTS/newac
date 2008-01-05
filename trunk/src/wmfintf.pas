(*
  This unit contains fragmets from Windows Media SDK header files
  relevant to encoding/decoding audio.
*)

unit wmfintf;

{ Unit: wmfintf.pas
    Delphi headers for Windows Media related stuff. Contains the bits from the 
    Windows Media SDK header files relevant to encoding/decoding data. }

interface
uses
  Windows, ActiveX, _DirectSound, MMSystem;

const

  IID_IWMSyncReader             : TGUID = '{9397f121-7705-4dc9-b049-98b698188414}';
  {$EXTERNALSYM IID_IWMSyncReader}
  IID_IWMOutputMediaProps       : TGUID = '{96406bd7-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMOutputMediaProps}
  IID_IWMHeaderInfo             : TGUID = '{96406bda-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMHeaderInfo}
  IID_IWMWriterFileSink         : TGUID = '{96406be5-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriterFileSink}
  IID_IWMWriter                 : TGUID = '{96406bd4-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriter}
  IID_IWMWriterSink             : TGUID = '{96406be4-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriterSink}
  IID_IWMWriterAdvanced         : TGUID = '{96406be3-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriterAdvanced}
  IID_IWMProfile                : TGUID = '{96406bdb-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMProfile}
  IID_IWMProfileManager         : TGUID = '{d16679f2-6ca0-472d-8d31-2f5d55aee155}';
  {$EXTERNALSYM IID_IWMProfileManager}
  IID_IWMInputMediaProps        : TGUID = '{96406bd5-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMInputMediaProps}
  IID_IWMCodecInfo              : TGUID = '{a970f41e-34de-4a98-b3ba-e4b3ca7528f0}';
  {$EXTERNALSYM IID_IWMCodecInfo}
  IID_IWMCodecInfo2             : TGUID = '{aa65e273-b686-4056-91ec-dd768d4df710}';
  {$EXTERNALSYM IID_IWMCodecInfo2}
  IID_IWMCodecInfo3             : TGUID = '{7e51f487-4d93-4f98-8ab4-27d0565adc51}';
  {$EXTERNALSYM IID_IWMCodecInfo3}

  WMMEDIASUBTYPE_WMAudioV9        : TGUID = '{00000162-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV9}
  WMMEDIASUBTYPE_WMAudio_Lossless : TGUID = '{00000163-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudio_Lossless}
  WMMEDIASUBTYPE_WMSP1            : TGUID = '{0000000A-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMSP1}
  WMMEDIASUBTYPE_WMAudioV8 : TGUID = '{00000161-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV8}
  WMMEDIASUBTYPE_WMAudioV7 : TGUID = '{00000161-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV7}
  WMMEDIASUBTYPE_WMAudioV2 : TGUID = '{00000161-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV2}



  WMProfile_V70_6VoiceAudio            : TGUID = '{EABA9FBF-B64F-49b3-AA0C-73FBDD150AD0}';
  {$EXTERNALSYM WMProfile_V70_6VoiceAudio}
  WMProfile_V70_64Audio                : TGUID = '{B29CFFC6-F131-41db-B5E8-99D8B0B945F4}';
  {$EXTERNALSYM WMProfile_V70_64Audio}
  WMProfile_V70_96Audio                : TGUID = '{A9D4B819-16CC-4a59-9F37-693DBB0302D6}';
  {$EXTERNALSYM WMProfile_V70_96Audio}
  WMProfile_V70_128Audio               : TGUID = '{C64CF5DA-DF45-40d3-8027-DE698D68DC66}';
  {$EXTERNALSYM WMProfile_V70_128Audio}

  WMMEDIATYPE_Audio               : TGUID = '{73647561-0000-0010-8000-00AA00389B71}'; // 'auds'
  {$EXTERNALSYM WMMEDIATYPE_Audio}
  WMFORMAT_WaveFormatEx    : TGUID = '{05589f81-c356-11ce-bf01-00aa0055595a}';
  {$EXTERNALSYM WMFORMAT_WaveFormatEx}

  g_dwWMSpecialAttributes       = LongWord(20);
  {$EXTERNALSYM g_dwWMSpecialAttributes}
  g_wszWMDuration               = WideString('Duration');
  {$EXTERNALSYM g_wszWMDuration}
  g_wszWMBitrate                = WideString('Bitrate');
  {$EXTERNALSYM g_wszWMBitrate}
  g_wszWMSeekable               = WideString('Seekable');
  {$EXTERNALSYM g_wszWMSeekable}
  g_wszWMStridable              = WideString('Stridable');
  {$EXTERNALSYM g_wszWMStridable}
  g_wszWMBroadcast              = WideString('Broadcast');
  {$EXTERNALSYM g_wszWMBroadcast}
  g_wszWMProtected              = WideString('Is_Protected');
  {$EXTERNALSYM g_wszWMProtected}
  g_wszWMTrusted                = WideString('Is_Trusted');
  {$EXTERNALSYM g_wszWMTrusted}
  g_wszWMSignature_Name         = WideString('Signature_Name');
  {$EXTERNALSYM g_wszWMSignature_Name}
  g_wszWMHasAudio               = WideString('HasAudio');
  {$EXTERNALSYM g_wszWMHasAudio}
  g_wszWMHasImage               = WideString('HasImage');
  {$EXTERNALSYM g_wszWMHasImage}
  g_wszWMHasScript              = WideString('HasScript');
  {$EXTERNALSYM g_wszWMHasScript}
  g_wszWMHasVideo               = WideString('HasVideo');
  {$EXTERNALSYM g_wszWMHasVideo}
  g_wszWMCurrentBitrate         = WideString('CurrentBitrate');
  {$EXTERNALSYM g_wszWMCurrentBitrate}
  g_wszWMOptimalBitrate         = WideString('OptimalBitrate');
  {$EXTERNALSYM g_wszWMOptimalBitrate}
  g_wszWMHasAttachedImages      = WideString('HasAttachedImages');
  {$EXTERNALSYM g_wszWMHasAttachedImages}
  g_wszWMSkipBackward           = WideString('Can_Skip_Backward');
  {$EXTERNALSYM g_wszWMSkipBackward}
  g_wszWMSkipForward            = WideString('Can_Skip_Forward');
  {$EXTERNALSYM g_wszWMSkipForward}
  g_wszWMNumberOfFrames         = WideString('NumberOfFrames');
  {$EXTERNALSYM g_wszWMNumberOfFrames}
  g_wszWMFileSize               = WideString('FileSize');
  {$EXTERNALSYM g_wszWMFileSize}
  g_wszWMHasArbitraryDataStream = WideString('HasArbitraryDataStream');
  {$EXTERNALSYM g_wszWMHasArbitraryDataStream}
  g_wszWMHasFileTransferStream  = WideString('HasFileTransferStream');
  {$EXTERNALSYM g_wszWMHasFileTransferStream}
  g_wszWMContainerFormat        = WideString('WM/ContainerFormat');
  {$EXTERNALSYM g_wszWMContainerFormat}

////////////////////////////////////////////////////////////////
//
// The content description object supports 5 basic attributes.
//

  g_dwWMContentAttributes = LongWord(5);
  {$EXTERNALSYM g_dwWMContentAttributes}
  g_wszWMTitle        = WideString('Title');
  {$EXTERNALSYM g_wszWMTitle}
  g_wszWMAuthor       = WideString('Author');
  {$EXTERNALSYM g_wszWMAuthor}
  g_wszWMDescription  = WideString('Description');
  {$EXTERNALSYM g_wszWMDescription}
  g_wszWMRating       = WideString('Rating');
  {$EXTERNALSYM g_wszWMRating}
  g_wszWMCopyright    = WideString('Copyright');
  {$EXTERNALSYM g_wszWMCopyright}

////////////////////////////////////////////////////////////////
//
// These attributes are used to configure and query DRM settings in the reader and writer.
//

  g_wszWMUse_DRM                   = WideString('Use_DRM');
  {$EXTERNALSYM g_wszWMUse_DRM}
  g_wszWMDRM_Flags                 = WideString('DRM_Flags');
  {$EXTERNALSYM g_wszWMDRM_Flags}
  g_wszWMDRM_Level                 = WideString('DRM_Level');
  {$EXTERNALSYM g_wszWMDRM_Level}
  g_wszWMUse_Advanced_DRM          = WideString('Use_Advanced_DRM');
  {$EXTERNALSYM g_wszWMUse_Advanced_DRM}
  g_wszWMDRM_KeySeed               = WideString('DRM_KeySeed');
  {$EXTERNALSYM g_wszWMDRM_KeySeed}
  g_wszWMDRM_KeyID                 = WideString('DRM_KeyID');
  {$EXTERNALSYM g_wszWMDRM_KeyID}
  g_wszWMDRM_ContentID             = WideString('DRM_ContentID');
  {$EXTERNALSYM g_wszWMDRM_ContentID}
  g_wszWMDRM_IndividualizedVersion = WideString('DRM_IndividualizedVersion');
  {$EXTERNALSYM g_wszWMDRM_IndividualizedVersion}
  g_wszWMDRM_LicenseAcqURL         = WideString('DRM_LicenseAcqURL');
  {$EXTERNALSYM g_wszWMDRM_LicenseAcqURL}
  g_wszWMDRM_V1LicenseAcqURL       = WideString('DRM_V1LicenseAcqURL');
  {$EXTERNALSYM g_wszWMDRM_V1LicenseAcqURL}
  g_wszWMDRM_HeaderSignPrivKey     = WideString('DRM_HeaderSignPrivKey');
  {$EXTERNALSYM g_wszWMDRM_HeaderSignPrivKey}
  g_wszWMDRM_LASignaturePrivKey    = WideString('DRM_LASignaturePrivKey');
  {$EXTERNALSYM g_wszWMDRM_LASignaturePrivKey}
  g_wszWMDRM_LASignatureCert       = WideString('DRM_LASignatureCert');
  {$EXTERNALSYM g_wszWMDRM_LASignatureCert}
  g_wszWMDRM_LASignatureLicSrvCert = WideString('DRM_LASignatureLicSrvCert');
  {$EXTERNALSYM g_wszWMDRM_LASignatureLicSrvCert}
  g_wszWMDRM_LASignatureRootCert   = WideString('DRM_LASignatureRootCert');
  {$EXTERNALSYM g_wszWMDRM_LASignatureRootCert}

////////////////////////////////////////////////////////////////
//
// These are the additional attributes defined in the WM attribute
// namespace that give information about the content.
//

  g_wszWMAlbumTitle    = WideString('WM/AlbumTitle');
  {$EXTERNALSYM g_wszWMAlbumTitle}
  g_wszWMTrack         = WideString('WM/Track');
  {$EXTERNALSYM g_wszWMTrack}
  g_wszWMPromotionURL  = WideString('WM/PromotionURL');
  {$EXTERNALSYM g_wszWMPromotionURL}
  g_wszWMAlbumCoverURL = WideString('WM/AlbumCoverURL');
  {$EXTERNALSYM g_wszWMAlbumCoverURL}
  g_wszWMGenre         = WideString('WM/Genre');
  {$EXTERNALSYM g_wszWMGenre}
  g_wszWMYear          = WideString('WM/Year');
  {$EXTERNALSYM g_wszWMYear}
  g_wszWMGenreID       = WideString('WM/GenreID');
  {$EXTERNALSYM g_wszWMGenreID}
  g_wszWMMCDI          = WideString('WM/MCDI');
  {$EXTERNALSYM g_wszWMMCDI}
  g_wszWMComposer      = WideString('WM/Composer');
  {$EXTERNALSYM g_wszWMComposer}
  g_wszWMLyrics        = WideString('WM/Lyrics');
  {$EXTERNALSYM g_wszWMLyrics}
  g_wszWMTrackNumber   = WideString('WM/TrackNumber');
  {$EXTERNALSYM g_wszWMTrackNumber}
  g_wszWMToolName      = WideString('WM/ToolName');
  {$EXTERNALSYM g_wszWMToolName}
  g_wszWMToolVersion   = WideString('WM/ToolVersion');
  {$EXTERNALSYM g_wszWMToolVersion}
  g_wszWMIsVBR         = WideString('IsVBR');
  {$EXTERNALSYM g_wszWMIsVBR}

//
// WM/AlbumArtist is a potentially different value than Author
//
  g_wszWMAlbumArtist = WideString('WM/AlbumArtist');
  {$EXTERNALSYM g_wszWMAlbumArtist}

////////////////////////////////////////////////////////////////
//
// These optional attributes may be used to give information
// about the branding of the content.
//

  g_wszWMBannerImageType = WideString('BannerImageType');
  {$EXTERNALSYM g_wszWMBannerImageType}
  g_wszWMBannerImageData = WideString('BannerImageData');
  {$EXTERNALSYM g_wszWMBannerImageData}
  g_wszWMBannerImageURL  = WideString('BannerImageURL');
  {$EXTERNALSYM g_wszWMBannerImageURL}
  g_wszWMCopyrightURL    = WideString('CopyrightURL');
  {$EXTERNALSYM g_wszWMCopyrightURL}

  g_wszComplexityMax     = WideString('_COMPLEXITYEXMAX');
  {$EXTERNALSYM g_wszComplexityMax}
  g_wszComplexityOffline = WideString('_COMPLEXITYEXOFFLINE');
  {$EXTERNALSYM g_wszComplexityOffline}
  g_wszComplexityLive    = WideString('_COMPLEXITYEXLIVE');
  {$EXTERNALSYM g_wszComplexityLive}
  g_wszIsVBRSupported    = WideString('_ISVBRSUPPORTED');
  {$EXTERNALSYM g_wszIsVBRSupported}
  g_wszVBREnabled         = WideString('_VBRENABLED');
  {$EXTERNALSYM g_wszVBREnabled}
  g_wszNumPasses = WideString('_PASSESUSED');
  {$EXTERNALSYM g_wszNumPasses}

    WMT_VER_4_0 = $00040000;
    {$EXTERNALSYM WMT_VER_4_0}
    WMT_VER_7_0 = $00070000;
    {$EXTERNALSYM WMT_VER_7_0}
    WMT_VER_8_0 = $00080000;
    {$EXTERNALSYM WMT_VER_8_0}
    WMT_VER_9_0 = $00090000;
    {$EXTERNALSYM WMT_VER_9_0}


  NS_E_PROTECTED_CONTENT           = HRESULT($C00D0BBD);

type

  WMT_VERSION = {$IFDEF TYPE_IDENTITY}type {$ENDIF} LongWord;

  PWMTStreamSelection = ^TWMTStreamSelection;
  WMT_STREAM_SELECTION = (
    WMT_OFF,
    WMT_CLEANPOINT_ONLY,
    WMT_ON
  );
  {$EXTERNALSYM WMT_STREAM_SELECTION}
  TWMTStreamSelection = WMT_STREAM_SELECTION;

  WMT_ATTR_DATATYPE = (
    WMT_TYPE_DWORD,
    WMT_TYPE_STRING,
    WMT_TYPE_BINARY,
    WMT_TYPE_BOOL,
    WMT_TYPE_QWORD,
    WMT_TYPE_WORD,
    WMT_TYPE_GUID
  );
  {$EXTERNALSYM WMT_ATTR_DATATYPE}
  TWMTAttrDataType = WMT_ATTR_DATATYPE;

  PWMMediaType = ^TWMMediaType;
  _WMMediaType = packed record
    majortype            : TGUID;
    subtype              : TGUID;
    bFixedSizeSamples    : BOOL;
    bTemporalCompression : BOOL;
    lSampleSize          : ULONG;
    formattype           : TGUID;
    pUnk                 : IUnknown;
    cbFormat             : ULONG;
    pbFormat             : PBYTE; // size_is(cbFormat)
  end;
  {$EXTERNALSYM _WMMediaType}
  WM_MEDIA_TYPE = _WMMediaType;
  {$EXTERNALSYM WM_MEDIA_TYPE}
  TWMMediaType = _WMMediaType;

  PWMWriterStatistics = ^TWMWriterStatistics;
  _WMWriterStatistics = packed record
    qwSampleCount        : Int64;
    qwByteCount          : Int64;

    qwDroppedSampleCount : Int64;
    qwDroppedByteCount   : Int64;

    dwCurrentBitrate     : LongWord;
    dwAverageBitrate     : LongWord;
    dwExpectedBitrate    : LongWord;

    //
    // Sample rates are given as 1000 * (samples / second).
    //
    dwCurrentSampleRate  : LongWord;
    dwAverageSampleRate  : LongWord;
    dwExpectedSampleRate : LongWord;
  end;
  {$EXTERNALSYM _WMWriterStatistics}
  WM_WRITER_STATISTICS = _WMWriterStatistics;
  {$EXTERNALSYM WM_WRITER_STATISTICS}
  TWMWriterStatistics = _WMWriterStatistics;

  INSSBuffer = interface(IUnknown)
  ['{E1CD3524-03D7-11d2-9EED-006097D2D7CF}']
  (*** INSSBuffer methods ***)
    function GetLength(out pdwLength: LongWord): HRESULT; stdcall;
    function SetLength(dwLength: LongWord): HRESULT; stdcall;
    function GetMaxLength(out pdwLength: LongWord): HRESULT; stdcall;
    function GetBuffer(out ppdwBuffer: PBYTE): HRESULT; stdcall;
    function GetBufferAndLength(out ppdwBuffer: PBYTE; out pdwLength: LongWord): HRESULT; stdcall;
  end;

  IWMHeaderInfo = interface(IUnknown)
  ['{96406BDA-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMHeaderInfo methods ***)
    // For attributes, the stream number passed in means:
    // -1 (0xffff) to specifies "any or no stream".
    // 0 specifies "no stream".
    // Any other value indicates the stream number.
    //
    // Windows Media version 4 and earlier does not support per stream
    // attributes, so any stream number other than 0 will fail.
    //
    function GetAttributeCount(wStreamNum: Word; out pcAttributes: Word): HRESULT; stdcall;

    function GetAttributeByIndex(wIndex: Word; var pwStreamNum: Word;
      {out} pwszName: PWideChar; var pcchNameLen: Word;
      out pType: TWMTAttrDataType; {out} pValue: PBYTE;
      var pcbLength: Word): HRESULT; stdcall;

    function GetAttributeByName(var pwStreamNum: Word; pszName: PWideChar;
      out pType: TWMTAttrDataType; {out} pValue: PBYTE;
      var pcbLength: Word): HRESULT; stdcall;

    function SetAttribute(wStreamNum: Word; pszName: PWideChar;
      Type_: TWMTAttrDataType; {in} pValue: PBYTE;
      cbLength: Word): HRESULT; stdcall;

    // Marker methods.
    function GetMarkerCount(out pcMarkers: Word): HRESULT; stdcall;

    function GetMarker(wIndex: Word; {out} pwszMarkerName: PWideChar;
      var pcchMarkerNameLen: Word; out pcnsMarkerTime: Int64): HRESULT; stdcall;

    function AddMarker(pwszMarkerName: PWideChar; cnsMarkerTime: Int64): HRESULT; stdcall;

    function RemoveMarker(wIndex: Word): HRESULT; stdcall;

    // Script command methods.
    function GetScriptCount(out pcScripts: Word): HRESULT; stdcall;

    function GetScript(wIndex: Word; {out} pwszType: PWideChar;
      var pcchTypeLen: Word; {out} pwszCommand: PWideChar;
      var pcchCommandLen: Word; out pcnsScriptTime: Int64): HRESULT; stdcall;

    function AddScript(pwszType, pwszCommand: PWideChar;
      cnsScriptTime: Int64): HRESULT; stdcall;

    function RemoveScript(wIndex: Word): HRESULT; stdcall;
  end;


  IWMMediaProps = interface(IUnknown)
  ['{96406BCE-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMMediaProps methods ***)
    //
    // GetType is provided for convenience; it returns the same as the
    // majortype of the WM_MEDIA_TYPE.
    //
    function GetType(out pguidType: TGUID): HRESULT; stdcall;
    function GetMediaType(pType: PWMMediaType;
                          var pcbType: LongWord): HRESULT; stdcall;
    function SetMediaType(pType: PWMMediaType): HRESULT; stdcall;
  end;

  IWMOutputMediaProps = interface(IWMMediaProps)
  ['{96406BD7-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMOutputMediaProps methods ***)
    //
    // A Stream Group and type together uniquely identify each output. (The
    // type is on IWMMediaProps).
    //
    function GetStreamGroupName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
    function GetConnectionName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
  end;

 IWMSyncReader = interface(IUnknown)
  ['{9397F121-7705-4dc9-B049-98B698188414}']
  (*** IWMSyncReader methods ***)
    //
    // This interface can be QI'ed for IWMProfile, IWMHeaderInfo and
    // IWMReaderTimecode interfaces
    //

    //
    // Open is an synchronous call. We do not support streaming at this time.
    // All open with http:// etc would fail. We do support MP3 or Windows
    // media files from both local file or UNC path.
    //
    function Open(pwszFilename: PWideChar): HRESULT; stdcall;

    function Close: HRESULT; stdcall;

    //
    // SetRange is basically a seek method, you can only set same range
    // for all streams. Use cnsDuration=0 to specify reading to end of file.
    //
    function SetRange(cnsStartTime, cnsDuration: Int64): HRESULT; stdcall;

    //
    // SetRangeByFrame is frame-based access, the file has to be frame
    // indexed to succeeded this call. If the call is successful, all
    // streams are synchronized to the same position based on time.
    // Also use cFramesToRead=0 to specify reading to end of file.
    //
    function SetRangeByFrame(wStreamNum: Word; qwFrameNumber, cFramesToRead: Int64 ): HRESULT; stdcall;

    //
    // If a valid stream number is specified, next sample from that stream
    // will be returned, pwStreamNum can be NULL in this case. Otherwise,
    // GetNextSample returns the next sample in time line (regardless of
    // which stream). The sample's stream number will be returned.
    // Time line is presentation time if no output setting is specified.
    // To get early delivery for some stream, use SetOutputSetting
    //
    function GetNextSample(wStreamNum: Word; out ppSample: INSSBuffer;
      out pcnsSampleTime: Int64; out pcnsDuration: Int64;
      out pdwFlags: LongWord; out pdwOutputNum: LongWord;
      out pwStreamNum: Word): HRESULT; stdcall;

    //
    // Stream selection methods are the same as asynchronous interface
    //
    function SetStreamsSelected(cStreamCount: Word; pwStreamNumbers: PWORD;
      pSelections: PWMTStreamSelection): HRESULT; stdcall;

    function GetStreamSelected(wStreamNum: Word; out pSelection: TWMTStreamSelection): HRESULT; stdcall;

    function SetReadStreamSamples(wStreamNum: Word; fCompressed: BOOL): HRESULT; stdcall;

    function GetReadStreamSamples(wStreamNum: Word; out pfCompressed: BOOL): HRESULT; stdcall;

    //
    // The following two methods are the same as the ones in
    // IWMReaderAdvanced2 interface. We don't support JustInTimeDecode
    // in this interface
    //
    function GetOutputSetting(dwOutputNum: LongWord; pszName: PWideChar;
      out pType: TWMTAttrDataType; {out} pValue: PBYTE;
      var pcbLength: Word): HRESULT; stdcall;

    //
    // Sets a named setting for a particular output
    //
    function SetOutputSetting(dwOutputNum: LongWord; pszName: PWideChar;
      Type_: TWMTAttrDataType; {in} pValue: PBYTE; cbLength: Word): HRESULT; stdcall;

    //
    // The following methods are important for receiving uncompressed samples,
    // they are identical to methods in asynchronous Reader interface.
    //
    function GetOutputCount(out pcOutputs: LongWord): HRESULT; stdcall;

    function GetOutputProps(dwOutputNum: LongWord; out ppOutput: IWMOutputMediaProps): HRESULT; stdcall;

    function SetOutputProps(dwOutputNum: LongWord; pOutput: IWMOutputMediaProps): HRESULT; stdcall;

    //
    // Used for determining all possible format types supported by this
    // output on the reader.
    //
    function GetOutputFormatCount(dwOutputNum: LongWord; out pcFormats: LongWord): HRESULT; stdcall;

    function GetOutputFormat(dwOutputNum, dwFormatNum: LongWord; out ppProps: IWMOutputMediaProps): HRESULT; stdcall;

    //
    // Methods provided to relate output numbers with stream numbers
    //
    function GetOutputNumberForStream(wStreamNum: Word; out pdwOutputNum: LongWord): HRESULT; stdcall;

    function GetStreamNumberForOutput(dwOutputNum: LongWord; out pwStreamNum: Word): HRESULT; stdcall;

    function GetMaxOutputSampleSize(dwOutput: LongWord; out pcbMax: LongWord): HRESULT; stdcall;

    function GetMaxStreamSampleSize(wStream: Word; out pcbMax: LongWord): HRESULT; stdcall;

    //
    // Same as IWMSyncReader::Open but takes an IStream interface pointer
    // instead of an file name to be opened. This method is typically
    // used for custom source.
    //
    function OpenStream(pStream: IStream): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterSink> _di_IWMWriterSink;'}
  {$EXTERNALSYM IWMWriterSink}
  IWMWriterSink = interface(IUnknown)
    ['{96406BE4-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMWriterSink methods ***)
    function OnHeader(pHeader: INSSBuffer): HResult; stdcall;
    // Some sinks require that data be fed to them in real-time.
    function IsRealTime(out pfRealTime: BOOL): HResult; stdcall;
    function AllocateDataUnit(cbDataUnit: LongWord; out ppDataUnit: INSSBuffer): HResult; stdcall;
    function OnDataUnit(pDataUnit: INSSBuffer): HResult; stdcall;
    // This function is called when the writer is done sending data.
    function OnEndWriting: HResult; stdcall;
  end;


  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterFileSink> _di_IWMWriterFileSink;'}
  {$EXTERNALSYM IWMWriterFileSink}
    IWMWriterFileSink = interface(IWMWriterSink)
    ['{96406BE5-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMWriterFileSink methods ***)
    function Open(pwszFilename: PWideChar): HResult; stdcall;
  end;

   {$HPPEMIT 'typedef System::DelphiInterface<IWMWriter> _di_IWMWriter;'}
  {$EXTERNALSYM IWMWriter}


  IWMStreamConfig = interface(IUnknown)
  ['{96406BDC-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMStreamConfig methods ***)
    // This interface QI's for IWMMediaProps and one of it's inheritors.
    // (IWMVideoMediaProps, for instance).
    function GetStreamType(out pguidStreamType: TGUID): HRESULT; stdcall;
    function GetStreamNumber(out pwStreamNum: Word): HRESULT; stdcall;
    function SetStreamNumber(wStreamNum: Word): HRESULT; stdcall;
    function GetStreamName({out} pwszStreamName: PWideChar; var pcchStreamName: Word): HRESULT; stdcall;
    function SetStreamName(pwszStreamName: PWideChar): HRESULT; stdcall;
    function GetConnectionName({out} pwszInputName: PWideChar; var pcchInputName: Word): HRESULT; stdcall;
    function SetConnectionName(pwszInputName: PWideChar): HRESULT; stdcall;
    function GetBitrate(out pdwBitrate: LongWord): HRESULT; stdcall;
    function SetBitrate(pdwBitrate: LongWord): HRESULT; stdcall;

    //
    // A buffer window of -1 (0xffffffff) indicates that the buffer window
    // is unknown. On the writer side, this means the writer can use whatever
    // buffer window it chooses.
    //
    function GetBufferWindow(out pmsBufferWindow: LongWord): HRESULT; stdcall;
    function SetBufferWindow(msBufferWindow: LongWord): HRESULT; stdcall;
  end;

  IWMStreamList = interface(IUnknown)
  ['{96406BDD-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMStreamList methods ***)
    function GetStreams({out} pwStreamNumArray: PWORD; var pcStreams: PWORD): HRESULT; stdcall;
    function AddStream(wStreamNum: Word): HRESULT; stdcall;
    function RemoveStream(wStreamNum: Word): HRESULT; stdcall;
  end;

    {$HPPEMIT 'typedef System::DelphiInterface<IWMCodecInfo> _di_IWMCodecInfo;'}
  {$EXTERNALSYM IWMCodecInfo}
  IWMCodecInfo = interface(IUnknown)
    ['{A970F41E-34DE-4A98-B3BA-E4B3CA7528F0}']
    (*** IWMCodecInfo methods ***)
    function GetCodecInfoCount(const guidType: TGUID; out pcCodecs: LongWord): HResult; stdcall;
    function GetCodecFormatCount(const guidType: TGUID; dwCodecIndex: LongWord; out pcFormat: LongWord): HResult; stdcall;
    function GetCodecFormat(const guidType: TGUID; dwCodecIndex: LongWord; dwFormatIndex: LongWord;
                            out ppIStreamConfig: IWMStreamConfig): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMCodecInfo2> _di_IWMCodecInfo2;'}
  {$EXTERNALSYM IWMCodecInfo2}
  IWMCodecInfo2 = interface(IWMCodecInfo)
    ['{AA65E273-B686-4056-91EC-DD768D4DF710}']
    (*** IWMCodecInfo2 methods ***)
    function GetCodecName(const guidType: TGUID; dwCodecIndex: LongWord; {out} wszName: PWideChar;
                          var pcchName: LongWord): HResult; stdcall;
    function GetCodecFormatDesc(const guidType: TGUID; dwCodecIndex: LongWord;
                                dwFormatIndex: LongWord; out ppIStreamConfig: IWMStreamConfig;
                                {out} wszDesc: PWideChar; var pcchDesc: LongWord): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMCodecInfo3> _di_IWMCodecInfo3;'}
  {$EXTERNALSYM IWMCodecInfo3}
  IWMCodecInfo3 = interface(IWMCodecInfo2)
    ['{7E51F487-4D93-4F98-8AB4-27D0565ADC51}']
    (*** IWMCodecInfo3 methods ***)
    function GetCodecFormatProp(const guidType: TGUID; dwCodecIndex: LongWord;
                                dwFormatIndex: LongWord; pszName: PWideChar; 
                                out pType: TWMTAttrDataType; {out} pValue: PByte;
                                var pdwSize: LongWord): HResult; stdcall;
    function GetCodecProp(const guidType: TGUID; dwCodecIndex: LongWord; pszName: PWideChar;
                          out pType: TWMTAttrDataType; {out} pValue: PByte; var pdwSize: LongWord): HResult; stdcall;
    function SetCodecEnumerationSetting(const guidType: TGUID; dwCodecIndex: LongWord;
                                        pszName: PWideChar; Type_: TWMTAttrDataType;
                                        {in} pValue: PByte; dwSize: LongWord): HResult; stdcall;
    function GetCodecEnumerationSetting(const guidType: TGUID; dwCodecIndex: LongWord;
                                        pszName: PWideChar; out pType: TWMTAttrDataType;
                                        {out} pValue: PByte; var pdwSize: LongWord): HResult; stdcall;
  end;


  IWMMutualExclusion = interface(IWMStreamList)
  ['{96406BDE-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMMutualExclusion methods ***)
    // The possible types of mutual exclusion are defined in the ASF
    // header.
    function GetType(out pguidType: TGUID): HRESULT; stdcall;
    function SetType(guidType: TGUID): HRESULT; stdcall;
  end;


  {$EXTERNALSYM IWMProfile}
  IWMProfile = interface(IUnknown)
  ['{96406BDB-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMProfile methods ***)
    // By default, when the user creates a profile, it will use the latest
    // version of Windows Media. To create a backward-compatible profile,
    // call IWMProfileManager::CreateEmptyProfile with the appropriate version
    // number.
    function GetVersion(out pdwVersion: WMT_VERSION): HRESULT; stdcall;

    // Profiles have names and descriptions, for use when displaying lists
    // of profiles, etc.
    function GetName({out} pwszName: PWideChar; var pcchName: LongWord): HRESULT; stdcall;
    function SetName(pwszName: PWideChar): HRESULT; stdcall;

    function GetDescription({out} pwszDescription: PWideChar;
      var pcchDescription: LongWord): HRESULT; stdcall;

    function SetDescription(pwszDescription: PWideChar): HRESULT; stdcall;

    // Methods for enumerating the streams. Note that updating the
    // returned IWMStreamConfig has no effect on the profile until you
    // call ReconfigStream().
    function GetStreamCount(out pcStreams: LongWord): HRESULT; stdcall;

    function GetStream(dwStreamIndex: LongWord; out ppConfig: IWMStreamConfig): HRESULT; stdcall;

    function GetStreamByNumber(wStreamNum: Word; out ppConfig: IWMStreamConfig): HRESULT; stdcall;

    // Remove a stream.
    function RemoveStream(pConfig: IWMStreamConfig): HRESULT; stdcall;

    function RemoveStreamByNumber(wStreamNum: Word): HRESULT; stdcall;

    // Adding a stream copies the config into the profile.
    function AddStream(pConfig: IWMStreamConfig): HRESULT; stdcall;

    function ReconfigStream(pConfig: IWMStreamConfig): HRESULT; stdcall;

    // Create a new stream config object (avoiding the need to CoCreate).
    // This will still need to be added to the profile using the AddStream()
    // call (but only after it has been configured).
    function CreateNewStream(const guidStreamType: TGUID;
      out ppConfig: IWMStreamConfig): HRESULT; stdcall;

    // Mutual Exclusion. As above, only Add and Remove actual change the
    // profile.
    function GetMutualExclusionCount(out pcME: LongWord): HRESULT; stdcall;

    function GetMutualExclusion(dwMEIndex: LongWord; out ppME: IWMMutualExclusion): HRESULT; stdcall;

    function RemoveMutualExclusion(pME: IWMMutualExclusion): HRESULT; stdcall;

    function AddMutualExclusion(pME: IWMMutualExclusion): HRESULT; stdcall;

    function CreateNewMutualExclusion(out ppME: IWMMutualExclusion): HRESULT; stdcall;
  end;

   {$EXTERNALSYM IWMProfileManager}
  IWMProfileManager = interface(IUnknown)
  ['{d16679f2-6ca0-472d-8d31-2f5d55aee155}']
  (*** IWMProfileManager methods ***)
    // Create a profile with nothing in it.
    function CreateEmptyProfile(dwVersion: WMT_VERSION; out ppProfile: IWMProfile): HRESULT; stdcall;

    // Load a system profile given its ID.
    function LoadProfileByID(const guidProfile: TGUID; out ppProfile: IWMProfile): HRESULT; stdcall;

    // Load a profile from a stored string.
    function LoadProfileByData(pwszProfile: PWideChar; out ppProfile: IWMProfile): HRESULT; stdcall;

    // Save profile specified by its string.
    function SaveProfile(pIWMProfile: IWMProfile; pwszProfile: PWideChar;
      var pdwLength: LongWord): HRESULT; stdcall;

    // Iterate through the system profiles.
    function GetSystemProfileCount(out pcProfiles: LongWord): HRESULT; stdcall;

    function LoadSystemProfile(dwProfileIndex: LongWord; out ppProfile: IWMProfile): HRESULT; stdcall;
  end;

  {$EXTERNALSYM IWMInputMediaProps}
  IWMInputMediaProps = interface(IWMMediaProps)
  ['{96406BD5-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMInputMediaProps methods ***)
    function GetConnectionName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
    function GetGroupName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
  end;


  IWMWriter = interface(IUnknown)
  ['{96406BD4-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMWriter methods ***)
    // This interface QI's for IWMHeaderInfo, and IWMWriterAdvanced.

    //
    // There are 3 options for setting the profile on the writer. Note that
    // setting the profile means that the profile is copied into the writer.
    // Further editing of that profile object will have no effect, unless
    // SetProfile() is called again.
    //
    // Calling SetProfile() removes any previously set header attribute info
    //
    function SetProfileByID(const guidProfile: TGUID): HRESULT; stdcall;

    function SetProfile(pProfile: IWMProfile): HRESULT; stdcall;

    //
    // The easiest way to use the writer is just to write to file.
    //
    function SetOutputFilename(pwszFilename: PWideChar): HRESULT; stdcall;

    //
    // The user can enumerate through the various inputs, and get the input
    // format. Note that these are not ASF streams; one input stream may map
    // to multiple ASF streams in a MEB scenario.
    //
    // Manipulating the IWMInputMediaProps has no effect on the writer, unless
    // the user calls SetInputProps to configure the input.
    //
    function GetInputCount(out pcInputs: LongWord): HRESULT; stdcall;

    function GetInputProps(dwInputNum: LongWord; out ppInput: IWMInputMediaProps): HRESULT; stdcall;

    function SetInputProps(dwInputNum: LongWord; pInput: IWMInputMediaProps): HRESULT; stdcall;

    //
    // Used for determining all possible format types supported by this
    // input on the writer.
    //
    function GetInputFormatCount(dwInputNumber: LongWord; out pcFormats: LongWord): HRESULT; stdcall;

    function GetInputFormat(dwInputNumber, dwFormatNumber: LongWord;
       out pProps: IWMInputMediaProps): HRESULT; stdcall;

    //
    // You must call BeginWriting before sending any samples, and
    // you must call EndWriting when you're done sending samples.
    //
    function BeginWriting: HRESULT; stdcall;

    //
    // EndWriting flushes everything, updates indices and headers,
    // and closes the file.
    //
    function EndWriting: HRESULT; stdcall;

    //
    // Allocate a sample. This is optional; the user is welcome to allocate
    // their own buffer class.
    //
    function AllocateSample(dwSampleSize: LongWord; out ppSample: INSSBuffer): HRESULT; stdcall;

    function WriteSample(dwInputNum: LongWord; cnsSampleTime: Int64; dwFlags: LongWord;
      pSample: INSSBuffer): HRESULT; stdcall;

    //
    // Flush() will flush the writer, but leaves the writer prepared to run
    // again, when WriteSample() is called again.
    // Flush() also causes an updated header to be sent to the sink.
    //
    function Flush: HRESULT; stdcall;
  end;

  // The writer can be QI'd for this interface, which provides advanced writing
  // functionality.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterAdvanced> _di_IWMWriterAdvanced;'}
  {$EXTERNALSYM IWMWriterAdvanced}
  IWMWriterAdvanced = interface(IUnknown)
    ['{96406BE3-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMWriterAdvanced methods ***)
    // Sinks are where the output ASF data goes.
    function GetSinkCount(out pcSinks: LongWord): HResult; stdcall;
    function GetSink(dwSinkNum: LongWord; out ppSink: IWMWriterSink): HResult; stdcall;
    function AddSink(pSink: IWMWriterSink): HResult; stdcall;
    function RemoveSink(pSink: IWMWriterSink): HResult; stdcall;
    // By default, the user provides samples to an input on the
    // IWMWriter interface, and the samples may be compressed, put
    // into a MEB stream, etc. However, the user can use this interface to
    // put the samples directly into the ASF, with no compression etc.
    function WriteStreamSample(wStreamNum: Word; cnsSampleTime: Int64;
                               msSampleSendTime: LongWord; cnsSampleDuration: Int64;
                               dwFlags: LongWord; pSample: INSSBuffer): HResult; stdcall;
    // The writer may be running in real-time. If so, it's interesting to
    // get the current time from the writer.
    function SetLiveSource(fIsLiveSource: BOOL): HResult; stdcall;
    function IsRealTime(out pfRealTime: BOOL): HResult; stdcall;
    function GetWriterTime(out pcnsCurrentTime: Int64): HResult; stdcall;
    // To get statistics, pass in a WM_WRITER_STATISTICS structure, which
    // will be filled out by the GetStatistics() call with the requested
    // stats.
    //
    // Pass in a stream number to get statistics for a specific stream, or
    // pass 0 to get statistics for the entire ASF file.
    function GetStatistics(wStreamNum: Word; out pStats: TWMWriterStatistics): HResult; stdcall;
    // Sync tolerance determines how far out of sync the inputs will be allowed
    // to get before samples are thrown away.  Default is 3000 ms.
    function SetSyncTolerance(msWindow: LongWord): HResult; stdcall;
    function GetSyncTolerance(out pmsWindow: LongWord): HResult; stdcall;
  end;

  function WMCreateSyncReader(pUnkCert: IUnknown; dwRights: LongWord; out ppSyncReader: IWMSyncReader): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateSyncReader}
  function WMCreateWriter(pUnkCert: IUnknown; out ppWriter: IWMWriter): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateWriter}
  function WMCreateWriterFileSink(out ppSink: IWMWriterFileSink ): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateWriterFileSink}
  function WMCreateProfileManager(out ppProfileManager: IWMProfileManager): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateProfileManager}



implementation

const
  WMVCORE = 'WMVCORE.DLL';

  function WMCreateSyncReader; external WMVCORE name 'WMCreateSyncReader';
  function WMCreateWriter; external WMVCORE name 'WMCreateWriter';
  function WMCreateWriterFileSink; external WMVCORE name 'WMCreateWriterFileSink';
  function WMCreateProfileManager; external WMVCORE name 'WMCreateProfileManager';

end.
 