(*
  This file is a part of New Audio Components package 1.7
  Copyright (c) 2002-2008 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit msdmo;

interface

uses
  Windows, Classes, SysUtils, ACS_Classes, ACS_Procs, ActiveX, MMSystem, SyncObjs, ComObj, _DXTypes, _Direct3D9, _DirectShow9, wmfintf;

const

 CLSID_CWMAEncMediaObject : TGUID = '{70f598e9-f4ab-495a-99e2-a7c4d3d89abf}';
 CLSID_CWMADecMediaObject : TGUID = '{2eeb4adf-4578-4d10-bca7-bb955f56320a}';
 CLSID_CResamplerMediaObject : TGUID = '{f447b69e-1884-4a7e-8055-346f74d6edb3}';
 CLSID_CWMAudioAEC : TGUID = '{745057c7-f353-4f2d-a7ee-58434477730e}';
 IID_IPropertyStore : TGUID = '{886d8eeb-8cf2-4446-8d02-cdba1dbdcf99}';

 PID_FIRST_USABLE = $02;

type

 PROPERTYKEY = record
   fmtid : TGUID;
   pid : DWORD;
 end;

 PPROPERTYKEY = ^PROPERTYKEY;
 REFPROPERTYKEY = PROPERTYKEY;
 REFPROPVARIANT = TPropVariant;

const
  MFPKEY_WMAAECMA_SYSTEM_MODE : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 0);
  MFPKEY_WMAAECMA_DMO_SOURCE_MODE : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 1);
  MFPKEY_WMAAECMA_DEVICE_INDEXES : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 2);
  MFPKEY_WMAAECMA_FEATURE_MODE   : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 3);
  MFPKEY_WMAAECMA_FEATR_FRAME_SIZE : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 4);
  MFPKEY_WMAAECMA_FEATR_ECHO_LENGTH : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 5);
  MFPKEY_WMAAECMA_FEATR_NS : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 6);
  MFPKEY_WMAAECMA_FEATR_AGC : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 7);
  MFPKEY_WMAAECMA_FEATR_AES : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 8);
  MFPKEY_WMAAECMA_FEATR_VAD : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 9);
  MFPKEY_WMAAECMA_FEATR_CENTER_CLIP  : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 10);
  MFPKEY_WMAAECMA_FEATR_NOISE_FILL : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 11);
  MFPKEY_WMAAECMA_RETRIEVE_TS_STATS  : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 12);
  MFPKEY_WMAAECMA_QUALITY_METRICS  : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 13);
  MFPKEY_WMAAECMA_MICARRAY_DESCPTR : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 14);
  MFPKEY_WMAAECMA_DEVICEPAIR_GUID : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 15);
  MFPKEY_WMAAECMA_FEATR_MICARR_MODE : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 16);
  MFPKEY_WMAAECMA_FEATR_MICARR_BEAM : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 17);
  MFPKEY_WMAAECMA_FEATR_MICARR_PREPROC : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 18);
  MFPKEY_WMAAECMA_MIC_GAIN_BOUNDER : PROPERTYKEY = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 19);


type

 IPropertyStore = interface
 ['{886d8eeb-8cf2-4446-8d02-cdba1dbdcf99}']
   function GetCount(var cProps : DWORD) : HRESULT; stdcall;
   function GetAt(iProp : DWORD; var pkey : PROPERTYKEY) : HRESULT; stdcall;
   function GetValue(key : REFPROPERTYKEY; var pv : TPROPVARIANT) : HRESULT; stdcall;
   function SetValue(key : REFPROPERTYKEY; propvar : REFPROPVARIANT) : HRESULT; stdcall;
   function Commit : HRESULT; stdcall;
 end;

 dmo_resampler_spec = record
    sample_rate : LongWord;
    channels : Word;
    bps : Word;
  end;

  dmo_resampler = record
    resampler : IMediaObject;
    input_spec : dmo_resampler_spec;
    output_sr : LongWord;
    InputBuffer, OutputBuffer : IMediaBuffer;
  end;

  procedure dmo_resampler_init(var resampler : dmo_resampler);
  procedure dmo_resampler_set_formats(var resampler : dmo_resampler);
  function dmo_resampler_accepts_data(var resampler : dmo_resampler) : Boolean;
  procedure dmo_resampler_prepare_input(var resampler : dmo_resampler; var Buffer : Pointer; var BufSize : LongWord);
  procedure dmo_resampler_reset_input(var resampler : dmo_resampler; var BufSize : LongWord);
  procedure dmo_resampler_write_input(var resampler : dmo_resampler);
  function dmo_resampler_get_output(var resampler : dmo_resampler; var Buffer : Pointer; var BufSize : LongWord) : Boolean;
  procedure dmo_resampler_free_output(var resampler : dmo_resampler);
  procedure dmo_resampler_free(var resampler : dmo_resampler);

implementation

type
 TMediaBuffer = class(TInterfacedObject, IMediaBuffer)
 private
   m_cbLength : DWORD;
   m_cbMaxLength : DWORD;
   m_pbData : PByte;
 public
   constructor Create(cbMaxLength : DWORD);
   destructor Destroy; override;
   function SetLength(cbLength: DWORD): HResult; stdcall;
   function GetMaxLength(out pcbMaxLength: DWORD): HResult; stdcall;
   function GetBufferAndLength(out ppBuffer: PByte; // not filled if NULL
                                out pcbLength: DWORD    // not filled if NULL
                                ): HResult; stdcall;
 end;


  procedure dmo_resampler_init(var resampler : dmo_resampler);
  var
    res : HResult;
  begin
    FillChar(resampler, SizeOf(resampler), 0);
    CoInitialize(nil);
    res := CoCreateInstance(CLSID_CResamplerMediaObject, nil, CLSCTX_INPROC_SERVER, IID_IMediaObject, resampler.resampler);
    if res <> S_OK then
      raise EAuException.Create('Failed to initialize resampler: ' + IntToHex(res, 8));
    resampler.resampler.AllocateStreamingResources; //?
  end;

  procedure dmo_resampler_set_formats(var resampler : dmo_resampler);
  var
    res : HResult;
    i : LongWord;
    MT : TAMMediaType;
    pWave  : PWAVEFORMATEX;
    Wave : TWAVEFORMATEX;
  begin
    i := 0;

    res := resampler.resampler.GetInputType(0, i, MT);
    while res <> DMO_E_NO_MORE_ITEMS do
    begin
      if GUIDSEqual(MT.subtype, MEDIASUBTYPE_PCM) then
      //if GUIDSEqual(MT.formattype, WMFORMAT_WaveFormatEx) then
      begin
        MT.formattype := WMFORMAT_WaveFormatEx;
        MT.cbFormat := SizeOf(Wave);
        MT.pbFormat := @Wave;
        Wave.wFormatTag := 1;
        Wave.nChannels := resampler.input_spec.channels;
        Wave.nSamplesPerSec := resampler.input_spec.sample_rate;
        Wave.wBitsPerSample := resampler.input_spec.bps;
        Wave.nBlockAlign := Wave.wBitsPerSample*Wave.nChannels div 8;
        Wave.nAvgBytesPerSec := Wave.nSamplesPerSec * Wave.nBlockAlign;
        Wave.cbSize := 0;
        res := resampler.resampler.SetInputType(0, @MT, 0);
       if res <> S_OK then
         raise EAuException.Create('Failed to set up input: ' + IntToHex(res, 8));
       Break;
      end;
      MoFreeMediaType(@MT);
      Inc(i);
      res := resampler.resampler.GetInputType(0, i, MT);
    end;
    FillChar(MT, SizeOf(MT), 0);
    resampler.resampler.GetInputCurrentType(0, MT);
    if GUIDSEqual(MT.subtype, MEDIASUBTYPE_PCM) then
    if GUIDSEqual(MT.formattype, WMFORMAT_WaveFormatEx) then
    begin
      pWave := PWAVEFORMATEX(MT.pbFormat);
      resampler.input_spec.sample_rate := pWave.nSamplesPerSec;
      resampler.input_spec.channels := pWave.nChannels;
      resampler.input_spec.bps := pWave.wBitsPerSample;
    end;
    MoFreeMediaType(@MT);

    i := 0;
    res := resampler.resampler.GetOutputType(0, i, MT);
    while res <> DMO_E_NO_MORE_ITEMS do
    begin
      if GUIDSEqual(MT.subtype, MEDIASUBTYPE_PCM) then
      begin
        MT.formattype := WMFORMAT_WaveFormatEx;
        MT.cbFormat := SizeOf(Wave);
        MT.pbFormat := @Wave;
        Wave.wFormatTag := 1;
        Wave.nChannels := resampler.input_spec.channels;
        Wave.nSamplesPerSec := resampler.output_sr;
        Wave.wBitsPerSample := resampler.input_spec.bps;
        Wave.nBlockAlign := Wave.wBitsPerSample*Wave.nChannels div 8;;
        Wave.nAvgBytesPerSec := Wave.nSamplesPerSec * Wave.nBlockAlign;
        Wave.cbSize := 0;
        res := resampler.resampler.SetOutputType(0, @MT, 0);
        if res <> S_OK then
          raise EAuException.Create('Failed to set up output: ' + IntToHex(res, 8));
       Break;
      end;
      MoFreeMediaType(@MT);
      Inc(i);
      res := resampler.resampler.GetOutputType(0, i, MT);
    end;

    FillChar(MT, SizeOf(MT), 0);
    resampler.resampler.GetOutputCurrentType(0, MT);
    if GUIDSEqual(MT.subtype, MEDIASUBTYPE_PCM) then
    if GUIDSEqual(MT.formattype, WMFORMAT_WaveFormatEx) then
    begin
       pWave := PWAVEFORMATEX(MT.pbFormat);
       resampler.output_sr := pWave.nSamplesPerSec;
    end;
    MoFreeMediaType(@MT);

  end;

  procedure dmo_resampler_prepare_input(var resampler : dmo_resampler; var Buffer : Pointer; var BufSize : LongWord);
  var
    MB : TMediaBuffer;
  begin
    MB := TMediaBuffer.Create(BufSize);
    MB.SetLength(BufSize);
    MB.GetBufferAndLength(PByte(Buffer), BufSize);
    resampler.InputBuffer := MB as IMediaBuffer;
  end;

  procedure dmo_resampler_reset_input(var resampler : dmo_resampler; var BufSize : LongWord);
  begin
    resampler.InputBuffer.SetLength(BufSize);
  end;

  function dmo_resampler_accepts_data(var resampler : dmo_resampler) : Boolean;
  var
    Flags : LongWord;
  begin
    resampler.resampler.GetInputStatus(0, Flags);
    Result := (Flags and DMO_INPUT_STATUSF_ACCEPT_DATA) <> 0;
  end;

  procedure dmo_resampler_write_input(var resampler : dmo_resampler);
  var
    res : HResult;
  begin
    res := resampler.resampler.ProcessInput(0, resampler.InputBuffer, 0, 0, 0);
    resampler.InputBuffer := nil;
    if res <> S_OK then
      raise EAuException.Create('Reading input failed: ' + IntToHex(res, 8));
  end;

  function dmo_resampler_get_output(var resampler : dmo_resampler; var Buffer : Pointer; var BufSize : LongWord) : Boolean;
  var
    res : HResult;
    Outp : LongWord;
    OutputDataBuffer : TDMOOutputDataBuffer;
    A : TDMOOutputDataBufferArray;
  begin
    resampler.OutputBuffer := TMediaBuffer.Create(1024*1024*3) as IMediaBuffer;
    OutputDataBuffer.pBuffer := resampler.OutputBuffer;
    OutputDataBuffer.rtTimestamp := 0;
    OutputDataBuffer.rtTimelength := 0;
    OutputDataBuffer.dwStatus := 0;
    A[0] := OutputDataBuffer;
    res := resampler.resampler.ProcessOutput(0, 1, A, Outp);
    OutputDataBuffer.pBuffer := nil;
    if res <> S_OK then
      raise EAuException.Create('Data processing failed: ' + IntToHex(res, 8));
    resampler.OutputBuffer.GetBufferAndLength(PByte(Buffer), BufSize);
    Result := (OutputDataBuffer.dwStatus and DMO_OUTPUT_DATA_BUFFERF_INCOMPLETE) = 0;
  end;

  procedure dmo_resampler_free_output(var resampler : dmo_resampler);
  begin
    resampler.OutputBuffer := nil;
  end;

  procedure dmo_resampler_free(var resampler : dmo_resampler);
  begin
    resampler.resampler.FreeStreamingResources;
    resampler.resampler := nil;
  end;

  constructor TMediaBuffer.Create(cbMaxLength: Cardinal);
  begin
    inherited Create;
    m_cbMaxLength := cbMaxLength;
    GetMem(m_pbData, cbMaxLength);
  end;

  destructor TMediaBuffer.Destroy;
  begin
    FreeMem(m_pbData);
    inherited Destroy;
  end;

  function TMediaBuffer.SetLength;
  begin
    if cbLength > m_cbMaxLength then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;
    m_cbLength := cbLength;
    Result := S_OK;
  end;

  function TMediaBuffer.GetMaxLength;
  begin
    pcbMaxLength := m_cbMaxLength;
    Result := S_OK;
  end;

  function TMediaBuffer.GetBufferAndLength;
  begin
    if @ppBuffer <> nil then
     ppBuffer := m_pbData;
    pcbLength := m_cbLength;
    Result := S_OK;
  end;

end.
