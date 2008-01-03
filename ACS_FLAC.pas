(*
  This file is a part of New Audio Components package 1.3
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Revision: 1.15 $ $Date: 2007/11/26 20:56:26 $ *)

unit ACS_FLAC;

interface

uses

 Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Tags, FLAC,
{$IFDEF LINUX}
  libc;
{$ENDIF}

{$IFDEF WIN32}
  Windows;
{$ENDIF}

const

  BUF_SIZE = $6000;

type

  TFLACOut = class(TAuFileOut)
  private
    Buffer : PBuffer8;
    FBufSize : Integer;
    _encoder : P_FLAC__StreamEncoder;
    FVerify : Boolean;
    FBlockSize : Word;
    FBestModelSearch : Boolean;
    FEnableMidSideStereo : Boolean;
    FMaxLPCOrder : Word;
    EndOfInput : Boolean;
    FEnableLooseMidSideStereo : Boolean;
    FQLPCoeffPrecision : Word;
    FQLPCoeffPrecisionSearch : Boolean;
    FMaxResidualPartitionOrder : Word;
    FMinResidualPartitionOrder : Word;
    FCompressionLevel : Integer;
    procedure SetEnableLooseMidSideStereo(val : Boolean);
    procedure SetBestModelSearch(val : Boolean);
    procedure SetCompressionLevel(val : Integer);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BestModelSearch : Boolean read FBestModelSearch write SetBestModelSearch;
    property BlockSize : Word read FBlockSize write FBlockSize;
    property CompressionLevel : Integer read FCompressionLevel write SetCompressionLevel;
    property EnableMidSideStereo : Boolean read FEnableMidSideStereo write FEnableMidSideStereo;
    property EnableLooseMidSideStereo : Boolean read FEnableLooseMidSideStereo write SetEnableLooseMidSideStereo;
    property MaxLPCOrder : Word read FMaxLPCOrder write FMaxLPCOrder;
    property MaxResidualPartitionOrder : Word read FMaxResidualPartitionOrder write FMaxResidualPartitionOrder;
    property MinResidualPartitionOrder : Word read FMinResidualPartitionOrder write FMinResidualPartitionOrder;
    property QLPCoeffPrecision : Word read FQLPCoeffPrecision write FQLPCoeffPrecision;
    property QLPCoeffPrecisionSearch : Boolean read FQLPCoeffPrecisionSearch write FQLPCoeffPrecisionSearch;
    property Verify : Boolean read FVerify write FVerify;
  end;

  TFLACIn = class(TAuFileIn)
  private
    FComments : TVorbisTags;
    Residue : Integer;
    Buff : PBuffer8;
    BuffSize : Integer;
    _decoder : P_FLAC__StreamDecoder;
    FBlockSize: Integer;
    BytesPerBlock : Integer;
    MinFrameSize : Integer;
    procedure ReadHeader;
    function GetComments : TVorbisTags;
    procedure ReadComments;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : Integer); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property VorbisComments : TVorbisTags read GetComments;
  end;


implementation

type

  FLACBuf = array[0..0] of FLAC__int32;
  PFLACBuf = ^FLACBuf;
  FLACUBuf = array[0..0] of FLAC__uint32;
  PFLACUBuf = ^FLACBuf;


  function EncWriteCBFunc(encoder : P_FLAC__StreamEncoder;
                                buffer : PFLAC__byte;
                                bytes, samples, current_frame : LongWord;
                                client_data : Pointer) : Integer; cdecl;
  var
    FLACOut : TFLACOut;
  begin
    FLACOut := TFLACOut(client_data);
    Result := FLAC__STREAM_ENCODER_WRITE_STATUS_OK;
    try
      FLACOut.FStream.Write(buffer^, bytes);
    except
      Result := FLAC__STREAM_ENCODER_WRITE_STATUS_FATAL_ERROR;
    end;
  end;

  function EncSeekCBFunc(encoder : P_FLAC__StreamEncoder;
                      absolute_byte_offset : FLAC__uint64;
                      client_data : Pointer) : Integer; cdecl;
  var
    FLACOut : TFLACOut;
  begin
    FLACOut := TFLACOut(client_data);
    Result := FLAC__STREAM_ENCODER_SEEK_STATUS_OK;
    try
      FLACOut.FStream.Seek(absolute_byte_offset, soFromBeginning);
    except
      Result := FLAC__STREAM_ENCODER_SEEK_STATUS_ERROR;
    end;
  end;

  function EncTellCBFunc(decoder : P_FLAC__StreamDecoder;
                         var absolute_byte_offset : FLAC__uint64;
                         client_data : Pointer) : Integer; cdecl;

  var
    FLACOut : TFLACOut;
  begin
    FLACOut := TFLACOut(client_data);
    absolute_byte_offset := FLACOut.Stream.Position;
    Result := FLAC__STREAM_ENCODER_TELL_STATUS_OK;
  end;


  procedure EncMetadataCBFunc(decoder : P_FLAC__StreamDecoder;
                                        metadata : Pointer;
                                        client_data : Pointer); cdecl;
  begin
    // Nothing to do here
  end;


  function DecReadCBFunc(decoder : P_FLAC__StreamDecoder;
                         buffer : PFLAC__byte;
                         var bytes : LongWord;
                         client_data : Pointer) : Integer; cdecl;
  var
    FLACIn : TFLACIn;
  begin
    FLACIn := TFLACIn(client_data);
    Result := FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
    if FLACIn.FStream.Position >= FLACIn.FStream.Size then
    begin
      Result := FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
      Exit;
    end;
    try
      bytes := FLACIn.FStream.Read(buffer^, bytes);
      if bytes = 0 then
        Result := FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
      except
      Result := FLAC__STREAM_DECODER_READ_STATUS_ABORT;
    end;
  end;

  function DecSeekCBFunc(decoder : P_FLAC__StreamDecoder;
                         absolute_byte_offset : FLAC__uint64;
                         client_data : Pointer) : Integer; cdecl;
  var
    FLACIn : TFLACIn;
  begin
    FLACIn := TFLACIn(client_data);
    if not FLACIn.FSeekable then
    begin
      Result := FLAC__STREAM_DECODER_SEEK_STATUS_UNSUPPORTED;
      Exit;
    end;
    Result := FLAC__STREAM_DECODER_SEEK_STATUS_OK;
    try
      FLACIn.FStream.Seek(absolute_byte_offset, soFromBeginning);
      if absolute_byte_offset > FlacIn.FSize then
        Result := FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
    except
      Result := FLAC__STREAM_DECODER_SEEK_STATUS_ERROR;
    end;
  end;

  function DecTellCBFunc(decoder : P_FLAC__StreamDecoder;
                         var absolute_byte_offset : FLAC__uint64;
                         client_data : Pointer) : Integer; cdecl;
  var
    FLACIn : TFLACIn;
  begin
    FLACIn := TFLACIn(client_data);
    if FLACIn.FSize = 0 then
    begin
      Result := FLAC__STREAM_DECODER_TELL_STATUS_UNSUPPORTED;
      Exit;
    end;
    Result := FLAC__STREAM_DECODER_TELL_STATUS_OK;
    try
      absolute_byte_offset := FLACIn.FStream.Position;
    except
      Result := FLAC__STREAM_DECODER_TELL_STATUS_ERROR;
    end;
  end;

  function DecLengthCBFunc(decoder : P_FLAC__StreamDecoder;
                           var stream_length : FLAC__uint64;
                           client_data : Pointer) : Integer; cdecl;
  var
    FLACIn : TFLACIn;
  begin
    FLACIn := TFLACIn(client_data);
    Result := FLAC__STREAM_DECODER_LENGTH_STATUS_OK;
    try
      stream_length := FLACIn.FStream.Size;
    except
      Result := FLAC__STREAM_DECODER_LENGTH_STATUS_ERROR;
    end;
  end;

  function DecEOFCBFunc(decoder : P_FLAC__StreamDecoder;
                        client_data : Pointer) : LongBool; cdecl;
  var
    FLACIn : TFLACIn;
  begin
    FLACIn := TFLACIn(client_data);
    if FLACIn.FStream.Position >= FLACIn.FStream.Size then Result := True
    else Result := False;
  end;

  function DecWriteCBFunc(decoder : P_FLAC__StreamDecoder;
                          frame : PFLAC__Frame;
                          buffer : PFLACInt32BufArray;
                          client_data : Pointer) : Integer; cdecl;
  var
    FLACIn : TFLACIn;
    Header : PFLAC__FrameHeader;
    buffer1 : PFLACInt32Buf;
    buffer2 : PFLACInt32Buf;
    B16 : PBuffer16;
    i : Integer;
  begin
    FLACIn := TFLACIn(client_data);
    Header := PFLAC__FrameHeader(frame);
    FLACIn.FBlockSize := Header.blocksize;
    FLACIn.BytesPerBlock := FLACIn.FBlockSize*(FLACIn.FBPS shr 3)*FLACIn.FChan;
    if FLACIn.BytesPerBlock > FLACIn.BuffSize then
    begin
      FreeMem(FLACIn.Buff, FLACIn.BuffSize);
      FLACIn.BuffSize := FLACIn.BytesPerBlock;
      GetMem(FLACIn.Buff, FLACIn.BuffSize);
    end;
//    FillChar(FLACIn.Buff[0], FLACIn.BytesPerBlock, 255);
    if FLACIn.FBPS = 16 then
    begin
      B16 := PBuffer16(FLACIn.Buff);
      if FLACIn.FChan = 1 then
      begin
       buffer1 := buffer[0];
       for i := 0 to FLACIn.FBlockSize-1 do B16[i] := buffer1[i]
      end else
      begin
        buffer1 := buffer[0];
        buffer2 := buffer[1];
        for i := 0 to FLACIn.FBlockSize-1 do
        begin
          B16[i shl 1] := buffer1[i];
          B16[(i shl 1)+1] := buffer2[i];
        end;
      end;
    end else
    begin
      if FLACIn.FBPS = 8 then
      begin
        if FLACIn.FChan = 1 then
        begin
          buffer1 := buffer[0];
          for i := 0 to FLACIn.FBlockSize-1 do FLACIn.Buff[i] := buffer1[i];
        end else
        begin
          buffer1 := buffer[0];
          buffer2 := buffer[1];
          for i := 0 to FLACIn.FBlockSize-1 do
          begin
            FLACIn.Buff[i shl 1] := buffer1[i];
            FLACIn.Buff[(i shl 1)+1] := buffer2[i];
          end;
        end;
      end else
      begin
        if FLACIn.FChan = 1 then
        begin
          buffer1 := buffer[0];
          for i := 0 to FLACIn.FBlockSize-1 do
          begin
            FLACIn.Buff[i*3] := (LongWord(buffer1[i]) and $000000FF);
            FLACIn.Buff[i*3 + 1] := (LongWord(buffer1[i]) and $0000FF00) div $100;
            FLACIn.Buff[i*3 + 2] := (LongWord(buffer1[i]) and $00FF0000) div $10000;
          end;
        end else
        begin
          buffer1 := buffer[0];
          buffer2 := buffer[1];
          for i := 0 to FLACIn.FBlockSize-1 do
          begin
            FLACIn.Buff[i*6] := (LongWord(buffer1[i]) and $000000FF);
            FLACIn.Buff[i*6 + 1] := (LongWord(buffer1[i]) and $0000FF00) div $100;
            FLACIn.Buff[i*6 + 2] := (LongWord(buffer1[i]) and $00FF0000) div $10000;
            FLACIn.Buff[i*6 + 3] := (LongWord(buffer2[i]) and $000000FF);
            FLACIn.Buff[i*6 + 4] := (LongWord(buffer2[i]) and $0000FF00) div $100;
            FLACIn.Buff[i*6 + 5] := (LongWord(buffer2[i]) and $00FF0000) div $10000;
          end;
        end;
      end;
    end;
    Result := FLAC__STREAM_ENCODER_OK;
  end;

  procedure DecMetadataCBProc(decoder : P_FLAC__StreamDecoder;
                              metadata : PFLAC__StreamMetadata;
                              client_data : Pointer); cdecl;
  var
    FLACIn : TFLACIn;
    FI : FLAC__StreamMetadata_StreamInfo;
  begin
    if metadata._type = FLAC__METADATA_TYPE_STREAMINFO then
    begin
      FI := metadata.stream_info;
      FLACIn := TFLACIn(client_data);
      FLACIn.FSR := FI.sample_rate;
      FLACIn.FChan := FI.channels;
      if FLACIn.FChan > 2 then FLACIn.FValid := False;
      FLACIn.FBPS := FI.bits_per_sample;
      FLACIn.FTotalSamples := FI.total_samples;
      FLACIn.FSize := FLACIn.FTotalSamples*(FLACIn.FBPS shr 3)*FLACIn.FChan;
      FLACIn.MinFrameSize := FI.min_framesize;
    end;
    if metadata._type = FLAC__METADATA_TYPE_VORBIS_COMMENT then
    begin
      raise exception.Create('ggg');
    end;
  end;

  procedure DecErrorCBProc(decoder : P_FLAC__StreamDecoder;
                           status : Integer;
                           client_data : Pointer); cdecl;
  var
    FLACIn : TFLACIn;
  begin
    FLACIn := TFLACIn(client_data);
    FLACIn.FValid := False;
  end;

  constructor TFLACOut.Create;
  begin
    inherited Create(AOwner);
    FVerify := False;
    FBlockSize := 4608;
    FBestModelSearch := False;
    FEnableMidSideStereo := True;
    FCompressionLevel := -1;
    if not (csDesigning	in ComponentState) then
    if not LibFLACLoaded then
    raise EAuException.Create(LibFLACPath + ' library could not be loaded.');
  end;

  destructor TFLACOut.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TFLACOut.Prepare;
  begin
    if not FStreamAssigned then
    begin
      if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
      if (not FileExists(FWideFileName)) or (FFileMode = foRewrite) then
      FStream := TAuFileStream.Create(FWideFileName, fmCreate or fmShareExclusive, FAccessMask)
      else FStream := TAuFileStream.Create(FWideFileName, fmOpenReadWrite or fmShareExclusive, FAccessMask);
    end;
    EndOfInput := False;
    _encoder := FLAC__stream_encoder_new;
    if _encoder = nil then
    raise EAuException.Create('Failed to initialize FLAC encoder.');
    FInput.Init;
    FLAC__stream_encoder_set_verify(_encoder, FVerify);
    FLAC__stream_encoder_set_channels(_encoder, FInput.Channels);
    FLAC__stream_encoder_set_bits_per_sample(_encoder, FInput.BitsPerSample);
    FLAC__stream_encoder_set_sample_rate(_encoder, FInput.SampleRate);
    if FInput.Channels = 2 then
    begin
      FLAC__stream_encoder_set_do_mid_side_stereo(_encoder, FEnableMidSideStereo);
      FLAC__stream_encoder_set_loose_mid_side_stereo(_encoder, FEnableLooseMidSideStereo);
    end;
    FLAC__stream_encoder_set_blocksize(_encoder, FBlockSize);
    if FCompressionLevel >= 0 then
      FLAC__stream_encoder_set_compression_level(_encoder, FCompressionLevel)
    else begin
      FLAC__stream_encoder_set_max_lpc_order(_encoder, FMaxLPCOrder);
      if FQLPCoeffPrecision + FInput.BitsPerSample > 31 then FQLPCoeffPrecision := 31 - FInput.BitsPerSample;
      FLAC__stream_encoder_set_qlp_coeff_precision(_encoder, FQLPCoeffPrecision);
      FLAC__stream_encoder_set_do_qlp_coeff_prec_search(_encoder, FQLPCoeffPrecisionSearch);
      FLAC__stream_encoder_set_min_residual_partition_order(_encoder, FMinResidualPartitionOrder);
      FLAC__stream_encoder_set_max_residual_partition_order(_encoder, FMaxResidualPartitionOrder);
      FLAC__stream_encoder_set_do_exhaustive_model_search(_encoder, FBestModelSearch);
    end;
    if FInput.Size > 0 then
    FLAC__stream_encoder_set_total_samples_estimate(_encoder, Round(FInput.Size/(FInput.BitsPerSample shr 3)/FInput.Channels));
   // FLAC__stream_encoder_set_seek_callback(_encoder, EncSeekCBFunc);
//    FLAC__stream_encoder_set_write_callback(_encoder, EncWriteCBFunc);
//    FLAC__seekable_stream_encoder_set_client_data(_encoder, Self);
    if FLAC__stream_encoder_init_stream(_encoder, EncWriteCBFunc, EncSeekCBFunc,
    EncTellCBFunc, EncMetadataCBFunc, Self) <>
    FLAC__STREAM_ENCODER_OK then
    begin
      FInput.Flush;
      raise EAuException.Create('Failed to initialize FLAC encoder.');
    end;
    FBufSize := FBlockSize * (FInput.BitsPerSample shr 3) * FInput.Channels;
    GetMem(Buffer, FBufSize);
  end;

  procedure TFLACOut.Done;
  begin
    if not FStreamAssigned then
    FLAC__stream_encoder_finish(_encoder);
    FLAC__stream_encoder_delete(_encoder);
    if Buffer <> nil then
    FreeMem(Buffer);
    Buffer := nil;
    FStream.Free;
    FInput.Flush;
  end;

  function TFLACOut.DoOutput;
  var
    Len, i, l, samples : Integer;
    FB : PFLACBuf;
    FBU : PFLACUBuf;
    B16 : PBuffer16;
  begin
    Result := True;
    if not CanOutput then Exit;
    if Abort or EndOfInput then
    begin
      Result := False;
      Exit;
    end;
    Len := FInput.FillBuffer(Buffer, FBufSize, EndOfInput);
    (*while Len < FBufSize do
    begin
      l := Finput.CopyData(@Buffer[Len], FBufSize-Len);
      Inc(Len, l);
      if l = 0 then
      begin
        EndOfInput := True;
        Break;
      end;
    end; *)
    if Len = 0 then
    begin
      Result := False;
      Exit;
    end;
    samples := (Len shl 3) div Finput.BitsPerSample;
    GetMem(FB, samples*SizeOF(FLAC__int32));
    if FInput.BitsPerSample = 16 then
    begin
      B16 := @Buffer[0];
      for i := 0 to samples - 1 do FB[i] := B16[i];
    end else
    begin
      if FInput.BitsPerSample = 8 then
        for i := 0 to samples - 1 do FB[i] := Buffer[i]
      else
      begin
        FBU := PFLACUBuf(FB);
        for i := 0 to samples - 1 do FBU[i] := (Buffer[i*3 + 2] shl 16) + (Buffer[i*3 + 1] shl 8) + (Buffer[i*3]);
      end;

    end;
    if not FLAC__stream_encoder_process_interleaved(_encoder, @FB[0], samples div FInput.Channels) then
    raise EAuException.Create('Failed to encode data.');
    FreeMem(FB);
  end;

  procedure TFLACOut.SetEnableLooseMidSideStereo;
  begin
    if Val then FEnableMidSideStereo := True;
    FEnableLooseMidSideStereo := Val;
  end;

  procedure TFLACOut.SetBestModelSearch;
  begin
    if Val then
    begin
      FEnableMidSideStereo := True;
      FEnableLooseMidSideStereo := False;
    end;
    FBestModelSearch := Val;
  end;

  constructor TFLACIn.Create;
  begin
    inherited Create(AOwner);
    if not (csDesigning	in ComponentState) then
    if not LibFLACLoaded then
    raise EAuException.Create(LibFLACPath + ' library could not be loaded.');
    FComments := TVorbisTags.Create;
  end;

  destructor TFLACIn.Destroy;
  begin
    CloseFile;
    FComments.Free;
    inherited Destroy;
  end;

  procedure TFLACIn.OpenFile;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      Inc(FOpened);
      Residue := 0;
      if (not FStreamAssigned) and (FWideFileName = '') then
      raise EAuException.Create('File name is not assigned');
      if not FStreamAssigned then FStream := TAuFileStream.Create(FWideFileName, fmOpenRead, fmShareDenyNone);
      FValid := True;
      ReadHeader;
      _decoder := FLAC__stream_decoder_new;
      if _decoder = nil then
      raise EAuException.Create('Failed to initialize the FLAC decoder.');
//      FLAC__seekable_stream_decoder_set_metadata_ignore_all(_decoder);
      if FLAC__stream_decoder_init_stream(_decoder, DecReadCBFunc, DecSeekCBFunc,
                                       DecTellCBFunc, DecLengthCBFunc, DecEOFCBFunc,
                                       DecWriteCBFunc, DecMetadataCBProc,
                                       DecErrorCBProc, Self) <> FLAC__STREAM_DECODER_INIT_STATUS_OK then
      raise EAuException.Create('Failed to set up the FLAC decoder.');
      if not FLAC__stream_decoder_process_until_end_of_metadata(_decoder) then
      FValid := False;
      BuffSize := 0;
      Buff := nil;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TFlacIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      if _decoder <> nil then
      begin
        FLAC__stream_decoder_flush(_decoder);
        FLAC__stream_decoder_finish(_decoder);
        FLAC__stream_decoder_delete(_decoder);
        _decoder := nil;
      end;
      if Buff <> nil then FreeMem(Buff);
      Buff := nil;
      if not FStreamAssigned then FStream.Free
      else FStream.Seek(0, soFromBeginning);
      FOpened := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TFLACIn.GetDataInternal;
  var
    dec_state : Integer;
  begin
    if not Busy then raise EAuException.Create('The Stream is not opened');
    if BufStart >= BufEnd then
    begin
      BufStart := 0;
      BufEnd := 0;
      if not FLAC__stream_decoder_process_single(_decoder) then
      begin
        dec_state := FLAC__stream_decoder_get_state(_decoder);
        if dec_state = FLAC__STREAM_DECODER_END_OF_STREAM then
        begin
          Buffer :=  nil;
          Bytes := 0;
          Exit;
        end
        else raise EAuException.Create('Error reading FLAC file');
      end else BufEnd := Self.BytesPerBlock;
      if Buff = nil then
      begin
        Bytes := 0;
        if FStream.Position < FStream.Size then
          raise EAuException.Create('Sinc lost or corrupt data');
      end;
    end;
    if Residue <> 0 then
    begin
      BufStart := (Residue - 1)*FSampleSize;
      if BufStart >= BufEnd then
        raise EAuException.Create('Seek failed');
      Residue := 0;
      Inc(FPosition, BufStart - FSampleSize);
    end;
    Bytes := Bytes - (Bytes mod FSampleSize);
    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer := @Buff[BufStart];
    Inc(BufStart, Bytes);
  end;

  function TFLACIn.SeekInternal;
  var
    Aligned : Int64;
  begin
    Result := False;
    if FBlockSize <> 0 then
    begin
      Residue := SampleNum mod FBlockSize;
      Aligned := SampleNum - Residue;
    end
    else
      Aligned := SampleNum;
    Result := FLAC__stream_decoder_seek_absolute(_decoder, Aligned);
    if not Result then FLAC__stream_decoder_reset(_decoder);
    SampleNum := Aligned;
    BufStart := 0;
    BufEnd := 0;
  end;

  procedure TFlacOut.SetCompressionLevel;
  begin
    if Val > 8 then
      FCompressionLevel := 8
    else FCompressionLevel := Val;
  end;

type
  TBlockInfo = record
   BlockType : Word;
   BlockLength : LongWord;
   HasNext : Boolean;
  end;

  procedure GetBlockInfo(FS : TStream; var BI : TBlockInfo);
  var
    Header  : array [0..3] of Byte;
    t : Byte;
  begin
    FS.Read(Header, 4);
    BI.HasNext := (Header[0] shr 7) = 0;
    BI.BlockType := Header[0] mod 128;
    BI.BlockLength := 0;
    t := Header[1];
    Header[1] := Header[3];
    Header[3] := t;
    Move(Header[1], BI.BlockLength, 3);
  end;

  procedure TFlacIn.ReadHeader;
  var
    Signature : array [0..3] of Char;
    BI : TBlockInfo;
  begin
    FComments.Clear;
    FStream.Read(Signature, 4);
    if Signature = 'fLaC' then
    begin
      FValid := True;
      repeat
        GetBlockInfo(FStream, BI);
        if BI.BlockType = FLAC__METADATA_TYPE_VORBIS_COMMENT then
        begin
          ReadComments;
          FStream.Seek(0, soFromBeginning);
          Break;
        end;
        FStream.Seek(BI.BlockLength, soFromCurrent);
      until not BI.HasNext;
    end else
      FValid := False;
  end;

  procedure TFLACIn.ReadComments;
  var
    i : integer;
    l, ul : LongWord;
    S : String;
    WS : WideString;
    SL : TStringList;
  begin
    // Reading Vendor
    FStream.Read(l, 4);
    SetLength(S, l);
    FStream.Read(S[1], l);
    WS := Utf8Decode(S);
    FStream.Read(ul, 4);
    SL := TStringList.Create;
    for i := 1 to ul do
    begin
      FStream.Read(l, 4);
      SetLength(S, l);
      FStream.Read(S[1], l);
      SL.Add(S);
    end;
    S := SL.Values['TITLE'];
    FComments.Title := Utf8Decode(S);
    S := SL.Values['ARTIST'];
    FComments.Artist := Utf8Decode(S);
    S := SL.Values['ALBUM'];
    FComments.Album := Utf8Decode(S);
    S := SL.Values['DATE'];
    FComments.Date := Utf8Decode(S);
    S := SL.Values['GENRE'];
    FComments.Genre := Utf8Decode(S);
    S := SL.Values['TRACK'];
    if S <> '' then
      FComments.Track := StrToInt(S)
    else  FComments.Track := 0;
    SL.Free;
  end;

  function TFLACIn.GetComments;
  begin
    OpenFile;
    Result := FComments;
  end;



end.
