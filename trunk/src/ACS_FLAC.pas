(*
  This file is a part of New Audio Components package 1.4
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_FLAC;

(* Title: ACS_FLAC
    NewAC interface to flac.dll *)

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

(* Class: TFLACOut
    FLAC encoder component.
    Descends from <TAuFileOut>.
    Requires libFLAC.dll
    More information about FLAC can be found at http://flac.sourceforge.com. *)

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
    BolckInserted : Boolean;
    FTags : TVorbisTags;
    procedure SetEnableLooseMidSideStereo(val : Boolean);
    procedure SetBestModelSearch(val : Boolean);
    procedure SetCompressionLevel(val : Integer);
    procedure SetTags(Value : TVorbisTags);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  (* Property: BestModelSearch
      Similar to America's Next Top Model, except for algorithms. *)
    property BestModelSearch : Boolean read FBestModelSearch write SetBestModelSearch;
  (* Property: Blocksize
      The size you want some blocks to be. Has nothing to do with <BestModelSearch> *)
    property BlockSize : Word read FBlockSize write FBlockSize;
  (* Property: CompressionLevel
      What level you want your compression at. *)
    property CompressionLevel : Integer read FCompressionLevel write SetCompressionLevel;
  (* Property: EnableMidSideStereo
      Set this property to True to get a bit more compression. *)
    property EnableMidSideStereo : Boolean read FEnableMidSideStereo write FEnableMidSideStereo;
    property EnableLooseMidSideStereo : Boolean read FEnableLooseMidSideStereo write SetEnableLooseMidSideStereo;
    property MaxLPCOrder : Word read FMaxLPCOrder write FMaxLPCOrder;
    property MaxResidualPartitionOrder : Word read FMaxResidualPartitionOrder write FMaxResidualPartitionOrder;
    property MinResidualPartitionOrder : Word read FMinResidualPartitionOrder write FMinResidualPartitionOrder;
    property QLPCoeffPrecision : Word read FQLPCoeffPrecision write FQLPCoeffPrecision;
    property QLPCoeffPrecisionSearch : Boolean read FQLPCoeffPrecisionSearch write FQLPCoeffPrecisionSearch;
  (* Property: Tags
      Use this property to add a set of Vorbis-style comments (artist, title, etc.) to the output file. *)
    property Tags : TVorbisTags read FTags write SetTags;
  (* Property: Verify
      Setting Verify to True forces the FLAC encoder to verify its own output. It slows down encoding process and usually unnecessary. *)
    property Verify : Boolean read FVerify write FVerify;
  end;


(* Class: TFLACIn
    FLAC decoder component.
    Descends from <TAuFileIn>.
    Requires libFLAC.dll
    More information about FLAC can be found at http://flac.sourceforge.com. *)

  TFLACIn = class(TAuFileIn)
  private
    EndOfMetadata : Boolean;
    FComments : TVorbisTags;
    Residue : Integer;
    Buff : PBuffer8;
    BuffSize : LongWord;
    _decoder : P_FLAC__StreamDecoder;
    FBlockSize: LongWord;
    BytesPerBlock : LongWord;
    MinFrameSize : LongWord;
    FCheckMD5Signature : Boolean;
    FSignatureValid : Boolean;
    function GetComments : TVorbisTags;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  (* Property: IsMD5SignatureValid
      If MD5 signature checking is turned on, this property returns True if the signature is correct (i.e. file contents is not broken).
      This property value becomes meaningful only after the file has finished playing.
      Note that if FLAC codec cannot check the signature for some internal reason, this property still returns True.*)
    property IsMD5SignatureValid : Boolean read FSignatureValid;
  (* Property: VorbisComments
      Read this property to get tags (artist, title, etc.) that may be attached to the file.*)
    property VorbisComments : TVorbisTags read GetComments;
  published
  (* Property:  CheckMD5Signature
      This property specifies whether the input file's MD5 signature should be checked.
      The MD5 signature checking should be turned on before the file starts playing.
      If you set this property to True, you can use <IsMD5SignatureValid> value to check the signature after file has finished playing.
      Note, that seeking in the input file turns the signature checking off (the value of CheckMD5Signature becomes False).
      In this case <IsMD5SignatureValid> will aalways return True.*)
    property CheckMD5Signature : Boolean read FCheckMD5Signature write FCheckMD5Signature;
  end;


implementation

type

  FLACBuf = array[0..0] of FLAC__int32;
  PFLACBuf = ^FLACBuf;
  FLACUBuf = array[0..0] of FLAC__uint32;
  PFLACUBuf = ^FLACBuf;

  type
  TBlockInfo = record
   BlockType : Word;
   BlockLength : LongWord;
   HasNext : Boolean;
  end;

  TVComments = record
    Vendor : Utf8String;
    Title : Utf8String;
    Artist : Utf8String;
    Album : Utf8String;
    Date : Utf8String;
    Genre : Utf8String;
    Track : Utf8String;
  end;

  function BuildCommentsBlock(var Comments : TVComments; var Block : Pointer; HasNext : Boolean) : Integer;
  var
    BS : Integer;
    Header : array[0..4] of Byte;
    t : byte;
    P : PChar;
    i, StrCount : Integer;
    procedure AddString(const Str : Utf8String);
    var
      l : Integer;
    begin
      l := Length(Str);
      Move(l, P[i], 4);
      Inc(i, 4);
      Move(Str[1], P[i], l);
      Inc(i, l);
    end;
  begin
    BS := 4;
    StrCount := 0;
    Comments.Vendor := Utf8Encode('VENDOR=Hacked');
    Inc(BS, Length(Comments.Vendor) + 4);
    if Comments.Title <> '' then
    begin
      Inc(StrCount);
      Inc(BS, Length(Comments.Title) + 4);
    end;
    if Comments.Artist <> '' then
    begin
      Inc(StrCount);
      Inc(BS, Length(Comments.Artist) + 4);
    end;
    if Comments.Album <> '' then
    begin
      Inc(StrCount);
      Inc(BS, Length(Comments.Album) + 4);
    end;
    if Comments.Date <> '' then
    begin
      Inc(StrCount);
      Inc(BS, Length(Comments.Date) + 4);
    end;
    if Comments.Genre <> '' then
    begin
      Inc(StrCount);
      Inc(BS, Length(Comments.Genre) + 4);
    end;
    if Comments.Track <> '' then
    begin
      Inc(StrCount);
      Inc(BS, Length(Comments.Track) + 4);
    end;
    Header[0] := FLAC__METADATA_TYPE_VORBIS_COMMENT;
    if not HasNext then
      Inc(Header[0], 128);
    Move(BS, Header[1], 3);
    t := Header[3];
    Header[3] := Header[1];
    Header[1] := t;
    Result := BS + 4;
    GetMem(Block, Result);
    P := PChar(Block);
    Move(Header[0], P[0], 4);
    i := 4;
    AddString(Comments.Vendor);
    Move(StrCount, P[i], 4);
    Inc(i, 4);
    if Comments.Title <> '' then
      AddString(Comments.Title);
    if Comments.Artist <> '' then
      AddString(Comments.Artist);
    if Comments.Album <> '' then
      AddString(Comments.Album);
    if Comments.Date <> '' then
      AddString(Comments.Date);
    if Comments.Genre <> '' then
      AddString(Comments.Genre);
    if Comments.Track <> '' then
      AddString(Comments.Track);
  end;


  function EncWriteCBFunc(encoder : P_FLAC__StreamEncoder;
                                buffer : PFLAC__byte;
                                bytes, samples, current_frame : LongWord;
                                client_data : Pointer) : Integer; cdecl;
  var
    FLACOut : TFLACOut;
    BI : TBlockInfo;
    Header : array[0..3] of Byte;
    Comm : TVComments;
    Block : Pointer;
    BL : Integer;
  begin
    FLACOut := TFLACOut(client_data);
    Result := FLAC__STREAM_ENCODER_WRITE_STATUS_OK;
    try
      if not FLACOut.BolckInserted then
      begin
        Move(buffer^, Header, 4);
        BI.HasNext := (Header[0] shr 7) = 0;
        BI.BlockType := Header[0] mod 128;
        if (BI.BlockType = FLAC__METADATA_TYPE_VORBIS_COMMENT) then
        begin
          FLACOut.BolckInserted := True;
          if FlacOut.FTags.Artist <> '' then
            Comm.Artist := Utf8Encode(WideString('ARTIST=') + FlacOut.FTags.Artist);
          if FlacOut.FTags.Album <> '' then
            Comm.Album := Utf8Encode(WideString('ALBUM=') + FlacOut.FTags.Album);
          if FlacOut.FTags.Title <> '' then
            Comm.Title := Utf8Encode(WideString('TITLE=') + FlacOut.FTags.Title);
          if FlacOut.FTags.Date <> '' then
            Comm.Date := Utf8Encode(WideString('DATE=') + FlacOut.FTags.Date);
          if FlacOut.FTags.Genre <> '' then
            Comm.Genre := Utf8Encode(WideString('GENRE=') + FlacOut.FTags.Genre);
          if FlacOut.FTags.Track <> 0 then
            Comm.Track := Utf8Encode(WideString('TRACKNUM=') + IntToStr(FlacOut.FTags.Track));
          BL := BuildCommentsBlock(Comm, Block, BI.HasNext);
          FLACOut.FStream.Write(Block^, BL);
          FreeMem(Block);
        end else
        FLACOut.FStream.Write(buffer^, bytes);
      end else
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
    i : Integer;
    S : String;
    Entry : PFLAC__StreamMetadata_VorbisComment_Entry;
    SL : TStringList;
  begin
    FLACIn := TFLACIn(client_data);
    if metadata._type = FLAC__METADATA_TYPE_STREAMINFO then
    begin
      FI := metadata.stream_info;
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
      SL := TStringList.Create;
      Entry := metadata.vorbis_comment.comments;
      for i := 0 to  metadata.vorbis_comment.num_comments - 1 do
      begin
        SetLength(S, Entry.length);
        Move(Entry.entry^, S[1], Length(S));
        SL.Add(S);
        Inc(LongWord(Entry), SizeOf(FLAC__StreamMetadata_VorbisComment_Entry));
      end;
      S := SL.Values['TITLE'];
      FLACIn.FComments.Title := Utf8Decode(S);
      S := SL.Values['ARTIST'];
      FLACIn.FComments.Artist := Utf8Decode(S);
      S := SL.Values['ALBUM'];
      FLACIn.FComments.Album := Utf8Decode(S);
      S := SL.Values['DATE'];
      FLACIn.FComments.Date := Utf8Decode(S);
      S := SL.Values['GENRE'];
      FLACIn.FComments.Genre := Utf8Decode(S);
      S := SL.Values['TRACKNUM'];
      if S <> '' then
        FLACIn.FComments.Track := StrToInt(S)
      else  FLACIn.FComments.Track := 0;
      SL.Free; 
    end;
    FLacIn.EndOfMetadata := metadata.is_last;
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
    FTags := TVorbisTags.Create;
    if not (csDesigning in ComponentState) then
    if not LibFLACLoaded then
    raise EAuException.Create(LibFLACPath + ' library could not be loaded.');
  end;

  destructor TFLACOut.Destroy;
  begin
    FTags.Free;
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
    BolckInserted := False;
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
    Len, i, samples : LongWord;
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
    if not (csDesigning in ComponentState) then
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
      _decoder := FLAC__stream_decoder_new;
      if _decoder = nil then
      raise EAuException.Create('Failed to initialize the FLAC decoder.');
      if not FLAC__stream_decoder_set_md5_checking(_decoder, LongBool(FCheckMD5Signature)) then
        raise EAuException.Create('Internal error 113, please report to NewAC developers');
      FLAC__stream_decoder_set_metadata_respond_all(_decoder);
      if FLAC__stream_decoder_init_stream(_decoder, DecReadCBFunc, DecSeekCBFunc,
                                       DecTellCBFunc, DecLengthCBFunc, DecEOFCBFunc,
                                       DecWriteCBFunc, DecMetadataCBProc,
                                       DecErrorCBProc, Self) <> FLAC__STREAM_DECODER_INIT_STATUS_OK then
      raise EAuException.Create('Failed to set up the FLAC decoder.');
      FComments.Clear;
      EndOfMetadata := False;
      while not EndOfMetadata do
      begin
        if not FLAC__stream_decoder_process_single(_decoder) then
        begin
          FValid := False;
          Break;
        end;
      end;
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
        FSignatureValid := FLAC__stream_decoder_finish(_decoder);
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
    FCheckMD5Signature := False;
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

  function TFLACIn.GetComments;
  begin
    OpenFile;
    Result := FComments;
  end;

  procedure TFLACOut.SetTags;
  begin
    FTags.Assign(Value);
  end;


end.
