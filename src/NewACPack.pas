unit NewACPack;

(* $Id$ *)

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Procs, PasPack;

type

  TCodecType = (ctAdaptive, ctFast, ctNLMS);

  TPasPackOut = class(TAuFileOut)
  private
    EndOfInput : Boolean;
    Encoder : TPPBaseEncoder;
    FCodecType : TCodecType;
    Buffer : Pointer;
    BufLen : LongWord;
    FJointStereo : Boolean;
    FFilterLength : Byte;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CodecType : TCodecType read FCodecType write FCodecType;
    property JointStereo : Boolean read FJointStereo write FJointStereo; // Only for ctFast
    property FilterLength : Byte read FFilterLength write FFilterLength; // Only for ctAdaptive
  end;

  TPasPackIn = class(TAuFileIn)
  private
    Decoder : TPPBaseDecoder;
    FCodecType : TCodecType;
    FBuffer : Pointer;
    BufLen : LongWord;
    procedure DecoderFactory;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
    property CodecType : TCodecType read FCodecType;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation

  constructor TPasPackOut.Create;
  begin
    inherited;
  end;

  destructor TPasPackOut.Destroy;
  begin
    inherited;
  end;

  procedure TPasPackOut.Prepare;
  begin
    if not FStreamAssigned then
    begin
      if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
      if (not FileExists(FWideFileName)) or (FFileMode = foRewrite) then
      FStream := TAuFileStream.Create(FWideFileName, fmCreate or fmShareExclusive, FAccessMask)
      else FStream := TAuFileStream.Create(FWideFileName, fmOpenReadWrite or fmShareExclusive, FAccessMask);
    end;
    FInput.Init;
    if FInput.BitsPerSample > 16 then
      raise EAuException.Create('Only 16 bits per sample are currently supported');
    case FCodecType of
      ctFast :
      begin
        Encoder := TPPFastEncoder.Create(FStream);
        if FJointStereo then (Encoder as TPPFastEncoder).SetJoinedStereo;
      end;
      ctAdaptive :
      begin
        Encoder := TPPAdaptiveEncoder.Create(FStream);
        if FFilterLength > 0 then (Encoder as TPPAdaptiveEncoder).SetFilterLength(FFilterLength);
      end;
      ctNLMS :
        Encoder := TPPNLMSEncoder.Create(FStream);
    end;    
    Encoder.Init(FInput.BitsPerSample, FInput.Channels, FInput.SampleRate);
    Encoder.QueryBuffer(Buffer, BufLen);
    FInput.FillBuffer(Buffer, BufLen, EndOfInput);
  end;

  function TPasPackOut.DoOutput;
  begin
    if EndOfInput then
    begin
      Result := False;
      Exit;
    end;
    Result := True;
    Encoder.Encode;
    Encoder.QueryBuffer(Buffer, BufLen);
    FInput.FillBuffer(Buffer, BufLen, EndOfInput);
  end;

  procedure TPasPackOut.Done;
  begin
    Encoder.EncodeLastBlock(BufLen);
    Encoder.Free;
    if (not FStreamAssigned) and (FStream <> nil) then FStream.Free;
    FInput.Flush;
  end;

  constructor TPasPackIn.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TPasPackIn.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TPasPackIn.DecoderFactory;
  var
    FastDecoder : TPPFastDecoder;
    AdaptiveDecoder : TPPAdaptiveDecoder;
    NLMSDecoder : TPPNLMSDecoder;
  begin
    FValid := False;
    FStream.Seek(0, soFromBeginning);
    FastDecoder := TPPFastDecoder.Create(FStream);
    if FastDecoder.ReadHeader then
    begin
      FValid := True;
      FCodecType := ctFast;
      Decoder := FastDecoder;
    end else
    begin
      FastDecoder.Free;
      FStream.Seek(0, soFromBeginning);
      AdaptiveDecoder := TPPAdaptiveDecoder.Create(FStream);
      if AdaptiveDecoder.ReadHeader then
      begin
        FValid := True;
        FCodecType := ctAdaptive;
        Decoder := AdaptiveDecoder;
      end else
      begin
        AdaptiveDecoder.Free;
        FStream.Seek(0, soFromBeginning);
        NLMSDecoder := TPPNLMSDecoder.Create(FStream);
        if NLMSDecoder.ReadHeader then
        begin
          FValid := True;
          FCodecType := ctNLMS;
          Decoder := NLMSDecoder;
        end else
        begin
          NLMSDecoder.Free;
          Decoder :=nil
        end;
      end;
    end;
  end;

  procedure TPasPackIn.OpenFile;
  begin
    OpenCS.Enter;
    try
      if FOpened = 0 then
      begin
        if not FStreamAssigned then
          FStream := TAuFileStream.Create(FWideFileName, fmOpenRead  or fmShareDenyWrite);
        DecoderFactory;
        FSize := Decoder.Size;
        FBPS := Decoder.BisPerSample;
        FChan := Decoder.Channels;
        FSR := Decoder.SampleRate;
        Inc(FOpened);
      end; // if FOpened = 0 then
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TPasPackIn.CloseFile;
  begin
    OpenCS.Enter;
    try
      if FOpened > 0 then
      begin
        Decoder.Free;
        Dec(Fopened);
      end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TPasPackIn.GetDataInternal;
  begin
    if BufStart >= BufEnd then
    begin
      Decoder.Decode(FBuffer, BufLen);
      BufStart := 0;
      BufEnd := BufLen;
    end;
    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer :=  Pointer(LongWord(FBuffer) + BufStart);
    Inc(BufStart, Bytes)
  end;

  function TPasPackIn.SeekInternal;
  begin
    Result := False // No support for seeking yet
  end;
end.
