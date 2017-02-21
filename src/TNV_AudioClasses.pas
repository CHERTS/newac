
(* $Id: TNV_AudioClasses.pas (C)DJ VK 2016 $ *)

unit TNV_AudioClasses;

interface

{$WARNINGS OFF}

uses
  Classes, SysUtils, ACS_Types, ACS_Procs, ACS_Classes, 
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Math;

const
  BUF_SIZE = $4000;
  BufSize = $6000;

type
  //TSingleArray = array of Single;
  //PSingleArray = ^TSingleArray;
  TDblArray = array of Double;
  PDblArray = ^TDblArray;
  TBitsPerSample = (bpsNoChange = 0, bps8bit = 8, bps16bit = 16, bps24bit = 24, bps32bit = 32);
  TChannels= (chNoChange, chMono, chStereo);
  TResamplerType= (rsNone, rsInput, rsOutput);

  TProcUnit = class(TPersistent)
  private
    procedure SetBypass(Value : Boolean);
    procedure SetInputGain(Value : Double);
    procedure SetOutputGain(Value : Double);
  protected
    FChannelsCnt : Integer;
    FSampleRate : Integer;
    FBypass : Boolean;
    FInputGain : Double;
    FOutputGain : Double;
    FOnChange: TNotifyEvent;
    FActive: Boolean;
    ReInitFlag : Boolean;
    procedure Changed; virtual; abstract;
    procedure InitUnit(sr, ch: Integer); virtual; abstract;
    procedure ResetUnitBuffer(); virtual; abstract;
    procedure ResetUnit(); virtual; abstract;
    procedure ProcSample(var sL : Double; var sR : Double); virtual; abstract;
  published
    property Bypass : Boolean read FBypass write SetBypass default true;
    property InputGain : Double read FInputGain write SetInputGain;
    property OutputGain : Double read FOutputGain write SetOutputGain;
  public
    procedure SetActive(Value : Boolean);
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TProcPlugin = class(TAuConverter)
  private

  protected
    FBypass : Boolean;
    InputBuffer : array of Single;
    SampleSize, FrameSize, SamplesInFrame : Word;
    _Buffer : array of Byte;
    FOnChange: TNotifyEvent;
    procedure Changed;
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure InitProc(); virtual; abstract;
    procedure ProcData(var Sample : TDblArray); virtual; abstract;
    procedure ResetProc(); virtual; abstract;
    procedure SetBypass(Value : Boolean);
  published
    property Bypass : Boolean read FBypass write SetBypass default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TResampler = record
    A, B : array of Single;
    X : array[0..1, 0..3] of array of Single;
    Y : array[0..1, 0..3] of array of Single;
    OffsX, OffsY : Integer;
  end;

  TConvPlugin = class(TAuConverter)
  private

  protected
    FBypass : Boolean;
    FMonoToStereo : TMSConverterMode;       
    FStereoToMono : TSMConverterMode;
    FOutputMinSampleRate : LongWord;
    FOutputChannels : TChannels;
    FOutputBitsPerSample : TBitsPerSample;

    FInSampleRate : LongWord;
    FInChannels : LongWord;                  //samples in frame
    FInBitsPerSample : LongWord;
    FInSampleSize : LongWord;                //bytes per sample
    FInFrameSize : LongWord;                 //bytes per frame
    FInBitsPerFrame : LongWord;
    
    FOutSampleRate : LongWord;
    FOutChannels : LongWord;
    FOutBitsPerSample : LongWord;
    FOutSampleSize : LongWord;                //bytes per sample
    FOutFrameSize : LongWord;                 //bytes per frame
    FOutBitsPerFrame : LongWord;

    FProcSampleRate : LongWord;
    FProcChannels : LongWord;
    FInFrames : LongWord;
    FOutFrames : LongWord;
    FInSize : LongWord;
    FOutSize : LongWord;
    FBufSize : LongWord;
    FProcFrames : LongWord;

    RType : TResamplerType;
    Resampler : TResampler;
    _Buffer : array of Byte;
    ProcBuffer : array of Single;

    (*Smp conv
    OutBuf : PBuffer8;
    FloatBuf : PBufferSingle;
    OutFrames : Integer;
    ICh, IBPS : Integer;
    *)
    
    FOnChange: TNotifyEvent;
    procedure Changed;
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure InitProc(); virtual; abstract;
    procedure ProcData(var Sample : TDblArray); virtual; abstract;
    procedure ProcSample(var sL : Double; var sR : Double); virtual; abstract;
    procedure ResetProc(); virtual; abstract;
    procedure SetBypass(Value : Boolean);
    procedure InitResampler();
    procedure ResetResampler();
    procedure SetOutputMinSampleRate(Value : LongWord);
    procedure SampleRateChanged; virtual; abstract;
  published
    property Bypass : Boolean read FBypass write SetBypass default true;
    property MonoToStereo : TMSConverterMode read FMonoToStereo write FMonoToStereo;
    property StereoToMono : TSMConverterMode read FStereoToMono write FStereoToMono;
    property OutputMinSampleRate : LongWord read FOutputMinSampleRate write SetOutputMinSampleRate;
    property OutputBitsPerSample : TBitsPerSample read FOutputBitsPerSample write FOutputBitsPerSample;
    property OutputChannels : TChannels read FOutputChannels write FOutputChannels;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

//////////////////////////////////

implementation

//////////////////////////////////

  constructor TProcUnit.Create(AOwner: TComponent);
  begin
    FChannelsCnt := 2;
    FSampleRate := 44100;
    FBypass := true;
    FInputGain := 1.0;
    FOutputGain := 1.0;

    FActive := false;
    ReInitFlag := false;
  end;

  destructor TProcUnit.Destroy;
  begin
    //inherited;
  end;

  procedure TProcUnit.SetActive(Value : Boolean);
  begin
    if(FActive <> Value) then
    begin
      FActive := Value;
    end;
  end;

  procedure TProcUnit.SetBypass(Value : Boolean);
  begin
    if(FBypass <> Value) then
    begin
      FBypass := Value;
    end;
  end;

  procedure TProcUnit.SetInputGain(Value : Double);
  begin
    if(FInputGain <> Value) then
    begin
      if(Value < 0.015625) then Value := 0.015625;
      if(Value > 64) then Value := 64;
      FInputGain := Value;
    end;
  end;

  procedure TProcUnit.SetOutputGain(Value : Double);
  begin
    if(FOutputGain <> Value) then
    begin
      if(Value < 0.015625) then Value := 0.015625;
      if(Value > 64) then Value := 64;
      FOutputGain := Value;
    end;
  end;

  (*procedure TProcUnit.Changed;
  begin
    if Assigned(FOnChange) then
    FOnChange(Self);
  end;*)

  //////////////////////////////////

  constructor TProcPlugin.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FBypass := true;
  end;

  destructor TProcPlugin.Destroy;
  begin
    Inherited Destroy;
  end;

  procedure TProcPlugin.Assign(Source: TPersistent);
  begin
  if (Source is TProcPlugin) then
  begin
    FBypass := (Source as TProcPlugin).FBypass;
  end
  else
    inherited Assign(Source);
  end;

  function TProcPlugin.GetBPS;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TProcPlugin.GetCh;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TProcPlugin.GetSR;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TProcPlugin.InitInternal;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Busy := True;
    FInput.Init;
    SampleSize := FInput.BitsPerSample div 8;
    FrameSize := SampleSize * FInput.Channels;
    SamplesInFrame := Finput.Channels;
    FPosition := 0;
    SetLength(_Buffer, BufSize);
    SetLength(InputBuffer, BufSize div SampleSize);
    InitProc();
    BufStart := 0;
    BufEnd := 0;
    FSize := FInput.Size;
  end;

  procedure TProcPlugin.FlushInternal;
  begin
    FInput.Flush;
    SetLength(_Buffer, 0);
    SetLength(InputBuffer, 0);
    ResetProc();
    Busy := False;
  end;

  procedure TProcPlugin.GetDataInternal;
  var
    i, j, SamplesRead, FramesRead : Integer;
    P : PBufferSingle;
    var Sample : TDblArray;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart >= BufEnd then
    begin
      BufStart := 0;
      BufEnd := FInput.CopyData(@_Buffer[0], BufSize);
      if BufEnd = 0 then
      begin
        Buffer := nil;
        Bytes := 0;
        Exit;
      end;
      if(not FBypass) then
      begin
        SamplesRead := BufEnd div SampleSize;
        FramesRead := BufEnd div FrameSize;
        P := PBufferSingle(@InputBuffer[0]);
        SetLength(Sample, 2);
        case SampleSize of
          1 : ByteToSingle(PBuffer8(_Buffer), P, SamplesRead);
          2 : SmallIntToSingle(PBuffer16(_Buffer), P, SamplesRead);
          3 : Int24ToSingle(PBuffer8(_Buffer), P, SamplesRead);
          4 : Int32ToSingle(PBuffer32(_Buffer), P, SamplesRead);
        end;
        for i := 0 to FramesRead - 1 do
        begin
          for j := 0 to SamplesInFrame - 1 do
            Sample[j] := InputBuffer[i * SamplesInFrame + j];
          ProcData(Sample);
          for j := 0 to SamplesInFrame - 1 do
          begin
            if Sample[j] > 32767.0 then Sample[j] := 32767.0;
            if Sample[j] < -32768.0 then Sample[j] := -32768.0;
            InputBuffer[i * SamplesInFrame +j] := Sample[j];
          end;
        end;
        P := PBufferSingle(@InputBuffer[0]);
        case SampleSize of
          1 : SingleToByte(P, PBuffer8(_Buffer), SamplesRead);
          2 : SingleToSmallInt(P, PBuffer16(_Buffer), SamplesRead);
          3 : SingleToInt24(P, PBuffer8(_Buffer), SamplesRead);
          4 : SingleToInt32(P, PBuffer32(_Buffer), SamplesRead);
        end;
      end;
    end;
    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer := @_Buffer[BufStart];
    Inc(BufStart, Bytes);
  end;

  procedure TProcPlugin.SetBypass(Value : Boolean);
  begin
    if(FBypass <> Value) then
    begin
      FBypass := Value;
      Changed();
    end;
  end;

  procedure TProcPlugin.Changed;
  begin
    if Assigned(FOnChange) then
    FOnChange(Self);
  end;

  ////////////////////////////////////

  constructor TConvPlugin.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FBypass := true;
    FMonoToStereo := msmMonoToBoth;
    FStereoToMono := smmAverage;
    FOutputMinSampleRate := 44100;
    FOutputChannels := chStereo;
    FOutputBitsPerSample := bpsNoChange;
  end;

  destructor TConvPlugin.Destroy;
  begin
    Inherited Destroy;
  end;

  procedure TConvPlugin.Assign(Source: TPersistent);
  begin
  if (Source is TConvPlugin) then
  begin
    FBypass := (Source as TConvPlugin).FBypass;
  end
  else
    inherited Assign(Source);
  end;

  function TConvPlugin.GetBPS;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    if(FOutputBitsPerSample = bpsNoChange) or FBypass then Result := FInput.BitsPerSample
    else Result := FOutBitsPerSample;
  end;

  function TConvPlugin.GetCh;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    if(FOutputChannels = chNoChange) or FBypass then Result := FInput.Channels
    else Result := FOutChannels;
  end;

  function TConvPlugin.GetSR;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    if FBypass then Result := FInput.SampleRate
    else Result := FOutSampleRate;
  end;

  procedure TConvPlugin.InitInternal;
  var i : Integer;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Busy := True;
    FInput.Init;

    FPosition := 0;
    BufStart := 0;
    BufEnd := 0;

    FInSampleRate := FInput.SampleRate;
    FInChannels := FInput.Channels;                  //samples in frame
    FInBitsPerSample := FInput.BitsPerSample;
    FInSampleSize := FInBitsPerSample div 8;         //bytes per sample
    FInFrameSize := FInSampleSize * FInChannels;     //bytes per frame
    FInBitsPerFrame := FInBitsPerSample * FInChannels;

    FOutSampleRate := FInSampleRate;
    if(FOutSampleRate < FOutputMinSampleRate) then FOutSampleRate := FOutputMinSampleRate;
    FOutChannels := Integer(FOutputChannels);        //samples in frame
    if(FOutChannels = 0) then FOutChannels := FInChannels;
    FOutBitsPerSample := Integer(FOutputBitsPerSample);
    if(FOutBitsPerSample = 0) then FOutBitsPerSample := FInBitsPerSample;
    FOutSampleSize := FOutBitsPerSample div 8;       //bytes per sample
    FOutFrameSize := FOutSampleSize * FOutChannels;  //bytes per frame
    FOutBitsPerFrame := FOutBitsPerSample * FOutChannels;

    FProcSampleRate := max(FInSampleRate, FOutSampleRate);
    FProcChannels := 2;
    FInFrames := FInSampleRate div 5;
    FOutFrames := FOutSampleRate div 5;
    FInSize := FInFrames * FInFrameSize;
    FOutSize := FOutFrames * FOutFrameSize;
    FProcFrames := FInFrames;
    if(FInFrames < FOutFrames) then
    begin
      i := 2;
      while (FInFrames * i < FOutFrames) do Inc(i);
      FProcFrames := FInFrames * i;
    end;
    FBufSize := FProcFrames * max(FInFrameSize, FOutFrameSize);
    if(FInFrames = FOutFrames) then RType := rsNone
    else if(FInFrames < FOutFrames) then RType := rsInput
    else RType := rsOutput;

    if(FInput.Size > 0) then FSize := Round(FInput.Size * FOutSize / FInSize)
    else FSize := FInput.Size;
    SetLength(_Buffer, FBufSize);
    SetLength(ProcBuffer, FProcFrames * max(FInChannels, FProcChannels));
    InitResampler();
    InitProc();
  end;

  procedure TConvPlugin.FlushInternal;
  begin
    FInput.Flush;
    ResetProc();
    ResetResampler();
    SetLength(_Buffer, 0);
    SetLength(ProcBuffer, 0);
    Busy := False;
  end;

  procedure TConvPlugin.GetDataInternal;
  var
    i, j, Frame, Residue : Integer;
    SamplesRead, FramesRead : Integer;
    SamplesWrite, FramesWrite : Integer;
    SamplesProc, FramesProc : Integer;
    //ISize, OSize : Integer;
    P : PBufferSingle;
    Sample : array [0..1] of Double;
    Lim : Double;
    Acc : Single;
    EndOfFile : Boolean;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if FBypass then
    begin
      FInput.GetData(Buffer, Bytes);
      Exit;
    end;
    if BufStart >= BufEnd then
    begin
      //get buffer
      BufStart := 0;
      //BufEnd := FInput.CopyData(@_Buffer[0], BufSize);
      BufEnd := FInput.FillBuffer(@_Buffer[0], FInSize, EndOfFile);
      if BufEnd = 0 then
      begin
        Buffer := nil;
        Bytes := 0;
        Exit;
      end;

      //read input
      SamplesRead := BufEnd div FInSampleSize;
      FramesRead := BufEnd div FInFrameSize;
      P := PBufferSingle(@ProcBuffer[0]);
      case FInSampleSize of
        1 : ByteToSingle(PBuffer8(_Buffer), P, SamplesRead);
        2 : SmallIntToSingle(PBuffer16(_Buffer), P, SamplesRead);
        3 : Int24ToSingle(PBuffer8(_Buffer), P, SamplesRead);
        4 : Int32ToSingle(PBuffer32(_Buffer), P, SamplesRead);
      end;

      //convert to stereo
      if(FInChannels < FProcChannels) then
        SingleMonoToStereo(P, SamplesRead, FMonoToStereo);
      if(FInChannels > FProcChannels) then
        SingleMultiToStereo(P, SamplesRead, FInChannels);

      //ISize := FInFrames * FInSampleSize * FProcChannels;
      //OSize := FOutFrames * FInSampleSize * FProcChannels;

      FramesProc := FramesRead;
      SamplesProc := FramesProc * FProcChannels;

      //input resampling
      if(RType = rsInput) then
        with Resampler do
      begin
        FramesProc := 0;
        Frame := 0;
        Residue := 0;
        while Frame < FramesRead do
        begin
          Residue := Residue - FInFrames;       //-ISize
          for i := 0 to FProcChannels - 1 do
            X[i, 0][OffsX + FramesProc] := ProcBuffer[Frame * FProcChannels + i];
          if Residue < 0 then
          begin
            Inc(Frame);
            Residue := Residue + FOutFrames;    //+OSize
          end;
          Inc(FramesProc);
        end; // while Frame < Frames do
        SamplesProc := FramesProc * FProcChannels;
        for j := 0 to 3 do
        begin
          for i :=  0 to FProcChannels - 1 do
          begin
            for Frame := 0 to FramesProc -1 do
            begin
              Acc := 0;
              MultAndSumSingleArrays(@(X[i, j][Frame]), @A[0], Acc, OffsX + 1);
              MultAndSumSingleArrays(@(Y[i, j][Frame]), @B[0], Acc, OffsY);
              Y[i, j][Frame + OffsY] := Acc;
            end;
            if(j = 3) then
            begin
              for Frame := 0 to FramesProc -1 do
                ProcBuffer[Frame * FProcChannels + i] :=  Y[i, j][OffsY + Frame];
            end
            else
              Move(Y[i, j][OffsY], X[i, j + 1][OffsX], SizeOf(Single) * FramesProc);
            for Frame := 0 to OffsX - 1 do X[i, j][Frame] := X[i, j][Frame + FramesProc];
            for Frame := 0 to OffsY - 1 do Y[i, j][Frame] := Y[i, j][Frame + FramesProc];
          end;
        end;
      end;

      //processing
      Lim := (1 shl (8 * FOutSampleSize - 1));
      for i := 0 to FramesProc - 1 do
      begin
        for j := 0 to FProcChannels - 1 do Sample[j] := ProcBuffer[i * FProcChannels + j];
        ProcSample(Sample[0], Sample[1]);
        for j := 0 to FProcChannels - 1 do
        begin
          if (Sample[j] > Lim - 1) then Sample[j] := Lim - 1;
          if (Sample[j] < -Lim) then Sample[j] := -Lim;
          ProcBuffer[i * FProcChannels + j] := Sample[j];
        end;
      end;

      FramesWrite := FramesProc;
      SamplesWrite := FramesWrite * FOutChannels;

      //output resampling
      if(RType = rsOutput) then
        with Resampler do
      begin
        for i := 0 to SamplesProc -1 do
          X[i mod FProcChannels, 0][OffsX + i div FProcChannels] := ProcBuffer[i];
        
        for j := 0 to 3 do
        begin
          for i :=  0 to FProcChannels - 1 do
          begin
            for Frame := 0 to FramesProc -1 do
            begin
              Acc := 0;
              MultAndSumSingleArrays(@(X[i, j][Frame]), @A[0], Acc, OffsX + 1);
              MultAndSumSingleArrays(@(Y[i, j][Frame]), @B[0], Acc, OffsY);
              Y[i, j][Frame + OffsY] := Acc;
            end;
            if (j < 3) then Move(Y[i, j][OffsY], X[i, j + 1][OffsX], SizeOf(Single) * FramesProc);
          end;
          if (j = 3) then
          begin
            Frame := OffsY;
            FramesWrite := 0;
            Residue := 0;
            while Frame < FramesProc + OffsY do
            begin
              Residue := Residue - FOutSize;
              if Residue < 0 then
              begin
                for i :=  0 to FProcChannels - 1 do
                  ProcBuffer[FramesWrite * FProcChannels + i] :=  Y[i, j][Frame];
                Residue := Residue + FInSize;
                Inc(FramesWrite);
              end;
              Inc(Frame);
            end; // while Frame < FramesProc + OffsY do
          end;
          for i :=  0 to FProcChannels - 1 do
          begin
            for Frame := 0 to OffsX - 1 do
              X[i, j][Frame] := X[i, j][Frame + FramesProc];
            for Frame := 0 to OffsY - 1 do
              Y[i, j][Frame] := Y[i, j][Frame + FramesProc];
          end;
        end;
        SamplesWrite :=  FramesWrite * FOutChannels;
      end;

      //covert to output channels
      if(FOutChannels < FProcChannels) then
        SingleStereoToMono(P, SamplesWrite, FStereoToMono);

      //write output
      P := PBufferSingle(@ProcBuffer[0]);
      case FOutSampleSize of
        1 : SingleToByte(P, PBuffer8(_Buffer), SamplesWrite);
        2 : SingleToSmallInt(P, PBuffer16(_Buffer), SamplesWrite);
        3 : SingleToInt24(P, PBuffer8(_Buffer), SamplesWrite);
        4 : SingleToInt32(P, PBuffer32(_Buffer), SamplesWrite);
      end;
      BufEnd := SamplesWrite * FOutSampleSize;
    end;
    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer := @_Buffer[BufStart];
    Inc(BufStart, Bytes);
    FPosition := Round(FInput.Position * (FSize / FInput.Size));
  end;

  procedure TConvPlugin.InitResampler();
  var
    i, j, NP : Integer;
    Coef, Tmp : Single;
  begin
    with Resampler do
    begin
      if RType = rsNone then Exit;
      if FInSize > FOutSize then Coef := FOutSize / FInSize
      else Coef := FInSize / FOutSize;
      NP := 4;
      SetLength(A, NP + 4);
      SetLength(B, NP + 4);
      for i := 0 to NP + 3 do
      begin
        A[i] := 0;
        B[i] := 0;
      end;
      CalculateChebyshev(0.85 * Coef / 2, 5, NP, False, A, B);
      SetLength(A, NP + 1);
      SetLength(B, NP);
      for i := 0 to Length(A) div 2 - 1 do
      begin
        Tmp := A[i];
        A[i] := A[Length(A) - i - 1];
        A[Length(A) - i -1] := Tmp;
      end;
      for i := 0 to Length(B) div 2 - 1 do
      begin
        Tmp := B[i];
        B[i] := B[Length(B) - i - 1];
        B[Length(B) - i -1] := Tmp;
      end;
      OffsX := Length(A) - 1;
      OffsY := Length(B);
      for i := 0 to FProcChannels - 1 do
        for j := 0 to 3 do
        begin
          SetLength(X[i, j], FProcFrames * FInSampleSize + OffsX);   // какой SampleSize ?
          FillChar(X[i, j][0], OffsX * SizeOf(Single), 0);
          SetLength(Y[i, j], FProcFrames * FInSampleSize + OffsY);
          FillChar(Y[i, j][0], OffsY * SizeOf(Single), 0);
        end;
    end;
  end;

  procedure TConvPlugin.ResetResampler();
  var
    i, j : Integer;
  begin
    with Resampler do
    begin
      //SetLength(A, 0);
      //SetLength(B, 0);
      for i := 0 to FProcChannels - 1 do
        for j := 0 to 3 do
        begin
          SetLength(X[i, j], 0);
          SetLength(Y[i, j], 0);
        end;
    end;
  end;
  
  procedure TConvPlugin.SetBypass(Value : Boolean);
  begin
    if(FBypass <> Value) then
    begin
      FBypass := Value;
      Changed();
    end;
  end;

  procedure TConvPlugin.SetOutputMinSampleRate(Value : LongWord);
  begin
    if(FOutputMinSampleRate <> Value) then
    begin
      FOutputMinSampleRate := Value;
      SampleRateChanged;
      Changed();
    end;
  end;

  procedure TConvPlugin.Changed;
  begin
    if Assigned(FOnChange) then
    FOnChange(Self);
  end;

{$WARNINGS ON}

end.
