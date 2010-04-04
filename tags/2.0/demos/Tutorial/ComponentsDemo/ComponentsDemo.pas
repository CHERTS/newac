
unit ComponentsDemo;

(* Title: Components Demo
    This unit is a demo showing how to build new input and output components
    for NewAC. *)

interface

uses
  Classes, SysUtils,
  ACS_Types,
  // ACS_Classes unit holds declarations of all fundamental NewAC classes
  ACS_Classes;

const

(* Const: InitialBufferSize
    An Integer equal to $1000 (that's 4096 bytes, if you don't speak hex). The
    InitialBufferSize constant holds the initial size of the input buffer.
    This way we can change the default buffer size easily, without delving
    into the code. *)
  InitialBufferSize : Integer = $1000;

type

(* Class: TDemoWaveIn 
    A demo input component capable of reading data from raw PCM wav files.
    Like most input components, it descends from the TAuFileIn class. *)

  TDemoWaveIn = class(TAuFileIn)
  private
    (* These are internal fields specific to the component implementation.
       See the comments below. *)
    _Buffer : Pointer;
    CurrentBufSize : Integer;
  protected

    (* Procedure: OpenFile
        Called by the base methods of TAuFileIn class to open the file. It
        should prepare the component for reading data. This method also sets
        values for several internal and inherited fields. *)
      
    procedure OpenFile; override;

    (* Procedure: CloseFile
      Called by the base methods of TAuFileIn class to close the file. It
      should clear all resources assosiated with input. *)

    procedure CloseFile; override;
  public

    (* Constructor: Create
      Reimplementing constructor and destructor is optional. It depends on the
      requirements of your component. *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    (* Procedure: GetDataInternal 
      Used by NewAC to retrieve decoded data. You should pass the number of
      bytes you want to read in the  Bytes parameter. This method returns the
      pointer to the data buffre in the Buffer variable parameter and the
      number of bytes actually read in the buffer in the Bytes paramter. The
      buffer pointer must be provided by THIS method and not by the caller.
      The pointer returned by GetData should be valid until the next call to
      the method. When the method cannot return any more data it should return
      nil in the Buffer and 0 in the Bytes parameter. The GetDataInternal
      method can return less bytes than it is asked to and it should not be
      considered as an end of file indication.

    Parameters:

      Buffer: Pointer - an *unassigned* pointer to use for the results,
        GetData initializes the pointer itself. 

      Bytes: Integer - the number of bytes to read, returns bytes read. *)

    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;


    (* Function: SeekInternal
      Performs the file seeking. The new position is set in samples, not in
      bytes, relatively to the beginning of the file. Seek should return True
      if the input is seekable and False otherwise. This method implementation
      is optional. If your input component doesn't support seeking.

  > function TSomeInputComponent.Seek(SampleNum : Integer) : Boolean;
  > begin
  > Result := False;
  > end;

      In the SampleNum parameter we pass the new position in the input stream
      in frames (which are called samples ;-) relative to the beginning of the
      stream.
    *)
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  published
    property Loop;
  end;

  (* Class: TDemoWaveOut 
      A demo output component capable of storing data on disk or in a stream
      in the raw PCM wav format. Like most output components, it descends from
      the <TAuFileOut> class. *)

  TDemoWaveOut = class(TAuFileOut)
  private
    EndOfInput : Boolean;

  protected

    (* Procedure: Prepare
        Performs all the steps required to initialize output. *)

    procedure Prepare; override;


    (* Function: DoOutput 
        Called in a loop to perform actual output. If the Abort parameter is
        set to True the function must do whatever is needed to stop the
        output. The function may be called several times with the Abort
        value set to True. The return value indicartes wheter an output
        operation should continue. If the function has done its job (because
        of an end of input file condition, or because the Abort parameter is
        set to True, or because of some failure, it should return False as a
        result. Otherwise it returns True.

    Parameters:

      Abort: Boolean - set to True in order to stop the output operation

    Returns:

      Success - Boolean *)

    function DoOutput(Abort : Boolean):Boolean; override;

    (* Procedure: Done
        Called to do whatever is needed to close the output as well as freeing
        any associated resources. *)

    procedure Done; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

type

  (* Record: TAudioInfo
    The data structure for the <ReadRIFFHeader> helper function. 

    Properties:

      BitsPerSample : Integer - Bits per sample
      SampleRate : Integer - sample rate
      Channels : Integer - channels (1 for mono, 2 for stereo)
      TotalSamples : Integer - total number of samples?
  *)

  TAudioInfo = record
    BitsPerSample : Integer;
    SampleRate : Integer;
    Channels : Integer;
    TotalSamples : Integer;
  end;

const


  (* Constants:
      For the <ReadRIFFHeader> helper function. 

    WAVE_FORMAT_PCM - 1 some explanation
    LookingForRIFF - 0 some explanation
    LookingForWave - 1 some explanation
    LookingForFMT - 2 some explanation
    LookingForFACT - 3 some explanation
    LookingForDATA - 4 some explanation

  *)
    
  WAVE_FORMAT_PCM = 1;

  LookingForRIFF = 0;
  LookingForWave = 1;
  LookingForFMT = 2;
  LookingForFACT = 3;
  LookingForDATA = 4;

  (* Function: Compare4 
      Used internally by <ReadRIFFHeader>. *)

  function Compare4(S1, S2 : PChar) : Boolean;
  var
    i, Diff : Byte;
  begin
    Result := False;
    for i := 0 to 3 do
    begin
      Diff := Byte(S1[i]) - Byte(S2[i]);
      if not (Diff in [0, 32, 224]) then Exit;
    end;
    Result := True;
  end;


  (* Function: ReadRIFFHeader 
      A helper function that parses wav file header and gets audio data
      parameters. This is relevant only to wave file readers, so no further
      comments are necessary. *)

  function ReadRIFFHeader(Stream : TStream; var AudioInfo : TAudioInfo) : Boolean;
  var
    i : Integer;
    WordVal : Word;
    IntVal : Integer;
    Buff : array[0..$fff] of Char;
    State : Integer;
    ChunkSize : Integer;
  begin
    Result := False;
    State := LookingForRIFF;
    i := 4;
    Stream.Read(Buff[0], 4);
    while i < $2000 do
    begin
      case State of
        LookingForRIFF :
        begin
          if not Compare4(@Buff[i-4], 'RIFF') then
          begin
            Stream.Read(Buff[i], 1);
            Inc(i);
          end else
          begin
            Stream.Read(Buff[i], 4);
            Inc(i, 4);
            State := LookingForWAVE;
          end;
        end;
        LookingForWAVE :
        begin
          if not Compare4(@Buff[i-4], 'WAVE') then
          begin
            Stream.Read(Buff[i], 1);
            Inc(i);
          end else
          begin
            Stream.Read(Buff[i], 4);
            Inc(i, 4);
            State := LookingForFMT;
          end;
        end;
        LookingForFMT :
        begin
          if not Compare4(@Buff[i-4], 'fmt ') then
          begin
            Stream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            Stream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            Stream.Read(Buff[i], 4);
            Inc(i, 4);
          end else
          begin
            Stream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            Stream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            Move(Buff[i-ChunkSize], WordVal, 2);
            if WordVal <>  WAVE_FORMAT_PCM then
            begin
              WriteLn('Unsupported wave format');
              Exit;
            end;
            Move(Buff[i+2-ChunkSize], WordVal, 2);
            AudioInfo.Channels := WordVal;
            Move(Buff[i+4-ChunkSize], IntVal, 4);
            AudioInfo.SampleRate := IntVal;
//            Move(Buff[i+12-ChunkSize], WordVal, 2);
            Move(Buff[i+14-ChunkSize], WordVal, 2);
            AudioInfo.BitsPerSample := WordVal;
            State := LookingForDATA;
            Stream.Read(Buff[i], 4);
            Inc(i, 4);
          end;
        end;
        LookingForDATA :
        begin
          if not Compare4(@Buff[i-4], 'data') then
          begin
            Stream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], ChunkSize, 4);
            Stream.Read(Buff[i], ChunkSize);
            Inc(i, ChunkSize);
            Stream.Read(Buff[i], 4);
            Inc(i, 4);
          end else
          begin
            Stream.Read(Buff[i], 4);
            Inc(i, 4);
            Move(Buff[i-4], IntVal, 4);
            AudioInfo.TotalSamples := (IntVal div (AudioInfo.BitsPerSample div 8)) div AudioInfo.Channels;
            Result := True;
            Exit;
          end;
        end;
      end;
      if Stream.Position >= Stream.Size then Break;
    end;
  end;

  constructor TDemoWaveIn.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
  end;

  destructor TDemoWaveIn.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TDemoWaveIn.OpenFile;
  var
    Info : TAudioInfo;
  begin
    //  FOpened is a reference counter. The FileOpen method can be called several times.
    //  If the file is already open we just increment FOpened
    if FOpened = 0 then
    begin
      // FValid indicates whether the audio data source is valid.
      // We set it to True by default and then, if needed - to False
      FValid := True;
      // We can assign either a file name or a TStream object to the input component.
      // If the file name is assigned, we create a stream using that file name.
      // Otherwise FStream is used to access an externam TStream object.
      if not FStreamAssigned then
        FStream := TFileStream.Create(FFileName, fmOpenRead);
      if not ReadRIFFHeader(FStream, Info) then
      begin
        // Could not read the file header. The file is of wrong type or corrupt.
        FValid := False;
        Exit;
      end;
      // Now we fill some internal fields, inherited from the base classes.
      // We obtain corresponding values from the RIFF file header,
      // You should do it using the codec's API.
      // FBPS holds the number of bits per sample.
      FBPS := Info.BitsPerSample;
      // FSR Holds the sample rate in Hz
      FSR := Info.SampleRate;
      // FChan holds the number of channles
      FChan := Info.Channels;
      // FSize holds the size of the audio data in bytes.
      // Note thet you provide the size of the decoded stream here,
      // not the original stream you are decoding from.
      // You are required to provide an FSize Value only if
      // the steram is seekable.
      // If the stream is not seekable, write -1 to FSize.
      FSize := Info.TotalSamples * (Info.BitsPerSample div 8) * Info.Channels;
      // Our component is seekable.
      FSeekable := True;
      // We have to create the input buffer
      CurrentBufSize := InitialBufferSize;
    end;
    // Now increment the reference counter
    Inc(FOpened);
  // That's all folks. Now the stream is ready for reading data.
  end;

  procedure TDemoWaveIn.GetDataInternal(var Buffer: Pointer; var Bytes: LongWord);
  begin
    // The data buffer is implemented as simply as it gets.
    // If more data is asked than the buffer can hold, we resize the buffer.
    // Some codecs return data in fixed size chunks, Some require
    // reading ahead. In all these cases you may need a more sophisticated buffer
    // than this. Look at TCDIn and TFLACIn components for more examples.
    if CurrentBufSize < Bytes then
    begin
      CurrentBufSize := Bytes;
      FreeMem(_Buffer);
      GetMem(_Buffer, CurrentBufSize);
    end;
    // Correct Bytes value to prevent reading data beyond the end of file.
    if Bytes > FSize - FPosition then
      Bytes := FSize - FPosition;
    // All we have to do now is to read Bytes bytes into the _Buffer
    FStream.Read(_Buffer^, Bytes);
    // We will return the address of the _Buffer
    Buffer := _Buffer;
  end;

  procedure TDemoWaveIn.CloseFile;
  begin
    // This one is easy.
    if FOpened > 0 then Dec(FOpened);
    if FOpened = 0 then
    begin
      // We don't free FStream if it was created outside the component.
      if (not FStreamAssigned) and (FStream <> nil) then
        FStream.Free;
      FreeMem(_Buffer);
    end;
  end;

  function TDemoWaveIn.SeekInternal(var SampleNum: Int64) : Boolean;
  var
    Offset : Integer;
  begin
    if SampleNum > TotalSamples then
      Result := False
    else
    begin
      // Converting frames offset into bytes offset.
      // 44 is the RIFF header length.
      Offset := SampleNum*FChan*(FBPS div 8) + 44;
      FStream.Seek(Offset, soFromBeginning);
      // Set the new position value
      FPosition := FStream.Position;
      Result := True;
    end;
  end;

  type

    (*
      The TWaveHeader record is used internally by the TDemoWaveOut component
      for creating the RIFF file header.
    *)

    TWaveHeader = record
    // RIFF file header
    RIFF: array [0..3] of Char;          // = 'RIFF'
    FileSize: Integer;                   // = FileSize - 8
    RIFFType: array [0..3] of Char;      // = 'WAVE'
    // Format chunk
    FmtChunkId: array [0..3] of Char;    // = 'fmt'
    FmtChunkSize: Integer;               // = 16
    FormatTag: Word;                     // One of WAVE_FORMAT_XXX constants
    Channels: Word;                      // = 1 - mono = 2 - stereo
    SampleRate: Integer;
    BytesPerSecond: Integer;
    BlockAlign: Word;
    BitsPerSample: Word;                 // = 8, 16 or 32 Bits/sample
    // Data Chunk
    DataChunkId: array [0..3] of Char;   // = 'data'
    DataSize: Integer;   // Data size in bytes
  end;

  constructor TDemoWaveOut.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
  end;

  destructor  TDemoWaveOut.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TDemoWaveOut.Prepare;
  var
    Header : TWaveHeader;
  begin
    // This field serves the same purpose as  FEOF in TDemoWaveIn.
    EndOfInput := False;
    // Output components can sore data to TStream objects just like
    // input components can read data from them.
    if not FStreamAssigned then
    begin
      if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
      FStream := TAuFileStream.Create(FWideFileName, fmCreate or fmShareExclusive);
    end;
    // The FInput property holds a reference to an input component (TAuInput descendant)
    // where the output component reads data from.
    // We call the Init method to initialise the input component.
    FInput.Init;
    // Now fill the RIFF header
    Header.RIFF := 'RIFF';
    // The FInput.Size value is an input audio data full size.
    // There is some point to note regarding input components that don't know
    // the input size in advance (they return value of -1 in FInput.Size).
    // When dealing with Wav files we can count the number of bytes actually written
    // to the stream, when return to the header and rewrite the value.
    // In other codecs other tricks may be required (although most codecs handle
    // this situation easily).
    Header.FileSize := FInput.Size + 44;
    Header.RIFFType := 'WAVE';
    Header.FmtChunkId := 'fmt ';
    Header.FmtChunkSize := 16;
    Header.FormatTag := WAVE_FORMAT_PCM;
    // FInput.Channels - the number of channels
    Header.Channels := FInput.Channels;
    // FInput.SampleRate - the sample rate.
    Header.SampleRate := FInput.SampleRate;
    Header.BitsPerSample := FInput.BitsPerSample;
    Header.BlockAlign := (Header.BitsPerSample * Header.Channels) shr 3;
    Header.BytesPerSecond := Header.SampleRate * Header.BlockAlign;
    Header.DataChunkId := 'data';
    Header.DataSize := FInput.Size;
    // Now write the header to the output stream, ...
    FStream.Write(Header, 44);
    // ... and we are ready to write data.
  end;

  function TDemoWaveOut.DoOutput(Abort: Boolean) : Boolean;
  var
    P : Pointer;
    Bytes : LongWord;
  begin
    if Abort then
      EndOfInput := True;
    if EndOfInput then
    begin
      Result := False;
      Exit;
    end;
    Bytes := 1024;
    // This is a GetData interface that exposes (a part of) the input component
    // internal buffer. The GetData method provides no more data than there is
    // in the buffer currently. Sometimes you may want to specify that getting
    // some particular amount of data is strongly desirable. Use CopyData method
    // in that case. The CopyData method takes in a pointer to the buffer that you
    // provide for it and the buffer size. It fills the buffer with data and
    // returns the number of bytes actually written. Note that while
    // the CopyData method tries to return you as many bytes as you want, it is not
    // guaranteed to do so (there may be simply not enogh data in the input stream).
    FInput.GetData(P, Bytes);
    if Bytes = 0 then
    begin
      // The end of input is reached.
      Result := False;
      EndOfInput := True;
    end else
    begin
      Result := True;
      // Write the data to the output stream
      FStream.Write(P^, Bytes);
    end;
  end;

  procedure TDemoWaveOut.Done;
  var
    size : Integer;
  begin
    // We didn't know the input size from the begininning
    // So we write it now to the file header.
    if FInput.Size <= 0 then
    begin
      size := FStream.Position;
      FStream.Seek(40, soFromBeginning);
      FStream.Write(size, 4);
    end;
    // The Flush method closes the input
    FInput.Flush;
    if not FStreamAssigned then FStream.Free;
  end;

end.
