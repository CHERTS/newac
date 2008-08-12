(* $Id$ *)

unit PasPack;

interface

uses
  SysUtils, Classes, ACS_Procs, Golomb;

type

  TFileHeader = packed record
    Magic : array[0..3] of Char;
    Channels : Word;
    BitsPerSample : Word;
    SampleRate : Word;
    Version : Word;
    TotalFrames : LongWord;
    FramesInBlock : Word;
    VectorLen : Word;
    u, Beta : Single;
  end;

  TBlockHeader = packed record
    M : Word;
    BlockSize : Word;
  end;

  TPPEncoder = class
  private
    FileHeader : TFileHeader;
    A : array[0..1] of array of Integer;
    X0 : array[0..1] of array of SmallInt;
    HeaderWritten : Boolean;
    FileStream : TStream;
    FBuffer : PSmallInt;
    X, Y : array[0..1] of array of SmallInt;
    OutData : array of Integer;
    FBufferSize : LongWord;
    FramesRead : Word;
    procedure Deinterlace;
    procedure Recalculate(Channel, Offset : Integer);
    procedure Filter(Channel, Offset : Integer);
    procedure Decorrelate;
    procedure InitFilter;
  public
    constructor Create(const OutputStream : TStream);
    destructor Destroy;
    procedure Init(Channels, Samplerate : Word);
    procedure SetTweaks(p1, p2 : Single; l1, l2 : Word);
    procedure QueryBuffer(var Buffer : PSmallInt; var BufLength : LongWord);
    procedure Encode;
    procedure Close;
  end;

  TPPDecoder = class
  private
    FileHeader : TFileHeader;
    A : array[0..1] of array of Integer;
    X0 : array[0..1] of array of SmallInt;
    FileStream : TStream;
    FBuffer : PSmallInt;
    X, Y : array[0..1] of array of SmallInt;
    InData : array of Integer;
    FBufferSize : LongWord;
    FramesRead : LongWord;
    FSize : LongWord;
    function GetChannels : Word;
    function GetSampleRate : Word;
    procedure Interlace;
    procedure Recalculate(Channel, Offset : Integer);
    procedure Restore(Channel, Offset : Integer);
    procedure Decorrelate;
    procedure InitFilter;
  public
    constructor Create(const InputStream : TStream);
    destructor Destroy;
    function ReadHeader : Boolean;
    procedure Decode(var Buffer : PSmallInt; var BufLength : LongWord);
    property Channels : Word read GetChannels;
    property SampleRate : Word read GetSampleRate;
    property Size : LongWord read FSize;
  end;


implementation

  constructor TPPEncoder.Create;
  begin
    FileStream := OutputStream;
    HeaderWritten := False;
  end;

  procedure TPPEncoder.Init;
  begin
    FileHeader.Channels := Channels;
    FileHeader.SampleRate := Samplerate;
    FileHeader.BitsPerSample := 16;
    FileHeader.Magic := 'PPAK';
    FileHeader.Version := 1;
    FileHeader.TotalFrames := 0;
  end;

  procedure TPPEncoder.SetTweaks;
  var
    i : Integer;
  begin
    FileHeader.u := p1;
    FileHeader.Beta := p2;
    FileHeader.FramesInBlock := l1;
    FileHeader.VectorLen := l2;
    for i := 0 to FileHeader.Channels - 1 do
    begin
      SetLength(A[i], FileHeader.VectorLen);
      SetLength(X0[i], FileHeader.VectorLen);
      SetLength(X[i], SizeOf(SmallInt)*FileHeader.FramesInBlock + FileHeader.VectorLen);
      SetLength(Y[i], SizeOf(SmallInt)*FileHeader.FramesInBlock);
    end;
    SetLength(OutData, FileHeader.FramesInBlock*FileHeader.Channels);
    FBufferSize := FileHeader.FramesInBlock*(FileHeader.BitsPerSample div 8)*FileHeader.Channels;
    GetMem(FBuffer, FBufferSize);
  end;

  destructor TPPEncoder.Destroy;
  begin
    FreeMem(FBuffer);
    inherited;
  end;

  procedure TPPEncoder.QueryBuffer;
  begin
    if not HeaderWritten then
    begin
      Buffer := FBuffer;
      BufLength := Length(X0)*FileHeader.Channels*(FileHeader.BitsPerSample div 8);
      Exit;
    end;
    Buffer := FBuffer;
    BufLength := FBufferSize;
  end;

  procedure TPPEncoder.Encode;
  var
    i, j : Integer;
    P : PArrayOfByte;
    BS : LongWord;
    BlockHeader : TBlockHeader;
  begin
    if not HeaderWritten then
    begin
      FileStream.Write(FileHeader, SizeOf(FileHeader));
      Deinterlace;
      Decorrelate;
      for i := 0 to FileHeader.VectorLen - 1 do
        for j := 0 to FileHeader.Channels - 1 do
          X0[j][i] := X[j][i + FileHeader.VectorLen];
      InitFilter;
      for j := 0 to FileHeader.Channels - 1 do
        FileStream.Write(X0[j][0], Length(X0[j])*(FileHeader.BitsPerSample div 8));
      HeaderWritten := True;
      Exit;
    end;
    Deinterlace;
    Decorrelate;
    for i := 0 to FileHeader.FramesInBlock -1 do
      for j := 0 to FileHeader.Channels -1 do
      begin
        Filter(j, i);
        Recalculate(j, i);
      end;
    for j := 0 to FileHeader.Channels -1 do
      Move(X[j][FileHeader.FramesInBlock - FileHeader.VectorLen + 1], X0[j][0], FileHeader.VectorLen);
    for j := 0 to FileHeader.Channels -1 do
      for i := 0 to FileHeader.FramesInBlock -1 do
        OutData[j*FileHeader.FramesInBlock + i] := Y[j][i];
    GolombNewBlock(@OutData[0], FileHeader.FramesInBlock*FileHeader.Channels, P, BS, BlockHeader.M);
    BlockHeader.BlockSize := BS;
    FileStream.Write(BlockHeader, SizeOf(BlockHeader));
    FileStream.Write(P^, BlockHeader.BlockSize);
    Inc(FileHeader.TotalFrames, FileHeader.FramesInBlock);
    FreeMem(P);
  end;

  procedure TPPEncoder.Deinterlace;
  var
    P : PSmallInt;
    i, j : Integer;
  begin
    P := FBuffer;
    for i := 0 to FileHeader.VectorLen -1 do
      for j := 0 to FileHeader.Channels -1 do
       X[j][i] := X0[j][i];
    for i := 0 to FileHeader.FramesInBlock -1 do
      for j := 0 to FileHeader.Channels -1 do
      begin
        X[j][i + FileHeader.VectorLen] := P^;
        Inc(P);
      end;
  end;

  procedure TPPEncoder.Decorrelate;
  begin
  end;

  procedure TPPEncoder.Filter;
  var
    i : Integer;
    Sum : Integer;
  begin
    Sum := 0;
    for i := 0 to FileHeader.VectorLen - 1 do
      Sum := Sum + A[Channel][i]*X[Channel][i + Offset];
    Y[Channel][Offset] := X[Channel][FileHeader.VectorLen + Offset] - Sum;
  end;

  procedure TPPEncoder.Recalculate;
  begin
  end;

  procedure TPPEncoder.InitFilter;
  var
    i : Integer;
  begin
    for i := 0 to FileHeader.Channels - 1 do
    begin
      A[i][0] := -1;////0.25;//-1;
      A[i][1] := 2;//0.25;//4;
      A[i][2] := -2;//0.25;//-6;
      A[i][3] := 2;//0.25;//4;
    end;
  end;

  procedure TPPEncoder.Close;
  begin
    FileStream.Seek(12, soFromBeginning);
    FileStream.Write(FileHeader.TotalFrames, 4);
    FileStream.Seek(0, soFromEnd);
  end;

  constructor TPPDecoder.Create;
  begin
    FileStream := InputStream;
  end;

  destructor TPPDecoder.Destroy;
  begin
    FreeMem(FBuffer);
  end;

  function TPPDecoder.ReadHeader;
  var
    i : Integer;
  begin
    FileStream.Read(FileHeader, SizeOf(FileHeader));
    if FileHeader.Magic <> 'PPAK' then
    begin
      Result := False;
      Exit;
    end;
    FramesRead := 0;
    for i := 0 to FileHeader.Channels - 1 do
    begin
      SetLength(A[i], FileHeader.VectorLen);
      SetLength(X0[i], FileHeader.VectorLen);
      SetLength(X[i], SizeOf(SmallInt)*FileHeader.FramesInBlock + FileHeader.VectorLen);
      SetLength(Y[i], SizeOf(SmallInt)*FileHeader.FramesInBlock);
    end;
    SetLength(InData, FileHeader.FramesInBlock*FileHeader.Channels);
    FBufferSize := FileHeader.FramesInBlock*FileHeader.Channels*(FileHeader.BitsPerSample div 8);
    GetMem(FBuffer, FBufferSize);
    FSize := FileHeader.TotalFrames*FileHeader.Channels*FileHeader.BitsPerSample div 8;
    InitFilter;
    FramesRead := 0;
    Result := True;
  end;

  function TPPDecoder.GetChannels;
  begin
    Result := FileHeader.Channels;
  end;

  function TPPDecoder.GetSampleRate;
  begin
    Result := FileHeader.SampleRate;
  end;

  procedure TPPDecoder.Interlace;
  var
    i, j : Integer;
    P : PSmallInt;
  begin
    P := FBuffer;
    for i := 0 to FileHeader.FramesInBlock -1 do
      for j := 0 to FileHeader.Channels -1 do
      begin
        P^ := Y[j][i];
        Inc(P);
      end;
  end;

  procedure TPPDecoder.Decorrelate;
  begin
  end;

  procedure TPPDecoder.Recalculate;
  begin
  end;

  procedure TPPDecoder.InitFilter;
  var
    i : Integer;
  begin
    for i := 0 to FileHeader.Channels - 1 do
    begin
      A[i][0] := -1;////0.25;//-1;
      A[i][1] := 2;//0.25;//4;
      A[i][2] := -2;//0.25;//-6;
      A[i][3] := 2;//0.25;//4;
    end;
  end;
  
  procedure TPPDecoder.Restore;
  var
    i : Integer;
    Sum : Integer;
  begin
    Sum := 0;
    for i := 0 to FileHeader.VectorLen - 1 do
      Sum := Sum + A[Channel][i]*X[Channel][i + Offset];
    X[Channel][FileHeader.VectorLen + Offset] := X[Channel][FileHeader.VectorLen + Offset] + Sum;
    Y[Channel][Offset] := X[Channel][FileHeader.VectorLen + Offset];
  end;

  procedure TPPDecoder.Decode;
  var
    i, j : Integer;
    P : PSmallInt;
    BlockHeader : TBlockHeader;
    Block : PArrayOfByte;
    BlockLen : LongWord;
  begin
    if FramesRead = 0 then
    begin
      for j := 0 to FileHeader.Channels -1 do
        FileStream.Read(X0[j][0], Length(X0[j])*(FileHeader.BitsPerSample div 8));
      P := FBuffer;
      for i := 0 to FileHeader.VectorLen -1 do
        for j := 0 to FileHeader.Channels -1 do
        begin
          P^ := X0[j][i];
          Inc(P);
        end;
      Buffer := FBuffer;
      BufLength := FileHeader.VectorLen*FileHeader.Channels*(FileHeader.BitsPerSample div 8);
      Inc(FramesRead);
      Exit;
    end;
    if FramesRead > FileHeader.TotalFrames then
    begin
      Buffer := FBuffer;
      BufLength := 0;
      Exit;
    end;
    FileStream.Read(BlockHeader, SizeOf(BlockHeader));
    GetMem(Block, BlockHeader.BlockSize);
    BlockLen := BlockHeader.BlockSize;
    FileStream.Read(Block^, BlockLen);
    GolombDecodeBlock(@InData[0], FileHeader.FramesInBlock*FileHeader.Channels, Block, BlockHeader.BlockSize, BlockHeader.M);
    FreeMem(Block);
    for i := 0 to FileHeader.VectorLen -1 do
      for j := 0 to FileHeader.Channels -1 do
        X[j][i] := X0[j][i];
    for i := 0 to FileHeader.FramesInBlock -1 do
      for j := 0 to FileHeader.Channels -1 do
        X[j][i + FileHeader.VectorLen] := InData[FileHeader.FramesInBlock*j + i];
    Decorrelate;
    for j := 0 to FileHeader.Channels -1 do
      for i := 0 to FileHeader.FramesInBlock -1 do
      begin
        Restore(j, i);
        Recalculate(j, i);
      end;
    for j := 0 to FileHeader.Channels -1 do
      Move(X[j][FileHeader.FramesInBlock - FileHeader.VectorLen + 1], X0[j][0], FileHeader.VectorLen);
    Interlace;
    Buffer := FBuffer;
    BufLength := FBufferSize;
    Inc(FramesRead);
  end;
end.
