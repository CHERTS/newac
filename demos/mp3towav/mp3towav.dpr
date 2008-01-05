program mp3towav;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, mp3decdr;

type

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

  const
    BUF_SIZE = $1000;

  var
    IFS, OFS : TFileStream;
    mp3dec : mp3dec_t;
    SampleRate, SampleSize, Channels, Samples, Res, Size : Integer;
    WaveHeader  : TWaveHeader;
    Buf : array[0..BUF_SIZE-1] of Byte;
    BytesUsed : LongWord;
    InputFile, OutputFile : String;

  //callbacks

  function Seek(cb_data : Pointer; offset : Integer; origin : Integer) : Integer; cdecl;
  var
    SOR : Word;
  begin
    case origin of
      SEEK_CUR : SOR := soFromCurrent;
      SEEK_END : SOR := soFromEnd;
      SEEK_SET : SOR := soFromBeginning;
    end;
    Result := IFS.Seek(offset, SOR);
  end;

  function read(cb_data : Pointer; buf : Pointer; bufsize : Integer) : Integer; cdecl;
  var
    OldPos : Int64;
  begin
    OldPos := IFS.Position;
    if OldPos >= IFS.Size then
    begin
      Result := 0;
      Exit;
    end;
    Result := IFS.Read(buf^, bufsize);
    if IFS.Position > IFS.Size then Result := IFS.Size - OldPos;
  end;

  function close(cb_data : Pointer) : Integer; cdecl;
  begin
    if IFS <> nil then IFS.Free;
    Result := 0;
  end;


begin
  { TODO -oUser -cConsole Main : Insert code here }

  if ParamCount < 2 then
  begin
    WriteLn('Usage: mp32wav <input_file.mp3> <output_file.wav>');
    Halt(0);
  end;

  InputFile := ParamStr(1);
  OutputFile := ParamStr(2);

  IFS := TFileStream.Create(InputFile, fmOpenRead);
  IFS.Seek(0, soFromEnd);
  Size := IFS.Position;
  IFS.Seek(0, soFromBeginning);
  OFS := TFileStream.Create(OutputFile, fmCreate or fmOpenWrite);
  mp3dec := mp3dec_init;
  mp3dec_set_config(mp3dec, MP3DEC_CONFIG_AUTO, MP3DEC_CONFIG_16BIT);
  res := mp3dec_init_file(mp3dec, nil, seek, read, close, Size, 0);
  if res <> MP3DEC_RETCODE_OK then
  Halt(res);
  mp3dec_get_stream_info(mp3dec, Channels, SampleRate, SampleSize, Samples);
  Move('RIFF', WaveHeader.RIFF, 4);
  WaveHeader.FileSize := Samples*SampleSize + SizeOf(WaveHeader);
  Move('WAVE', WaveHeader.RIFFType, 4);
  Move('fmt ', WaveHeader.FmtChunkId, 4);
  WaveHeader.FmtChunkSize := 16;
  WaveHeader.FormatTag := 1;
  WaveHeader.Channels := Channels;
  WaveHeader.SampleRate := SampleRate;
  WaveHeader.BytesPerSecond := SampleRate*Channels*2;
  WaveHeader.BlockAlign := SampleSize;
  WaveHeader.BitsPerSample := (SampleSize div Channels)*8;
  Move('data', WaveHeader.DataChunkId, 4);
  WaveHeader.DataSize := Samples*SampleSize;
  OFS.Write(WaveHeader, SizeOf(WaveHeader));
  WriteLn('Converting, please wait...');
  res := mp3dec_decode(mp3dec, @buf[0], BUF_SIZE, bytesused);
  while (bytesused <> 0) and (res = MP3DEC_RETCODE_OK) do
  begin
    OFS.Write(buf, bytesused);
    res := mp3dec_decode(mp3dec, @buf[0], BUF_SIZE, bytesused);
  end;
  mp3dec_uninit(mp3dec);
  OFS.Free;
end.
