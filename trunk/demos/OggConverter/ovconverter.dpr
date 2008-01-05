(*
  The ovencode API demo program.
  (c) 2007 Andrei Borovsky, <anb@symmetrica.net>
 *)

program ovconverter;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  Math,
  ovencode;

const

  WAVE_FORMAT_PCM = 1;

  LookingForRIFF = 0;
  LookingForWave = 1;
  LookingForFMT = 2;
  LookingForFACT = 3;
  LookingForDATA = 4;


type

  TAudioInfo = record
    BitsPerSample : Integer;
    SampleRate : Integer;
    Channels : Integer;
    TotalSamples : Integer;
  end;

  TProgramOptions = record
    InputFile : String;
    OutputFile : String;
    UseQuality : Boolean;
    Quality : Single;
    MinBitrate : Integer;
    MaxBitrate : Integer;
    NomBitrate : Integer;
    Artist : String;
    Album : String;
    Title : String;
    Tracknum : String;
    Date : String;
    Genre : String;
  end;

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

  // This is a wave header reader. Most demos cheat at wave headers, this one doesn't. 

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

var
  AudioInfo : TAudioInfo;
  FileIn, FileOut : TFileStream;
  InFileName, OutFileName : String;
  CurrentProgress : Integer;
  enc_state : oe_enc_opt;
  Artist : PChar = 'GR';
  ProgramOptions : TProgramOptions;

  procedure PrintUsage;
  begin
    WriteLn(Format('usage: %s [options] <inupt file> <output file>', [ParamStr(0)]));
    WriteLn('optinons:');
    WriteLn;
    WriteLn('-q <quality> - set output quality in the range of [1 - 10]');
    WriteLn;
    WriteLn('-nombr <bitrate> - set nominal output bitrate (use thins option instead of -q)');
    WriteLn;
    WriteLn('-minbr <bitrate> - set minimum desired output bitrate (you may use thins option with -nombr)');
    WriteLn;
    WriteLn('-maxbr <bitrate> - set  maximum desired output bitrate (you may use thins option with -nombr)');
    WriteLn;
    WriteLn('-artist <"artist"> - add an "artist" tag to the output file');
    WriteLn;
    WriteLn('-album <"album"> - add an "album" tag to the output file');
    WriteLn;
    WriteLn('-title <"title"> - add a "title" tag to the output file');
    WriteLn;
    WriteLn('-tracknum <"tracknum"> - add a "tracknum" tag to the output file');
    WriteLn;
    WriteLn('-date <"date"> - add a "date" tag to the output file');
    WriteLn;
    WriteLn('-h - show this help screen');
  end;

  // Command line options parser

  procedure ParseProgramOptions;
  type
    TParserState = (read_unknown, read_q, read_nombr,
                    read_minbr, read_maxbr, read_artist, read_album, read_title,
                    read_tracknum, read_date, read_genre);
  var
    ParserState : TParserState;
    i : Integer;
    S : String;
  begin
    if ParamCount < 2 then
    begin
      PrintUsage;
      Halt(0);
    end;
    ParserState := read_unknown;
    for i := 1 to ParamCount do
    begin
      S := ParamStr(i);
      case ParserState of
        read_unknown :
        begin
          if S = '-q' then ParserState := read_q
          else
          if S = '-nombr' then ParserState := read_nombr
          else
          if S = '-minbr' then ParserState := read_minbr
          else
          if S = '-maxbr' then ParserState := read_maxbr
          else
          if S = '-artist' then ParserState := read_artist
          else
          if S = '-album' then ParserState := read_album
          else
          if S = '-title' then ParserState := read_title
          else
          if S = '-tracknum' then ParserState := read_tracknum
          else
          if S = '-date' then ParserState := read_date
          else
          if S = '-genre' then ParserState := read_genre
          else
          if S = '-h' then
          begin
            PrintUsage;
            Halt(0);
          end
          else
          begin
            if ProgramOptions.InputFile = '' then
               ProgramOptions.InputFile := S
            else
            if ProgramOptions.OutputFile = '' then
               ProgramOptions.OutputFile := S
            else
            begin
              WriteLn('Failed to parse options, use -h to get help.');
              Halt(1);
            end;
          end;
        end;
        read_q :
        begin
          if ProgramOptions.NomBitrate <> 0 then
          begin
            WriteLn('Use either -q or -nombr, not both.');
            Halt(1);
          end;
          ProgramOptions.Quality := StrToInt(S)/10;
          ParserState := read_unknown;
        end;
        read_nombr :
        begin
          if ProgramOptions.Quality <> 0 then
          begin
            WriteLn('Use either -q or -nombr, not both.');
            Halt(1);
          end;
          ProgramOptions.NomBitrate := StrToInt(S);
          ParserState := read_unknown;
        end;
        read_minbr :
        begin
          ProgramOptions.MinBitrate := StrToInt(S);
          ParserState := read_unknown;
        end;
        read_maxbr :
        begin
          ProgramOptions.MaxBitrate := StrToInt(S);
          ParserState := read_unknown;
        end;
        read_artist :
        begin
          ProgramOptions.Artist := S;
          ParserState := read_unknown;
        end;
        read_album :
        begin
          ProgramOptions.Album := S;
          ParserState := read_unknown;
        end;
        read_title :
        begin
          ProgramOptions.Title := S;
          ParserState := read_unknown;
        end;
        read_tracknum :
        begin
          ProgramOptions.Tracknum := S;
          ParserState := read_unknown;
        end;
        read_date :
        begin
          ProgramOptions.Date := S;
          ParserState := read_unknown;
        end;
        read_genre :
        begin
          ProgramOptions.Genre := S;
          ParserState := read_unknown;
        end;
      end;
    end;
  end;

  // Just to keep the tag builder happy

  function StrToPChar(const S : String) : PChar;
  begin
    if S <> '' then
      Result := @S[1]
    else
      Result :=  nil;
  end;

  procedure StartEnc(cb_data : Pointer); stdcall;
  begin
    WriteLn('Input file parameters -');
    WriteLn('sample rate: ', AudioInfo.SampleRate);
    WriteLn('channels: ', AudioInfo.Channels);
    WriteLn('bits per sample: ', AudioInfo.BitsPerSample);
    WriteLn('total samples: ', AudioInfo.TotalSamples);
    WriteLn('Started encoding');
    WriteLn('Progress:');
  end;

  function ReadInput(cb_data : Pointer; buf : Pointer; bufsize : Integer) : Integer stdcall;
  begin
    Result := FileIn.Read(buf^, bufsize);
  end;

  procedure WriteOutput(cb_data : Pointer; buf : Pointer; bufsize : Integer); stdcall;
  begin
    FileOut.Write(buf^, bufsize);
  end;

  procedure Progress(cb_data : Pointer; totalsamples : Integer; samples : Integer) stdcall;
  var
    Pr : Integer;
  begin
    Pr := Floor(samples/totalsamples*400/5);
    if Pr > CurrentProgress then
    begin
      Write('.');
      CurrentProgress := Pr;
    end;
  end;

  procedure Error(cb_data : Pointer; errormessage : PChar); stdcall;
  begin
    WriteLn('ENCODER ERROR: ', errormessage);
  end;

  procedure EndEncode(cb_data : Pointer; rate : Integer;
                                samples : Integer; bytes : Integer) stdcall;
  begin
    WriteLn;
    WriteLn('Done encoding');
    WriteLn('samples: ', samples);
    WriteLn('rate : ', rate);
  end;

begin
  ParseProgramOptions;
  if ProgramOptions.InputFile = '' then
  begin
    WriteLn('No input file name is given.');
    Halt(1);
  end;
  if ProgramOptions.OutputFile = '' then
  begin
    WriteLn('No output file name is given.');
    Halt(1);
  end;
  FileIn := TFileStream.Create(ProgramOptions.InputFile, fmOpenRead);
  if not ReadRIFFHeader(FileIn, AudioInfo) then
  begin
    WriteLn('Failed to read input file header');
    Halt(1);
  end;
  if (AudioInfo.TotalSamples = 0) or (AudioInfo.BitsPerSample = 0)
  or (AudioInfo.Channels = 0) or (AudioInfo.SampleRate = 0) then
  begin
    WriteLn('Input file is wrong type or corrupt');
    Halt(1);
  end;
  FileOut := TFileStream.Create(ProgramOptions.OutputFile, fmCreate);
  enc_state := oe_encode_state_init;
  oe_set_callbacks(enc_state, nil, StartEnc, ReadInput, WriteOutput, Progress,
                        EndEncode, Error);
  oe_set_serialno(enc_state, Round(Random*10000000));
  oe_add_comments(enc_state, StrToPChar(ProgramOptions.Artist), StrToPChar(ProgramOptions.Album),
                  StrToPChar(ProgramOptions.Title), StrToPChar(ProgramOptions.Tracknum),
                  StrToPChar(ProgramOptions.Date), StrToPChar(ProgramOptions.Genre));
  oe_set_audio_params(enc_state, AudioInfo.SampleRate, AudioInfo.Channels, AudioInfo.BitsPerSample);
  if ProgramOptions.Quality = 0 then
  begin
    if ProgramOptions.MinBitrate = 0 then
       ProgramOptions.MinBitrate := -1;
    if ProgramOptions.MaxBitrate = 0 then
       ProgramOptions.MaxBitrate := -1;
    if ProgramOptions.NomBitrate <> 0 then
    begin
      oe_set_bitrates(enc_state, ProgramOptions.NomBitrate, ProgramOptions.MinBitrate, ProgramOptions.MaxBitrate);
      oe_set_managed(enc_state, True);
    end
    else
    begin
      WriteLn('No quality hint is given, setting quality to the default');
      oe_set_quality(enc_state, 0.2);
    end;
  end else
    oe_set_quality(enc_state, ProgramOptions.Quality);
  oe_set_total_samples(enc_state, AudioInfo.TotalSamples);
  oe_encode(enc_state);
  oe_encode_state_free(enc_state);
  FileIn.Free;
  FileOut.Free;
end.
