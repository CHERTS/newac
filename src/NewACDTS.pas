(* $Id$ *)

unit NewACDTS;

interface

uses
  Windows, Classes, SysUtils, ACS_Classes, ACS_Procs, ACS_Types, libdca;

type

  TDTSIn = class(TAuFileIn)
  private
    state : pdca_state;
    _Buf : array of SmallInt;
    FrameBuf : array of Byte;
    FrameSize : Integer;
    _BufSize : Integer;
    _SampleSize : Word;
    _StartSample, _StartFrom : LongWord;
    Offset, BufEnd : Integer;
    BlockCount, CurrentBlock : Integer;
    FBitRate : LongWord;
    FFlags : Integer;
    function ReadFrame : Boolean;
    function GetBitrate : LongWord;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
   (* Property: ReportSize
       Due to internal limitations the component cannot report the exact audio
       stream size when dealing with some compression formats. Setting
       ReportSize to False forces the component not to report the audio stream
       size at all. Set this property to False if you are saving AVI audio
       data as a .wav file. *)
    property ReportSize : Boolean read FReportSize write FReportSize;
    (* Property: HasAudio
       Read this property to determine the bit rate fo the DTS file.
    *)
    property BitRate : LongWord read GetBitrate;
  published
    property EndSample;
    property StartSample;
  end;


implementation

  function TDTSIn.ReadFrame;
  var
    CurPos : Int64;
    tmpbuf : array[0..13] of Byte;
    i : Integer;
    sample_rate, bit_rate, frame_length : Integer;
    ChanInfo : Integer;
  begin
    Result := False;
    CurPos := FStream.Position;
    for i := 0 to 255 do
    begin
      FStream.Seek(i+CurPos, soFromBeginning);
      FStream.Read(tmpbuf, 14);
      FrameSize := dca_syncinfo(state, tmpbuf, FFlags, sample_rate, bit_rate, frame_length);
      if FrameSize <> 0 then
      begin
        Result := True;
        FSR := sample_rate;
        FBitRate := bit_rate;
        FBPS := 16;
        SetLength(FrameBuf, FrameSize);
        Move(tmpbuf, FrameBuf[1], 14);
        FStream.Read(FrameBuf[15], FrameSize-14);
        ChanInfo := FFlags and DCA_CHANNEL_MASK;
        case ChanInfo of
          0 : FChan := 1;
          1,2,3,4 : FChan := 2;
          5,6 : FChan := 3;
          7,8 : FChan := 4;
          9 : FChan := 5;
          10 : FChan := 6;
        end;
          if (FFlags and DCA_LFE) <> 0 then
            Inc(FChan);
        Break;
      end;
    end;
  end;

  procedure TDTSIn.OpenFile;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      LoadDCALib;
      FValid := False;
      if not LibDCALoaded then
      raise EAuException.Create(LibDCAPath + ' library could not be loaded.');
      FValid := False;
      if not FStreamAssigned then
      try
        FStream := TAuFileStream.Create(FWideFileName, fmOpenRead or fmShareDenyWrite);
      except
        raise EAuException.Create('Failed to open stream');
      end;
      state := dca_init(0);
      CurrentBlock := 0;
      FValid := ReadFrame;
      if FValid = False then
      begin
        OpenCS.Leave;
        raise Exception.Create('');
      end;
      FSize := -1;
      _SampleSize := (FBPS div 8) * FChan;
      FSeekable := False;
      Inc(FOpened);
      _Buf := nil;
      _BufSize := 0;
      Offset := 0;
      BufEnd := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;


end.
