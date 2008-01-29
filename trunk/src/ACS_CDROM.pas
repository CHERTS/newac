(*
  This file is a part of New Audio Components package v. 1.2
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net

  Special thanks to Thomas Grelle <grelle@online.de> for improving this unit.
*)

(* $Id$ *)

unit ACS_CDROM;

(* Title: ACS_CDROM
    Delphi interface to read audio data from a CD-ROM. *)

interface

uses
  Windows, MMSystem, Classes, SysUtils, ACS_Classes, CDRip;

type
  (* Enum: TCDStatus
      The status of the CD-ROM drive.

    cdsNotReady - Drive is not ready.
    cdsReady - Drive is ready to play.
    cdsPlaying - Drive is already playing a disc.
    cdsPaused - Drive is paused.
  *)
  TCDStatus = (cdsNotReady, cdsReady, cdsPlaying, cdsPaused);
  (* Enum: TTrackType
      The type of the current track.

    ttAudio - Audio track
    ttData - Data track
  *)
  TTrackType = (ttAudio, ttData);
  (* Enum: TCDInfo
      General information about the disc in the drive.

    cdiNoDisc - No disc in the drive.
    cdiDiscAudio - An audio disc.
    cdiDiscData - A data disc.
    cdiDiscMixed - A mixed audio-data disc.
    cdiUnknown - A disc of unknown format.
  *)
  TCDInfo = (cdiNoDisc, cdiDiscAudio, cdiDiscData, cdiDiscMixed, cdiUnknown);


  TMCN = array[0..13] of Char;

  (* Struct: TCDMSF
     The standard measure of time duration (and byte length) when dealing
     with CD-DA.

     Properties:
       Minute - the number of full minutes in a track.
       Second - the number of full seconds in a track.
       Frame - the number of frames. One frame constitutes 1/75 of a second and 2352 bytes.
  *)

  TCDMSF = record
    Minute : Byte;
    Second : Byte;
    Frame : Byte;
  end;

  (* Struct: TCDTrackInfo
     Carries information about a CD_DA track.

     Properties:
     TrackLength : TCDMSF - length of the track in MSF format.
     TrackType : TTrackType - type of the track (possible values are ttAudio - for audio tracks and ttData for data tracks).
   *)
  TCDTrackInfo = record
    TrackLength : TCDMSF;
    TrackType : TTrackType;
  end;

  (* Struct: TCDPosition
     Represents the current reader position on disc.
     
     Properties:
     Track - the track number (valid values vary from 1 to 99).
     MSF - the position within a track in a MSF format.
   *)

  TCDPosition = record
    Track : Integer;
    MSF : TCDMSF;
  end;

const

  (* Constant: EndOfDisc
    This is a constant representing the logical position beyond the end of disc (very much like EOF for files). *)
  EndOfDisc : TCDPosition = (Track : 100; MSF : (Minute : 0; Second : 0; Frame : 0));
  CD_FRAMESIZE_RAW = 2352;
  BUF_SIZE = 75 * CD_FRAMESIZE_RAW;  // 75 frames - 1 sec

var
  AppPath : String;
  WinPath : String;

type

  TCDPlayer = class(TComponent)
  private
    FOpened : Integer;
    FCurrentDrive : Integer;
    _cd_fd : Integer;
    aux_ind : Integer;
    CDDrives : array[0..32] of Char;
    CDDNum : Integer;
    procedure OpenCD;
    procedure CloseCD;
    procedure ForceClose;
    function GetDiscInfo : TCDInfo;
    procedure GetTrackStart(Track : Integer; var MSF : TCDMSF);
    function GetMCN : TMCN;
    function GetMediaChanged : Boolean;
    function GetNumTracks : Integer;
    function GetPosition : TCDPosition;
    function GetTrackInfo(const vIndex : Integer) : TCDTrackInfo;
    procedure SetCurrentDrive(vDrive : Integer);
    function GetStatus : TCDStatus;
    procedure SetLVolume(aVolume : Word);
    procedure SetRVolume(aVolume : Word);
    function  GetLVolume : Word;
    function  GetRVolume : Word;
    function GetDriveLetter(anIndex : Integer) : Char;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseTray;
    procedure Eject;
    procedure Pause;
    procedure Play(Track : Integer); overload;
    procedure Play(PlayFrom, PlayTo : TCDPosition); overload;
    procedure Resume;
    procedure Stop;
    property DiscInfo : TCDInfo read GetDiscInfo;
    property DriveLetter[anIndex : Integer] : Char read GetDriveLetter;
    property MCN : TMCN read GetMCN;
    property MediaChanged : Boolean read GetMediaChanged;
    property Position : TCDPosition read GetPosition;
    property Status : TCDStatus read GetStatus;
    property TracksCount : Integer read GetNumTracks;
    property Tracks[const vIndex : Integer] : TCDTrackInfo read GetTrackInfo;
  published
    property DrivesCount : Integer read CDDNum;
    property CurrentDrive : Integer read FCurrentDrive write SetCurrentDrive;
    property LVolume : Word read GetLVolume write SetLVolume;
    property RVolume : Word read GetRVolume write SetRVolume;
  end;

  (* Class: TCDIn
     This component reads data from CD-DA directly. It is suitable for
     creating CD-rippers. Descends from <TAuInput>. Requires CDRip.dll. *)

  TCDIn = class(TAuInput)
  private
    FOpened : Integer;
    FCurrentDrive : Integer;
    FStartTrack, FEndTrack : Integer;
    FStartPos, FEndPos: TCDPosition;
    FRipSize : Integer;
    buf : array[1..BUF_SIZE] of Byte;
    BufSize : Integer;
    procedure OpenCD;
    procedure CloseCD;
    function GetStatus : TCDStatus;
    function GetNumTracks : Integer;
    function GetTrackInfo(const vIndex : Integer) : TCDTrackInfo;
    procedure SetST(Track : Integer);
    procedure SetET(Track : Integer);
    procedure SetSP(Pos : TCDPosition);
    procedure SetEP(Pos : TCDPosition);
    function GetSize : Integer;
    function GetInfo : TCDInfo;
    function GetDrivesCount : Integer;
    procedure SetCurrentDrive(Value : Integer);
    function GetDriveName : String;
  protected
    function GetTotalSamples : Int64; override;
    function GetTotalTime : LongWord; override;
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Procedure: Eject
      Ejects CD-ROM drive's tray. *)
    procedure Eject;
    (* Procedure: CloseTray
      Closes CD-ROM drive's tray. *)
    procedure CloseTray;
    (* Property: DiscInfo
      Read this property to get a <TCDInfo> about a disc in the drive. See how
      I can link to both the name of the enum and a member, such as
      <cdiNoDisc> or <cdiUnknown>. That's a function of the Enum keyword.
    *)
    property DiscInfo: TCDInfo read GetInfo;
    (* Property: Status
    Read Status to get a <TCDStatus> about the drive.
    *)
    property Status: TCDStatus read GetStatus;
    (* Property: Tracks
      Returns information about a track specified by its number. The possible
      values of indexes range from 1 to <TracksCount>. The information about a
      track is returned as <TCDTrackInfo> record.
    *)
    property Tracks[const vIndex : Integer] : TCDTrackInfo read GetTrackInfo;
    (* Property: TracksCount
      The number of tracks on the disc. *)
    property TracksCount : Integer read GetNumTracks;
    (* Property: DriveName
      The name of the current CD-ROM drive as returned by the drive unit. *)
    property DriveName : String read GetDriveName;
    (* Property: DrivesCount
      The total number of the CD-ROM drives detected in the system. *)
    property DrivesCount : Integer read GetDrivesCount;
    (* Property: StartPos
      Set this property to specify the starting position for data transfer in
      a <TCDPosition> format. *)
    property StartPos : TCDPosition read FStartPos write SetSP;
    (* Property: EndPos
      Set this property to specify the end position for data transfer in a
      <TCDPosition> format. There is a special constant <EndOfDisc> of type
      <TCDPosition> If you want to record from a certain point to the end of
      disc, set this property value to <EndOfDisc>. *)
    property EndPos : TCDPosition read FEndPos write SetEP;
    (* Property: CurrentDrive
      Use this property to set or get the number of the current CD-ROM drive.
      Possible values range from 0 to <DrivesCount> - 1. *)
    property CurrentDrive : Integer read FCurrentDrive write SetCurrentDrive;
    (*Property: StartTrack
      Set StartTrack to specify the starting position for data transfer at the
      beginning of the track identified by number. The tracks are numbered
      starting from 1. *)
    property StartTrack: Integer read FStartTrack write SetSt;
    (*Property: EndTrack
      Set EndTrack to specify the ending position for data transfer at the end
      of the track identified by number. If you want to get data from a single
      track, the end track number should be the same as the start track
      number. *)
    property EndTrack: Integer read FEndTrack write SetET;
  end;

  function MSFToStr(const MSF : TCDMSF) : String;
  procedure Frames2MSF(Frames : Integer; var MSF : TCDMSF);
  function MSF2Frames(const MSF : TCDMSF) : Integer;

implementation

  function MSFToStr(const MSF : TCDMSF) : String;
  var
    sep : String;
    sec, min : Integer;
  begin
    min := MSF.Minute;
    if MSF.Frame > 37 then
    begin
      sec := MSF.Second + 1;
      if sec = 60 then
      begin
        Inc(min);
        sec := 0;
      end;
    end
    else sec := MSF.Second;
    if sec<10 then sep := ':0'
    else sep := ':';
    Result := IntToStr(min) + sep + IntToStr(sec);
  end;

  procedure Frames2MSF(Frames : Integer; var MSF : TCDMSF);
  var
    Temp : Integer;
  begin
    Temp := Frames div 75;
    MSF.Minute := Temp div 60;
    MSF.Second := Temp mod 60;
    MSF.Frame := Frames mod 75;
  end;

  function MSF2Frames(const MSF : TCDMSF) : Integer;
  begin
    Result := ((MSF.Minute * 60) + MSF.Second) * 75 + MSF.Frame;
  end;

  constructor TCDPlayer.Create(AOwner: TComponent);
  var
    numaux, i : Integer;
    AuxCaps : TAuxCaps;
    Ch : Char;
    DriveName : String;
  begin
    inherited Create(AOwner);
    aux_ind := -1;
    // Looking for an auxiliary volume control device
    numaux := auxGetNumDevs;
    for i := 0 to numaux - 1 do
    begin
      auxGetDevCaps(i, @AuxCaps, SizeOf(AuxCaps));
      if AuxCaps.wTechnology = AUXCAPS_CDAUDIO then
      begin
        aux_ind := i;
        Break;
      end;
    end;
    CDDNum := 0;
    for Ch := 'A' to 'Z' do
    begin
      DriveName := Ch + ':\';
      if GetDriveType(@DriveName[1]) = DRIVE_CDROM then
      begin
        CDDrives[CDDNum] := Ch;
        Inc(CDDNum);
      end;
    end;
    FCurrentDrive := 0;
  end;

  destructor TCDPlayer.Destroy;
  begin
    while FOpened > 0 do CloseCD;
    inherited Destroy;
  end;

  procedure TCDPlayer.OpenCD;
  var
   mciSetParms : MCI_SET_PARMS;
   DevName : String;
   Cmd : String;
  begin
    if FOpened = 0 then
    begin
      DevName := CDDrives[FCurrentDrive] + '\:';
      Cmd := 'open ' + DevName + ' type cdaudio alias cd2 shareable';
      if mciSendString(@Cmd[1], nil, 0, 0) <> 0 then
            raise EAuException.Create('Cannot open MCI audiocd device');
      _cd_fd := mciGetDeviceID('cd2');
      mciSetParms.dwTimeFormat := MCI_FORMAT_MSF;
      mciSendCommand(_cd_fd, MCI_SET, MCI_SET_TIME_FORMAT, Integer(@mciSetParms));
    end;
    Inc(FOpened);
  end;

  procedure TCDPlayer.CloseCD;
  begin
    if FOpened <= 1 then
    mciSendCommand(_cd_fd, MCI_CLOSE, 0, 0);
    Dec(FOpened);
  end;

  procedure TCDPlayer.ForceClose;
  begin
    mciSendCommand(_cd_fd, MCI_CLOSE, 0, 0);
    FOpened:=0;
  end;

  function TCDPlayer.GetDriveLetter;
  begin
   if (anIndex >= 0) and (anIndex < Self.CDDNum) then
     Result := CDDrives[anIndex]
   else
     Result := #0;
  end;

  function TCDPlayer.GetLVolume;
  var
    Volume : LongWord;
  begin
    if aux_ind < 0 then
    begin
      Result := 0;
      Exit;
    end;
    auxGetVolume(aux_ind, @Volume);
    Result := Volume and $0000FFFF;
  end;

  function TCDPlayer.GetRVolume;
  var
    Volume : LongWord;
  begin
    if aux_ind < 0 then
    begin
      Result := 0;
      Exit;
    end;
    auxGetVolume(aux_ind, @Volume);
    Result := Volume shr 16;
  end;

  procedure TCDPlayer.SetCurrentDrive;
  begin
    if (vDrive >= 0) and (vDrive < CDDNum) then
     FCurrentDrive := vDrive;
  end;

  procedure TCDPlayer.SetLVolume;
  var
    Volume : LongWord;
  begin
    if aux_ind < 0 then Exit;
    auxGetVolume(aux_ind, @Volume);
    Volume := (Volume and $FFFF0000) or ($0000FFFF and aVolume);
    auxSetVolume(aux_ind, Volume);
  end;

  procedure TCDPlayer.SetRVolume;
  var
    Volume : LongWord;
  begin
    if aux_ind < 0 then Exit;
    auxGetVolume(aux_ind, @Volume);
    Volume := (Volume and $0000FFFF) or ($FFFF0000 and (aVolume shl 16));
    auxSetVolume(aux_ind, Volume);
  end;

  procedure TCDPlayer.GetTrackStart;
  var
    mciStatusParms : MCI_STATUS_PARMS;
  begin
    OpenCD;
    if Track <= GetNumTracks then
    begin
      mciStatusParms.dwItem := MCI_STATUS_POSITION;
      mciStatusParms.dwTrack := Track;
      mciSendCommand(_cd_fd, MCI_STATUS, MCI_STATUS_ITEM or MCI_TRACK,
         Integer(@mciStatusParms));
      MSF.Minute := MCI_MSF_MINUTE(mciStatusParms.dwReturn);
      MSF.Second := MCI_MSF_SECOND(mciStatusParms.dwReturn);
      MSF.Frame := MCI_MSF_FRAME(mciStatusParms.dwReturn);
    end else
    begin
      mciStatusParms.dwItem := MCI_STATUS_LENGTH;
      mciSendCommand(_cd_fd, MCI_STATUS, MCI_STATUS_ITEM, Integer(@mciStatusParms));
      MSF.Minute := MMSystem.mci_MSF_Minute(mciStatusParms.dwReturn);
      MSF.Second := MMSystem.mci_MSF_Second(mciStatusParms.dwReturn);
      MSF.Frame := MMSystem.mci_MSF_Frame(mciStatusParms.dwReturn);
    end;
    CloseCD;
  end;

  function TCDPlayer.GetNumTracks;
  var
    mciStatusParms : MCI_STATUS_PARMS;
  begin
    Result := 0;
    OpenCD;
   if GetStatus = cdsNotReady then
    begin
      ForceClose;
      Exit;
    end;
    mciStatusParms.dwItem := MCI_STATUS_NUMBER_OF_TRACKS;
    mciSendCommand(_cd_fd, MCI_STATUS, MCI_STATUS_ITEM, Integer(@mciStatusParms));
    Result := mciStatusParms.dwReturn;
    CloseCD;
  end;

  procedure TCDPlayer.CloseTray;
  begin
    OpenCD;
    mciSendCommand(_cd_fd, MCI_SET, MCI_SET_DOOR_CLOSED, 0);
    CloseCD;
  end;

  procedure TCDPlayer.Eject;
  begin
    OpenCD;
    if GetStatus in [cdsPlaying, cdsPaused] then
    begin
      ForceClose;
      raise EAuException.Create('Drive is not ready.');
    end;
    mciSendCommand(_cd_fd, MCI_SET, MCI_SET_DOOR_OPEN, 0);
    CloseCD;
  end;

  procedure TCDPlayer.Pause;
  begin
    OpenCD;
    if GetStatus = cdsPlaying then
    mciSendCommand(_cd_fd, MCI_PAUSE, 0, 0);
    CloseCD;
  end;

  procedure TCDPlayer.Play(Track : Integer);
  var
    MSF1, MSF2 : TCDMSF;
    mciPlayParms : MCI_PLAY_PARMS;
    res : LongWord;
    Error : String;
  begin
    OpenCD;
    if GetStatus = cdsNotReady then
    begin
      ForceClose;
      raise EAuException.Create('Device is not ready');
    end;
    if GetTrackInfo(Track).TrackType <> ttAudio then
    begin
      ForceClose;
      raise EAuException.Create('Trying to play a data track.');
    end;
    GetTrackStart(Track, MSF1);
    GetTrackStart(Track+1, MSF2);
    mciPlayParms.dwFrom := mci_Make_MSF(MSF1.Minute, MSF1.Second, MSF1.Frame);
    mciPlayParms.dwTo := mci_Make_MSF(MSF2.Minute, MSF2.Second, MSF2.Frame);
    res := mciSendCommand(_cd_fd, MCI_PLAY, MCI_FROM or MCI_TO, Integer(@mciPlayParms));
    if res <> 0 then
    begin
      SetLength(Error, 255);
      mciGetErrorString(res, @Error[1], 255);
      raise EAuException.Create(Error);
    end;
    CloseCD;
  end;

  procedure TCDPlayer.Play(PlayFrom, PlayTo : TCDPosition);
  var
    MSF, MSF1, MSF2 : TCDMSF;
    mciPlayParms : MCI_PLAY_PARMS;
    mciStatusParms : MCI_STATUS_PARMS;
  begin
    OpenCD;
    if GetStatus = cdsNotReady then
    begin
      ForceClose;
      raise EAuException.Create('Device is not ready');
    end;
    if (PlayFrom.Track in [1..GetNumTracks]) = False then
    begin
      ForceClose;
      raise EAuException.Create('Track out of range.');
    end;
    if PlayTo.Track < PlayFrom.Track then
    begin
      ForceClose;
      raise EAuException.Create('Track out of range.');
    end;
    GetTrackStart(PlayFrom.Track, MSF);
    Frames2MSF(MSF2Frames(MSF)+MSF2Frames(PlayFrom.MSF), MSF1);
    if PlayTo.Track = EndOfDisc.Track then
    begin
      mciStatusParms.dwItem := MCI_STATUS_LENGTH;
      mciSendCommand(_cd_fd, MCI_STATUS, MCI_STATUS_ITEM, Integer(@mciStatusParms));
      MSF.Minute := MMSystem.mci_MSF_Minute(mciStatusParms.dwReturn);
      MSF.Second := MMSystem.mci_MSF_Second(mciStatusParms.dwReturn);
      MSF.Frame := MMSystem.mci_MSF_Frame(mciStatusParms.dwReturn);
    end
    else GetTrackStart(PlayTo.Track, MSF);
    Frames2MSF(MSF2Frames(MSF)+MSF2Frames(PlayTo.MSF), MSF2);
    mciPlayParms.dwFrom := mci_Make_MSF(MSF1.Minute, MSF1.Second, MSF1.Frame);
    mciPlayParms.dwTo := mci_Make_MSF(MSF2.Minute, MSF2.Second, MSF2.Frame);
    mciSendCommand(_cd_fd, MCI_PLAY, MCI_FROM or MCI_TO, Integer(@mciPlayParms));
    CloseCD;
  end;

  procedure TCDPlayer.Resume;
  begin
    OpenCD;
    if GetStatus <> cdsPlaying then
    mciSendCommand(_cd_fd, MCI_RESUME, 0, 0);
    CloseCD;
  end;

  procedure TCDPlayer.Stop;
  begin
    OpenCD;
    mciSendCommand(_cd_fd, MCI_STOP, 0, 0);
    CloseCD;
  end;

  function TCDPlayer.GetPosition;
  var
    mciStatusParms : MCI_STATUS_PARMS;
    mciSetParms : MCI_SET_PARMS;
  begin
    OpenCD;
    mciSetParms.dwTimeFormat := MCI_FORMAT_TMSF;
    mciSendCommand(_cd_fd, MCI_SET, MCI_SET_TIME_FORMAT, Integer(@mciSetParms));
    mciStatusParms.dwItem := MCI_STATUS_POSITION;
    mciSendCommand(_cd_fd, MCI_STATUS, MCI_STATUS_ITEM, Integer(@mciStatusParms));
    Result.Track := mci_TMSF_Track(mciStatusParms.dwReturn);
    Result.MSF.Minute := mci_TMSF_Minute(mciStatusParms.dwReturn);
    Result.MSF.Second := mci_TMSF_Second(mciStatusParms.dwReturn);
    Result.MSF.Frame := mci_TMSF_Frame(mciStatusParms.dwReturn);
    mciSetParms.dwTimeFormat := MCI_FORMAT_MSF;
    mciSendCommand(_cd_fd, MCI_SET, MCI_SET_TIME_FORMAT, Integer(@mciSetParms));
    CloseCD;
  end;

  function TCDPlayer.GetTrackInfo;
  var
    MSF1, MSF2 : TCDMSF;
    mciStatusParms : MCI_STATUS_PARMS;
  begin
    OpenCD;
    if Status = cdsNotReady then
    begin
      CloseCD;
      Exit;
    end;
    if (vIndex in [1..GetNumTracks]) = False then
    begin
      ForceClose;
      raise EAuException.Create('Track out of range.');
    end;
    GetTrackStart(vIndex, MSF1);
    GetTrackStart(vIndex+1, MSF2);
    Frames2MSF(MSF2Frames(MSF2)-MSF2Frames(MSF1), Result.TrackLength);
    mciStatusParms.dwTrack := vIndex;
    mciStatusParms.dwItem := MCI_CDA_STATUS_TYPE_TRACK;
    mciSendCommand(_cd_fd, MCI_STATUS, MCI_TRACK or MCI_STATUS_ITEM, Integer(@mciStatusParms));
    if mciStatusParms.dwReturn = MCI_CDA_TRACK_AUDIO then Result.TrackType := ttAudio
    else Result.TrackType := ttData;
    CloseCD;
  end;

  function TCDPlayer.GetStatus;
  var
    mciStatusParms : MCI_STATUS_PARMS;
    res : Integer;
  begin
    Result := cdsReady;
    OpenCD;
    mciStatusParms.dwItem := MCI_STATUS_MEDIA_PRESENT;
    res := mciSendCommand(_cd_fd, MCI_STATUS, MCI_STATUS_ITEM, Integer(@mciStatusParms));
    if res <> 0 then Exit;
    if mciStatusParms.dwReturn = 0 then
    begin
      Result := cdsNotReady;
      Exit;
    end;
    mciStatusParms.dwItem := MCI_STATUS_MODE;
    res := mciSendCommand(_cd_fd, MCI_STATUS, MCI_STATUS_ITEM, Integer(@mciStatusParms));
    if res <> 0 then Exit;
    if mciStatusParms.dwReturn = MCI_MODE_NOT_READY then
    Result := cdsNotReady else
    if mciStatusParms.dwReturn = MCI_MODE_PAUSE then
    Result := cdsPaused else
    if mciStatusParms.dwReturn = MCI_MODE_PLAY then
    Result := cdsPlaying;
    CloseCD;
  end;

  function TCDPlayer.GetDiscInfo;
  var
    i : Integer;
    TI : TCDTrackInfo;
    mciStatusParms : MCI_STATUS_PARMS;
  begin
    OpenCD;
    mciStatusParms.dwItem := MCI_STATUS_MEDIA_PRESENT;
    mciSendCommand(_cd_fd, MCI_STATUS, MCI_STATUS_ITEM, Integer(@mciStatusParms));
    if mciStatusParms.dwReturn = 0 then
    begin
      Result := cdiNoDisc;
      Exit;
    end;
    Result := cdiUnknown;
    for i := 1 to TracksCount do
    begin
      TI := GetTrackInfo(i);
      if (TI.TrackType = ttAudio) then
      case Result of
        cdiDiscData : Result := cdiDiscMixed;
        cdiUnknown : Result := cdiDiscAudio;
      end else
      case Result of
        cdiDiscAudio : Result := cdiDiscMixed;
        cdiUnknown : Result := cdiDiscMixed;
      end;
    end;
    CloseCD;
  end;

  function TCDPlayer.GetMCN;
  begin
    FillChar(Result, 14, Byte('0'));
  end;

  function TCDPlayer.GetMediaChanged;
  begin
    Result := True;
  end;

    procedure TCDIn.OpenCD;
  begin
    if FOpened = 0 then
    begin
      CR_SetActiveCDROM(FCurrentDrive);
    end;
    Inc(FOpened);
  end;

  procedure TCDIn.CloseCD;
  begin
 //   if FOpened <=1 then CR_DeInit;
    if FOpened > 0 then Dec(FOpened);
  end;

  function TCDIn.GetInfo;
  var
    i, en : Integer;
    TE : TTOCENTRY;
  begin
    Result := cdiUnknown;
    if not (csDesigning in ComponentState) then
    begin
      OpenCD;
      if CR_IsMediaLoaded(en) = RES_OK then
      begin
        if en <> 0 then
        begin
          Result := cdiNoDisc;
          Exit;
        end;
      end;
      if CR_ReadToc <> RES_OK then Exit;
       en := CR_GetNumTocEntries;
      for i := 0 to en-1 do
      begin
        TE := GetTOCEntry(i);
        if TE.btFlag = AUDIOTRKFLAG then
        begin
          case Result of
            cdiUnknown : Result := cdiDiscAudio;
            cdiDiscData : Result := cdiDiscMixed;
          end;
        end else
        begin
          case Result of
            cdiUnknown : Result := cdiDiscData;
            cdiDiscAudio : Result := cdiDiscMixed;
          end;
        end;
      end;
      CloseCD;
    end;
  end;

  function TCDIn.GetStatus;
  var
    ms : Integer;
    AP : LongBool;
  begin
    if not (csDesigning in ComponentState) then
    begin
      OpenCD;
      ms := 0;
      CR_IsMediaLoaded(ms);
      AP := CR_IsAudioPlaying;
      CloseCD;
      Result := cdsNotReady;
      if ms <> 0 then Exit;
      if AP then Result := cdsPlaying
      else Result := cdsReady;
    end else Result := cdsNotReady;
  end;

  function TCDIn.GetNumTracks;
  begin
    OpenCD;
    if CR_ReadToc = RES_OK then
    Result := CR_GetNumTocEntries
    else Result := -1;
    CloseCD;
  end;

  function TCDIn.GetTrackInfo;
  var
    TE1, TE2 : TTOCENTRY;
    Frames : Integer;
  begin
    if (vIndex in [1..GetNumTracks]) = False then
    raise EAuException.Create('Track out of range.');
    OpenCD;
    TE1 := GetTOCEntry(vIndex - 1);
    TE2 := GetTOCEntry(vIndex);
    if TE1.btFlag = AUDIOTRKFLAG then
    Result.TrackType := ttAudio
    else Result.TrackType := ttData;
    Frames := TE2.dwStartSector - TE1.dwStartSector;
    Frames2MSF(Frames, Result.TrackLength);
    CloseCD;
  end;

  procedure TCDIn.SetST;
  begin
    if Self.Busy then raise EAuException.Create('The component is busy');
    FStartTrack := Track;
    FStartPos.Track := FStartTrack;
    FillChar(FStartPos.MSF, SizeOf(FStartPos.MSF), 0);
  end;

  procedure TCDIn.SetET;
  begin
    if Self.Busy then raise EAuException.Create('The component is busy');
    FEndTrack := Track;
    FEndPos.Track := FEndTrack+1;
    FillChar(FEndPos.MSF, SizeOf(FEndPos.MSF), 0);
  end;

  procedure TCDIn.SetSP;
  begin
    if Self.Busy then raise EAuException.Create('The component is busy');
    FStartPos := Pos;
  end;

  procedure TCDIn.SetEP;
  begin
    if Self.Busy then raise EAuException.Create('The component is busy');
    FEndPos := Pos;
    if Pos.Track = EndOfDisc.Track then FEndPos.Track := TracksCount + 1;
  end;

  constructor TCDIn.Create;
  begin
    inherited Create(AOwner);
    AppPath := ExtractFilePath(ParamStr(0));
    if AppPath[length(AppPath)] <> '\' then AppPath := AppPath + '\';
    if not (csDesigning in ComponentState) then
    begin
      CDRIPInit(AppPath);
      if not CDRipLoaded then
        raise EAuException.Create(CDRipPath + ' could not be loaded.');
    end;
  end;

  destructor TCDIn.Destroy;
  begin
    if not (csDesigning in ComponentState) then
      CR_DeInit;
    inherited Destroy;
  end;

  function TCDIn.GetBPS : LongWord;
  begin
    Result := 16;
  end;

  function TCDIn.GetCh;
  begin
    Result := 2;
  end;

  function TCDIn.GetSR;
  begin
    Result := 44100;
  end;

  function TCDIn.GetSize;
  var
    Sect1, Sect2 : Integer;
    TE : TTOCENTRY;
  begin
    if Busy then
    begin
      Result := FRipSize;
      Exit;
    end;
    OpenCD;
    CR_ReadToc;
    TE := GetTocEntry(FStartPos.Track-1);
    Sect1 := TE.dwStartSector;
    Sect1 := Sect1 + MSF2Frames(FStartPos.MSF);
    TE := GetTocEntry(FEndPos.Track-1);
    Sect2 := TE.dwStartSector;
    Sect2 := Sect2 + MSF2Frames(FEndPos.MSF);
    CloseCD;
    Result := (Sect2 - Sect1)*CD_FRAMESIZE_RAW;
  end;

  procedure TCDIn.InitInternal;
  var
    Sect1, Sect2 : Integer;
    TE : TTOCENTRY;
  begin
    if Busy then raise EAuException.Create('The component is busy');
    if Status = cdsNotReady then raise EAuException.Create('The drive is not ready');
    if (FStartPos.Track in [1..GetNumTracks]) = False then
    raise EAuException.Create('The start track out of range');
    if GetTrackInfo(FStartPos.Track).TrackType = ttData then
    raise EAuException.Create('Trying to rip a data track');
    if (FEndPos.Track in [1..GetNumTracks+1]) = False then
    raise EAuException.Create('The end track out of range');
    FSize := GetSize;
    Busy := True;
    BufStart := 1;
    BufEnd := 0;
    FPosition := 0;
    OpenCD;
    TE := GetTocEntry(FStartPos.Track-1);
    Sect1 := TE.dwStartSector;
    Sect1 := Sect1 + MSF2Frames(FStartPos.MSF);
    TE := GetTocEntry(FEndPos.Track-1);
    Sect2 := TE.dwStartSector;
    Sect2 := Sect2 + MSF2Frames(FEndPos.MSF);
    FRipSize := (Sect2 - Sect1)*CD_FRAMESIZE_RAW;
    BufSize := BUF_SIZE;
    CR_OpenRipper(@BufSize, Sect1, Sect2);
  end;

  procedure TCDIn.FlushInternal;
  begin
    CR_CloseRipper;
    CloseCD;
    Busy := False;
    FSize := 0;
  end;

  procedure TCDIn.GetDataInternal;
  var
    Abort : LongBool;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      BufStart := 1;
      Abort := False;
      CR_RipChunk(@buf[1], @BufEnd, Abort);
    end;
    if Bytes > (BufEnd - BufStart + 1) then
      Bytes := BufEnd - BufStart + 1;
    Buffer := @Buf[BufStart];
    Inc(BufStart, Bytes);
    Inc(FPosition, Bytes);
  end;

  procedure TCDIn.SetCurrentDrive;
  begin
    if not (csDesigning in ComponentState) then
    begin
      OpenCD;
      if Value in [0..CR_GetNumCDROM-1] then
      FCurrentDrive := Value;
      CloseCD;
    end else FCurrentDrive :=  0;
  end;

  function TCDIn.GetDrivesCount;
  begin
    if not (csDesigning in ComponentState) then
    begin
      OpenCD;
      Result := CR_GetNumCDROM;
      CloseCD;
    end else Result := 0;
  end;

  function TCDIn.GetDriveName;
  var
    CDP : TCDROMPARAMS;
  begin
    if not (csDesigning in ComponentState) then
    begin
      OpenCD;
      CR_GetCDROMParameters(@CDP);
      Result := String(CDP.lpszCDROMID);
      CloseCD;
    end else Result := '';
  end;

  procedure TCDIn.Eject;
  begin
    if Busy then raise EAuException.Create('The component is busy');
    CR_EjectCD(True);
  end;

  procedure TCDIn.CloseTray;
  begin
    CR_EjectCD(False);
  end;

  function TCDIn.GetTotalSamples;
  begin
    Result := GetSize div 4;
  end;

  function TCDIn.GetTotalTime;
  begin
    Result := Round(GetTotalSamples/44100);
  end;


end.
