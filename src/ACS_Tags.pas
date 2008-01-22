(*
  This file is a part of New Audio Components package 1.4
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_Tags;

interface

uses
  Windows, Classes;

type

  TTagValueInfo = record
    Value: Variant;
  end;
  PTagValueInfo = ^TTagValueInfo;

  { class TAuTags }

  TAuTags = class(TPersistent)
  private
    FValues: TStrings;
    FEmpty: Boolean;
    FChanged: Boolean;

    function GetIdCount: Integer;
    function GetId(Index: Integer): String;
    function GetValue(const Id: String): Variant;
    procedure SetValue(const Id: String; const Value: Variant);
    function GetAsInteger(const Id: String): Integer;
    function GetAsString(const Id: String): String;
    function GetAsWideString(const Id: String): WideString;
  protected
    function AddId(const Id: String): Boolean;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure Clear;

    function ReadFromFile(InFile: HFILE): Boolean; virtual;
    function WriteToFile(OutFile: HFILE): Boolean; virtual;

    property Empty: Boolean read FEmpty;
    property Changed: Boolean read FChanged;
    property IdCount: Integer read GetIdCount;
    property Ids[Index: Integer]: String read GetId;
    property Values[const Id: String]: Variant read GetValue write SetValue; default;
    property AsInteger[const Id: String]: Integer read GetAsInteger;
    property AsString[const Id: String]: String read GetAsString;
    property AsWideString[const Id: String]: WideString read GetAsWideString;
  end;

  { class TId3Tags }

  TId3Tags = class(TAuTags)
  public
    constructor Create; override;
  end;

  { class TId3v1Tags }

  TId3v1TagsGenreId = 0 .. 147;

  TId3v1Tags = class(TId3Tags)
  private
    function GetTitle: String;
    procedure SetTitle(const Value: String);
    function GetArtist: String;
    procedure SetArtist(const Value: String);
    function GetAlbum: String;
    procedure SetAlbum(const Value: String);
    function GetYear: Word;
    procedure SetYear(Value: Word);
    function GetComment: String;
    procedure SetComment(const Value: String);
    function GetTrack: Byte;
    procedure SetTrack(Value: Byte);
    function GetGenre: String;
    procedure SetGenre(const Value: String);
    function GetGenreId: TId3v1TagsGenreId;
    procedure SetGenreId(Value: TId3v1TagsGenreId);
  public
    function ReadFromFile(InFile: HFILE): Boolean; override;
    function WriteToFile(OutFile: HFILE): Boolean; override;
  published
    property Title: String read GetTitle write SetTitle;
    property Artist: String read GetArtist write SetArtist;
    property Album: String read GetAlbum write SetAlbum;
    property Year: Word read GetYear write SetYear;
    property Comment: String read GetComment write SetComment;
    property Track: Byte read GetTrack write SetTrack;
    property Genre: String read GetGenre write SetGenre;
    property GenreId: TId3v1TagsGenreId read GetGenreId write SetGenreId;
  end;

  { class TId3v2Tags }

  TId3v2Tags = class(TId3Tags)
  private
    function GetArtist: WideString;
    procedure SetArtist(const Value: WideString);
    function GetAlbum: WideString;
    procedure SetAlbum(const Value: WideString);
    function GetGenre: WideString;
    procedure SetGenre(const Value: WideString);
    function GetYear: WideString;
    procedure SetYear(const Value: WideString);
    function GetTrack: WideString;
    procedure SetTrack(const Value: WideString);
    function GetTitle: WideString;
    procedure SetTitle(const Value: WideString);
    function GetComment: WideString;
    procedure SetComment(const Value: WideString);
  published
    property Artist: WideString read GetArtist write SetArtist;
    property Album: WideString read GetAlbum write SetAlbum;
    property Genre: WideString read GetGenre write SetGenre;
    property Year: WideString read GetYear write SetYear;
    property Track: WideString read GetTrack write SetTrack;
    property Title: WideString read GetTitle write SetTitle;
    property Comment: WideString read GetComment write SetComment;
  end;

  { class TAPEv2Tags }

  TAPEv2Tags = class(TAuTags)
  private
    function GetArtist: WideString;
    procedure SetArtist(const Value: WideString);
    function GetComposer: WideString;
    procedure SetComposer(const Value: WideString);
    function GetAlbum: WideString;
    procedure SetAlbum(const Value: WideString);
    function GetDebutAlbum: WideString;
    procedure SetDebutAlbum(const Value: WideString);
    function GetPublisher: WideString;
    procedure SetPublisher(const Value: WideString);
    function GetConductor: WideString;
    procedure SetConductor(const Value: WideString);
    function GetCopyright: WideString;
    procedure SetCopyright(const Value: WideString);
    function GetISBN: String;
    procedure SetISBN(const Value: String);
    function GetISRC: String;
    procedure SetISRC(const Value: String);
    function GetBarCode: String;
    procedure SetBarCode(const Value: String);
    function GetLabelCode: String;
    procedure SetLabelCode(const Value: String);
    function GetCatalog: String;
    procedure SetCatalog(const Value: String);
    function GetLanguage: WideString;
    procedure SetLanguage(const Value: WideString);
    function GetGenre: WideString;
    procedure SetGenre(const Value: WideString);
    function GetYear: String;
    procedure SetYear(const Value: String);
    function GetRecordDate: String;
    procedure SetRecordDate(const Value: String);
    function GetRecordLocation: WideString;
    procedure SetRecordLocation(const Value: WideString);
    function GetFile: WideString;
    procedure SetFile(const Value: WideString);
    function GetRelated: WideString;
    procedure SetRelated(const Value: WideString);
    function GetAbstract: WideString;
    procedure SetAbstract(const Value: WideString);
    function GetBibliography: WideString;
    procedure SetBibliography(const Value: WideString);
    function GetTrack: String;
    procedure SetTrack(const Value: String);
    function GetTitle: WideString;
    procedure SetTitle(const Value: WideString);
    function GetSubTitle: WideString;
    procedure SetSubTitle(const Value: WideString);
    function GetComment: WideString;
    procedure SetComment(const Value: WideString);
  public
    constructor Create; override;
  published
    property Artist: WideString read GetArtist write SetArtist;
    property Composer: WideString read GetComposer write SetComposer;
    property Album: WideString read GetAlbum write SetAlbum;
    property DebutAlbum: WideString read GetDebutAlbum write SetDebutAlbum;
    property Publisher: WideString read GetPublisher write SetPublisher;
    property Conductor: WideString read GetConductor write SetConductor;
    property Copyright: WideString read GetCopyright write SetCopyright;
    property ISBN: String read GetISBN write SetISBN;
    property ISRC: String read GetISRC write SetISRC;
    property BarCode: String read GetBarCode write SetBarCode;
    property LabelCode: String read GetLabelCode write SetLabelCode;
    property Catalog: String read GetCatalog write SetCatalog;
    property Language: WideString read GetLanguage write SetLanguage;
    property Genre: WideString read GetGenre write SetGenre;
    property Year: String read GetYear write SetYear;
    property RecordDate: String read GetRecordDate write SetRecordDate;
    property RecordLocation: WideString read GetRecordLocation write SetRecordLocation;
    property File_: WideString read GetFile write SetFile;
    property Related: WideString read GetRelated write SetRelated;
    property Abstract: WideString read GetAbstract write SetAbstract;
    property Bibliography: WideString read GetBibliography write SetBibliography;
    property Track: String read GetTrack write SetTrack;
    property Title: WideString read GetTitle write SetTitle;
    property SubTitle: WideString read GetSubTitle write SetSubTitle;
    property Comment: WideString read GetComment write SetComment;
  end;

const
  Id3v1Genres: array [TId3v1TagsGenreId] of String = (
    'Blues',
    'Classic Rock',
    'Country',
    'Dance',
    'Disco',
    'Funk',
    'Grunge',
    'Hip-Hop',
    'Jazz',
    'Metal',
    'New Age',
    'Oldies',
    'Other',
    'Pop',
    'R&B',
    'Rap',
    'Reggae',
    'Rock',
    'Techno',
    'Industrial',
    'Alternative',
    'Ska',
    'Death metal',
    'Pranks',
    'Soundtrack',
    'Euro-Techno',
    'Ambient',
    'Trip-Hop',
    'Vocal',
    'Jazz+Funk',
    'Fusion',
    'Trance',
    'Classical',
    'Instrumental',
    'Acid',
    'House',
    'Game',
    'Sound Clip',
    'Gospel',
    'Noise',
    'Alt. Rock',
    'Bass',
    'Soul',
    'Punk',
    'Space',
    'Meditative',
    'Instrumental pop',
    'Instrumental rock',
    'Ethnic',
    'Gothic',
    'Darkwave',
    'Techno-Industrial',
    'Electronic',
    'Pop-Folk',
    'Eurodance',
    'Dream',
    'Southern Rock',
    'Comedy',
    'Cult',
    'Gangsta',
    'Top 40',
    'Christian Rap',
    'Pop/Funk',
    'Jungle',
    'Native American',
    'Cabaret',
    'New Wave',
    'Psychedelic',
    'Rave',
    'Showtunes',
    'Trailer',
    'Lo-Fi',
    'Tribal',
    'Acid Punk',
    'Acid Jazz',
    'Polka',
    'Retro',
    'Musical',
    'Rock & Roll',
    'Hard Rock',
    'Folk',
    'Folk-Rock',
    'National Folk',
    'Swing',
    'Fast Fusion',
    'Bebob',
    'Latin',
    'Revival',
    'Celtic',
    'Bluegrass',
    'Avantgarde',
    'Gothic Rock',
    'Progressive Rock',
    'Psychedelic Rock',
    'Symphonic Rock',
    'Slow Rock',
    'Big Band',
    'Chorus',
    'Easy Listening',
    'Acoustic',
    'Humour',
    'Speech',
    'Chanson',
    'Opera',
    'Chamber music',
    'Sonata',
    'Symphony',
    'Booty Bass',
    'Primus',
    'Porn Groove',
    'Satire',
    'Slow Jam',
    'Club',
    'Tango',
    'Samba',
    'Folklore',
    'Ballad',
    'Power Ballad',
    'Rhythmic soul',
    'Freestyle',
    'Duet',
    'Punk Rock',
    'Drum Solo',
    'A Cappella',
    'Euro-House',
    'Dance Hall',
    'Goa',
    'Drum & Bass',
    'Club-House',
    'Hardcore',
    'Terror',
    'Indie',
    'BritPop',
    'Negerpunk',
    'Polsk Punk',
    'Beat',
    'Christian Gangsta Rap',
    'Heavy Metal',
    'Black Metal',
    'Crossover',
    'Contemporary Christian',
    'Christian Rock',
    'Merengue',
    'Salsa',
    'Thrash Metal',
    'Anime',
    'JPop',
    'Synthpop'
  );

  _id3_Artist  = 'TPE1';
  _id3_Album   = 'TALB';
  _id3_Genre   = 'TCON';
  _id3_Year    = 'TYER';
  _id3_Track   = 'TRCK';
  _id3_Title   = 'TIT2';
  _id3_Comment = 'COMM';

  _ape_Artist         = 'Artist';
  _ape_Composer       = 'Composer';
  _ape_Album          = 'Album';
  _ape_DebutAlbum     = 'Debut album';
  _ape_Publisher      = 'Publisher';
  _ape_Conductor      = 'Conductor';
  _ape_Copyright      = 'Copyright';
  _ape_ISBN           = 'ISBN';
  _ape_ISRC           = 'ISRC';
  _ape_BarCode        = 'EAN/UPC';
  _ape_LabelCode      = 'LC';
  _ape_Catalog        = 'Catalog';
  _ape_Language       = 'Language';
  _ape_Genre          = 'Genre';
  _ape_Year           = 'Year';
  _ape_RecordDate     = 'Record Date';
  _ape_RecordLocation = 'Record Location';
  _ape_File           = 'File';
  _ape_Related        = 'Related';
  _ape_Abstract       = 'Abstract';
  _ape_Bibliography   = 'Bibliography';
  _ape_Track          = 'Track';
  _ape_Title          = 'Title';
  _ape_SubTitle       = 'Subtitle';
  _ape_Comment        = 'Comment';

  _vorbis_Album = 'album';
  _vorbis_Artist = 'artist';
  _vorbis_Date = 'date';
  _vorbis_Genre = 'genre';
  _vorbis_Title = 'title';
  _vorbis_Track = 'track';

type

  TVorbisTags = class(TAuTags)
  private
     function GetAlbum : WideString;
     procedure SetAlbum(Value : WideString);
     function GetArtist :  WideString;
     procedure SetArtist(Value : WideString);
     function GetDate :  WideString;
     procedure SetDate(Value : WideString);
     function GetGenre :  WideString;
     procedure SetGenre(Value : WideString);
     function GetTitle : WideString;
     procedure SetTitle(Value : WideString);
     function GetTrack : Integer;
     procedure SetTrack(Value : Integer);
  public
    constructor Create; override;
  published
    property Album : WideString read GetAlbum write SetAlbum;
    property Artist : WideString read GetArtist write SetArtist;
    property Date : WideString read GetDate write SetDate;
    property Genre : WideString read GetGenre write SetGenre;
    property Title : WideString read GetTitle write SetTitle;
    property Track : Integer read GetTrack write SetTrack;
  end;


implementation

uses Variants, SysUtils, Math;

{ class TAuTags }

constructor TAuTags.Create;
begin
  inherited Create();

  FValues := TStringList.Create();
  TStringList(FValues).CaseSensitive := False;
  TStringList(FValues).Duplicates := dupIgnore;
  TStringList(FValues).Sorted := True;
end;

destructor TAuTags.Destroy;
var
  i: Integer;
begin
  for i := 0 to FValues.Count - 1 do
    Dispose(PTagValueInfo(Pointer(FValues.Objects[i])));
  FValues.Free();

  inherited;
end;

function TAuTags.GetIdCount: Integer;
begin
  Result := FValues.Count;
end;

function TAuTags.GetId(Index: Integer): String;
begin
  if (Index >= 0) and (Index < FValues.Count) then
    Result := FValues[Index]
  else
    Result := '';
end;

function TAuTags.GetValue(const Id: String): Variant;
var
 n: Integer;
begin
  n := FValues.IndexOf(Id);
  if n >= 0 then
    Result := PTagValueInfo(Pointer(FValues.Objects[n])).Value
  else
    Result := Null;
end;

procedure TAuTags.SetValue(const Id: String; const Value: Variant);
var
  n: Integer;
  p_value_info: PTagValueInfo;
begin
  n := FValues.IndexOf(Id);
  if n >= 0 then begin
    p_value_info := Pointer(FValues.Objects[n]);
    if not VarSameValue(Value, p_value_info.Value) then begin
      p_value_info.Value := Value;

      FEmpty := False;
      FChanged := True;
    end;
  end;
end;

function TAuTags.GetAsInteger(const Id: String): Integer;
var
  n: Integer;
  tag_value: Variant;
begin
  Result := 0;

  n := FValues.IndexOf(Id);
  if n >= 0 then begin
    tag_value := PTagValueInfo(Pointer(FValues.Objects[n])).Value;
    case VarType(tag_value) of
      varByte, varShortInt,
      varWord, varSmallint,
      varLongWord, varInteger:
        Result := tag_value;
      varString, varOleStr:
        if TryStrToInt(tag_value, n) then
          Result := n;
    end;
  end;
end;

function TAuTags.GetAsString(const Id: String): String;
var
  n: Integer;
  tag_value: Variant;
begin
  Result := '';

  n := FValues.IndexOf(Id);
  if n >= 0 then begin
    tag_value := PTagValueInfo(Pointer(FValues.Objects[n])).Value;
    case VarType(tag_value) of
      varByte, varShortInt,
      varWord, varSmallint,
      varLongWord, varInteger:
        Result := IntToStr(tag_value);
      varString, varOleStr:
        Result := tag_value;
    end;
  end;
end;

function TAuTags.GetAsWideString(const Id: String): WideString;
var
  n: Integer;
  tag_value: Variant;
begin
  Result := '';

  n := FValues.IndexOf(Id);
  if n >= 0 then begin
    tag_value := PTagValueInfo(Pointer(FValues.Objects[n])).Value;
    case VarType(tag_value) of
      varByte, varShortInt,
      varWord, varSmallint,
      varLongWord, varInteger:
        Result := IntToStr(tag_value);
      varString, varOleStr:
        Result := tag_value;
    end;
  end;
end;

function TAuTags.AddId(const Id: String): Boolean;
var
  p_value_info: PTagValueInfo;
begin
  Result := (Id <> '');
  if Result and (FValues.IndexOf(Id) < 0) then begin
    New(p_value_info);
    p_value_info.Value := Null;
    FValues.AddObject(Id, Pointer(p_value_info));
  end;
end;

procedure TAuTags.Assign(Source: TPersistent);
var
  i: Integer;
  tag_id: String;
begin
  if Source is TAuTags then begin
    Clear();

    for i := 0 to TAuTags(Source).IdCount - 1 do begin
      tag_id := TAuTags(Source).Ids[i];
      Values[tag_id] := TAuTags(Source).Values[tag_id];
    end;
  end
  else
    inherited;
end;

procedure TAuTags.AssignTo(Dest: TPersistent);
var
  i: Integer;
  tag_id: String;
begin
  if Dest is TAuTags then begin
    TAuTags(Dest).Clear();

    for i := 0 to IdCount - 1 do begin
      tag_id := Ids[i];
      TAuTags(Dest).Values[tag_id] := Values[tag_id];
    end;
  end
  else
    inherited;
end;

procedure TAuTags.Clear;
var
  i: Integer;
begin
  for i := 0 to FValues.Count - 1 do
    PTagValueInfo(Pointer(FValues.Objects[i])).Value := Null;
  
  FEmpty := True;
  FChanged := True;
end;

function TAuTags.ReadFromFile(InFile: HFILE): Boolean;
begin
  Result := False;
end;

function TAuTags.WriteToFile(OutFile: HFILE): Boolean;
begin
  Result := False;
end;


{ class TId3Tags }

constructor TId3Tags.Create;
begin
  inherited;

  AddId(_id3_Artist);
  AddId(_id3_Album);
  AddId(_id3_Genre);
  AddId(_id3_Year);
  AddId(_id3_Track);
  AddId(_id3_Title);
  AddId(_id3_Comment);
end;


{ class TId3v1Tags }

function TId3v1Tags.GetTitle: String;
begin
  Result := AsString[_id3_Title];
end;

procedure TId3v1Tags.SetTitle(const Value: String);
begin
  Values[_id3_Title] := Value;
end;

function TId3v1Tags.GetArtist: String;
var
  tag_value: Variant;
begin
  tag_value := Values[_id3_Artist];
  if VarIsType(tag_value, [varString, varOleStr]) then
    Result := tag_value;
end;

procedure TId3v1Tags.SetArtist(const Value: String);
begin
  Values[_id3_Artist] := Value;
end;

function TId3v1Tags.GetAlbum: String;
begin
  Result := AsString[_id3_Album];
end;

procedure TId3v1Tags.SetAlbum(const Value: String);
begin
  Values[_id3_Album] := Value;
end;

function TId3v1Tags.GetYear: Word;
begin
  Result := AsInteger[_id3_Year];
end;

procedure TId3v1Tags.SetYear(Value: Word);
begin
  Values[_id3_Year] := Value;
end;

function TId3v1Tags.GetComment: String;
begin
  Result := AsString[_id3_Comment];
end;

procedure TId3v1Tags.SetComment(const Value: String);
begin
  Values[_id3_Comment] := Value;
end;

function TId3v1Tags.GetTrack: Byte;
begin
  Result := AsInteger[_id3_Track];
end;

procedure TId3v1Tags.SetTrack(Value: Byte);
begin
  Values[_id3_Track] := Value;
end;

function TId3v1Tags.GetGenre: String;
begin
{  Result := AsString[_id3_Genre];}

  Result := Id3v1Genres[GenreId];
end;

procedure TId3v1Tags.SetGenre(const Value: String);
var
  i: TId3v1TagsGenreId;
  buf: String;
begin
{  Values[_id3_Genre] := Value;}

  buf := Trim(Value);
  for i := Low(i) to High(i) do
    if SameText(buf, Id3v1Genres[i]) then begin
      Values[_id3_Genre] := i;

      Break;
    end;
end;

function TId3v1Tags.GetGenreId: TId3v1TagsGenreId;
var
  n: Integer;
begin
  n := AsInteger[_id3_Genre];
  if n in [Low(Result) .. High(Result)] then
    Result := n
  else
    Result := 12; // other 
end;

procedure TId3v1Tags.SetGenreId(Value: TId3v1TagsGenreId);
begin
  Values[_id3_Genre] := Value;
end;

type
  TId3v1TagsRec = packed record
    Header : array [1 ..  3] of Char; // tag header - must be "TAG"
    Title  : array [1 .. 30] of Char; // title
    Artist : array [1 .. 30] of Char; // artist data
    Album  : array [1 .. 30] of Char; // album data
    Year   : array [1 ..  4] of Char; // year
    Comment: array [1 .. 30] of Char; // comment
    GenreId: Byte;                    // genre
  end;

function TId3v1Tags.ReadFromFile(InFile: HFILE): Boolean;
var
  tags_rec: TId3v1TagsRec;
  bytes_read: Cardinal;
begin
  Result :=
    (InFile <> INVALID_HANDLE_VALUE) and
    (SetFilePointer(InFile, - SizeOf(tags_rec), nil, FILE_END) <> $FFFFFFFF) and
    ReadFile(InFile, tags_rec, SizeOf(tags_rec), bytes_read, nil) and
    (bytes_read = SizeOf(tags_rec)) and
    (tags_rec.Header = 'TAG');
  if Result then begin
    Title  := Trim(tags_rec.Title);
    Artist := Trim(tags_rec.Artist);
    Album  := Trim(tags_rec.Album);
    Year   := StrToIntDef(Trim(tags_rec.Year), 0);

    if ((tags_rec.Comment[High(tags_rec.Comment) - 1] = #0) and
        (tags_rec.Comment[High(tags_rec.Comment)] <> #0)) or
       ((tags_rec.Comment[High(tags_rec.Comment) - 1] = #32) and
        (tags_rec.Comment[High(tags_rec.Comment)] <> #32))
    then begin
      Comment := Trim(Copy(tags_rec.Comment,
        Low(tags_rec.Comment), High(tags_rec.Comment) - 2));
      Track := Byte(tags_rec.Comment[High(tags_rec.Comment)]);
    end
    else begin
      Comment := Trim(tags_rec.Comment);
      Track := 0;
    end;
    GenreId := tags_rec.GenreId;
  end;
end;

function TId3v1Tags.WriteToFile(OutFile: HFILE): Boolean;

  function min(n1, n2: Integer): Integer;
  begin
    if n1 < n2 then
      Result := n1
    else
      Result := n2;
  end;

var
  tags_rec: TId3v1TagsRec;
  buf: String;
  bytes_written: Cardinal;
begin
  Result :=
    (OutFile <> INVALID_HANDLE_VALUE) and
    (SetFilePointer(OutFile, 0, nil, FILE_END) <> $FFFFFFFF);
  if Result then begin
    FillChar(tags_rec, SizeOf(tags_rec), 0);

    tags_rec.Header := 'TAG';
    Move(Title[1], tags_rec.Title[1],
      min(Length(Title), SizeOf(tags_rec.Title)));
    Move(Artist[1], tags_rec.Artist[1],
      min(Length(Artist), SizeOf(tags_rec.Artist)));
    Move(Album[1], tags_rec.Album[1],
      min(Length(Album), SizeOf(tags_rec.Album)));
    buf := IntToStr(Year);
    Move(buf[1], tags_rec.Year[1],
      min(Length(buf), SizeOf(tags_rec.Year)));

    if Track <> 0 then begin
      Move(Comment[1], tags_rec.Comment[1],
        min(Length(Comment), SizeOf(tags_rec.Comment) - 2));
      tags_rec.Comment[High(tags_rec.Comment) - 1] := #0;
      tags_rec.Comment[High(tags_rec.Comment)] := Char(Track);
    end
    else
      Move(Comment[1], tags_rec.Comment[1],
        min(Length(Comment), SizeOf(tags_rec.Comment)));

    tags_rec.GenreId := GenreId;

    Result :=
      WriteFile(OutFile, tags_rec, SizeOf(tags_rec), bytes_written, nil) and
      (bytes_written = SizeOf(tags_rec));
  end;
end;


{ class TId3v2Tags }

function TId3v2Tags.GetArtist: WideString;
begin
  Result := AsWideString[_id3_Artist];
end;

procedure TId3v2Tags.SetArtist(const Value: WideString);
begin
  Values[_id3_Artist] := Value;
end;

function TId3v2Tags.GetAlbum: WideString;
begin
  Result := AsWideString[_id3_Album];
end;

procedure TId3v2Tags.SetAlbum(const Value: WideString);
begin
  Values[_id3_Album] := Value;
end;

function TId3v2Tags.GetGenre: WideString;
begin
  Result := AsWideString[_id3_Genre];
end;

procedure TId3v2Tags.SetGenre(const Value: WideString);
begin
  Values[_id3_Genre] := Value;
end;

function TId3v2Tags.GetYear: WideString;
begin
  Result := AsWideString[_id3_Year];
end;

procedure TId3v2Tags.SetYear(const Value: WideString);
begin
  Values[_id3_Year] := Value;
end;

function TId3v2Tags.GetTrack: WideString;
begin
  Result := AsWideString[_id3_Track];
end;

procedure TId3v2Tags.SetTrack(const Value: WideString);
begin
  Values[_id3_Track] := Value;
end;

function TId3v2Tags.GetTitle: WideString;
begin
  Result := AsWideString[_id3_Title];
end;

procedure TId3v2Tags.SetTitle(const Value: WideString);
begin
  Values[_id3_Title] := Value;
end;

function TId3v2Tags.GetComment: WideString;
begin
  Result := AsWideString[_id3_Comment];
end;

procedure TId3v2Tags.SetComment(const Value: WideString);
begin
  Values[_id3_Comment] := Value;
end;


{ class TAPEv2Tags }

constructor TAPEv2Tags.Create;
begin
  inherited;

  AddId(_ape_Artist);
  AddId(_ape_Composer);
  AddId(_ape_Album);
  AddId(_ape_DebutAlbum);
  AddId(_ape_Publisher);
  AddId(_ape_Conductor);
  AddId(_ape_Copyright);
  AddId(_ape_ISBN);
  AddId(_ape_ISRC);
  AddId(_ape_BarCode);
  AddId(_ape_LabelCode);
  AddId(_ape_Catalog);
  AddId(_ape_Language);
  AddId(_ape_Genre);
  AddId(_ape_Year);
  AddId(_ape_RecordDate);
  AddId(_ape_RecordLocation);
  AddId(_ape_File);
  AddId(_ape_Related);
  AddId(_ape_Abstract);
  AddId(_ape_Bibliography);
  AddId(_ape_Track);
  AddId(_ape_Title);
  AddId(_ape_SubTitle);
  AddId(_ape_Comment);
end;

function TAPEv2Tags.GetArtist: WideString;
begin
  Result := AsWideString[_ape_Artist];
end;

procedure TAPEv2Tags.SetArtist(const Value: WideString);
begin
  Values[_ape_Artist] := Value;
end;

function TAPEv2Tags.GetComposer: WideString;
begin
  Result := AsWideString[_ape_Composer];
end;

procedure TAPEv2Tags.SetComposer(const Value: WideString);
begin
  Values[_ape_Composer] := Value;
end;

function TAPEv2Tags.GetAlbum: WideString;
begin
  Result := AsWideString[_ape_Album];
end;

procedure TAPEv2Tags.SetAlbum(const Value: WideString);
begin
  Values[_ape_Album] := Value;
end;

function TAPEv2Tags.GetDebutAlbum: WideString;
begin
  Result := AsWideString[_ape_DebutAlbum];
end;

procedure TAPEv2Tags.SetDebutAlbum(const Value: WideString);
begin
  Values[_ape_DebutAlbum] := Value;
end;

function TAPEv2Tags.GetPublisher: WideString;
begin
  Result := AsWideString[_ape_Publisher];
end;

procedure TAPEv2Tags.SetPublisher(const Value: WideString);
begin
  Values[_ape_Publisher] := Value;
end;

function TAPEv2Tags.GetConductor: WideString;
begin
  Result := AsWideString[_ape_Conductor];
end;

procedure TAPEv2Tags.SetConductor(const Value: WideString);
begin
  Values[_ape_Conductor] := Value;
end;

function TAPEv2Tags.GetCopyright: WideString;
begin
  Result := AsWideString[_ape_Copyright];
end;

procedure TAPEv2Tags.SetCopyright(const Value: WideString);
begin
  Values[_ape_Copyright] := Value;
end;

function TAPEv2Tags.GetISBN: String;
begin
  Result := AsString[_ape_ISBN];
end;

procedure TAPEv2Tags.SetISBN(const Value: String);
begin
  Values[_ape_ISBN] := Value;
end;

function TAPEv2Tags.GetISRC: String;
begin
  Result := AsString[_ape_ISRC];
end;

procedure TAPEv2Tags.SetISRC(const Value: String);
begin
  Values[_ape_ISRC] := Value;
end;

function TAPEv2Tags.GetBarCode: String;
begin
  Result := AsString[_ape_BarCode];
end;

procedure TAPEv2Tags.SetBarCode(const Value: String);
begin
  Values[_ape_BarCode] := Value;
end;

function TAPEv2Tags.GetLabelCode: String;
begin
  Result := AsString[_ape_LabelCode];
end;

procedure TAPEv2Tags.SetLabelCode(const Value: String);
begin
  Values[_ape_LabelCode] := Value;
end;

function TAPEv2Tags.GetCatalog: String;
begin
  Result := AsString[_ape_Catalog];
end;

procedure TAPEv2Tags.SetCatalog(const Value: String);
begin
  Values[_ape_Catalog] := Value;
end;

function TAPEv2Tags.GetLanguage: WideString;
begin
  Result := AsWideString[_ape_Language];
end;

procedure TAPEv2Tags.SetLanguage(const Value: WideString);
begin
  Values[_ape_Language] := Value;
end;

function TAPEv2Tags.GetGenre: WideString;
begin
  Result := AsWideString[_ape_Genre];
end;

procedure TAPEv2Tags.SetGenre(const Value: WideString);
begin
  Values[_ape_Genre] := Value;
end;

function TAPEv2Tags.GetYear: String;
begin
  Result := AsString[_ape_Year];
end;

procedure TAPEv2Tags.SetYear(const Value: String);
begin
  Values[_ape_Year] := Value;
end;

function TAPEv2Tags.GetRecordDate: String;
begin
  Result := AsString[_ape_RecordDate];
end;

procedure TAPEv2Tags.SetRecordDate(const Value: String);
begin
  Values[_ape_RecordDate] := Value;
end;

function TAPEv2Tags.GetRecordLocation: WideString;
begin
  Result := AsWideString[_ape_RecordLocation];
end;

procedure TAPEv2Tags.SetRecordLocation(const Value: WideString);
begin
  Values[_ape_RecordLocation] := Value;
end;

function TAPEv2Tags.GetFile: WideString;
begin
  Result := AsWideString[_ape_File];
end;

procedure TAPEv2Tags.SetFile(const Value: WideString);
begin
  Values[_ape_File] := Value;
end;

function TAPEv2Tags.GetRelated: WideString;
begin
  Result := AsWideString[_ape_Related];
end;

procedure TAPEv2Tags.SetRelated(const Value: WideString);
begin
  Values[_ape_Related] := Value;
end;

function TAPEv2Tags.GetAbstract: WideString;
begin
  Result := AsWideString[_ape_Abstract];
end;

procedure TAPEv2Tags.SetAbstract(const Value: WideString);
begin
  Values[_ape_Abstract] := Value;
end;

function TAPEv2Tags.GetBibliography: WideString;
begin
  Result := AsWideString[_ape_Bibliography];
end;

procedure TAPEv2Tags.SetBibliography(const Value: WideString);
begin
  Values[_ape_Bibliography] := Value;
end;

function TAPEv2Tags.GetTrack: String;
begin
  Result := AsString[_ape_Track];
end;

procedure TAPEv2Tags.SetTrack(const Value: String);
begin
  Values[_ape_Track] := Value;
end;

function TAPEv2Tags.GetTitle: WideString;
begin
  Result := AsWideString[_ape_Title];
end;

procedure TAPEv2Tags.SetTitle(const Value: WideString);
begin
  Values[_ape_Title] := Value;
end;

function TAPEv2Tags.GetSubTitle: WideString;
begin
  Result := AsWideString[_ape_SubTitle];
end;

procedure TAPEv2Tags.SetSubTitle(const Value: WideString);
begin
  Values[_ape_SubTitle] := Value;
end;

function TAPEv2Tags.GetComment: WideString;
begin
  Result := AsWideString[_ape_Comment];
end;

procedure TAPEv2Tags.SetComment(const Value: WideString);
begin
  Values[_ape_Comment] := Value;
end;

  constructor TVorbisTags.Create;
  begin
    inherited Create;
    AddId(_vorbis_Album);
    AddId(_vorbis_Artist);
    AddId(_vorbis_Date);
    AddId(_vorbis_Genre);
    AddId(_vorbis_Title);
    AddId(_vorbis_Track);
  end;

  function TVorbisTags.GetAlbum;
  begin
    Result := AsWideString[_vorbis_Album];
  end;

  procedure TVorbisTags.SetAlbum;
  begin
    Values[_vorbis_Album] := Value;
  end;

  function TVorbisTags.GetArtist;
  begin
    Result := AsWideString[_vorbis_Artist];
  end;

  procedure TVorbisTags.SetArtist;
  begin
    Values[_vorbis_Artist] := Value;
  end;

  function TVorbisTags.GetDate;
  begin
    Result := AsWideString[_vorbis_Date];
  end;

  procedure TVorbisTags.SetDate;
  begin
    Values[_vorbis_Date] := Value;
  end;

  function TVorbisTags.GetGenre;
  begin
    Result := AsWideString[_vorbis_Genre];
  end;

  procedure TVorbisTags.SetGenre;
  begin
    Values[_vorbis_Genre] := Value;
  end;

  function TVorbisTags.GetTitle;
  begin
    Result := AsWideString[_vorbis_Title];
  end;

  procedure TVorbisTags.SetTitle;
  begin
    Values[_vorbis_Title] := Value;
  end;

  function TVorbisTags.GetTrack;
  begin
    Result := AsInteger[_vorbis_Track];
  end;

  procedure TVorbisTags.SetTrack;
  begin
    Values[_vorbis_Track] := Value;
  end;

end.

