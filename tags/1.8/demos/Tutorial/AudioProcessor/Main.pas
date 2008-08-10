(* Title: Audio Processor
    This demo converts stereo to mono. If the input is mono it is left as is, if 
    the input is stereo it is converted to mono. 

    - This demo accepts 16 bps input. 
    - In order to use AudioProcessor component you should set its OnGeData event
      handler at least.
    - In this demo we also set OnGetChannels event handler.
    -  Don't forget to assign WaveIn1's FileName property. *)

(* (c) Andrei Borovsky, anb@symmetrica.net *)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_Misc, ACS_Wave, ACS_DXAudio, ACS_Types,
  StdCtrls, ComCtrls;

const
  TmpBufSize = $10000;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    AudioProcessor1: TAudioProcessor;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    ProgressBar1: TProgressBar;
    DXAudioOut1: TDXAudioOut;
    procedure AudioProcessor1GetChannels(Sender: TComponent;
      var Param: Integer);
    procedure Button1Click(Sender: TObject);
    procedure DXAudioOut1Done(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AudioProcessor1GetData(Sender: TComponent;
      var Buffer: Pointer; var Bytes: Integer);
    procedure AudioProcessor1GetSize(Sender: TComponent; var Param: Int64);
    procedure DXAudioOut1Progress(Sender: TComponent);
    procedure AudioProcessor1Init(Sender: TComponent;
      var TotalSize: Int64);
  private
    (* Private declarations *)
  public
    (* Public declarations *)
    TmpBuf : array[0..TmpBufSize - 1] of Byte;
    EndOfInput : Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

(* Here Data is the pointer to the processor output data,
n - data length in bytes 

What does this mean? - Wayne *)

procedure TForm1.AudioProcessor1GetChannels(Sender: TComponent;
  var Param: Integer);
begin
  Param := 1; // always mono
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    DXAudioOut1.Run;
    Button1.Enabled := False;
  end;  
end;

procedure TForm1.DXAudioOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
end;

(* Procedure: FormClose
    This OnClose event prevents exceptions if he form is closed while playing. *)
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DXAudioOut1.Stop;
  while DXAudioOut1.Status <> tosIdle do Sleep(5);
end;



procedure TForm1.AudioProcessor1GetData(Sender: TComponent;
  var Buffer: Pointer; var Bytes: Integer);
var
  (* Variables: Buffers
      We assume the input is 16 bits per sample.
      
    PS16: PStereoBuffer16 - pointer to a buffer for stereo data
    P16: PBuffer16 - pointer to a buffer for mono data. *)
   
  PS16 : PStereoBuffer16;
  P16 : PBuffer16;
  i, DataSize : Integer;
  b : Boolean;
begin
  (* Topic: Get Data
      A all to Input.GetData(Buffer, Bytes) retrieves a part of the input
      component's buffer (pointed to by Buffer) that contains The number of
      bytes returned in Bytes.
      
      See the <Component Writer's Guide> on more detail about how GetData() operates. *)
  // if the input is mono we simply pass data through...
  if AudioProcessor1.Input.Channels = 1 then
    AudioProcessor1.Input.GetData(Buffer, Bytes)
  // ...otherwise convert stereo to mono.
  else begin
    // This is a bit more complicated.
    // We make a copy of data
    if EndOfInput then
    begin
      Buffer := nil;
      Bytes := 0;
      Exit;
    end;
    DataSize := Bytes * 2;
    if DataSize > TmpBufSize then DataSize := TmpBufSize;
    (* Topic: FillBuffer
        EndOfInput is set to True when FillBuffer has encountered the end of
        data in AudioProcessor1.Input. There still may be some data in the
        input, but the next time this method is called we should return nil.
        *)
    DataSize := AudioProcessor1.Input.FillBuffer(@TmpBuf[0], DataSize, EndOfInput);
    P16 := @TmpBuf[0];
    PS16 := @TmpBuf[0];
    for i := 0 to (DataSize div 4) - 1 do // one stereo sample -  4 bytes
      P16[i] := (PS16[i].Left + PS16[i].Right) div 2;
    if DataSize = 0 then
       Buffer := nil
    else
       Buffer := @TmpBuf[0];
    Bytes := DataSize div 2;
  end;
end;

procedure TForm1.AudioProcessor1GetSize(Sender: TComponent;
  var Param: Int64);
begin
  Param := AudioProcessor1.Input.Size div AudioProcessor1.Input.Channels;
end;

procedure TForm1.DXAudioOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := DXAudioOut1.Progress;
end;

procedure TForm1.AudioProcessor1Init(Sender: TComponent;
  var TotalSize: Int64);
begin
  AudioProcessor1.Input.Init;
  TotalSize := AudioProcessor1.Input.Size div AudioProcessor1.Input.Channels;
  EndOfInput := False;
end;

end.
