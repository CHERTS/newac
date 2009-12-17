object Form1: TForm1
  Left = 246
  Top = 304
  ActiveControl = AddtoPLButton
  Caption = 'Audio Player'
  ClientHeight = 302
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    464
    302)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 64
    Top = 40
    Width = 53
    Height = 13
    Caption = 'Total Time:'
  end
  object Label2: TLabel
    Left = 120
    Top = 40
    Width = 3
    Height = 13
  end
  object PlayButton: TButton
    Left = 120
    Top = 8
    Width = 41
    Height = 25
    Caption = 'Play'
    TabOrder = 0
    OnClick = PlayButtonClick
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 160
    Width = 464
    Height = 17
    Align = alBottom
    Smooth = True
    Step = 2
    TabOrder = 1
    ExplicitTop = 77
  end
  object StopButton: TButton
    Left = 216
    Top = 8
    Width = 41
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = StopButtonClick
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 38
    Width = 49
    Height = 17
    Caption = 'Loop'
    TabOrder = 3
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 283
    Width = 464
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 200
  end
  object AddtoPLButton: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Add To PlayList...'
    Default = True
    TabOrder = 5
    OnClick = AddtoPLButtonClick
  end
  object ListBox1: TListBox
    Left = 0
    Top = 194
    Width = 464
    Height = 89
    Align = alBottom
    ItemHeight = 13
    TabOrder = 6
    OnClick = ListBox1Click
    ExplicitTop = 111
  end
  object SkipButton: TButton
    Left = 168
    Top = 8
    Width = 41
    Height = 25
    Caption = '>>|'
    TabOrder = 7
    OnClick = SkipButtonClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 177
    Width = 464
    Height = 17
    Align = alBottom
    TabOrder = 8
    ExplicitTop = 94
  end
  object ForwardButton: TButton
    Left = 272
    Top = 8
    Width = 33
    Height = 25
    Caption = '>>'
    TabOrder = 9
    OnClick = ForwardButtonClick
  end
  object BackwardButton: TButton
    Left = 312
    Top = 8
    Width = 33
    Height = 25
    Caption = '<<'
    TabOrder = 10
    OnClick = BackwardButtonClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 61
    Width = 464
    Height = 94
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    TabOrder = 11
    ExplicitHeight = 89
  end
  object VorbisIn1: TVorbisIn
    FileName = 'D:\Program Files\Borland\Delphi6\Projects\mozart.ogg'
    Loop = False
    EndSample = -1
    Left = 8
    Top = 160
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'All files|*.*|Ogg Vorbis files|*.ogg|MP3 files|*.mp3|FLAC files|' +
      '*.flac|Monkey Audio|*.ape|Wavpack|*.wv|Wave files|*.wav'
    Left = 368
    Top = 160
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    Left = 56
    Top = 160
  end
  object FLACIn1: TFLACIn
    Loop = False
    CheckMD5Signature = False
    EndSample = -1
    Left = 168
    Top = 160
  end
  object MP3In1: TMP3In
    Loop = False
    EndSample = -1
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 232
    Top = 160
  end
  object DXAudioOut1: TDXAudioOut
    Input = VorbisIn1
    OnDone = AudioOut1Done
    OnProgress = AudioOut1Progress
    DeviceNumber = 0
    FramesInBuffer = 24576
    Latency = 80
    PollingInterval = 100
    PrefetchData = True
    Left = 280
    Top = 160
  end
  object MACIn1: TMACIn
    Loop = False
    EndSample = -1
    Left = 104
    Top = 160
  end
  object WVIn1: TWVIn
    Loop = False
    EndSample = -1
    Left = 8
    Top = 120
  end
end
