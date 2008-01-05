object Form1: TForm1
  Left = 651
  Top = 375
  Width = 379
  Height = 246
  ActiveControl = AddtoPLButton
  Caption = 'Audio Player'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
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
    Top = 70
    Width = 371
    Height = 17
    Align = alBottom
    Min = 0
    Max = 100
    Smooth = True
    Step = 2
    TabOrder = 1
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
    Top = 193
    Width = 371
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
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
    Top = 104
    Width = 371
    Height = 89
    Align = alBottom
    ItemHeight = 13
    TabOrder = 6
    OnClick = ListBox1Click
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
    Top = 87
    Width = 371
    Height = 17
    Align = alBottom
    TabOrder = 8
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
  object VorbisIn1: TVorbisIn
    EndSample = -1
    FileName = 'D:\Program Files\Borland\Delphi6\Projects\mozart.ogg'
    Loop = False
    Left = 8
    Top = 160
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'Wave files|*.wav|Ogg Vorbis files|*.ogg|MP3 files|*.mp3|FLAC fil' +
      'es|*.flac'
    Left = 176
    Top = 160
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    Left = 40
    Top = 160
  end
  object FLACIn1: TFLACIn
    EndSample = -1
    Loop = False
    Left = 72
    Top = 160
  end
  object MP3In1: TMP3In
    EndSample = -1
    Loop = False
    Left = 104
    Top = 160
  end
  object DXAudioOut1: TDXAudioOut
    Input = VorbisIn1
    SuspendWhenIdle = True
    OnDone = AudioOut1Done
    OnProgress = AudioOut1Progress
    DeviceNumber = 0
    Left = 144
    Top = 160
  end
end
