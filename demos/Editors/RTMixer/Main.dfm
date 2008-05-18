object Form1: TForm1
  Left = 194
  Top = 110
  Width = 325
  Height = 200
  Caption = 'Real Time Mixer Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 104
    Top = 16
    Width = 44
    Height = 13
    Caption = 'Volume 1'
  end
  object Label2: TLabel
    Left = 104
    Top = 40
    Width = 44
    Height = 13
    Caption = 'Volume 2'
  end
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Input1...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Input2...'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 16
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Go!'
    TabOrder = 2
    OnClick = Button3Click
  end
  object TrackBar1: TTrackBar
    Left = 152
    Top = 16
    Width = 150
    Height = 25
    Max = 65535
    Orientation = trHorizontal
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 3
    ThumbLength = 16
    TickMarks = tmBottomRight
    TickStyle = tsNone
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 152
    Top = 40
    Width = 150
    Height = 25
    Max = 65535
    Orientation = trHorizontal
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 4
    ThumbLength = 16
    TickMarks = tmBottomRight
    TickStyle = tsNone
    OnChange = TrackBar2Change
  end
  object RadioGroup2: TRadioGroup
    Left = 120
    Top = 72
    Width = 105
    Height = 57
    Caption = 'Output'
    ItemIndex = 1
    Items.Strings = (
      'File'
      'Speakers')
    TabOrder = 5
  end
  object Button4: TButton
    Left = 16
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 6
    OnClick = Button4Click
  end
  object WaveIn1: TWaveIn
    FileName = 'E:\TestAudio\01-Cluster One.wav'
    Loop = False
    EndSample = -1
    Left = 16
    Top = 128
  end
  object WaveIn2: TWaveIn
    FileName = 'E:\TestAudio\I'#39've seen this face before.wav'
    Loop = False
    EndSample = -1
    Left = 48
    Top = 128
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WAV files|*.wav'
    Left = 216
    Top = 128
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wav'
    Left = 248
    Top = 128
  end
  object DXAudioOut1: TDXAudioOut
    Input = RealTimeMixer1
    OnDone = WaveOut1Done
    DeviceNumber = 0
    FramesInBuffer = 65536
    PollingInterval = 200
    Left = 168
    Top = 128
  end
  object WaveOut1: TWaveOut
    Input = RealTimeMixer1
    OnDone = WaveOut1Done
    WavType = wtPCM
    BlockSize = 512
    CreateNonMsHeaders = False
    FileMode = foRewrite
    Left = 128
    Top = 128
  end
  object RealTimeMixer1: TRealTimeMixer
    Input1 = WaveIn1
    Input2 = WaveIn2
    OutSampleRate = 44100
    OutBitsPerSample = 16
    OutChannels = 2
    Volume1 = 65535
    Volume2 = 65535
    Left = 88
    Top = 128
  end
end
