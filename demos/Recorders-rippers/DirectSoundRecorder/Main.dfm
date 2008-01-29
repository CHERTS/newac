object Form1: TForm1
  Left = 150
  Top = 88
  Width = 339
  Height = 244
  Caption = 'Sound Recorder'
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
    Left = 128
    Top = 112
    Width = 53
    Height = 13
    Caption = 'Ogg quality'
  end
  object Label2: TLabel
    Left = 128
    Top = 144
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 8
    Top = 48
    Width = 34
    Height = 13
    Caption = 'Device'
  end
  object Label4: TLabel
    Left = 96
    Top = 48
    Width = 48
    Height = 13
    Caption = 'DevName'
  end
  object Label5: TLabel
    Left = 0
    Top = 80
    Width = 53
    Height = 13
    Caption = 'Samplerate'
  end
  object Label6: TLabel
    Left = 8
    Top = 168
    Width = 55
    Height = 13
    Caption = 'Underruns: '
  end
  object Label7: TLabel
    Left = 72
    Top = 168
    Width = 6
    Height = 13
    Caption = '0'
  end
  object SelectFileButton: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select File...'
    TabOrder = 0
    OnClick = SelectFileButtonClick
  end
  object RecordButton: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Record'
    TabOrder = 1
    OnClick = RecordButtonClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 191
    Width = 331
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object StopButton: TButton
    Left = 248
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 3
    OnClick = StopButtonClick
  end
  object SpinEdit1: TSpinEdit
    Left = 200
    Top = 112
    Width = 41
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 4
    Value = 2
  end
  object SpinEdit2: TSpinEdit
    Left = 48
    Top = 40
    Width = 41
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 5
    Value = 2
    OnChange = SpinEdit2Change
  end
  object SREdit: TEdit
    Left = 72
    Top = 72
    Width = 57
    Height = 21
    TabOrder = 6
    Text = '44100'
  end
  object StereoCheckBox: TCheckBox
    Left = 144
    Top = 80
    Width = 57
    Height = 17
    Caption = 'Stereo'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 104
    Width = 105
    Height = 57
    Caption = 'Bits per sample'
    ItemIndex = 0
    Items.Strings = (
      '16'
      '24')
    TabOrder = 8
  end
  object PauseButton: TButton
    Left = 168
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Pause'
    TabOrder = 9
    OnClick = PauseButtonClick
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.wav'
    Filter = 'Wave|*.wav|Ogg Vorbis|*.ogg|FLAC|*.flac'
    OnTypeChange = SaveDialog1TypeChange
    Left = 160
    Top = 176
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 192
    Top = 176
  end
  object FLACOut1: TFLACOut
    Input = DXAudioIn1
    OnDone = OutputDone
    FileMode = foRewrite
    BestModelSearch = False
    BlockSize = 4608
    CompressionLevel = -1
    EnableMidSideStereo = True
    EnableLooseMidSideStereo = False
    MaxLPCOrder = 0
    MaxResidualPartitionOrder = 0
    MinResidualPartitionOrder = 0
    QLPCoeffPrecision = 0
    QLPCoeffPrecisionSearch = False
    Tags.Track = 0
    Verify = False
    Left = 128
    Top = 176
  end
  object VorbisOut1: TVorbisOut
    Input = DXAudioIn1
    OnDone = OutputDone
    FileMode = foRewrite
    Compression = 0.200000002980232
    Comments.Track = 0
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 96
    Top = 176
  end
  object WaveOut1: TWaveOut
    Input = DXAudioIn1
    OnDone = OutputDone
    FileMode = foRewrite
    WavType = wtPCM
    BlockSize = 512
    Left = 64
    Top = 176
  end
  object DXAudioIn1: TDXAudioIn
    SamplesToRead = -1
    DeviceNumber = 0
    InBitsPerSample = 8
    InChannels = 1
    InSampleRate = 8000
    RecTime = -1
    Left = 32
    Top = 176
  end
end
