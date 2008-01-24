object Form1: TForm1
  Left = 229
  Top = 225
  Width = 366
  Height = 214
  Caption = 'Rip and Listen'
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
    Left = 8
    Top = 8
    Width = 25
    Height = 13
    Caption = 'Drive'
  end
  object Label2: TLabel
    Left = 176
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Track'
  end
  object Label3: TLabel
    Left = 288
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Rip to'
  end
  object Button1: TButton
    Left = 8
    Top = 88
    Width = 73
    Height = 25
    Caption = 'Rip!'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 56
    Width = 345
    Height = 17
    Min = 0
    Max = 100
    ParentShowHint = False
    Smooth = True
    Step = 5
    ShowHint = True
    TabOrder = 1
  end
  object Button2: TButton
    Left = 88
    Top = 88
    Width = 73
    Height = 25
    Caption = 'Abort'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 24
    Width = 161
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnSelect = ComboBox1Select
  end
  object ComboBox2: TComboBox
    Left = 176
    Top = 24
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    OnDropDown = ComboBox2DropDown
    OnEnter = ComboBox2Enter
    OnSelect = ComboBox2Select
  end
  object Button3: TButton
    Left = 168
    Top = 88
    Width = 73
    Height = 25
    Caption = 'Eject'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 248
    Top = 88
    Width = 73
    Height = 25
    Caption = 'Close Tray'
    TabOrder = 6
    OnClick = Button4Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 161
    Width = 358
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object ComboBox3: TComboBox
    Left = 288
    Top = 24
    Width = 65
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 3
    TabOrder = 8
    Text = 'FLAC'
    Items.Strings = (
      'Ogg'
      'Wav'
      'Ape'
      'FLAC'
      'WMA')
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 128
    Width = 97
    Height = 17
    Caption = 'Mute'
    TabOrder = 9
    OnClick = CheckBox1Click
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ogg'
    Filter = 'Vorbis files|*.ogg'
    Left = 232
    Top = 144
  end
  object FLACOut1: TFLACOut
    Input = AudioPass1
    OnDone = OutputDone
    OnProgress = Progress
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
    Left = 72
    Top = 144
  end
  object MACOut1: TMACOut
    Input = AudioPass1
    OnDone = OutputDone
    OnProgress = Progress
    FileMode = foRewrite
    CompressionLevel = 2000
    MaxAudioBytes = -1
    Left = 104
    Top = 144
  end
  object WaveOut1: TWaveOut
    Input = AudioPass1
    OnDone = OutputDone
    OnProgress = Progress
    FileMode = foRewrite
    WavType = wtPCM
    BlockSize = 512
    Left = 136
    Top = 144
  end
  object VorbisOut1: TVorbisOut
    Input = AudioPass1
    OnDone = OutputDone
    OnProgress = Progress
    FileMode = foRewrite
    Compression = 0.200000002980232
    Comments.Track = 0
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 168
    Top = 144
  end
  object CDIn1: TCDIn
    Left = 8
    Top = 144
  end
  object AudioPass1: TAudioPass
    Input = CDIn1
    DeviceNumber = 0
    Mute = False
    Left = 40
    Top = 144
  end
  object WMAOut1: TWMAOut
    Input = AudioPass1
    OnDone = OutputDone
    OnProgress = Progress
    FileMode = foRewrite
    DesiredBitrate = 128
    Left = 200
    Top = 144
  end
end
