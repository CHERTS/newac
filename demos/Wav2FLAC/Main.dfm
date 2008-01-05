object Form1: TForm1
  Left = 109
  Top = 115
  Width = 225
  Height = 152
  Caption = 'Wav2FLAC Converter'
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
    Left = 144
    Top = 8
    Width = 56
    Height = 13
    Caption = 'CPU Usage'
  end
  object Label2: TLabel
    Left = 136
    Top = 48
    Width = 22
    Height = 13
    Caption = 'Less'
  end
  object Label3: TLabel
    Left = 184
    Top = 48
    Width = 24
    Height = 13
    Caption = 'More'
  end
  object Label4: TLabel
    Left = 8
    Top = 48
    Width = 60
    Height = 13
    Caption = 'Compression'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 72
    Width = 201
    Height = 9
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object TrackBar1: TTrackBar
    Left = 136
    Top = 24
    Width = 70
    Height = 20
    Max = 3
    Min = 1
    Orientation = trHorizontal
    Frequency = 1
    Position = 3
    SelEnd = 0
    SelStart = 0
    TabOrder = 2
    ThumbLength = 15
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 99
    Width = 217
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object SpinEdit1: TSpinEdit
    Left = 80
    Top = 40
    Width = 41
    Height = 22
    MaxValue = 8
    MinValue = 0
    TabOrder = 4
    Value = 2
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    Left = 72
    Top = 88
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 8
    Top = 88
  end
  object SaveDialog1: TSaveDialog
    Left = 40
    Top = 88
  end
  object FLACOut1: TFLACOut
    Input = WaveIn1
    SuspendWhenIdle = True
    OnDone = FLACOut1Done
    OnProgress = FLACOut1Progress
    FileMode = foRewrite
    BestModelSearch = False
    BlockSize = 4608
    CompressionLevel = 2
    EnableMidSideStereo = True
    EnableLooseMidSideStereo = False
    MaxLPCOrder = 0
    MaxResidualPartitionOrder = 0
    MinResidualPartitionOrder = 0
    QLPCoeffPrecision = 0
    QLPCoeffPrecisionSearch = False
    Verify = False
    Left = 112
    Top = 88
  end
end
