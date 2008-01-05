object Form1: TForm1
  Left = 192
  Top = 109
  Width = 280
  Height = 152
  Caption = 'APE2MP3 Converter'
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
    Left = 184
    Top = 8
    Width = 56
    Height = 13
    Caption = 'CPU Usage'
  end
  object Label2: TLabel
    Left = 176
    Top = 48
    Width = 22
    Height = 13
    Caption = 'Less'
  end
  object Label3: TLabel
    Left = 224
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
    Width = 257
    Height = 9
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object TrackBar1: TTrackBar
    Left = 176
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
    Width = 272
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
    Width = 81
    Height = 22
    Increment = 500
    MaxValue = 10000
    MinValue = 1000
    TabOrder = 4
    Value = 2000
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    StartSample = 0
    Left = 72
    Top = 88
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 8
    Top = 88
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ogg'
    Left = 40
    Top = 88
  end
  object MACOut1: TMACOut
    Input = WaveIn1
    SuspendWhenIdle = True
    OnDone = MACOut1Done
    OnProgress = MACOut1Progress
    FileMode = foRewrite
    CompressionLevel = 2000
    MaxAudioBytes = -1
    Left = 112
    Top = 88
  end
end
