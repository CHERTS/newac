object Form1: TForm1
  Left = 192
  Top = 109
  Width = 243
  Height = 192
  Caption = 'Wav2Ogg Converter'
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
    Left = 16
    Top = 48
    Width = 56
    Height = 13
    Caption = 'CPU Usage'
  end
  object Label2: TLabel
    Left = 8
    Top = 88
    Width = 22
    Height = 13
    Caption = 'Less'
  end
  object Label3: TLabel
    Left = 56
    Top = 88
    Width = 24
    Height = 13
    Caption = 'More'
  end
  object Label4: TLabel
    Left = 104
    Top = 16
    Width = 30
    Height = 13
    Caption = 'Bitrate'
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
    Top = 112
    Width = 217
    Height = 9
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object TrackBar1: TTrackBar
    Left = 8
    Top = 64
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
  object ComboBox1: TComboBox
    Left = 144
    Top = 8
    Width = 81
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 4
    TabOrder = 3
    Text = '192'
    OnSelect = ComboBox1Select
    Items.Strings = (
      '24'
      '32'
      '64'
      '128'
      '192'
      '256'
      '320'
      '499')
  end
  object CheckBox1: TCheckBox
    Left = 104
    Top = 64
    Width = 121
    Height = 17
    Caption = 'Switch mono/stereo'
    TabOrder = 4
    OnClick = CheckBox1Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 139
    Width = 235
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    StartSample = 0
    Left = 72
    Top = 128
  end
  object VorbisOut1: TVorbisOut
    Input = WaveIn1
    SuspendWhenIdle = True
    OnDone = VorbisOut1Done
    OnProgress = VorbisOut1Progress
    FileMode = foRewrite
    Compression = 0.400000005960464
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 104
    Top = 128
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 8
    Top = 128
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ogg'
    Left = 40
    Top = 128
  end
  object MSConverter1: TMSConverter
    Mode = msmMonoToBoth
    Left = 144
    Top = 128
  end
end
