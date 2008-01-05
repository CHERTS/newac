object Form1: TForm1
  Left = 192
  Top = 109
  Width = 233
  Height = 152
  Caption = 'Wav2MP3 Converter'
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
    Left = 152
    Top = 8
    Width = 56
    Height = 13
    Caption = 'CPU Usage'
  end
  object Label2: TLabel
    Left = 144
    Top = 48
    Width = 22
    Height = 13
    Caption = 'Less'
  end
  object Label3: TLabel
    Left = 192
    Top = 48
    Width = 24
    Height = 13
    Caption = 'More'
  end
  object Label4: TLabel
    Left = 8
    Top = 48
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
    Top = 72
    Width = 209
    Height = 9
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object TrackBar1: TTrackBar
    Left = 144
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
    Width = 225
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object ComboBox1: TComboBox
    Left = 48
    Top = 40
    Width = 73
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 5
    TabOrder = 4
    Text = '128'
    Items.Strings = (
      '48'
      '56'
      '64'
      '80'
      '96'
      '128'
      '192'
      '256'
      '320')
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
  object MP3Out1: TMP3Out
    Input = WaveIn1
    SuspendWhenIdle = True
    OnDone = MP3Out1Done
    OnProgress = MP3Out1Progress
    FileMode = foRewrite
    BitRate = br128
    Left = 112
    Top = 88
  end
end
