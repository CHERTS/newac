object Form1: TForm1
  Left = 192
  Top = 109
  Width = 258
  Height = 192
  Caption = 'Wav2Ogg Converter'
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
  object Label4: TLabel
    Left = -8
    Top = 72
    Width = 30
    Height = 13
    Caption = 'Bitrate'
  end
  object Button1: TButton
    Left = 88
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
    Width = 233
    Height = 9
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object ComboBox1: TComboBox
    Left = 32
    Top = 64
    Width = 81
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 4
    TabOrder = 2
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
    Left = 128
    Top = 64
    Width = 121
    Height = 17
    Caption = 'Switch mono/stereo'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 139
    Width = 250
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Button2: TButton
    Left = 0
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 5
    OnClick = Button2Click
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    Left = 72
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
  object VorbisOut1: TVorbisOut
    Input = WaveIn1
    OnDone = VorbisOut1Done
    OnProgress = VorbisOut1Progress
    FileMode = foRewrite
    Compression = 0.200000002980232
    Comments.Track = 0
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 104
    Top = 128
  end
  object MSConverter1: TMSConverter
    Mode = msmMonoToBoth
    Left = 144
    Top = 128
  end
end
