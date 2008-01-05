object Form1: TForm1
  Left = 192
  Top = 109
  Width = 385
  Height = 200
  Caption = 'Mixer Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
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
    Max = 51
    Orientation = trHorizontal
    Frequency = 1
    Position = 25
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
    Max = 51
    Orientation = trHorizontal
    Frequency = 1
    Position = 25
    SelEnd = 0
    SelStart = 0
    TabOrder = 4
    ThumbLength = 16
    TickMarks = tmBottomRight
    TickStyle = tsNone
    OnChange = TrackBar2Change
  end
  object RadioGroup1: TRadioGroup
    Left = 112
    Top = 64
    Width = 105
    Height = 57
    Caption = 'Action'
    ItemIndex = 0
    Items.Strings = (
      'Mix'
      'Concatenate')
    TabOrder = 5
  end
  object RadioGroup2: TRadioGroup
    Left = 232
    Top = 64
    Width = 105
    Height = 57
    Caption = 'Output'
    ItemIndex = 0
    Items.Strings = (
      'File'
      'Speakers')
    TabOrder = 6
  end
  object Button4: TButton
    Left = 16
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 7
    OnClick = Button4Click
  end
  object CheckBox1: TCheckBox
    Left = 248
    Top = 136
    Width = 89
    Height = 17
    Caption = 'Normalize'
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    StartSample = 0
    Left = 16
    Top = 128
  end
  object WaveIn2: TWaveIn
    EndSample = -1
    Loop = False
    StartSample = 0
    Left = 48
    Top = 128
  end
  object AudioMixer1: TAudioMixer
    Input1 = WaveIn1
    Input2 = WaveIn2
    Mode = amMix
    Input2Start = 0
    Volume1 = 255
    Volume2 = 255
    Left = 80
    Top = 128
  end
  object WaveOut1: TWaveOut
    Input = AudioMixer1
    SuspendWhenIdle = True
    OnDone = WaveOut1Done
    FileMode = foRewrite
    WavType = wtPCM
    BlockSize = 512
    Left = 144
    Top = 128
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WAV files|*.wav'
    Left = 120
    Top = 128
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wav'
    Left = 176
    Top = 128
  end
  object DXAudioOut1: TDXAudioOut
    Input = AudioMixer1
    SuspendWhenIdle = True
    OnDone = WaveOut1Done
    DeviceNumber = 0
    Left = 208
    Top = 128
  end
end
