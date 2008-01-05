object Form1: TForm1
  Left = 106
  Top = 69
  Width = 243
  Height = 335
  Caption = 'Wav2WavPack Converter'
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
    Left = 136
    Top = 8
    Width = 56
    Height = 13
    Caption = 'CPU Usage'
  end
  object Label2: TLabel
    Left = 128
    Top = 48
    Width = 22
    Height = 13
    Caption = 'Less'
  end
  object Label3: TLabel
    Left = 176
    Top = 48
    Width = 24
    Height = 13
    Caption = 'More'
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
    Left = 0
    Top = 169
    Width = 235
    Height = 9
    Align = alBottom
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object TrackBar1: TTrackBar
    Left = 128
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
    Top = 282
    Width = 235
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 72
    Width = 105
    Height = 89
    Caption = 'Compression'
    ItemIndex = 0
    Items.Strings = (
      'Fast'
      'High'
      'Very High')
    TabOrder = 4
  end
  object CheckBox1: TCheckBox
    Left = 120
    Top = 112
    Width = 89
    Height = 17
    Caption = 'Joint Stereo'
    TabOrder = 5
  end
  object CheckBox2: TCheckBox
    Left = 120
    Top = 80
    Width = 89
    Height = 17
    Caption = 'Hybrid Mode'
    TabOrder = 6
  end
  object Panel1: TPanel
    Left = 0
    Top = 178
    Width = 235
    Height = 104
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 7
    object Label4: TLabel
      Left = 8
      Top = 16
      Width = 23
      Height = 13
      Caption = 'Artist'
    end
    object Label5: TLabel
      Left = 8
      Top = 40
      Width = 29
      Height = 13
      Caption = 'Album'
    end
    object Label6: TLabel
      Left = 8
      Top = 64
      Width = 20
      Height = 13
      Caption = 'Title'
    end
    object Label7: TLabel
      Left = 8
      Top = 88
      Width = 22
      Height = 13
      Caption = 'Year'
    end
    object Edit1: TEdit
      Left = 48
      Top = 8
      Width = 177
      Height = 21
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 48
      Top = 32
      Width = 177
      Height = 21
      TabOrder = 1
    end
    object Edit3: TEdit
      Left = 48
      Top = 56
      Width = 177
      Height = 21
      TabOrder = 2
    end
    object Edit4: TEdit
      Left = 48
      Top = 80
      Width = 177
      Height = 21
      TabOrder = 3
    end
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    Left = 72
    Top = 136
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 8
    Top = 136
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wv'
    FilterIndex = 0
    Left = 40
    Top = 136
  end
  object WVOut1: TWVOut
    Input = WaveIn1
    SuspendWhenIdle = True
    OnDone = WVOut1Done
    OnProgress = WVOut1Progress
    FileMode = foRewrite
    Left = 104
    Top = 136
  end
end
