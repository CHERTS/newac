object Form1: TForm1
  Left = 372
  Top = 218
  Width = 248
  Height = 157
  Caption = 'Wav2PPK Converter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 48
    Width = 31
    Height = 13
    Caption = 'Codec'
  end
  object Button1: TButton
    Left = 104
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 76
    Width = 240
    Height = 9
    Align = alBottom
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 104
    Width = 240
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 85
    Width = 240
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 4
    OnClick = Button2Click
  end
  object ComboBox1: TComboBox
    Left = 56
    Top = 48
    Width = 97
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 5
    Text = 'Adaptive'
    Items.Strings = (
      'Adaptive'
      'Fast'
      'NLMS')
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    Left = 72
    Top = 80
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 8
    Top = 80
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ppk'
    Left = 40
    Top = 80
  end
  object PasPackOut1: TPasPackOut
    Input = WaveIn1
    OnDone = PasPackOut1Done
    OnProgress = PasPackOut1Progress
    OnThreadException = PasPackOut1ThreadException
    CodecType = ctAdaptive
    Left = 104
    Top = 80
  end
end
