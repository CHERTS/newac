object Form1: TForm1
  Left = 192
  Top = 114
  Width = 399
  Height = 165
  Caption = 'mp3 -> wav converter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 114
    Width = 391
    Height = 17
    Align = alBottom
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object MP3In1: TMP3In
    EndSample = -1
    Loop = False
    Left = 16
    Top = 64
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'mp3'
    Filter = 'mp3|*.mp3'
    Left = 80
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wav'
    Filter = 'Wav files|*.wav'
    Left = 120
    Top = 64
  end
  object WaveOut1: TWaveOut
    Input = MP3In1
    OnDone = WaveOut1Done
    OnProgress = WaveOut1Progress
    FileMode = foRewrite
    WavType = wtPCM
    BlockSize = 512
    Left = 48
    Top = 64
  end
end
