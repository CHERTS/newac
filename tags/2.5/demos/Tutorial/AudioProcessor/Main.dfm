object Form1: TForm1
  Left = 354
  Top = 240
  Width = 252
  Height = 126
  Caption = 'AudioProcesssor Demo'
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
  object Button1: TButton
    Left = 152
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Play'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 72
    Width = 217
    Height = 17
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    Left = 24
    Top = 8
  end
  object AudioProcessor1: TAudioProcessor
    Input = WaveIn1
    OnGetChannels = AudioProcessor1GetChannels
    OnGetData = AudioProcessor1GetData
    OnGetSize = AudioProcessor1GetSize
    OnInit = AudioProcessor1Init
    Left = 64
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wave Files|*.wav'
    Left = 144
    Top = 8
  end
  object DXAudioOut1: TDXAudioOut
    Input = AudioProcessor1
    OnDone = DXAudioOut1Done
    OnProgress = DXAudioOut1Progress
    DeviceNumber = 0
    Left = 104
    Top = 8
  end
end
