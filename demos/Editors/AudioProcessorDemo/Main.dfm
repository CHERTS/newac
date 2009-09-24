object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'AudioProcessor Demo'
  ClientHeight = 348
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = Button2Click
  end
  object AudioProcessor1: TAudioProcessor
    Input = WaveIn1
    OnGetData = AudioProcessor1GetData
    OnInit = AudioProcessor1Init
    Left = 96
    Top = 40
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    Left = 32
    Top = 40
  end
  object DXAudioOut1: TDXAudioOut
    Input = AudioProcessor1
    OnDone = DXAudioOut1Done
    DeviceNumber = 0
    FramesInBuffer = 32768
    PollingInterval = 100
    Left = 176
    Top = 40
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'wav'
    Filter = 'Wave Files|*.wav'
    Left = 32
    Top = 96
  end
end
