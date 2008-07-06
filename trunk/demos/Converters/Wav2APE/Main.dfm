object Form1: TForm1
  Left = 192
  Top = 109
  Width = 288
  Height = 253
  Caption = 'Wav2Ape'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  DesignSize = (
    280
    219)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 120
    Top = 16
    Width = 60
    Height = 13
    Caption = 'Compression'
  end
  object Label5: TLabel
    Left = 8
    Top = 56
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label6: TLabel
    Left = 8
    Top = 80
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label7: TLabel
    Left = 8
    Top = 128
    Width = 29
    Height = 13
    Caption = 'Genre'
  end
  object Label8: TLabel
    Left = 8
    Top = 104
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label9: TLabel
    Left = 8
    Top = 152
    Width = 25
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Year'
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
    Top = 176
    Width = 260
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 200
    Width = 280
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object SpinEdit1: TSpinEdit
    Left = 192
    Top = 8
    Width = 57
    Height = 22
    Increment = 1000
    MaxValue = 5000
    MinValue = 1000
    TabOrder = 3
    Value = 2000
  end
  object Edit1: TEdit
    Left = 56
    Top = 48
    Width = 212
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
  object Edit2: TEdit
    Left = 56
    Top = 72
    Width = 212
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
  end
  object Edit3: TEdit
    Left = 56
    Top = 96
    Width = 212
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
  end
  object Edit4: TEdit
    Left = 56
    Top = 120
    Width = 212
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
  end
  object Edit5: TEdit
    Left = 56
    Top = 144
    Width = 212
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    Left = 64
    Top = 184
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Top = 184
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ape'
    Left = 32
    Top = 184
  end
  object MACOut1: TMACOut
    Input = WaveIn1
    OnDone = MACOut1Done
    OnProgress = MACOut1Progress
    OnThreadException = MACOut1ThreadException
    CompressionLevel = 2000
    MaxAudioBytes = -1
    Left = 104
    Top = 184
  end
end
