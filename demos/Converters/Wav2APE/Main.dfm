object Form1: TForm1
  Left = 192
  Top = 109
  Width = 285
  Height = 275
  Caption = 'Wav2Ape'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    277
    241)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 184
    Top = 8
    Width = 56
    Height = 13
    Caption = 'CPU Usage'
  end
  object Label2: TLabel
    Left = 176
    Top = 48
    Width = 22
    Height = 13
    Caption = 'Less'
  end
  object Label3: TLabel
    Left = 224
    Top = 48
    Width = 24
    Height = 13
    Caption = 'More'
  end
  object Label4: TLabel
    Left = 8
    Top = 48
    Width = 60
    Height = 13
    Caption = 'Compression'
  end
  object Label5: TLabel
    Left = 8
    Top = 80
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label6: TLabel
    Left = 8
    Top = 104
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label7: TLabel
    Left = 8
    Top = 152
    Width = 29
    Height = 13
    Caption = 'Genre'
  end
  object Label8: TLabel
    Left = 8
    Top = 128
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label9: TLabel
    Left = 8
    Top = 176
    Width = 22
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
    Top = 200
    Width = 257
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object TrackBar1: TTrackBar
    Left = 176
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
    Top = 222
    Width = 277
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object SpinEdit1: TSpinEdit
    Left = 80
    Top = 40
    Width = 81
    Height = 22
    Increment = 500
    MaxValue = 10000
    MinValue = 1000
    TabOrder = 4
    Value = 2000
  end
  object Edit1: TEdit
    Left = 56
    Top = 72
    Width = 209
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
  end
  object Edit2: TEdit
    Left = 56
    Top = 96
    Width = 209
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
  end
  object Edit3: TEdit
    Left = 56
    Top = 120
    Width = 209
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
  end
  object Edit4: TEdit
    Left = 56
    Top = 144
    Width = 209
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
  end
  object Edit5: TEdit
    Left = 56
    Top = 168
    Width = 209
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 9
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    Left = 64
    Top = 208
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Top = 208
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ape'
    Left = 32
    Top = 208
  end
  object MACOut1: TMACOut
    Input = WaveIn1
    OnDone = MACOut1Done
    OnProgress = MACOut1Progress
    CompressionLevel = 2000
    MaxAudioBytes = -1
    Left = 96
    Top = 208
  end
end
