object Form1: TForm1
  Left = 192
  Top = 114
  Width = 399
  Height = 165
  Caption = 'mp3 -> wma converter'
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
    Top = 24
    Width = 69
    Height = 13
    Caption = 'Desired Bitrate'
  end
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
  object Edit1: TEdit
    Left = 184
    Top = 16
    Width = 73
    Height = 21
    TabOrder = 2
    Text = '256'
  end
  object WMAOut1: TWMAOut
    Input = MP3In1
    OnDone = WMAOut1Done
    OnProgress = WMAOut1Progress
    DesiredBitrate = 0
    Left = 48
    Top = 64
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'mp3'
    Filter = 'mp3|*.mp3'
    Left = 80
    Top = 64
  end
  object MP3In1: TMP3In
    EndSample = -1
    Loop = False
    Left = 16
    Top = 64
  end
end
