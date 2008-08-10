object Form1: TForm1
  Left = 395
  Top = 226
  Width = 304
  Height = 317
  Caption = 'Wav2WMA Converter'
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
  object Label6: TLabel
    Left = 8
    Top = 80
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label7: TLabel
    Left = 8
    Top = 104
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label8: TLabel
    Left = 8
    Top = 128
    Width = 23
    Height = 13
    Caption = 'Date'
  end
  object Label9: TLabel
    Left = 8
    Top = 152
    Width = 29
    Height = 13
    Caption = 'Genre'
  end
  object Label10: TLabel
    Left = 8
    Top = 176
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label11: TLabel
    Left = 8
    Top = 200
    Width = 28
    Height = 13
    Caption = 'Track'
  end
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 69
    Height = 13
    Caption = 'Desired Bitrate'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 236
    Width = 296
    Height = 9
    Align = alBottom
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 264
    Width = 296
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 245
    Width = 296
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
  end
  object AlbumEdit: TEdit
    Left = 56
    Top = 80
    Width = 201
    Height = 21
    TabOrder = 4
  end
  object ArtistEdit: TEdit
    Left = 56
    Top = 104
    Width = 201
    Height = 21
    TabOrder = 5
  end
  object DateEdit: TEdit
    Left = 56
    Top = 128
    Width = 201
    Height = 21
    TabOrder = 6
  end
  object GenreEdit: TEdit
    Left = 56
    Top = 152
    Width = 201
    Height = 21
    TabOrder = 7
  end
  object TitleEdit: TEdit
    Left = 56
    Top = 176
    Width = 201
    Height = 21
    TabOrder = 8
  end
  object TrackSpinEdit: TSpinEdit
    Left = 56
    Top = 200
    Width = 49
    Height = 22
    MaxValue = 99
    MinValue = 0
    TabOrder = 9
    Value = 0
  end
  object Edit1: TEdit
    Left = 88
    Top = 48
    Width = 73
    Height = 21
    TabOrder = 10
    Text = '128'
  end
  object Button2: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 11
    OnClick = Button2Click
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    Left = 72
    Top = 240
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 8
    Top = 240
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wma'
    Left = 40
    Top = 240
  end
  object WMAOut1: TWMAOut
    Input = WaveIn1
    OnDone = WMAOut1Done
    OnProgress = WMAOut1Progress
    DesiredBitrate = 0
    Left = 104
    Top = 240
  end
end
