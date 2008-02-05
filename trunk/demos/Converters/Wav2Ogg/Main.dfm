object Form1: TForm1
  Left = 372
  Top = 218
  Width = 276
  Height = 309
  Caption = 'Wav2Ogg Converter'
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
    Width = 32
    Height = 13
    Caption = 'Quality'
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
    Top = 228
    Width = 268
    Height = 9
    Align = alBottom
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object SpinEdit1: TSpinEdit
    Left = 48
    Top = 40
    Width = 33
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 2
    Value = 2
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 256
    Width = 268
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 237
    Width = 268
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
  end
  object AlbumEdit: TEdit
    Left = 56
    Top = 80
    Width = 201
    Height = 21
    TabOrder = 5
  end
  object ArtistEdit: TEdit
    Left = 56
    Top = 104
    Width = 201
    Height = 21
    TabOrder = 6
  end
  object DateEdit: TEdit
    Left = 56
    Top = 128
    Width = 201
    Height = 21
    TabOrder = 7
  end
  object GenreEdit: TEdit
    Left = 56
    Top = 152
    Width = 201
    Height = 21
    TabOrder = 8
  end
  object TitleEdit: TEdit
    Left = 56
    Top = 176
    Width = 201
    Height = 21
    TabOrder = 9
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 10
    OnClick = Button2Click
  end
  object TrackEdit: TEdit
    Left = 56
    Top = 200
    Width = 97
    Height = 21
    TabOrder = 11
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    Left = 72
    Top = 232
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 8
    Top = 232
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ogg'
    Left = 40
    Top = 232
  end
  object VorbisOut1: TVorbisOut
    Input = WaveIn1
    OnDone = VorbisOut1Done
    OnProgress = VorbisOut1Progress
    Compression = 0.400000005960464
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    FileMode = foRewrite
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 104
    Top = 232
  end
end
