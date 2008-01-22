object Form1: TForm1
  Left = 106
  Top = 69
  Width = 243
  Height = 221
  Caption = 'Wav2TTA Converter'
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
    Top = 56
    Width = 235
    Height = 9
    Align = alBottom
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 168
    Width = 235
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 65
    Width = 235
    Height = 103
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
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
  object Button2: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 4
    OnClick = Button2Click
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    Left = 88
    Top = 56
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 16
    Top = 56
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'tta'
    FilterIndex = 0
    Left = 56
    Top = 56
  end
  object TTAOut1: TTTAOut
    Input = WaveIn1
    OnDone = TTAOut1Done
    OnProgress = TTAOut1Progress
    OnThreadException = TTAOut1ThreadException
    FileMode = foRewrite
    Id3v1Tags.Year = 0
    Id3v1Tags.Track = 0
    Id3v1Tags.Genre = 'Blues'
    Id3v1Tags.GenreId = 0
    Left = 128
    Top = 56
  end
end
