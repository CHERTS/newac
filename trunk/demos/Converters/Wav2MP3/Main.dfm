object Form1: TForm1
  Left = 192
  Top = 109
  Caption = 'Wav2MP3 Converter'
  ClientHeight = 287
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    326
    287)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 48
    Width = 30
    Height = 13
    Caption = 'Bitrate'
  end
  object Label5: TLabel
    Left = 8
    Top = 125
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label6: TLabel
    Left = 8
    Top = 150
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label7: TLabel
    Left = 8
    Top = 174
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label9: TLabel
    Left = 8
    Top = 210
    Width = 22
    Height = 13
    Caption = 'Year'
  end
  object Label1: TLabel
    Left = 144
    Top = 48
    Width = 57
    Height = 13
    Caption = 'VBR Quality'
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
    Left = 8
    Top = 253
    Width = 310
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 268
    Width = 326
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object ComboBox1: TComboBox
    Left = 48
    Top = 40
    Width = 73
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 5
    TabOrder = 3
    Text = '128'
    Items.Strings = (
      '48'
      '56'
      '64'
      '80'
      '96'
      '128'
      '192'
      '256'
      '320')
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
  object Edit1: TEdit
    Left = 48
    Top = 121
    Width = 250
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object Edit2: TEdit
    Left = 48
    Top = 150
    Width = 250
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object Edit3: TEdit
    Left = 48
    Top = 177
    Width = 250
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object Edit5: TEdit
    Left = 48
    Top = 206
    Width = 250
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
  end
  object ComboBox2: TComboBox
    Left = 207
    Top = 40
    Width = 73
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 5
    TabOrder = 9
    Text = '5'
    Items.Strings = (
      '0'
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9')
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 80
    Width = 65
    Height = 17
    Caption = 'VBR'
    TabOrder = 10
  end
  object CheckBox2: TCheckBox
    Left = 79
    Top = 80
    Width = 89
    Height = 17
    Caption = 'Bit Reservoir'
    TabOrder = 11
  end
  object CheckBox3: TCheckBox
    Left = 174
    Top = 80
    Width = 65
    Height = 17
    Caption = 'Strict ISO'
    TabOrder = 12
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    Left = 136
    Top = 211
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 16
    Top = 219
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'mp3'
    Left = 80
    Top = 211
  end
  object MP3Out1: TMP3Out
    Input = WaveIn1
    OnDone = MP3Out1Done
    OnProgress = MP3Out1Progress
    OnThreadException = MP3Out1ThreadException
    ShareMode = 0
    BitRate = mbrAuto
    CRC = True
    EnableBitReservoir = False
    StrictISO = False
    VBRQuality = mp3ql5
    EnableVBR = False
    AverageBitrate = mbrAuto
    MaximumBitrate = mbrAuto
    Left = 200
    Top = 211
  end
end
