object Form1: TForm1
  Left = 192
  Top = 114
  Width = 259
  Height = 302
  Caption = 'WMA to Ogg converter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    251
    268)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 160
    Top = 56
    Width = 55
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Ogg Quality'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 64
    Height = 13
    Caption = 'Input Formats'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 80
    Width = 137
    Height = 137
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object SpinEdit1: TSpinEdit
    Left = 160
    Top = 80
    Width = 65
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 10
    MinValue = 1
    TabOrder = 1
    Value = 5
  end
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 3
    OnClick = Button2Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 249
    Width = 251
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 232
    Width = 249
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    Min = 0
    Max = 100
    TabOrder = 5
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WMA files|*.wma'
    Left = 16
    Top = 200
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ogg'
    Filter = 'Ogg Vorbis Files|*.ogg'
    Left = 48
    Top = 200
  end
  object WMIn1: TWMIn
    Loop = False
    EndSample = -1
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 88
    Top = 200
  end
  object VorbisOut1: TVorbisOut
    Input = WMIn1
    OnDone = VorbisOut1Done
    OnProgress = VorbisOut1Progress
    Compression = 0.200000002980232
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    FileMode = foRewrite
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 128
    Top = 200
  end
end
