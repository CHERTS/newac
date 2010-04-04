object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'VOB to WMA Converter'
  ClientHeight = 238
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 66
    Width = 26
    Height = 13
    Caption = 'Artist'
  end
  object Label2: TLabel
    Left = 8
    Top = 93
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Button1: TButton
    Left = 24
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 105
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 50
    Top = 63
    Width = 159
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 50
    Top = 90
    Width = 159
    Height = 21
    TabOrder = 3
    Text = 'Edit2'
  end
  object Button3: TButton
    Left = 186
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 4
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 219
    Width = 643
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 329
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'vob'
    Filter = 'VOB files|*.vob'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Select files'
    Left = 240
    Top = 88
  end
  object WMAOut1: TWMAOut
    OnDone = WMAOut1Done
    OnThreadException = WMAOut1ThreadException
    DesiredBitrate = 0
    Lossless = True
    VBR = False
    VBRQuality = 0
    Left = 424
    Top = 88
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wma'
    Filter = 'Windows Media Audio|*.wma'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save as'
    Left = 488
    Top = 88
  end
  object DTSIn1: TDTSIn
    Loop = False
    OutputChannels = dts5dot1
    Left = 296
    Top = 88
  end
  object AC3In1: TAC3In
    Loop = False
    VobAudioSubstream = acvStreamFirst
    OutputChannels = acc5dot1
    Left = 296
    Top = 48
  end
end
