object Form1: TForm1
  Left = 244
  Top = 189
  Width = 299
  Height = 203
  Caption = 'Wave Player'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 110
  TextHeight = 16
  object Label6: TLabel
    Left = 128
    Top = 20
    Width = 59
    Height = 16
    Caption = 'Total time'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -14
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label7: TLabel
    Left = 201
    Top = 20
    Width = 3
    Height = 16
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -14
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object BitBtn1: TBitBtn
    Left = 10
    Top = 59
    Width = 80
    Height = 31
    Caption = 'Play...'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn3: TBitBtn
    Left = 98
    Top = 59
    Width = 61
    Height = 31
    Caption = 'Stop'
    TabOrder = 1
    OnClick = BitBtn3Click
    NumGlyphs = 2
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 149
    Width = 291
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 131
    Width = 291
    Height = 18
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 118
    Width = 291
    Height = 13
    Align = alBottom
    TabOrder = 4
  end
  object ForwardButton: TButton
    Left = 177
    Top = 59
    Width = 41
    Height = 31
    Caption = '>>'
    TabOrder = 5
    OnClick = ForwardButtonClick
  end
  object BackwardButton: TButton
    Left = 226
    Top = 59
    Width = 41
    Height = 31
    Caption = '<<'
    TabOrder = 6
    OnClick = BackwardButtonClick
  end
  object CheckBox1: TCheckBox
    Left = 10
    Top = 20
    Width = 100
    Height = 21
    Caption = 'Loop'
    TabOrder = 7
    OnClick = CheckBox1Click
  end
  object AudioOut1: TAudioOut
    SuspendWhenIdle = True
    OnDone = AudioOut1Done
    OnProgress = AudioOut1Progress
    BaseChannel = 0
    Volume = 255
    Left = 40
    Top = 96
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wave files|*.wav'
    Left = 80
    Top = 96
  end
end
