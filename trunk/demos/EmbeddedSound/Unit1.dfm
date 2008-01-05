object Form1: TForm1
  Left = 200
  Top = 337
  Width = 196
  Height = 161
  Caption = 'Alarm Clock'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 89
    Height = 16
    AutoSize = False
    Caption = 'Label1'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 38
    Height = 13
    Caption = 'Alarm at'
  end
  object Edit1: TEdit
    Left = 56
    Top = 48
    Width = 57
    Height = 21
    TabOrder = 0
    Text = '12:00'
  end
  object Button1: TButton
    Left = 120
    Top = 48
    Width = 57
    Height = 25
    Caption = 'Set'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 88
  end
  object WMIn1: TWMIn
    EndSample = -1
    Loop = False
    Left = 48
    Top = 88
  end
  object DXAudioOut1: TDXAudioOut
    Input = WMIn1
    DeviceNumber = 0
    Left = 88
    Top = 88
  end
end
