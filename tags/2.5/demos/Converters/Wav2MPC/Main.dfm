object Form1: TForm1
  Left = 97
  Top = 152
  Width = 243
  Height = 278
  Caption = 'Wav2MPC Converter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  DesignSize = (
    235
    244)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 56
    Width = 32
    Height = 13
    Caption = 'Quality'
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label3: TLabel
    Left = 8
    Top = 120
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label4: TLabel
    Left = 8
    Top = 144
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label5: TLabel
    Left = 8
    Top = 168
    Width = 22
    Height = 13
    Caption = 'Year'
  end
  object Label6: TLabel
    Left = 8
    Top = 192
    Width = 28
    Height = 13
    Caption = 'Track'
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
    Top = 235
    Width = 235
    Height = 9
    Align = alBottom
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 216
    Width = 235
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Button2: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 3
    OnClick = Button2Click
  end
  object SpinEdit1: TSpinEdit
    Left = 72
    Top = 56
    Width = 41
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 4
    Value = 5
  end
  object Edit1: TEdit
    Left = 48
    Top = 96
    Width = 177
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
  end
  object Edit2: TEdit
    Left = 48
    Top = 120
    Width = 177
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
  end
  object Edit3: TEdit
    Left = 48
    Top = 144
    Width = 177
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
  end
  object Edit4: TEdit
    Left = 48
    Top = 168
    Width = 177
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
  end
  object Edit5: TEdit
    Left = 48
    Top = 192
    Width = 177
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 9
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    Left = 80
    Top = 208
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 8
    Top = 208
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'mpc'
    FilterIndex = 0
    Left = 40
    Top = 208
  end
  object MPCOut1: TMPCOut
    Input = WaveIn1
    OnDone = MPCOut1Done
    OnProgress = MPCOut1Progress
    OnThreadException = MPCOut1ThreadException
    Quality = 5
    Left = 112
    Top = 208
  end
end
