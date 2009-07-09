object Form1: TForm1
  Left = 539
  Top = 393
  Caption = 'i-Radio ;)'
  ClientHeight = 191
  ClientWidth = 361
  Color = 3763336
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    361
    191)
  PixelsPerInch = 96
  TextHeight = 13
  object Label14: TLabel
    Left = 264
    Top = 56
    Width = 3
    Height = 13
  end
  object Label15: TLabel
    Left = 297
    Top = 45
    Width = 38
    Height = 13
    Caption = 'Label15'
  end
  object Label16: TLabel
    Left = 232
    Top = 45
    Width = 38
    Height = 13
    Caption = 'Latency'
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 40
    Width = 65
    Height = 25
    Caption = 'Play'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 88
    Top = 40
    Width = 49
    Height = 25
    Caption = 'Pause'
    DoubleBuffered = True
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 152
    Top = 40
    Width = 49
    Height = 25
    Caption = 'Stop'
    DoubleBuffered = True
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
    OnClick = BitBtn3Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 172
    Width = 361
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ParentColor = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 91
    Width = 361
    Height = 81
    Align = alBottom
    BevelOuter = bvNone
    Color = 2240534
    TabOrder = 4
    DesignSize = (
      361
      81)
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 23
      Height = 13
      Caption = 'Artist'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label3: TLabel
      Left = 48
      Top = 8
      Width = 32
      Height = 13
      Caption = 'Label3'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Top = 24
      Width = 29
      Height = 13
      Caption = 'Album'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label9: TLabel
      Left = 48
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Label9'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label10: TLabel
      Left = 8
      Top = 40
      Width = 20
      Height = 13
      Caption = 'Title'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label11: TLabel
      Left = 48
      Top = 40
      Width = 38
      Height = 13
      Caption = 'Label11'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label12: TLabel
      Left = 8
      Top = 56
      Width = 22
      Height = 13
      Caption = 'Year'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label13: TLabel
      Left = 48
      Top = 56
      Width = 38
      Height = 13
      Caption = 'Label13'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label4: TLabel
      Left = 230
      Top = 8
      Width = 62
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'samplerate'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label1: TLabel
      Left = 232
      Top = 24
      Width = 73
      Height = 17
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'bitrate'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label6: TLabel
      Left = 232
      Top = 56
      Width = 57
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Total time'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label7: TLabel
      Left = 296
      Top = 56
      Width = 39
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Label7'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label8: TLabel
      Left = 232
      Top = 40
      Width = 31
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'mono'
      Color = 1716752
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 80
    Width = 361
    Height = 11
    Align = alBottom
    TabOrder = 5
  end
  object ComboBox1: TComboBox
    Left = 0
    Top = 8
    Width = 353
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 6
    Text = 'ComboBox1'
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WAX Shortcuts|*.wax|ASX Shortcuts|*.asx'
    Left = 136
    Top = 152
  end
  object DXAudioOut1: TDXAudioOut
    Input = AudioSynchronizer1
    OnDone = AudioOut1Done
    OnProgress = AudioOut1Progress
    DeviceNumber = 0
    FramesInBuffer = 65536
    PollingInterval = 200
    OnUnderrun = DXAudioOut1Underrun
    Left = 96
    Top = 152
  end
  object WMStreamedIn1: TWMStreamedIn
    Loop = False
    BufferingTime = 2
    EnableHTTP = False
    EnableTCP = False
    EnableUDP = False
    MaxWaitMilliseconds = 10000
    ProxyPort = 0
    StretchFactor = 1.000000000000000000
    OnStreamOpened = WMStreamedIn1StreamOpened
    OnStartedPlaying = WMStreamedIn1StartedPlaying
    Left = 8
    Top = 152
  end
  object AudioSynchronizer1: TAudioSynchronizer
    Input = WMStreamedIn1
    Latency = 1500
    Threshold = 2000
    Left = 56
    Top = 152
  end
  object AudioHiResTimer1: TAudioHiResTimer
    Interval = 1000
    Resolution = 5
    FireOnce = False
    OnTimer = AudioHiResTimer1Timer
    Left = 192
    Top = 152
  end
end
