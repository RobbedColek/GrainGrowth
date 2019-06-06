object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Grain Growth - Piotr Swat'
  ClientHeight = 929
  ClientWidth = 1000
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 0
    Top = 129
    Width = 1000
    Height = 800
    Align = alClient
    OnMouseDown = ImageMouseDown
    ExplicitTop = 81
    ExplicitWidth = 1290
    ExplicitHeight = 500
  end
  object MainMenu: TPanel
    Left = 0
    Top = 0
    Width = 1000
    Height = 129
    Align = alTop
    TabOrder = 0
    object LblHeight: TLabel
      Left = 20
      Top = 17
      Width = 31
      Height = 13
      Alignment = taCenter
      Caption = 'Height'
    end
    object LblWidth: TLabel
      Left = 20
      Top = 67
      Width = 28
      Height = 13
      Alignment = taCenter
      Caption = 'Width'
    end
    object BtnClear: TSpeedButton
      Left = 888
      Top = 47
      Width = 97
      Height = 22
      Action = ActClearButton
      Caption = 'Clear'
    end
    object BtnStart: TSpeedButton
      Left = 888
      Top = 16
      Width = 97
      Height = 22
      Action = ActGrainGrowthTimer
    end
    object LblInterval: TLabel
      Left = 888
      Top = 75
      Width = 38
      Height = 13
      Alignment = taCenter
      Caption = 'Interval'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LblBoundaryConditions: TLabel
      Left = 168
      Top = 17
      Width = 99
      Height = 13
      Alignment = taCenter
      Caption = 'Boundary Conditions'
    end
    object LblNucleation: TLabel
      Left = 168
      Top = 67
      Width = 50
      Height = 13
      Alignment = taCenter
      Caption = 'Nucleation'
    end
    object LblNucleationVariable1: TLabel
      Left = 312
      Top = 17
      Width = 74
      Height = 13
      Alignment = taCenter
      Caption = 'Number in rows'
    end
    object LblNucleationVariable2: TLabel
      Left = 312
      Top = 67
      Width = 89
      Height = 13
      Alignment = taCenter
      Caption = 'Number in columns'
    end
    object LblNeighbourhoodType: TLabel
      Left = 456
      Top = 17
      Width = 100
      Height = 13
      Caption = 'Neighbourhood Type'
    end
    object BtnMonteCarlo: TSpeedButton
      Left = 753
      Top = 17
      Width = 97
      Height = 22
      Action = ActMonteCarlo
      Enabled = False
    end
    object LblKTParameter: TLabel
      Left = 753
      Top = 75
      Width = 64
      Height = 13
      Alignment = taCenter
      Caption = 'kT parameter'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object BtnMonteCarloDraw: TSpeedButton
      Left = 753
      Top = 47
      Width = 97
      Height = 22
      Action = ActMonteCarloDraw
      Enabled = False
    end
    object EdtIterationsGameOfLife: TEdit
      Left = 20
      Top = 36
      Width = 121
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 0
      Text = '100'
    end
    object EdtWidthGameOfLife: TEdit
      Left = 20
      Top = 86
      Width = 121
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 1
      Text = '125'
    end
    object EdtInterval: TEdit
      Left = 888
      Top = 94
      Width = 97
      Height = 21
      Alignment = taCenter
      TabOrder = 2
      Text = '1000'
    end
    object CmbBoundaryConditions: TComboBox
      Left = 168
      Top = 36
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'Periodic'
      Items.Strings = (
        'Periodic'
        'Absorb')
    end
    object EdtNucleationVariable1: TEdit
      Left = 312
      Top = 36
      Width = 121
      Height = 21
      TabOrder = 4
      Text = '20'
    end
    object EdtNucleationVariable2: TEdit
      Left = 312
      Top = 86
      Width = 121
      Height = 21
      TabOrder = 5
      Text = '25'
    end
    object CmbNucleation: TComboBox
      Left = 168
      Top = 86
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 6
      Text = 'Homogeneus'
      OnChange = CmbNucleationChange
      Items.Strings = (
        'Homogeneus'
        'Radius'
        'Random'
        'Custom')
    end
    object CmbNeighbourhoodType: TComboBox
      Left = 456
      Top = 36
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 7
      Text = 'Moore'
      Items.Strings = (
        'Moore'
        'Von Neumann'
        'Pentagonal (Random)'
        'Hexagonal (Left)'
        'Hexagonal (Right)'
        'Hexagonal (Random)'
        'Radial')
    end
    object EdtKTParameter: TEdit
      Left = 753
      Top = 94
      Width = 97
      Height = 21
      Alignment = taCenter
      Enabled = False
      TabOrder = 8
      Text = '0.6'
    end
  end
  object ActionList: TActionList
    Left = 944
    Top = 136
    object ActDraw: TAction
      Caption = 'Draw'
    end
    object ActClear: TAction
      Caption = 'Clear'
      OnExecute = ActClearExecute
    end
    object ActClearButton: TAction
      Caption = 'ActClearButton'
      OnExecute = ActClearButtonExecute
    end
    object ActGrainGrowthTimer: TAction
      Caption = 'Start'
      OnExecute = ActGrainGrowthTimerExecute
    end
    object ActMonteCarlo: TAction
      Caption = 'Run Monte Carlo'
      OnExecute = ActMonteCarloExecute
    end
    object ActMonteCarloDraw: TAction
      Caption = 'Draw Energy'
      OnExecute = ActMonteCarloDrawExecute
    end
  end
  object TimerGrainGrowth: TTimer
    Enabled = False
    OnTimer = TimerGrainGrowthTimer
    Left = 885
    Top = 137
  end
end
