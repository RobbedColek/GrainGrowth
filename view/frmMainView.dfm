object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Grain Growth - Piotr Swat'
  ClientHeight = 929
  ClientWidth = 800
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
    Width = 800
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
    Width = 800
    Height = 129
    Align = alTop
    TabOrder = 0
    object LblHeight: TLabel
      Left = 20
      Top = 25
      Width = 31
      Height = 13
      Alignment = taCenter
      Caption = 'Height'
    end
    object LblWidth: TLabel
      Left = 20
      Top = 75
      Width = 28
      Height = 13
      Alignment = taCenter
      Caption = 'Width'
    end
    object BtnClear: TSpeedButton
      Left = 680
      Top = 47
      Width = 97
      Height = 22
      Action = ActClearButton
      Caption = 'Clear'
    end
    object BtnStart: TSpeedButton
      Left = 680
      Top = 16
      Width = 97
      Height = 22
      Action = ActGrainGrowthTimer
    end
    object LblInterval: TLabel
      Left = 680
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
      Top = 25
      Width = 99
      Height = 13
      Alignment = taCenter
      Caption = 'Boundary Conditions'
    end
    object LblNucleation: TLabel
      Left = 168
      Top = 75
      Width = 50
      Height = 13
      Alignment = taCenter
      Caption = 'Nucleation'
    end
    object LblNucleationVariable1: TLabel
      Left = 312
      Top = 25
      Width = 74
      Height = 13
      Alignment = taCenter
      Caption = 'Number in rows'
    end
    object LblNucleationVariable2: TLabel
      Left = 312
      Top = 75
      Width = 89
      Height = 13
      Alignment = taCenter
      Caption = 'Number in columns'
    end
    object LblNeighbourhoodType: TLabel
      Left = 456
      Top = 25
      Width = 100
      Height = 13
      Caption = 'Neighbourhood Type'
    end
    object EdtIterationsGameOfLife: TEdit
      Left = 20
      Top = 44
      Width = 121
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 0
      Text = '200'
    end
    object EdtWidthGameOfLife: TEdit
      Left = 20
      Top = 94
      Width = 121
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 1
      Text = '200'
    end
    object EdtInterval: TEdit
      Left = 680
      Top = 94
      Width = 97
      Height = 21
      Alignment = taCenter
      TabOrder = 2
      Text = '1000'
    end
    object CmbBoundaryConditions: TComboBox
      Left = 168
      Top = 44
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
      Top = 44
      Width = 121
      Height = 21
      TabOrder = 4
    end
    object EdtNucleationVariable2: TEdit
      Left = 312
      Top = 94
      Width = 121
      Height = 21
      TabOrder = 5
    end
    object CmbNucleation: TComboBox
      Left = 168
      Top = 94
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
      Top = 44
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
  end
  object ActionList: TActionList
    Left = 672
    Top = 168
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
  end
  object TimerGrainGrowth: TTimer
    Enabled = False
    OnTimer = TimerGrainGrowthTimer
    Left = 741
    Top = 241
  end
end
