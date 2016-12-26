object FMain: TFMain
  Left = 215
  Top = 112
  Caption = 'Sample'
  ClientHeight = 536
  ClientWidth = 1080
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 1080
    Height = 536
    ActivePage = tsInit
    Align = alClient
    TabOrder = 0
    OnChange = pcMainChange
    OnChanging = pcMainChanging
    ExplicitLeft = 120
    ExplicitTop = 46
    ExplicitWidth = 289
    ExplicitHeight = 419
    object tsInit: TTabSheet
      Caption = 'tsInit'
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Label1: TLabel
        Left = 32
        Top = 21
        Width = 70
        Height = 13
        Hint = 'Speed'
        Caption = 'Naver ClientID'
      end
      object Label2: TLabel
        Left = 32
        Top = 53
        Width = 93
        Height = 13
        Hint = 'Speed'
        Caption = 'Naver Client Secret'
      end
      object etNAPIClientID: TEdit
        Left = 152
        Top = 18
        Width = 321
        Height = 21
        ImeName = 'Microsoft IME 2010'
        TabOrder = 0
      end
      object etNAPIClientSecret: TEdit
        Left = 152
        Top = 45
        Width = 321
        Height = 21
        ImeName = 'Microsoft IME 2010'
        TabOrder = 1
      end
    end
    object tsTTS: TTabSheet
      Caption = 'tsTTS'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 1072
        Height = 97
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 1080
        object lbSpeed: TLabel
          Left = 568
          Top = 13
          Width = 30
          Height = 13
          Hint = 'Speed'
          Caption = 'Speed'
        end
        object btTTS: TButton
          Left = 8
          Top = 8
          Width = 75
          Height = 25
          Caption = #51020#49457#54633#49457
          TabOrder = 0
          OnClick = btTTSClick
        end
        object rgSpeeker: TRadioGroup
          Left = 104
          Top = 8
          Width = 449
          Height = 83
          Caption = 'Speeker'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            #48120#51652'('#54620#44397#50612', '#50668#49457')'
            #51652#54840'('#54620#44397#50612', '#45224#49457')'
            #53364#46972#46972'('#50689#50612', '#50668#49457')'
            #47588#53916'('#50689#50612', '#45224#49457')'
            #50976#47532'('#51068#48376#50612', '#50668#49457')'
            #49888#51648'('#51068#48376#50612', '#45224#49457')'
            #47700#51060#47700#51060'('#51473#44397#50612', '#50668#49457')')
          TabOrder = 1
        end
        object tbSpeed: TTrackBar
          Left = 560
          Top = 32
          Width = 150
          Height = 45
          Max = 5
          Min = -5
          TabOrder = 2
          OnChange = tbSpeedChange
        end
      end
      object lbLog: TListBox
        Left = 0
        Top = 176
        Width = 1072
        Height = 332
        Align = alBottom
        ImeName = 'Microsoft IME 2010'
        ItemHeight = 13
        TabOrder = 1
      end
      object memoOut: TMemo
        Left = 0
        Top = 97
        Width = 1072
        Height = 79
        Align = alClient
        ImeName = 'Microsoft IME 2010'
        Lines.Strings = (
          'memoOut')
        TabOrder = 2
        ExplicitHeight = 120
      end
    end
  end
end
