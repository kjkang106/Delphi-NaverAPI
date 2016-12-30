object FMain: TFMain
  Left = 1764
  Top = 108
  Caption = 'Sample'
  ClientHeight = 683
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
    ActivePage = tsSearch
    Align = alClient
    TabOrder = 0
    OnChange = pcMainChange
    OnChanging = pcMainChanging
    ExplicitHeight = 579
    object tsInit: TTabSheet
      Caption = 'tsInit'
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
        Top = 48
        Width = 93
        Height = 13
        Hint = 'Speed'
        Caption = 'Naver Client Secret'
      end
      object Label3: TLabel
        Left = 32
        Top = 104
        Width = 82
        Height = 13
        Hint = 'Speed'
        Caption = 'Web Service URL'
      end
      object Label4: TLabel
        Left = 152
        Top = 128
        Width = 143
        Height = 13
        Hint = 'Speed'
        Caption = 'Web Service URL for MAP API'
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
      object etNAPIWebServiceURL: TEdit
        Left = 152
        Top = 101
        Width = 321
        Height = 21
        ImeName = 'Microsoft IME 2010'
        TabOrder = 2
      end
    end
    object tsTTS: TTabSheet
      Caption = 'tsTTS'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
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
      object memoTTS: TMemo
        Left = 0
        Top = 97
        Width = 1072
        Height = 411
        Align = alClient
        ImeName = 'Microsoft IME 2010'
        Lines.Strings = (
          'memoOut')
        TabOrder = 1
        ExplicitHeight = 120
      end
    end
    object tsMap: TTabSheet
      Caption = 'tsMap'
      ImageIndex = 2
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 1072
        Height = 81
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object btMapShow: TButton
          Left = 16
          Top = 16
          Width = 75
          Height = 25
          Caption = #51648#46020' '#54364#49884
          TabOrder = 0
          OnClick = btMapShowClick
        end
        object btMapGeoAddr: TButton
          Left = 369
          Top = 36
          Width = 75
          Height = 25
          Caption = #50948'/'#44221#46020#47196#52286#44592
          TabOrder = 1
          OnClick = btMapGeoAddrClick
        end
        object btMapLeft: TButton
          Tag = -10
          Left = 97
          Top = 17
          Width = 25
          Height = 25
          Caption = #9665
          TabOrder = 4
          OnClick = btMapLeftClick
        end
        object btMapUp: TButton
          Tag = -10
          Left = 122
          Top = 5
          Width = 25
          Height = 25
          Caption = #9651
          TabOrder = 2
          OnClick = btMapUpClick
        end
        object btMapDn: TButton
          Tag = 10
          Left = 122
          Top = 30
          Width = 25
          Height = 25
          Caption = #9661
          TabOrder = 5
          OnClick = btMapUpClick
        end
        object btMapRight: TButton
          Tag = 10
          Left = 147
          Top = 17
          Width = 25
          Height = 25
          Caption = #9655
          TabOrder = 3
          OnClick = btMapLeftClick
        end
        object btMapGeoCode: TButton
          Left = 369
          Top = 5
          Width = 75
          Height = 25
          Caption = #51452#49548#47196#52286#44592
          TabOrder = 6
          OnClick = btMapGeoCodeClick
        end
        object rgMapType: TRadioGroup
          Left = 178
          Top = -1
          Width = 185
          Height = 73
          Caption = 'Map Type'
          Columns = 2
          Items.Strings = (
            #51068#48152
            #50948#49457
            #44217#52840
            #51648#54805#46020)
          TabOrder = 7
          OnClick = rgMapTypeClick
        end
        object btMapClickEvent: TButton
          Left = 450
          Top = 5
          Width = 75
          Height = 25
          Caption = 'ClickEvent'
          TabOrder = 8
          OnClick = btMapClickEventClick
        end
        object Button2: TButton
          Left = 450
          Top = 36
          Width = 75
          Height = 25
          Caption = 'Button1'
          TabOrder = 9
        end
      end
      object panNMap: TPanel
        Left = 0
        Top = 81
        Width = 1072
        Height = 427
        Align = alClient
        TabOrder = 1
      end
    end
    object tsSearch: TTabSheet
      Caption = 'tsSearch'
      ImageIndex = 3
      ExplicitLeft = 8
      ExplicitTop = 22
      ExplicitWidth = 0
      ExplicitHeight = 551
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 1072
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object btSearch: TButton
          Left = 16
          Top = 14
          Width = 75
          Height = 25
          Caption = 'btSearch'
          TabOrder = 0
          OnClick = btSearchClick
        end
        object etSearch: TEdit
          Left = 97
          Top = 16
          Width = 121
          Height = 21
          ImeName = 'Microsoft IME 2010'
          TabOrder = 1
          Text = 'API'
          OnKeyPress = etSearchKeyPress
        end
        object cbSearchType: TComboBox
          Left = 288
          Top = 16
          Width = 145
          Height = 21
          Style = csDropDownList
          ImeName = 'Microsoft IME 2010'
          TabOrder = 2
          Items.Strings = (
            #48660#47196#44536
            #45684#49828
            #52293
            #49457#51064' '#44160#49353#50612' '#54032#48324
            #48177#44284#49324#51204
            #50689#54868
            #52852#54168#44544
            #51648#49885'iN'
            #51648#50669
            #50724#53440#48320#54872
            #50937#47928#49436
            #51060#48120#51648
            #49660#54609
            #51204#47928#51088#47308)
        end
      end
    end
  end
  object lbLog: TListBox
    Left = 0
    Top = 536
    Width = 1080
    Height = 147
    Align = alBottom
    ImeName = 'Microsoft IME 2010'
    ItemHeight = 13
    TabOrder = 1
  end
end
