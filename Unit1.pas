unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls,
  TMS.MQTT.Global, TMS.MQTT.Client, FMX.Layouts, FMX.ListBox,
  FMX.Effects, FMX.Filter.Effects, System.Skia, FMX.Skia, FMX.Ani,
  FMX.Styles.Objects;


// Background texture: Designed by Freepik - https://www.freepik.com/free-photo/steel-metallic-texture-background_208418939.htm
// Icons from fonts.google.com - Material icons

type
  TForm1 = class(TForm)
    MainPanel: TRectangle;
    TopPanel: TRectangle;
    Label1: TLabel;
    TMSMQTTClient1: TTMSMQTTClient;
    GoButton: TButton;
    ListBox1: TListBox;
    ShadowEffect1: TShadowEffect;
    ShadowEffect3: TShadowEffect;
    ShadowEffect4: TShadowEffect;
    CloseButton: TSkSvg;
    UpButton: TSkSvg;
    DownButton: TSkSvg;
    GlowEffect1: TGlowEffect;
    GlowEffect2: TGlowEffect;
    GlowEffect3: TGlowEffect;
    Timer1: TTimer;
    procedure TopPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure GoButtonClick(Sender: TObject);
    procedure TMSMQTTClient1ConnectedStatusChanged(ASender: TObject;
      const AConnected: Boolean; AStatus: TTMSMQTTConnectionStatus);
    procedure TMSMQTTClient1PublishReceived(ASender: TObject;
      APacketID: Word; ATopic: string; APayload: TBytes);
    procedure CloseButtonClick(Sender: TObject);
    procedure DownButtonClick(Sender: TObject);
    procedure UpButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FFirstMessage, FIsUp: boolean;
    procedure AddMsg(const AMessage: string);
    procedure SwapButtons(const IsUp: boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses modules.fmx.settingssaver;

{$i 'creds.inc'}

procedure TForm1.AddMsg(const AMessage: string);
begin
  ListBox1.Items.Insert(0, AMessage);
end;

procedure TForm1.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.DownButtonClick(Sender: TObject);
begin
  SwapButtons(False);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
{$IFDEF MSWINDOWS}
  SaveFormLoc;
{$ENDIF}
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  LoadFormLoc;
{$ENDIF}
end;

procedure TForm1.GoButtonClick(Sender: TObject);
begin
  if GoButton.Text.Equals(GoButton.Hint) then
    begin
      TMSMQTTClient1.BrokerHostName := cBrokerName;
      TMSMQTTClient1.ClientID := 'Delphi + ' + FormatDateTime('yyyymmddhhnnss', Now);
      FFirstMessage := True;
      TMSMQTTClient1.Connect;
      GoButton.Text := 'Stop';
    end
  else
    begin
      TMSMQTTClient1.Disconnect;
      GoButton.Text := GoButton.Hint;
    end
end;

procedure TForm1.SwapButtons(const IsUp: boolean);
begin
  UpButton.Visible := not IsUp;
  DownButton.Visible := IsUp;
  FIsUp := IsUp;
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);

const
  cInterval = 20;
  cFormHeight = 222;

begin
  if FIsUp then
    begin
      if Height > Succ(Trunc(TopPanel.Height)) then
        begin
          Height := Height - cInterval;
          Timer1.Enabled := True;
        end
      else
        begin
          Timer1.Enabled := false;
          Height := Succ(Trunc(TopPanel.Height));
        end;
    end
  else
    begin
      if Height < cFormHeight then
        begin
          Height := Height + cInterval;
          Timer1.Enabled := True;
        end
      else
        begin
          Timer1.Enabled := false;
          Height := cFormHeight;
        end;
    end;
end;

procedure TForm1.TMSMQTTClient1ConnectedStatusChanged(ASender: TObject; const AConnected: Boolean; AStatus: TTMSMQTTConnectionStatus);
begin
  if AConnected then
    begin
      AddMsg('Connected');
      TMSMQTTClient1.Subscribe(cChannel);
      FFirstMessage := false;
    end
  else
    if not FFirstMessage then
    begin
      AddMsg('NOT connected');
      FFirstMessage := True;
    end;
end;

procedure TForm1.TMSMQTTClient1PublishReceived(ASender: TObject;
  APacketID: Word; ATopic: string; APayload: TBytes);
begin
  AddMsg('Message received :' + TEncoding.UTF8.GetString(APayload));
end;

procedure TForm1.TopPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (Button = TMouseButton.mbLeft) then StartWindowDrag;
end;

procedure TForm1.UpButtonClick(Sender: TObject);
begin
  SwapButtons(True);
end;

end.
