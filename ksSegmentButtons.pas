unit ksSegmentButtons;

interface

uses
  Classes, FMX.Types, FMX.Controls, FMX.Graphics, Types, System.UITypes,
  FMX.StdCtrls, System.Generics.Collections;

type
  TKsSegmentButton = class(TSpeedButton)
  private
    FId: string;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ID: string read FId write FId;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or  pidiOSDevice)]
  TksSegmentButtons = class(TControl)
  private
    FButtonIndex: integer;
    FGroupID: string;
    FItemIndex: integer;
    FOnChange: TNotifyEvent;
    procedure ResizeButtons;
    procedure SetItemIndex(const Value: integer);
    function GetSelectedCaption: string;
    procedure SetSelectedCaption(const Value: string);
    function GetSelectedID: string;
    procedure DoButtonClick(Sender: TObject);
    function GetButton(index: integer): TKsSegmentButton;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddButton(ATitle, AID: string);
    procedure Clear;
    procedure SelectFirst;
    property SelectedCaption: string read GetSelectedCaption write SetSelectedCaption;
    property SelectedID: string read GetSelectedID;
    property Button[index: integer]: TKsSegmentButton read GetButton; default;
  published
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

  procedure Register;


implementation

uses SysUtils;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksSegmentButtons]);
end;

{ TksSegmentButtons }

procedure TksSegmentButtons.AddButton(ATitle, AID: string);
var
  ABtn: TKsSegmentButton;
begin
  ABtn := TKsSegmentButton.Create(Self);
  ABtn.Text := ATitle;
  ABtn.StaysPressed := True;
  ABtn.ID := AId;
  ABtn.GroupName := FGroupID;
  ABtn.OnClick := DoButtonClick;
  AddObject(ABtn);
  if FItemIndex = -1 then
    FItemIndex := 0;
  ResizeButtons;
end;

procedure TksSegmentButtons.Clear;
var
  ICount: integer;
  ABtn: TKsSegmentButton;
begin
  for ICount := ChildrenCount - 1 downto 0 do
  begin
    ABtn := Children[ICount] as TKsSegmentButton;
    RemoveObject(ABtn);
    ABtn.DisposeOf;
  end;
  FItemIndex := -1;
end;

constructor TksSegmentButtons.Create(AOwner: TComponent);
var
  AGuid: TGUID;
begin
  inherited Create(AOwner);
  CreateGUID(AGuid);
  FGroupID := GUIDToString(AGuid);
  Width := 200;
  Height := 40;
  FItemIndex := -1;
  Clear;
end;

destructor TksSegmentButtons.Destroy;
begin
  inherited;
end;

procedure TksSegmentButtons.DoButtonClick(Sender: TObject);
begin
  FItemIndex := Children.IndexOf(Sender as TFmxObject);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TksSegmentButtons.GetButton(index: integer): TKsSegmentButton;
begin
  Result := (Children[index] as TKsSegmentButton);
end;

function TksSegmentButtons.GetSelectedCaption: string;
begin
  Result := '';
  if ItemIndex > -1 then
    Result := (Children[ItemIndex] as TKsSegmentButton).Text;
end;

function TksSegmentButtons.GetSelectedID: string;
begin
  Result := '';
  if ItemIndex > -1 then
    Result := (Children[ItemIndex] as TKsSegmentButton).ID;
end;


procedure TksSegmentButtons.Paint;
begin
  inherited;
  if csDesigning in ComponentState then
    DrawDesignBorder;
end;

procedure TksSegmentButtons.ResizeButtons;
var
  ABtnWidth: Single;
  ICount: integer;
  AXpos: single;
  ABtn: TKsSegmentButton;
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;

  AXPos := 0;
  ABtnWidth := Width / ChildrenCount;
  for ICount := 0 to ChildrenCount-1 do
  begin

    ABtn := Children[ICount] as TKsSegmentButton;
    ABtn.Position.X := AXPos;
    ABtn.Position.Y := 0;
    ABtn.Width := ABtnWidth;
    ABtn.Height := Height;
    ABtn.IsPressed := FItemIndex = ICount;
    ABtn.StyleLookup := 'segmentedbuttonmiddle';
    if ICount = 0 then ABtn.StyleLookup := 'segmentedbuttonleft';
    if ICount = ChildrenCount-1 then
    begin
      ABtn.StyleLookup := 'segmentedbuttonright';
      ABtn.Position.X := ABtn.Position.X - 1;
    end;
    AXPos := AXpos + ABtnWidth;
  end;
end;

procedure TksSegmentButtons.SelectFirst;
begin
  if ChildrenCount > 0 then
    ItemIndex := 0;
end;

procedure TksSegmentButtons.SetItemIndex(const Value: integer);
begin
  (Children[Value] as TKsSegmentButton).IsPressed := True;
  FItemIndex := Value;
  ResizeButtons;
end;

procedure TksSegmentButtons.SetSelectedCaption(const Value: string);
var
  ICount: integer;
begin
  for ICount := 0 to ChildrenCount-1 do
    if (Children[ICount] as TKsSegmentButton).Text = Value then
      (Children[ICount] as TKsSegmentButton).IsPressed := True;
end;


constructor TKsSegmentButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StaysPressed := True;
end;

procedure TKsSegmentButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  IsPressed := True;
end;

initialization
  Classes.RegisterClass(TKsSegmentButton);


end.
