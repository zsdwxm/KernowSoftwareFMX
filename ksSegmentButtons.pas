unit ksSegmentButtons;

interface

uses
  Classes, FMX.Types, FMX.Controls, FMX.Graphics, Types, System.UITypes,
  FMX.Layouts, FMX.StdCtrls, System.Generics.Collections;

type
  TKsSegmentButton = class(TSpeedButton)
  private
    FId: string;
  public
    constructor Create(AOwner: TComponent); override;
    property ID: string read FId write FId;
  end;

  TKsSegmentButtonList = class(TObjectList<TKsSegmentButton>)
  private
    FGroupID: string;
    FOwner: TComponent;
  public
    constructor Create(AOwner: TComponent; AGroupID: string); virtual;
    procedure AddButton(AText, AID, AGroupID: string);
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or  pidiOSDevice)]
  TksSegmentButtons = class(TControl)
  private
    FButtons: TKsSegmentButtonList;
    FCaptions: TStrings;
    FButtonIndex: integer;
    FGroupID: string;
    FItemIndex: integer;
    //procedure RecreateButtons;
    procedure ResizeButtons;
    procedure SetItemIndex(const Value: integer);
    procedure DoCaptionsChanged(Sender: TObject);
    procedure SetCaptions(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //property Buttons: TKsSegmentButtonList read FButtons;
    procedure AddButton(ATitle, AID: string);
    procedure Clear;
  published
    property Align;
    property Captions: TStrings read FCaptions write SetCaptions;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Position;
    property Width;
    property Height;

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
begin
  FButtons.AddButton(ATitle, AId, FGroupID);
  if FItemIndex = -1 then
    FItemIndex := 0;
  ResizeButtons;
end;

procedure TksSegmentButtons.Clear;
begin
  FButtons.Clear;
  FCaptions.Clear;
end;

constructor TksSegmentButtons.Create(AOwner: TComponent);
var
  AGuid: TGUID;
begin
  inherited Create(AOwner);
  CreateGUID(AGuid);
  FGroupID := GUIDToString(AGuid);
  FButtons := TKsSegmentButtonList.Create(AOwner, FGroupID);
  FCaptions := TStringList.Create.Create;
  Width := 200;
  Height := 40;
  FItemIndex := -1;
  (FCaptions as TStringList).OnChange := DoCaptionsChanged;
end;

destructor TksSegmentButtons.Destroy;
begin
  FButtons.Free;
  FCaptions.Free;
  inherited;
end;

procedure TksSegmentButtons.DoCaptionsChanged(Sender: TObject);
var
  ICount: integer;
begin
  FButtons.Clear;
  for ICount := 0 to FCaptions.Count-1 do
  begin
    AddButton(FCaptions[ICount], '');
  end;
end;

{procedure TksSegmentButtons.RecreateButtons;
var
  ICount: integer;
begin
  //FButtons.Clear;
  //for ICount := 0 to FButtons.Count-1 do
  //  FButtons.AddButton(FCaptions.Names[ICount], FCaptions.ValueFromIndex[ICount]);
  ResizeButtons;
end; }

procedure TksSegmentButtons.ResizeButtons;
var
  ABtnWidth: Single;
  ICount: integer;
  AXpos: single;
  ABtn: TKsSegmentButton;
begin
  AXPos := 0;
  ABtnWidth := Width / FButtons.Count;
  for ICount := 0 to FButtons.Count-1 do
  begin
    ABtn := FButtons[ICount];
    ABtn.StyleLookup := 'toolbuttonmiddle';
    if ICount = 0 then ABtn.StyleLookup := 'toolbuttonleft';
    if ICount = FButtons.Count-1 then ABtn.StyleLookup := 'toolbuttonright';
    ABtn.Position.X := AXPos;
    ABtn.Position.Y := 0;
    ABtn.Width := ABtnWidth;
    ABtn.Height := Height;
    ABtn.IsPressed := FItemIndex = ICount;
    AddObject(ABtn);
    AXPos := AXpos + ABtnWidth;
  end;
end;

procedure TksSegmentButtons.SetCaptions(const Value: TStrings);
begin
  FCaptions.Assign(Value);
end;

procedure TksSegmentButtons.SetItemIndex(const Value: integer);
begin
  FButtons[FItemIndex].IsPressed := True;
  FItemIndex := Value;
  ResizeButtons;
end;

{ TKsSegmentButtonList }

procedure TKsSegmentButtonList.AddButton(AText, AID, AGroupID: string);
var
  ABtn: TKsSegmentButton;
begin
  ABtn := TKsSegmentButton.Create(nil);
  ABtn.Text := AText;
  ABtn.CanFocus := False;
  ABtn.StaysPressed := True;
  ABtn.ID := AId;
  ABtn.GroupName := AGroupID;
  Add(ABtn);
end;

constructor TKsSegmentButtonList.Create(AOwner: TComponent; AGroupID: string);
begin
  inherited Create(True);
  FOwner := AOwner;
  FGroupID := AGroupID;
end;

{ TKsSegmentButton }

constructor TKsSegmentButton.Create(AOwner: TComponent);
begin
  inherited;
  Lock;
end;

end.
