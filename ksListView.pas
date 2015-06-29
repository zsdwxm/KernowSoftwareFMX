unit ksListView;

interface

uses
  Classes, FMX.Types, FMX.Controls, FMX.ListView,
  FMX.ListView.Types, FMX.Graphics, Types, System.UITypes,
  FMX.TextLayout;

type
  TksListItemImage = class;

  TksClickImageEvent = procedure(Sender: TObject; x, y: single; AImage: TksListItemImage; var AHandled: Boolean) of object;

  TksListView = class;

  TksListItemImage = class(TListItemImage)
  private
    FTouchTargetExpansion: TBounds;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
    property TouchTargetExpansion: TBounds read FTouchTargetExpansion;
  end;


  TksListItemText = class(TListItemText)
  private
    FCached: TBitmap;
    FTextLayout: TTextLayout;
    FItemRect: TRectF;
    procedure Cache(ARect: TRectF); overload;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
    procedure UpdateCache; overload;
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const SubPassNo: Integer = 0); override;
  end;

  TksListViewCanvas = class
  private
    FListView: TksListView;
    FFont: TFont;
    FTextColor: TAlphaColor;
    function GetTextWidth(AText: string): Single;
    function GetTextHeight(AText: string): Single;
  public
    constructor Create(AListView: TksListView);
    destructor Destroy; override;
    function TextOut(AItem: TListViewItem; AText: string; X, Y, AWidth, AHeight: single; const ATrimming: TTextTrimming = TTextTrimming.None): TksListItemText;
    function TextOutRight(AItem: TListViewItem; AText: string; X, Y, AWidth, AHeight: single; const ATrimming: TTextTrimming = TTextTrimming.None): TksListItemText;
    function DrawBitmap(AItem: TListViewItem; ABmp: TBitmap; X, Y, AWidth, AHeight: Extended): TListItemImage;
    property Font: TFont read FFont;
    property TextColor: TAlphaColor read FTextColor write FTextColor;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or  pidiOSDevice)]
  TksListView = class(TListView)
  private
    FCanvas: TksListViewCanvas;
    FOnClickImage: TksClickImageEvent;
    FMouseDownPos: TPointF;
    { Private declarations }
  protected
    procedure DoItemClick(const AItem: TListViewItem); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(AName, AValue: string; const AClickId: string = '');
    procedure AddHeader(ATitle: string);
    procedure EndUpdate; override;
    procedure CacheObjects;
    property Canvas: TksListViewCanvas read FCanvas;
    { Public declarations }
  published
    property OnClickImage: TksClickImageEvent read FOnClickImage write FOnClickImage;
    { Published declarations }
  end;

procedure Register;

implementation

uses SysUtils, FMX.Platform, System.UIConsts, FMX.Forms, FMX.Dialogs;

procedure Register;
begin
  RegisterComponents('kernow Software FMX', [TksListView]);
end;

function GetScreenScale: Single;
var
   Service : IFMXScreenService;
begin
   Service := IFMXScreenService(
      TPlatformServices.Current.GetPlatformService(IFMXScreenService));
   Result := Service .GetScreenScale;
end;


procedure TksListItemText.Cache(ARect: TRectF);
begin
  CalculateLocalRect(ARect, GetScreenScale, []);
  FItemRect := FLocalRect;
  FCached.BitmapScale := GetScreenScale;
  FCached.Width := Round(FItemRect.Width * GetScreenScale);
  FCached.Height := Round(FItemRect.Height * GetScreenScale);
  FCached.Clear(claNull);
  FCached.Canvas.BeginScene;
  if FTextLayout = nil then
    FTextLayout := TTextLayoutManager.TextLayoutByCanvas(FCached.Canvas.ClassType).Create(FCached.Canvas);
  FTextLayout.BeginUpdate;
  FTextLayout.HorizontalAlign := TextAlign;
  FTextLayout.VerticalAlign := TextVertAlign;
  FTextLayout.Font := Font;
  FTextLayout.Color := TextColor;
  FTextLayout.RightToLeft := False;
  FTextLayout.MaxSize := PointF(FLocalRect.Width, FLocalRect.Height);
  FTextLayout.Text := Text;
  FTextLayout.Trimming := Trimming;
  FTextLayout.WordWrap := WordWrap;
  FTextLayout.EndUpdate;
  FTextLayout.BeginUpdate;
  FTextLayout.Opacity := Opacity;
  FTextLayout.TopLeft := PointF(0, 0);
  FTextLayout.EndUpdate;
  FTextLayout.RenderLayout(FCached.Canvas);
  FCached.Canvas.EndScene;
end;

procedure TksListItemText.UpdateCache;
begin
  Cache((TListViewItem(Owner).Parent).GetItemRect(TListViewItem(Owner).Index));
end;

constructor TksListItemText.Create(const AOwner: TListItem);
begin
  inherited;
  FCached := TBitmap.Create;
end;

destructor TksListItemText.Destroy;
begin
  FreeAndNil(FCached);
  FreeAndNil(FTextLayout);
  inherited;
end;

procedure TksListItemText.Render(const Canvas: TCanvas;
  const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
  const SubPassNo: Integer);
begin
  Canvas.DrawBitmap(FCached, RectF(0, 0, FCached.Width, FCached.Height),
                    FLocalRect,
                    1, True);
end;

{ TksListView }

procedure TksListView.AddHeader(ATitle: string);
var
  AHeader: TListViewItem;
begin
  AHeader := Items.Add;
  AHeader.Text := ATitle;
  AHeader.Purpose := TListItemPurpose.Header;
  AHeader.Height := 40;
end;

procedure TksListView.AddItem(AName, AValue: string; const AClickId: string = '');
var
  AItem: TListViewItem;
begin
  AItem := Items.Add;
  AItem.Text := AName;
  AItem.Data['id'] := AClickId;

  ItemAppearanceObjects.ItemObjects.Text.Visible := False;
  Canvas.TextColor := claDimgray;
  Canvas.TextOut(AItem,
                 AName,
                 4,
                 0,
                 Width,
                 0);

  Canvas.TextColor := claDodgerblue;
  with Canvas.TextOut(AItem, AValue, 4, 0, 160, 0, TTextTrimming.Character) do
  begin
    PlaceOffset.X := -20;
    TextAlign := TTextAlign.Trailing;
    Align := TListItemAlign.Trailing;
  end;
  AItem.Objects.AccessoryObject.Visible := AClickId <> '';
end;

procedure TksListView.CacheObjects;
var
  ICount: integer;
  ICount2: integer;
  AItem: TListViewItem;
  AObj: TListItemObject;
begin
  inherited;
  for ICount := 0 to Items.Count-1 do
  begin
    AItem := Items[ICount];
    for ICount2 := 0 to AItem.Objects.Count-1 do
    begin
      AObj := AItem.Objects[ICount2];
      if AItem.Text = '' then
        AItem.Text := '1';
      if AObj is TksListItemText then
        (AObj as TksListItemText).UpdateCache;
    end;
  end;
end;

constructor TksListView.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TksListViewCanvas.Create(Self);
end;

destructor TksListView.Destroy;
begin
  FCanvas.Free;
  inherited;
end;
procedure TksListView.DoItemClick(const AItem: TListViewItem);
var
  ICount: integer;
  AObj: TListItemObject;
  AHandled: Boolean;
begin
  AHandled := False;
  for ICount := 0 to AItem.Objects.Count-1 do
  begin
    AObj := AItem.Objects[ICount];
    if (AObj is TksListItemImage)  then
    begin
      if (FMouseDownPos.X >= AObj.PlaceOffset.X)  and (FMouseDownPos.X <= (AObj.PlaceOffset.X + AOBj.Width)) then
      begin
        if Assigned(FOnClickImage) then
          FOnClickImage(Self, FMouseDownPos.X, FMouseDownPos.Y, AObj as TksListItemImage, AHandled);
      end;
    end;
  end;

  if AHandled = False then
    inherited;
end;

procedure TksListView.EndUpdate;
begin
  inherited;
  CacheObjects;
end;

procedure TksListView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  FMouseDownPos := PointF(X, Y);
  inherited;
end;

procedure TksListView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  Application.ProcessMessages;
  Sleep(100);
  ItemIndex := -1;
end;

constructor TksListViewCanvas.Create(AListView: TksListView);
begin
  FListView := AListView;
  FFont := TFont.Create;
  FTextColor := claBlack;
end;

destructor TksListViewCanvas.Destroy;
begin
  FFont.Free;
  inherited;
end;

function TksListViewCanvas.DrawBitmap(AItem: TListViewItem; ABmp: TBitmap; X, Y,
  AWidth, AHeight: Extended): TListItemImage;
begin
  AItem.Text := ' ';
  //TKsListView(AItem.Parent).ItemAppearanceObjects.ItemObjects.Text.Visible := False;
  Result := TksListItemImage.Create(AItem);
  Result.VertAlign := TListItemAlign.Center;
  Result.PlaceOffset.X := X;
  Result.PlaceOffset.Y := Y;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Bitmap := ABmp;
  //TKsListView(AItem.Parent).ItemAppearanceObjects.ItemObjects.Text
end;

function TksListViewCanvas.GetTextWidth(AText: string): Single;
var
  ABmp: TBitmap;
begin
  ABmp := TBitmap.Create;
  try
    ABmp.BitmapScale := GetScreenScale;
    ABmp.Canvas.Font.Assign(FFont);
    Result := ABmp.Canvas.TextWidth(AText);
  finally
    ABmp.Free;
  end;
end;

function TksListViewCanvas.GetTextHeight(AText: string): Single;
var
  ABmp: TBitmap;
begin
  ABmp := TBitmap.Create;
  try
    ABmp.BitmapScale := GetScreenScale;
    ABmp.Canvas.Font.Assign(FFont);
    Result := ABmp.Canvas.TextWidth(AText);
  finally
    ABmp.Free;
  end;
end;

function TksListViewCanvas.TextOut(AItem: TListViewItem; AText: string; X, Y, AWidth, AHeight: single; const ATrimming: TTextTrimming = TTextTrimming.None): TksListItemText;
var
  ALbl: TksListItemText;
begin
  ALbl := TksListItemText.Create(AItem);
  ALbl.Text := AText;
  ALbl.Align := TListItemAlign.Leading;
  ALbl.TextVertAlign := TTextAlign.Center;
  ALbl.VertAlign := TListItemAlign.Center;
  ALbl.TextColor := FTextColor;
  ALbl.Font.Assign(FFont);
  ALbl.Trimming := ATrimming;
  if AWidth = 0 then
    AWidth := GetTextWidth(AText);
  ALbl.Width := AWidth;

  if AHeight = 0 then
    AHeight := GetTextHeight(AText);
  //ALbl.Height := AHeight;


  if X <> 0 then ALbl.PlaceOffset.X := X;
  if Y <> 0 then ALbl.PlaceOffset.Y := Y;
  Result := ALbl;
end;

function TksListViewCanvas.TextOutRight(AItem: TListViewItem; AText: string; X, Y, AWidth, AHeight: single; const ATrimming: TTextTrimming = TTextTrimming.None): TksListItemText;
begin
  Result := TextOut(AItem, AText, X, Y, AWidth, AHeight, ATrimming);
  Result.PlaceOffset.X := -20;
  Result.TextAlign := TTextAlign.Trailing;
  Result.Align := TListItemAlign.Trailing;
end;


{ TksListItemImage }

constructor TksListItemImage.Create(const AOwner: TListItem);
begin
  inherited;
  FTouchTargetExpansion := TBounds.Create(RectF(4,4,4,4));
end;

destructor TksListItemImage.Destroy;
begin
  FTouchTargetExpansion.Free;
  inherited;
end;

end.

