unit ksSlideMenu;

interface

{$IFDEF VER290}
  {$DEFINE XE8_OR_NEWER}
{$ENDIF}


uses System.UITypes, FMX.Controls, FMX.Layouts, FMX.Objects, System.Classes,
  FMX.Types, Generics.Collections, FMX.Graphics, System.UIConsts
  {$IFDEF XE8_OR_NEWER}
  ,FMX.ImgList
  {$ENDIF}
  ;



type
  TSelectMenuItemEvent = procedure(Sender: TObject; AId: string) of object;

  TMenuPosition = (mpLeft, mpRight);

  TksSlideMenu = class;

  TksSlideMenuItem = class
  private
    FText: string;
    FId: string;
    FImage: TBitmap;
    FHeight: integer;
    FIndex: integer;
  public
    constructor Create(AIndex: integer); virtual;
    destructor Destroy; override;
    property Height: integer read FHeight write FHeight;
    property Index: integer read FIndex;
  end;

  TksSlideMenuItems = class(TObjectList<TksSlideMenuItem>)
  private
    function AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem;
  end;

  TksSlideMenuCanvas = class(TImage)
  private
    FBackgroundColor: TAlphaColor;
    FSelectedColor: TAlphaColor;
    FSelectedFontColor: TAlphaColor;
    FUnselectedFontColor: TAlphaColor;
    FSlideMenu: TksSlideMenu;
    FBitmap: TBitmap;
    FItems: TksSlideMenuItems;
    FItemHeight: integer;
    FItemIndex: integer;
    FOnSelectMenuItemEvent: TSelectMenuItemEvent;
    function ItemAtPos(x, y: single): TksSlideMenuItem;

    procedure RedrawMenu;
    procedure SetItemHeight(const Value: integer);

    procedure SetItemIndex(const Value: integer);

  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

  public
    constructor Create(AOwner: TComponent; AItems: TksSlideMenuItems);
    destructor Destroy; override;
    property BackgroundColor: TAlphaColor read FBackgroundColor write FBackgroundColor default claNavy;
    property SelectedColor: TAlphaColor read FSelectedColor write FSelectedColor default claRed;
    property SelectedFontColor: TAlphaColor read FSelectedFontColor write FSelectedFontColor default claWhite;
    property UnselectedFontColor: TAlphaColor read FUnselectedFontColor write FUnselectedFontColor default claWhite;
    property ItemHeight: integer read FItemHeight write SetItemHeight default 40;
    property ItemIndex: integer read FItemIndex write SetItemIndex;

    // evnets..
    property OnSelectMenuItemEvent: TSelectMenuItemEvent read FOnSelectMenuItemEvent write FOnSelectMenuItemEvent;
  end;


  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or  pidiOSDevice)]
  TksSlideMenu = class(TFmxObject)
  private
    FCanvas: TksSlideMenuCanvas;
    FItems: TksSlideMenuItems;
    FBackground: TRectangle;

    FShowing: Boolean;
    {$IFDEF XE8_OR_NEWER}
    FImages: TImageList;
    {$ENDIF}
    FTopPadding: integer;
    FFadeBackground: Boolean;
    FMenuPosition: TMenuPosition;
    FOnSelectMenuItemEvent: TSelectMenuItemEvent;
    procedure SetItemHeight(const Value: integer);
    function GetItemHeight: integer;
    function GetItemIndex: integer;
    procedure SetItemIndex(const Value: integer);
    procedure SetTopPadding(const Value: integer);
    procedure DoBackgroundClick(Sender: TObject);
    procedure DoSelectMenuItemEvent(Sender: TObject; AId: string);
    function GetSelectedFontColor: TAlphaColor;
    procedure SetSelectedFontColor(const Value: TAlphaColor);
    function GetUnSelectedFontColor: TAlphaColor;
    procedure SetUnSelectedFontColor(const Value: TAlphaColor);
    function GetBackgroundColor: TAlphaColor;
    function GetSelectedColor: TAlphaColor;
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetSelectedColor(const Value: TAlphaColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    {$IFDEF XE8_OR_NEWER}
    function AddMenuItem(AId, AText: string; const AImageIndex: integer = -1): TksSlideMenuItem; overload;
    {$ENDIF}
    function AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem; overload;
    procedure ToggleMenu;
    procedure UpdateMenu;
  published
    property FadeBackgound: Boolean read FFadeBackground write FFadeBackground default True;
    {$IFDEF XE8_OR_NEWER}
    property Images: TImageList read FImages write FImages;
    {$ENDIF}
    property ItemHeight: integer read GetItemHeight write SetItemHeight;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property TopPadding: integer read FTopPadding write SetTopPadding default 0;
    property BackgroundColor: TAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property MenuPosition: TMenuPosition read FMenuPosition write FMenuPosition default mpLeft;
    property SelectedColor: TAlphaColor read GetSelectedColor write SetSelectedColor;
    property SelectedFontColor: TAlphaColor read GetSelectedFontColor write SetSelectedFontColor;
    property UnSelectedFontColor: TAlphaColor read GetUnSelectedFontColor write SetUnSelectedFontColor;
    property OnSelectMenuItemEvent: TSelectMenuItemEvent read FOnSelectMenuItemEvent write FOnSelectMenuItemEvent;

  end;

  procedure Register;

implementation

uses System.Types, FMX.Forms, FMX.Platform, SysUtils;

procedure Register;
begin
  RegisterComponents('Kernow Software', [TksSlideMenu]);
end;

function GetScreenScale: Single;
var
   Service : IFMXScreenService;
begin
   Service := IFMXScreenService(
      TPlatformServices.Current.GetPlatformService(IFMXScreenService));
   Result := Service .GetScreenScale;
end;

{ TSlideMenu }

{$IFDEF XE8_OR_NEWER}

function TksSlideMenu.AddMenuItem(AId, AText: string; const AImageIndex: integer = -1): TksSlideMenuItem;
var
  AImage: TBitmap;
  ASize: TSizeF;
begin
  AImage := nil;
  ASize.Width := 64;
  ASize.Height := 64;
  AImage := Images.Bitmap(ASize, AImageIndex);
  Result := AddMenuItem(AId, AText, AImage);
end;

{$ENDIF}

function TksSlideMenu.AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem;
begin
  Result := FItems.AddMenuItem(AId, AText, AImage);
  UpdateMenu;
end;

procedure TksSlideMenu.Clear;
begin
  FItems.Clear;
  UpdateMenu;
end;

constructor TksSlideMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TksSlideMenuItems.Create;
  FCanvas := TksSlideMenuCanvas.Create(Self, FItems);
  FShowing := False;
  FTopPadding := 0;

  FCanvas.OnSelectMenuItemEvent := DoSelectMenuItemEvent;
  FBackground := TRectangle.Create(Self);
  FFadeBackground := True;
  FMenuPosition := mpLeft;
end;

destructor TksSlideMenu.Destroy;
begin
  FItems.Free;
  if IsChild(FCanvas) then FCanvas.Free;
  if IsChild(FBackground) then FBackground.Free;
  inherited;
end;


procedure TksSlideMenu.DoBackgroundClick(Sender: TObject);
begin
  ToggleMenu;
end;

procedure TksSlideMenu.DoSelectMenuItemEvent(Sender: TObject; AId: string);
begin
  if Assigned(FOnSelectMenuItemEvent) then
    FOnSelectMenuItemEvent(Self, AId);
end;

function TksSlideMenu.GetBackgroundColor: TAlphaColor;
begin
  Result := FCanvas.BackgroundColor;
end;

function TksSlideMenu.GetItemHeight: integer;
begin
  Result := FCanvas.ItemHeight;
end;

function TksSlideMenu.GetItemIndex: integer;
begin
  Result := FCanvas.ItemIndex;
end;

function TksSlideMenu.GetSelectedColor: TAlphaColor;
begin
  Result := FCanvas.SelectedColor;
end;

function TksSlideMenu.GetSelectedFontColor: TAlphaColor;
begin
  Result := FCanvas.SelectedFontColor;
end;

function TksSlideMenu.GetUnSelectedFontColor: TAlphaColor;
begin
  Result := FCanvas.UnselectedFontColor;
end;

procedure TksSlideMenu.SetBackgroundColor(const Value: TAlphaColor);
begin
  FCanvas.BackgroundColor := Value;
end;

procedure TksSlideMenu.SetItemHeight(const Value: integer);
begin
  FCanvas.ItemHeight := Value;
end;


procedure TksSlideMenu.SetItemIndex(const Value: integer);
begin
  FCanvas.ItemIndex := Value;
end;

procedure TksSlideMenu.SetSelectedColor(const Value: TAlphaColor);
begin
  FCanvas.SelectedColor := Value;
end;

procedure TksSlideMenu.SetSelectedFontColor(const Value: TAlphaColor);
begin
  FCanvas.SelectedFontColor := Value;
end;

procedure TksSlideMenu.SetTopPadding(const Value: integer);
begin
  FTopPadding := Value;
end;

procedure TksSlideMenu.SetUnSelectedFontColor(const Value: TAlphaColor);
begin
  FCanvas.UnselectedFontColor := Value;
end;

procedure TksSlideMenu.ToggleMenu;
var
  ANewX: Extended;

begin
  if FShowing then
  begin
    ANewX := 0-FCanvas.Width;
    if FMenuPosition = mpRight then
      ANewX := (Owner as TForm).Width;
    if FFadeBackground then
      FBackground.AnimateFloat('Opacity', 0, 0.2);
    FCanvas.AnimateFloatWait('Position.X', ANewX, 0.2);
    TForm(Owner).RemoveObject(FCanvas);
    TForm(Owner).RemoveObject(FBackground);
  end
  else
  begin
    FCanvas.Height := (Owner as TForm).Height;
    FCanvas.Position.Y := FTopPadding;
    FCanvas.RedrawMenu;
    FBackground.Fill.Color := claBlack;
    FBackground.Align := TAlignLayout.Contents;
    FBackground.OnClick := DoBackgroundClick;
    FBackground.Opacity := 0;
    TForm(Owner).AddObject(FBackground);
    TForm(Owner).AddObject(FCanvas);
    ANewX := 0;
    FCanvas.Position.X := 0-FCanvas.Width;
    if FMenuPosition = mpRight then
    begin
      FCanvas.Position.X := TForm(Owner).Width;
      ANewX := (Owner as TForm).Width - FCanvas.Width;
    end;
    if FFadeBackground then
      FBackground.AnimateFloat('Opacity', 0.2, 0.2);
    FCanvas.AnimateFloatWait('Position.X', ANewX, 0.2);
  end;
  FShowing := not FShowing;
end;

procedure TksSlideMenu.UpdateMenu;
begin
  FCanvas.RedrawMenu;
end;

{ TksSlideMenuItem }

constructor TksSlideMenuItem.Create(AIndex: integer);
begin
  inherited Create;
  FImage := TBitmap.Create;
  FIndex := AIndex;
end;

destructor TksSlideMenuItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

{ TksSlideMenuItems }

function TksSlideMenuItems.AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem;
begin
  Result := TksSlideMenuItem.Create(Count);
  if AImage <> nil then
    Result.FImage.Assign(AImage);
  Result.FId := AId;
  Result.FText := AText;
  Add(Result);
end;

{ TksSlideMenuCanvas }

constructor TksSlideMenuCanvas.Create(AOwner: TComponent; AItems: TksSlideMenuItems);
begin
  inherited Create(AOwner);
  FSlideMenu := (AOwner as TksSlideMenu);
  FBitmap := TBitmap.Create;
  FItems := AItems;
  Position.X := -200;
  Width := 200;
  Top := 0;
  FItemHeight := 44;
  FItemIndex := -1;
  WrapMode := TImageWrapMode.Original;
  MarginWrapMode := TImageWrapMode.Original;
  FBackgroundColor := claNavy;
  FSelectedColor := claRed;
  FSelectedFontColor := claWhite;
  FUnselectedFontColor := claWhite;
end;

destructor TksSlideMenuCanvas.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TksSlideMenuCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  AItem: TksSlideMenuItem;
begin
  AItem := ItemAtPos(X, Y);
  if AItem <> nil then
  begin
    ItemIndex := AItem.Index;
    if Assigned(FOnSelectMenuItemEvent) then
      FOnSelectMenuItemEvent(FSlideMenu, AItem.FId);
    Application.ProcessMessages;
    Sleep(200);
    FSlideMenu.ToggleMenu;
  end;
end;

procedure TksSlideMenuCanvas.RedrawMenu;
var
  ARect: TRectF;
  ICount: integer;
  ABmpRect: TRectF;
  AScale: single;
begin
  AScale := GetScreenScale;
  FBitmap.BitmapScale := AScale;

  FBitmap.Width := Round(Width * AScale);
  FBitmap.Height := Round(Height * AScale);

  with FBitmap.Canvas do
  begin
    BeginScene;
    Fill.Color := FBackgroundColor;
    FillRect(ClipRect, 0, 0, [], 1);

    ARect := RectF(0, 0, Width, FItemHeight);
    Fill.Color := FBackgroundColor;
    Stroke.Color := claBlack;

    for ICount := 0 to FItems.Count-1 do
    begin

      Fill.Color := FBackgroundColor;
      if FItemIndex = ICount then
        Fill.Color := FSelectedColor;
      ARect.Left := 0;
      FillRect(ARect, 0, 0, [], 1);

      Fill.Color := claBlack;

      Stroke.Thickness := 1;
      if FSlideMenu.MenuPosition = mpLeft then
        DrawLine(PointF(Width/AScale, 0), PointF(Width/AScale, Height), 1)
      else
        DrawLine(PointF(0, 0), PointF(0, Height), 1);

      if FItems[ICount].FImage <> nil then
      begin
        ABmpRect := RectF(0, 0, 32, 32);
        OffsetRect(ABmpRect, 4, ARect.Top+((FItemHeight-32) div 2));
        DrawBitmap(FItems[ICount].FImage, RectF(0,0,64,64), ABmpRect, 1);
      end;
      ARect.Left := 44;
      Fill.Color := FUnselectedFontColor;
      if FItemIndex = ICount then
        Fill.Color := FSelectedFontColor;
      Font.Family := 'Arial';
      Font.Size := 14;
      FillText(ARect, FItems[ICount].FText, False, 1, [], TTextAlign.Leading, TTextAlign.Center);
      OffsetRect(ARect, 0, FItemHeight);
    end;
    EndScene;
  end;
  Bitmap := FBitmap;
end;

procedure TksSlideMenuCanvas.SetItemHeight(const Value: integer);
begin
  FItemHeight := Value;
  RedrawMenu;
end;

procedure TksSlideMenuCanvas.SetItemIndex(const Value: integer);
begin
  FItemIndex := Value;
  RedrawMenu;
end;


function TksSlideMenuCanvas.ItemAtPos(x, y: single): TksSlideMenuItem;
var
  AIndex: integer;
begin
  Result := nil;
  AIndex := Trunc(y / FItemHeight);
  if AIndex < FItems.Count then
    Result := FItems[AIndex];
end;

end.
