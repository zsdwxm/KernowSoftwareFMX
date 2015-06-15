unit ksSlideMenu;

interface

{$IFDEF VER290}
  {$DEFINE XE8_OR_NEWER}
{$ENDIF}


uses System.UITypes, FMX.Controls, FMX.Layouts, FMX.Objects, System.Classes,
  FMX.Types, Generics.Collections, FMX.Graphics, System.UIConsts, FMX.Effects,
  FMX.StdCtrls, System.Types
  {$IFDEF XE8_OR_NEWER}
  ,FMX.ImgList
  {$ENDIF}
  ;

type
  TSelectMenuItemEvent = procedure(Sender: TObject; AId: string) of object;

  TksMenuPosition = (mpLeft, mpRight);
  TKsMenuStyle = (msOverlap, msReveal, msPush);

  TksSlideMenu = class;

  TksSlideMenuItem = class
  strict private
    FText: string;
    FId: string;
    FFont: TFont;
    FImage: TBitmap;
    FHeight: integer;
    FIndex: integer;
  public
    constructor Create(AIndex: integer); virtual;
    destructor Destroy; override;
    property Height: integer read FHeight write FHeight;
    property Index: integer read FIndex;
    property Font: TFont read FFont write FFont;
    property Image: TBitmap read FImage write FImage;
    property ID: string read FId write FId;
    property Text: string read FText write FText;
  end;

  TksSlideMenuItems = class(TObjectList<TksSlideMenuItem>)
  private
    function AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem;
  end;


  TksSlideMenuToolbar = class
  private
    FLabel: TLabel;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure PaintTo(ACanvas: TCanvas; ARect: TRectF);
  end;


  TksSlideMenuCanvas = class(TImage)
  strict private
    FBackgroundColor: TAlphaColor;
    FSelectedColor: TAlphaColor;
    FSelectedFontColor: TAlphaColor;
    FUnselectedFontColor: TAlphaColor;
    FSlideMenu: TksSlideMenu;
    FBitmap: TBitmap;
    FItems: TksSlideMenuItems;
    FItemHeight: integer;
    FItemIndex: integer;
    FToolbar: TksSlideMenuToolbar;
    FOnSelectMenuItemEvent: TSelectMenuItemEvent;
    function ItemAtPos(x, y: single): TksSlideMenuItem;

    procedure SetItemHeight(const Value: integer);

    procedure SetItemIndex(const Value: integer);

  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

  public
    constructor Create(AOwner: TComponent; AItems: TksSlideMenuItems);
    destructor Destroy; override;

    procedure RedrawMenu(AddBorder: Boolean);
    property BackgroundColor: TAlphaColor read FBackgroundColor write FBackgroundColor default claNavy;
    property SelectedColor: TAlphaColor read FSelectedColor write FSelectedColor default claRed;
    property SelectedFontColor: TAlphaColor read FSelectedFontColor write FSelectedFontColor default claWhite;
    property UnselectedFontColor: TAlphaColor read FUnselectedFontColor write FUnselectedFontColor default claWhite;
    property ItemHeight: integer read FItemHeight write SetItemHeight default 40;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Toolbar: TksSlideMenuToolbar read FToolbar;
    // evnets..
    property OnSelectMenuItemEvent: TSelectMenuItemEvent read FOnSelectMenuItemEvent write FOnSelectMenuItemEvent;
  end;


  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or  pidiOSDevice)]
  TksSlideMenu = class(TFmxObject)
  strict private
    FCanvas: TksSlideMenuCanvas;
    FItems: TksSlideMenuItems;
    FShadowLeft: TImage;
    FShadowRight: TImage;
    FBackground: TRectangle;
    FFormImage: TImage;
    FFont: TFont;
    FShowing: Boolean;
    {$IFDEF XE8_OR_NEWER}
    FImages: TImageList;
    {$ENDIF}
    FTopPadding: integer;
    FMenuPosition: TksMenuPosition;
    FMenuStyle: TKsMenuStyle;
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
    procedure ToggleOverlap;
    procedure TogglePush(ACacheFormImage: Boolean);
    procedure ToggleReveal(ACacheFormImage: Boolean);
    procedure FadeBackground;
    procedure UnfadeBackground;
    procedure GenerateFormImage;
    procedure GenerateShadows;
  private
    function GetToolbar: TksSlideMenuToolbar;
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
    property Font: TFont read FFont write FFont;
    {$IFDEF XE8_OR_NEWER}
    property Images: TImageList read FImages write FImages;
    {$ENDIF}
    property ItemHeight: integer read GetItemHeight write SetItemHeight;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property TopPadding: integer read FTopPadding write SetTopPadding default 0;
    property BackgroundColor: TAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property MenuPosition: TksMenuPosition read FMenuPosition write FMenuPosition default mpLeft;
    property MenuStyle: TKsMenuStyle read FMenuStyle write FMenuStyle default msOverlap;
    property SelectedColor: TAlphaColor read GetSelectedColor write SetSelectedColor;
    property SelectedFontColor: TAlphaColor read GetSelectedFontColor write SetSelectedFontColor;
    property UnSelectedFontColor: TAlphaColor read GetUnSelectedFontColor write SetUnSelectedFontColor;
    property OnSelectMenuItemEvent: TSelectMenuItemEvent read FOnSelectMenuItemEvent write FOnSelectMenuItemEvent;
    property Toolbar: TksSlideMenuToolbar read GetToolbar;

  end;

  procedure Register;

implementation

uses FMX.Forms, FMX.Platform, SysUtils, FMX.Utils;

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
  if Images <> nil then
    AImage := Images.Bitmap(ASize, AImageIndex);
  Result := AddMenuItem(AId, AText, AImage);
end;

{$ENDIF}

function TksSlideMenu.AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem;
begin
  Result := FItems.AddMenuItem(AId, AText, AImage);
  Result.Font.Assign(FFont);
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
  FFont := TFont.Create;
  FItems := TksSlideMenuItems.Create;
  FShadowLeft := TImage.Create(Self);
  FShadowRight := TImage.Create(Self);
  FCanvas := TksSlideMenuCanvas.Create(Self, FItems);
  FShowing := False;
  FTopPadding := 0;
  FFont.Size := 14;
  FCanvas.OnSelectMenuItemEvent := DoSelectMenuItemEvent;
  FBackground := TRectangle.Create(Self);
  FFormImage := TImage.Create(Self);
  FFormImage.OnClick := DoBackgroundClick;
  FMenuPosition := mpLeft;
  FMenuStyle := msOverlap;
  GenerateShadows;
end;

destructor TksSlideMenu.Destroy;
begin
  FFont.Free;
  FItems.Free;
  if IsChild(FShadowLeft) then FShadowLeft.Free;
  if IsChild(FShadowRight) then FShadowRight.Free;
  if IsChild(FCanvas) then FCanvas.Free;
  if IsChild(FBackground) then FBackground.Free;
  if IsChild(FFormImage) then FFormImage.Free;
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

procedure TksSlideMenu.FadeBackground;
begin
  FBackground.Fill.Color := claBlack;
  FBackground.Align := TAlignLayout.Contents;
  FBackground.OnClick := DoBackgroundClick;
  FBackground.Opacity := 0;
  TForm(Owner).AddObject(FBackground);
  FBackground.AnimateFloat('Opacity', 0.2, 0.2);
end;

procedure TksSlideMenu.GenerateFormImage;
var
  AScale: single;
  AForm: TForm;
  ABmp: TBitmap;
begin
  ABmp := TBitmap.Create;
  try
    AScale := GetScreenScale;
    AForm := (Owner as TForm);
    ABmp.BitmapScale := AScale;
    ABmp.Width := Round(AForm.Width * AScale);
    ABmp.Height := Round(AForm.Height * AScale);
    ABmp.Canvas.BeginScene;
    AForm.PaintTo(ABmp.Canvas);
    ABmp.Canvas.EndScene;
    ABmp.Canvas.BeginScene;
    ABmp.Canvas.Stroke.Color := claBlack;
    ABmp.Canvas.StrokeThickness := 1;
    ABmp.Canvas.DrawLine(PointF(0, 0), PointF(0, ABmp.Height), 1);
    ABmp.Canvas.EndScene;
    FFormImage.Width := Round(AForm.Width);
    FFormImage.Height := Round(AForm.Height);
    FFormImage.Bitmap.Assign(ABmp);
  finally
    ABmp.Free;
  end;
end;

procedure TksSlideMenu.GenerateShadows;
var
  AScale: single;
  AForm: TForm;
  ABmp: TBitmap;
begin
  ABmp := TBitmap.Create;
  try
    AScale := GetScreenScale;
    AForm := (Owner as TForm);
    ABmp.Width := Round(16 * AScale);
    ABmp.Height := Round(AForm.Height * AScale);
    ABmp.Canvas.BeginScene;
    ABmp.Canvas.Fill.Kind := TBrushKind.Gradient;
    ABmp.Canvas.Fill.Gradient.Color := claNull;
    ABmp.Canvas.Fill.Gradient.Color1 := $AA000000;
    ABmp.Canvas.Fill.Gradient.StartPosition.X := 0;
    ABmp.Canvas.Fill.Gradient.StartPosition.Y := 1;
    ABmp.Canvas.Fill.Gradient.StopPosition.X := 1;
    ABmp.Canvas.FillRect(RectF(0, 0, ABmp.Width, ABmp.Height), 0, 0, [], 1);
    ABmp.Canvas.EndScene;
    FShadowLeft.Width := 16;
    FShadowLeft.Height := Round(AForm.Height);
    FShadowLeft.Bitmap.Assign(ABmp);
  finally
    ABmp.Free;
  end;

  ABmp := TBitmap.Create;
  try
    AScale := GetScreenScale;
    AForm := (Owner as TForm);
    ABmp.Width := Round(16 * AScale);
    ABmp.Height := Round(AForm.Height * AScale);
    ABmp.Canvas.BeginScene;
    ABmp.Canvas.Fill.Kind := TBrushKind.Gradient;
    ABmp.Canvas.Fill.Gradient.Color := $AA000000;
    ABmp.Canvas.Fill.Gradient.Color1 := claNull;
    ABmp.Canvas.Fill.Gradient.StartPosition.X := 0;
    ABmp.Canvas.Fill.Gradient.StartPosition.Y := 1;
    ABmp.Canvas.Fill.Gradient.StopPosition.X := 1;
    ABmp.Canvas.FillRect(RectF(0, 0, ABmp.Width, ABmp.Height), 0, 0, [], 1);
    ABmp.Canvas.EndScene;
    FShadowRight.Width := 16;
    FShadowRight.Height := Round(AForm.Height);
    FShadowRight.Bitmap.Assign(ABmp);
  finally
    ABmp.Free;
  end;
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

function TksSlideMenu.GetToolbar: TksSlideMenuToolbar;
begin
  Result := FCanvas.Toolbar;
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
begin
  if FCanvas.HitTest = False then
    Exit;
  FCanvas.HitTest := False;
  case FMenuStyle of
    msOverlap: ToggleOverlap;
    msPush: TogglePush(not FShowing);
    msReveal: ToggleReveal(not FShowing);
  end;
  FShowing := not FShowing;
  FCanvas.HitTest := True;
end;

procedure TksSlideMenu.ToggleOverlap;
var
  ANewX: Extended;
begin
  FCanvas.Width := 200;
  if FShowing then
  begin
    ANewX := 0-FCanvas.Width;
    if FMenuPosition = mpRight then
      ANewX := (Owner as TForm).Width;
    UnfadeBackground;
    FCanvas.AnimateFloatWait('Position.X', ANewX, 0.2);
    TForm(Owner).RemoveObject(FCanvas);
    TForm(Owner).RemoveObject(FBackground);
  end
  else
  begin
    FCanvas.Height := (Owner as TForm).Height;
    FCanvas.Position.Y := FTopPadding;
    FCanvas.RedrawMenu(True);
    ANewX := 0;
    FCanvas.Position.X := 0-200;//FCanvas.Width;
    if FMenuPosition = mpRight then
    begin
      FCanvas.Position.X := TForm(Owner).Width;
      ANewX := (Owner as TForm).Width - FCanvas.Width;
    end;
    FadeBackground;
    TForm(Owner).AddObject(FCanvas);
    FCanvas.AnimateFloatWait('Position.X', ANewX, 0.2);
  end;
end;

procedure TksSlideMenu.TogglePush(ACacheFormImage: Boolean);
var
  ANewX: Extended;
begin
  if ACacheFormImage then
    GenerateFormImage;
  FCanvas.Width := 200;
  if FShowing then
  begin
    ANewX := 0-200;//FCanvas.Width;
    if FMenuPosition = mpRight then
      ANewX := (Owner as TForm).Width;
    FCanvas.AnimateFloatWait('Position.X', ANewX, 0.2);
    FCanvas.RemoveObject(FFormImage);
    TForm(Owner).RemoveObject(FCanvas);
  end
  else
  begin
    FCanvas.Height := (Owner as TForm).Height;
    FCanvas.Position.Y := FTopPadding;
    FCanvas.RedrawMenu(False);
    ANewX := 0;
    FCanvas.Position.X := 0-200;
    FFormImage.Position.X := 200;
    if FMenuPosition = mpRight then
    begin
      FCanvas.Position.X := TForm(Owner).Width;
      ANewX := (Owner as TForm).Width - 200;//FCanvas.Width;
      FFormImage.Position.X := 0-FFormImage.Width;

    end;
    FCanvas.AddObject(FFormImage);
    TForm(Owner).AddObject(FCanvas);
    FCanvas.AnimateFloatWait('Position.X', ANewX, 0.2);
  end;
end;

procedure TksSlideMenu.ToggleReveal(ACacheFormImage: Boolean);
var
  ANewX: Extended;
  AShadow: TImage;
begin
  if ACacheFormImage then
    GenerateFormImage;

  case FMenuPosition of
    mpLeft: AShadow := FShadowLeft;
    mpRight: AShadow := FShadowRight;
  end;

  FCanvas.Width := 200;
  if FShowing then
  begin
    ANewX := 0;
    FFormImage.AnimateFloatWait('Position.X', ANewX, 0.2);
    TForm(Owner).RemoveObject(FFormImage);
    TForm(Owner).RemoveObject(FCanvas);
    FFormImage.RemoveObject(FShadowLeft);
    FFormImage.RemoveObject(FShadowRight);
  end
  else
  begin
    AShadow.Position.X := 0-16;
    FCanvas.Height := (Owner as TForm).Height;
    FCanvas.Position.Y := FTopPadding;
    FCanvas.RedrawMenu(False);
    FCanvas.Position.X := 0;
    ANewX := 200;
    if FMenuPosition = mpRight then
    begin
      FCanvas.Position.X := TForm(Owner).Width-200;
      AShadow.Position.X := TForm(Owner).Width;

      ANewX := 0-200;
    end;
    TForm(Owner).AddObject(FCanvas);
    TForm(Owner).AddObject(FFormImage);
    FFormImage.AddObject(AShadow);

    FFormImage.AnimateFloatWait('Position.X', ANewX, 0.2);
  end;
end;

procedure TksSlideMenu.UnfadeBackground;
begin
  FBackground.AnimateFloat('Opacity', 0, 0.2);
end;

procedure TksSlideMenu.UpdateMenu;
begin
  FCanvas.RedrawMenu(FMenuStyle = msOverlap);
end;

{ TksSlideMenuItem }

constructor TksSlideMenuItem.Create(AIndex: integer);
begin
  inherited Create;
  FImage := TBitmap.Create;
  FFont := TFont.Create;
  FIndex := AIndex;
end;

destructor TksSlideMenuItem.Destroy;
begin
  FImage.Free;
  FFont.Free;
  inherited;
end;


{ TksSlideMenuItems }

function TksSlideMenuItems.AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem;
begin
  Result := TksSlideMenuItem.Create(Count);
  if AImage <> nil then
    Result.Image.Assign(AImage);
  Result.Id := AId;
  Result.Text := AText;
  Add(Result);
end;

{ TksSlideMenuCanvas }

constructor TksSlideMenuCanvas.Create(AOwner: TComponent; AItems: TksSlideMenuItems);
begin
  inherited Create(AOwner);
  FSlideMenu := (AOwner as TksSlideMenu);
  FToolbar := TksSlideMenuToolbar.Create(Self);
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
  FToolbar.Free;
  inherited;
end;


procedure TksSlideMenuCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  AItem: TksSlideMenuItem;
begin
  if HitTest = False then
    Exit;
  AItem := ItemAtPos(X, Y);
  if AItem <> nil then
  begin
    ItemIndex := AItem.Index;
    if Assigned(FOnSelectMenuItemEvent) then
      FOnSelectMenuItemEvent(FSlideMenu, AItem.Id);
    Application.ProcessMessages;
    Sleep(200);
    FSlideMenu.ToggleMenu;
  end;
end;

procedure TksSlideMenuCanvas.RedrawMenu(AddBorder: Boolean);
var
  ARect: TRectF;
  ICount: integer;
  ABmpRect: TRectF;
  AScale: single;
  AToolbarRect: TRectF;
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
    ARect := RectF(0, 0, 200, FItemHeight);
    Fill.Color := FBackgroundColor;
    Stroke.Color := claBlack;

    // draw toolbar...
    AToolbarRect := ARect;
    AToolbarRect.Height := FToolbar.FLabel.Height;

    FToolbar.PaintTo(FBitmap.Canvas, AToolbarRect);


    {$IFDEF IOS}
    OffsetRect(ARect, 0, 44);
    {$ELSE}
    OffsetRect(ARect, 0, AToolbarRect.Height);
    {$ENDIF}
    // draw items..
    {for ICount := 0 to FItems.Count-1 do
    begin

      Fill.Color := FBackgroundColor;
      if FItemIndex = ICount then
        Fill.Color := FSelectedColor;
      ARect.Left := 0;
      FillRect(ARect, 0, 0, [], 1);

      Fill.Color := claBlack;

      if AddBorder then
      begin
        Stroke.Thickness := 1;
        if FSlideMenu.MenuPosition = mpLeft then
          DrawLine(PointF(200/AScale, 0), PointF(200/AScale, Height), 1)
        else
          DrawLine(PointF(0, 0), PointF(0, Height), 1);
      end;

      if FItems[ICount].Image <> nil then
      begin
        ABmpRect := RectF(0, 0, 32, 32);
        OffsetRect(ABmpRect, 4, ARect.Top+((FItemHeight-32) div 2));
        DrawBitmap(FItems[ICount].Image, RectF(0,0,64,64), ABmpRect, 1);
      end;
      ARect.Left := 44;
      Fill.Color := FUnselectedFontColor;
      if FItemIndex = ICount then
        Fill.Color := FSelectedFontColor;
      Font.Assign(FItems[ICount].Font);
      FillText(ARect, FItems[ICount].Text, False, 1, [], TTextAlign.Leading, TTextAlign.Center);
      OffsetRect(ARect, 0, FItemHeight);
    end;   }
    EndScene;
  end;
  Bitmap := FBitmap;
end;

procedure TksSlideMenuCanvas.SetItemHeight(const Value: integer);
begin
  FItemHeight := Value;
end;

procedure TksSlideMenuCanvas.SetItemIndex(const Value: integer);
begin
  FItemIndex := Value;
  RedrawMenu(FSlideMenu.MenuStyle = msOverlap);
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

{ TksSlideMenuToolbar }

constructor TksSlideMenuToolbar.Create(AOwner: TComponent);
begin
  FLabel := TLabel.Create(Application.MainForm);
  FLabel.Font.Size := 100;
  FLabel.Text := '12345';
  FLabel.Width := 300;
  FLabel.Height := 300;
  FLabel.StyledSettings := [];
  FLabel.Text := 'HEADER';
end;

destructor TksSlideMenuToolbar.Destroy;
begin
  if FLabel.Parent = nil then
    FLabel.Free;
  inherited;
end;

procedure TksSlideMenuToolbar.PaintTo(ACanvas: TCanvas; ARect: TRectF);
begin
  {lbl := TLabel.Create(Application.MainForm);
  lbl.Font.Size := 100;
  lbl.Text := '12345';
  lbl.Width := 300;
  lbl.Height := 300;
  lbl.StyledSettings := [];
  Application.MainForm.AddObject(lbl); }
 // FLabel.PaintTo(ACanvas, ARect);
  //Application.MainForm.AddObject(FLabel);

  ACanvas.FillText(ARect, 'TEST', False, 1, [], TTextAlign.Leading);

  FLabel.PaintTo(ACanvas, ARect);


end;

end.
