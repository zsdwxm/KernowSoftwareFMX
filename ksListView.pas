unit ksListView;

interface

uses
  Classes, FMX.Types, FMX.Controls, FMX.ListView, Types, FMX.TextLayout,
  FMX.ListView.Types, FMX.Graphics, Generics.Collections, System.UITypes,
  FMX.ImgList, System.UIConsts;

type

  TksListView = class;
  TKsListItemRow = class;
  TksListItemRowObj = class;

  TksListViewRowClickEvent = procedure(Sender: TObject; x, y: single; AItem: TListViewItem; AId: string; ARowObj: TksListItemRowObj) of object;

  TksListItemRowObj = class
  private
    FId: string;
    FRect: TRectF;
    FRow: TKsListItemRow;
    procedure SetRect(const Value: TRectF);
    procedure SetID(const Value: string);
    procedure Changed;
  public
    constructor Create(ARow: TksListItemRow); virtual;
    procedure Render(ACanvas: TCanvas); virtual; abstract;
    property Rect: TRectF read FRect write SetRect;
    property ID: string read FId write SetID;
  end;


  TksListItemRowText = class(TksListItemRowObj)
  private
    FFont: TFont;
    FAlignment: TTextAlign;
    FTextLayout: TTextLayout;
    FTextColor: TAlphaColor;
    FText: string;
    procedure SetFont(const Value: TFont);
  public
    constructor Create(ARow: TksListItemRow); override;
    destructor Destroy; override;
    procedure Render(ACanvas: TCanvas); override;
    property Font: TFont read FFont write SetFont;
    property TextAlignment: TTextAlign read FAlignment write FAlignment;
    property TextColor: TAlphaColor read FTextColor write FTextColor;
    property Text: string read FText write FText;
  end;

  TksListItemRowImage = class(TksListItemRowObj)
  private
    FBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
  public
    constructor Create(ARow: TksListItemRow); override;
    destructor Destroy; override;
    procedure Render(ACanvas: TCanvas); override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
  end;


  TksListItemRow = class(TListItemImage)
  private
    FCached: Boolean;
    FFont: TFont;
    FTextColor: TAlphaColor;
    FList: TObjectList<TksListItemRowObj>;
    FId: string;
    function TextHeight(AText: string): single;
    function TextWidth(AText: string): single;
    function RowHeight(const AScale: Boolean = True): single;
    function RowWidth(const AScale: Boolean = True): single;
    function GetListView: TCustomListView;
    function GetRowObject(AIndex: integer): TksListItemRowObj;
    function GetRowObjectCount: integer;
    property ListView: TCustomListView read GetListView;
    procedure DoOnListChanged(Sender: TObject; const Item: TksListItemRowObj; Action: TCollectionNotification);
    function ScreenWidth: single;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
    function DrawBitmap(ABmp: TBitmap; X, AWidth, AHeight: single): TksListItemRowImage overload;
    function DrawBitmap(ABmpIndex: integer; X, AWidth, AHeight: single): TksListItemRowImage overload;
    function DrawBitmap(ABmp: TBitmap; X, Y, AWidth, AHeight: single): TksListItemRowImage overload;

    function DrawBitmapRight(ABmp: TBitmap; AWidth, AHeight, ARightPadding: single): TksListItemRowImage;

    function TextOut(AText: string; X, AWidth: single; const AVertAlign: TTextAlign = TTextAlign.Center): TksListItemRowText; overload;
    function TextOut(AText: string; X, Y, AWidth, AHeight: single): TksListItemRowText; overload;

    function TextOutRight(AText: string; const AVertAlign: TTextAlign = TTextAlign.Center): TksListItemRowText; overload;
    function TextOutRight(AText: string; AWidth: single; const AVertAlign: TTextAlign = TTextAlign.Center): TksListItemRowText; overload;
    function TextOutRight(AText: string; Y, AWidth: single): TksListItemRowText; overload;
    property Font: TFont read FFont;
    property TextColor: TAlphaColor read FTextColor write FTextColor;
    procedure CacheRow;
    property RowObject[AIndex: integer]: TksListItemRowObj read GetRowObject;
    property RowObjectCount: integer read GetRowObjectCount;
    property ID: string read FId write FId;
    property Cached: Boolean read FCached write FCached;
  end;


  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or  pidiOSDevice)]
  TksListViewAppearence = class(TPersistent)
  private
    FListView: TksListView;
    FBackground: TAlphaColor;
    FItemBackground: TAlphaColor;
    FAlternatingItemBackground: TAlphaColor;
    procedure SetBackground(const Value: TAlphaColor);
    procedure SetItemBackground(const Value: TAlphaColor);
    procedure SetAlternatingItemBackground(const Value: TAlphaColor);
  public
    constructor Create(AListView: TksListView);
  published
    property Background: TAlphaColor read FBackground write SetBackground default claWhite;
    property ItemBackground: TAlphaColor read FItemBackground write SetItemBackground default claWhite;
    property AlternatingItemBackground: TAlphaColor read FAlternatingItemBackground write SetAlternatingItemBackground default claGainsboro;
  end;

  TksListView = class(TCustomListView)
  private
    FScreenScale: single;
    FAppearence: TksListViewAppearence;
    FOnClick: TksListViewRowClickEvent;
    FMouseDownPos: TPointF;
    { Private declarations }
  protected
    procedure SetColorStyle(AName: string; AColor: TAlphaColor);
    procedure ApplyStyle; override;
    procedure DoItemClick(const AItem: TListViewItem); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddRow(const ASearchIndex: string = '';
                    const APurpose: TListItemPurpose = TListItemPurpose.None;
                    const AId: string = ''): TksListItemRow;
    function AddHeader(AText: string): TksListItemRow;
    procedure EndUpdate; override;
    { Public declarations }
  published
    property Appearence: TksListViewAppearence read FAppearence write FAppearence;
    property OnUpdatingObjects;
    property OnUpdateObjects;
    property OnEditModeChange;
    property OnEditModeChanging;
    property EditMode;

    property Transparent default false;
    property AllowSelection;
    property AlternatingColors;
    property ItemIndex;
    property Images;
    property ScrollViewPos;
    property ItemSpaces;
    property SideSpace;

    property OnClick: TksListViewRowClickEvent read FOnClick write FOnClick;

    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren default True;
    property ClipParent default False;
    property Cursor default crDefault;
    property DisableFocusEffect default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property Visible default True;
    property Width;

    {events}
    property OnApplyStyleLookup;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;

    property HelpContext;
    property HelpKeyword;
    property HelpType;

    property StyleLookup;
    property TouchTargetExpansion;

    property OnDblClick;

    { ListView selection events }
    property CanSwipeDelete;

    property OnChange;
    property OnChangeRepainted;
    property OnItemsChange;
    property OnScrollViewChange;
    property OnItemClick;


    property OnButtonClick;
    property OnButtonChange;

    property OnDeletingItem;
    property OnDeleteItem;
    property OnDeleteChangeVisible;
    property OnSearchChange;
    property OnFilter;
    property OnPullRefresh;
    property DeleteButtonText;

    property AutoTapScroll;
    property AutoTapTreshold;
    property ShowSelection;
    property DisableMouseWheel;

    property SearchVisible;
    property SearchAlwaysOnTop;
    property SelectionCrossfade;
    property PullToRefresh;
    property PullRefreshWait;
  end;

procedure Register;

implementation

uses SysUtils, FMX.Platform, FMX.Forms;

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
   Result := Service.GetScreenScale;
  {$IFDEF IOS}
  if Result = 1 then
    Result := 1.5;
  {$ENDIF}
end;

{ TksListItemRowText }

constructor TksListItemRowText.Create(ARow: TksListItemRow);
begin
  inherited Create(ARow);
  FFont := TFont.Create;
  FTextColor := claBlack;
end;

destructor TksListItemRowText.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TksListItemRowText.Render(ACanvas: TCanvas);
begin
  ACanvas.Fill.Color := FTextColor;
  ACanvas.Font.Assign(FFont);
  ACanvas.FillText(FRect, FText, False, 1, [], FAlignment);
end;

procedure TksListItemRowText.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

{ TksListItemRowImage }

constructor TksListItemRowImage.Create(ARow: TksListItemRow);
begin
  inherited Create(ARow);
  FBitmap := TBitmap.Create;
end;

destructor TksListItemRowImage.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TksListItemRowImage.Render(ACanvas: TCanvas);
begin
  ACanvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height), FRect, 1, True);
end;

procedure TksListItemRowImage.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  FBitmap.BitmapScale := GetScreenScale;
end;

{ TksListItemRow }

procedure TksListItemRow.CacheRow;
var
  ICount: integer;
begin
  if FCached then
    Exit;
  BeginUpdate;
  Bitmap.Clear(claNull);
  Bitmap.Canvas.BeginScene;
  for ICount := 0 to FList.Count-1 do
  begin
    FList[ICount].Render(Bitmap.Canvas);
  end;
  Bitmap.Canvas.EndScene;
  EndUpdate;
  FCached := True;
end;

constructor TksListItemRow.Create(const AOwner: TListItem);
var
  ABmp: TBitmap;
begin
  inherited Create(AOwner);
  {$IFDEF MSWINDOWS}
  ScalingMode := TImageScalingMode.Original;
  {$ENDIF}
  OwnsBitmap := True;
  FList := TObjectList<TksListItemRowObj>.Create(True);
  FList.OnNotify := DoOnListChanged;
  ABmp := TBitmap.Create;
  ABmp.BitmapScale := GetScreenScale;
  ABmp.Width := Round(RowWidth);
  ABmp.Height := Round(RowHeight);
  Abmp.Clear(claNull);
  Bitmap := ABmp;
  FTextColor := claBlack;
  FFont := TFont.Create;
  FCached := True;
end;

destructor TksListItemRow.Destroy;
begin
  FList.Free;
  FFont.Free;
  inherited;
end;

function TksListItemRow.DrawBitmap(ABmp: TBitmap; X, AWidth,
  AHeight: single): TksListItemRowImage;
var
  AYpos: single;
begin
  AYpos := (RowHeight(False) - AHeight) / 2;
  Result := DrawBitmap(ABmp, X, AYpos, AWidth, AHeight);
end;

function TksListItemRow.DrawBitmapRight(ABmp: TBitmap; AWidth, AHeight, ARightPadding: single): TksListItemRowImage;
var
  AYpos: single;
  AXPos: single;
begin
  AYpos := (RowHeight(False) - AHeight) / 2;
  AXPos := ScreenWidth - (AWidth + ARightPadding);
  Result := DrawBitmap(ABmp, AXPos, AYpos, AWidth, AHeight);
end;

function TksListItemRow.DrawBitmap(ABmpIndex: integer; X, AWidth, AHeight: single): TksListItemRowImage overload;
var
  ABmp: TBitmap;
  il: TCustomImageList;
  ASize: TSizeF;
begin
  il := ListView.Images;
  if il = nil then
    Exit;
  ASize.cx := 64;
  ASize.cy := 64;
  ABmp := il.Bitmap(ASize, ABmpIndex);
  Result := DrawBitmap(ABmp, X, AWidth, AHeight);
end;

function TksListItemRow.RowHeight(const AScale: Boolean = True): single;
var
  lv: TksListView;
begin
  lv := TksListView(Owner.Parent);
  Result := lv.ItemAppearance.ItemHeight;
  if AScale then
    Result := Result * GetScreenScale;
end;

function TksListItemRow.RowWidth(const AScale: Boolean = True): single;
var
  lv: TksListView;
begin
  lv := TksListView(Owner.Parent);
  Result := lv.Width;
  if AScale then Result := Result * GetScreenScale;
end;

function TksListItemRow.ScreenWidth: single;
begin
  Result := TksListView(Owner.Parent).Width;
  {$IFDEF MSWINDOWS}
  Result := Result - 40;
  {$ENDIF}
end;

function TksListItemRow.DrawBitmap(ABmp: TBitmap; X, Y, AWidth, AHeight: single): TksListItemRowImage;
begin
  Result := TksListItemRowImage.Create(Self);
  Result.FRect := RectF(X, Y, X+AWidth, Y+AHeight);
  Result.Bitmap := ABmp;
  FList.Add(Result);
end;

function TksListItemRow.GetListView: TCustomListView;
begin
  Result := (Owner.Parent as TCustomListView);
end;

function TksListItemRow.GetRowObject(AIndex: integer): TksListItemRowObj;
begin
  Result := FList[AIndex];
end;

function TksListItemRow.GetRowObjectCount: integer;
begin
  Result := FList.Count;
end;

procedure TksListItemRow.DoOnListChanged(Sender: TObject; const Item: TksListItemRowObj; Action: TCollectionNotification);
begin
  FCached := False;
end;

function TksListItemRow.TextHeight(AText: string): single;
begin
  Bitmap.Canvas.Font.Assign(FFont);
  Result := Bitmap.Canvas.TextHeight(AText);
end;

function TksListItemRow.TextWidth(AText: string): single;
begin
  Bitmap.Canvas.Font.Assign(FFont);
  Result := Bitmap.Canvas.TextWidth(AText);
end;

function TksListItemRow.TextOut(AText: string; X, Y, AWidth, AHeight: single): TksListItemRowText;
begin
  Result := TksListItemRowText.Create(Self);
  if AHeight = 0 then
    AHeight := RowHeight(False);
  Result.FRect := RectF(X, Y, X+AWidth, Y+AHeight);
  Result.Font.Assign(FFont);
  Result.TextAlignment := TTextAlign.Leading;
  Result.TextColor := FTextColor;
  Result.Text := AText;
  FList.Add(Result);
end;

function TksListItemRow.TextOutRight(AText: string; const AVertAlign: TTextAlign): TksListItemRowText;
var
  AWidth: single;
begin
  AWidth := TextWidth(AText);
  Result := TextOutRight(AText, AWidth, AVertAlign);
end;

function TksListItemRow.TextOut(AText: string; X, AWidth: single; const AVertAlign: TTextAlign = TTextAlign.Center): TksListItemRowText;
var
  AHeight: single;
  AYPos: single;
begin
  AHeight := TextHeight(AText);
  case AVertAlign of
    TTextAlign.Leading: AYPos := 4;
    TTextAlign.Trailing: AYPos := (RowHeight(False) - AHeight) - 4;
    TTextAlign.Center: AYPos := (RowHeight(False) - AHeight) / 2;
  end;
  Result := TextOut(AText, X, AYPos, AWidth, AHeight)
end;

function TksListItemRow.TextOutRight(AText: string; Y, AWidth: single): TksListItemRowText;
var
  AHeight: single;
begin
  AHeight := TextHeight(AText);
  Result := TextOut(AText, 0, Y, AWidth, AHeight);
  Result.TextAlignment := TTextAlign.Trailing;
  Result.Rect.Offset(ScreenWidth - Result.Rect.Right, 0);
end;

function TksListItemRow.TextOutRight(AText: string; AWidth: single; const AVertAlign: TTextAlign = TTextAlign.Center): TksListItemRowText;
var
  AHeight: single;
  AYPos: single;
begin
  AHeight := TextHeight(AText);
  case AVertAlign of
    TTextAlign.Leading: AYPos := 4;
    TTextAlign.Trailing: AYPos := (RowHeight(False) - AHeight) - 4;
    TTextAlign.Center: AYPos := (RowHeight(False) - AHeight) / 2;
  end;
  Result := TextOutRight(AText, AYPos, AWidth);
end;

{ TksListViewAppearence }

constructor TksListViewAppearence.Create(AListView: TksListView);
begin
  inherited Create;
  FListView := AListView;
  FBackground := claWhite;
  FItemBackground := claWhite;
  FAlternatingItemBackground := claGainsboro;
end;

procedure TksListViewAppearence.SetAlternatingItemBackground(
  const Value: TAlphaColor);
begin
  FAlternatingItemBackground := Value;
  FListView.ApplyStyle;
end;

procedure TksListViewAppearence.SetBackground(const Value: TAlphaColor);
begin
  FBackground := Value;
  FListView.ApplyStyle;
end;

procedure TksListViewAppearence.SetItemBackground(const Value: TAlphaColor);
begin
  FItemBackground := Value;
  FListView.ApplyStyle;
end;

function TksListView.AddHeader(AText: string): TksListItemRow;
begin
  Result := AddRow('', TListItemPurpose.Header, '');
  Result.Font.Style := [];
  Result.TextColor := claSilver;
  Result.Font.size := 16;
  Result.TextOut(AText, 0, Result.TextWidth(AText), TTextAlign.Trailing);
  Result.CacheRow;
end;

function TksListView.AddRow(const ASearchIndex: string = '';
                            const APurpose: TListItemPurpose = TListItemPurpose.None;
                            const AId: string = ''): TksListItemRow;
var
  AItem: TListViewItem;
  AIndex: string;
begin
  AItem := Items.Add;
  AIndex := ASearchIndex;
  if AIndex = '' then
    AIndex := ' ';
  AItem.Text := AIndex;
  AItem.Objects.Clear;
  AItem.Purpose := APurpose;
  Result := TksListItemRow.Create(AItem);
  Result.Name := 'ksRow';
  Result.ID := AId;
end;

procedure TksListView.SetColorStyle(AName: string; AColor: TAlphaColor);
var
  StyleObject: TFmxObject;
begin
  StyleObject := FindStyleResource(AName);
  if StyleObject <> nil then
  begin
    (StyleObject as TColorObject).Color := AColor;
    Invalidate;
  end;
end;

procedure TksListView.ApplyStyle;
var
  StyleObject: TFmxObject;
begin
  SetColorStyle('background', FAppearence.Background);
  SetColorStyle('itembackground', FAppearence.ItemBackground);
  SetColorStyle('alternatingitembackground', FAppearence.AlternatingItemBackground);
  inherited;
end;

constructor TksListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScreenScale := GetScreenScale;
  FAppearence := TksListViewAppearence.Create(Self);
end;

destructor TksListView.Destroy;
begin
  FAppearence.Free;
  inherited;
end;

procedure TksListView.DoItemClick(const AItem: TListViewItem);
var
  ARow: TksListItemRow;
  AId: string;
  ICount: integer;
  ARowObj: TksListItemRowObj;
  AObjRect: TRectF;
begin
  inherited;
  ARowObj := nil;
  AId := '';
  ARow := AItem.Objects.FindObject('ksRow') as TksListItemRow;

  if ARow <> nil then
  begin
    AId := ARow.ID;
    for ICount := 0 to ARow.RowObjectCount-1 do
    begin
      AObjRect := ARow.RowObject[ICount].Rect;
      if (FMouseDownPos.X >= (AObjRect.Left-5)) and (FMouseDownPos.X <= (AObjRect.Right+5)) then
      begin
        ARowObj := ARow.RowObject[ICount];
        //Break;
      end;
    end;
  end;
  if Assigned(FOnClick) then
    FOnClick(Self, FMouseDownPos.X, FMouseDownPos.Y, AItem, AId, ARowObj);
end;

procedure TksListView.EndUpdate;
var
  ICount: integer;
  AItem: TListViewItem;
  ARow: TKsListItemRow;
  ARowObj: TKsListItemRow;
begin
  inherited EndUpdate;
  for ICount := 0 to Items.Count-1 do
  begin
    AItem := Items[ICount];
    ARow := AItem.Objects.FindObject('ksRow') as TKsListItemRow;
    if ARow <> nil then
      ARow.CacheRow;
  end;
end;

procedure TksListView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  {$IFDEF MSWINDOWS}
  FMouseDownPos := PointF(X-10, Y);
  {$ELSE}
  FMouseDownPos := PointF(X, Y);
  {$ENDIF}
  inherited;
end;

{ TksListItemRowObj }

procedure TksListItemRowObj.Changed;
begin
  FRow.Cached := False;
end;

constructor TksListItemRowObj.Create(ARow: TksListItemRow);
begin
  inherited Create;
  FRow := ARow;
end;

procedure TksListItemRowObj.SetID(const Value: string);
begin
  FId := Value;
  Changed;
end;

procedure TksListItemRowObj.SetRect(const Value: TRectF);
begin
  FRect := Value;
  Changed;
end;

end.

