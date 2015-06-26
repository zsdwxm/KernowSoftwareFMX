unit ksListView;

interface

uses
  Classes, FMX.Types, FMX.Controls, FMX.ListView,
  FMX.ListView.Types, FMX.Graphics, Types, System.UITypes,
  FMX.TextLayout;

type
  TksListView = class;

  TksListItemText = class(TListItemText)
  private
    FCached: TBitmap;
    FTextLayout: TTextLayout;
    FItemRect: TRectF;

    procedure Cache(ARect: TRectF); overload;
  protected

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
  public
    constructor Create(AListView: TksListView);
    destructor Destroy; override;
    function TextOut(AItem: TListViewItem; AText: string; X, Y, AWidth: single; const ATrimming: TTextTrimming = TTextTrimming.None): TksListItemText;
    function DrawBitmap(AItem: TListViewItem; ABmp: TBitmap; X, Y, AWidth, AHeight: Extended): TListItemImage;
    property Font: TFont read FFont;
    property TextColor: TAlphaColor read FTextColor write FTextColor;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or  pidiOSDevice)]
  TksListView = class(TListView)
  private
    FCanvas: TksListViewCanvas;
    procedure DoItemClick(const AItem: TListViewItem);
    { Private declarations }
  protected

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
    { Published declarations }
  end;

procedure Register;

implementation

uses SysUtils, FMX.Platform, System.UIConsts, FMX.Forms;

procedure Register;
begin
  RegisterComponents('kernow Software', [TksListView]);
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
  ItemAppearanceObjects.HeaderObjects.Text.Visible := False;
  AHeader.Purpose := TListItemPurpose.Header;
  Canvas.Font.Size := 12;
  Canvas.TextColor := claDimgray;
  Canvas.TextOut(AHeader, ATitle, 0, 0, 200);
  if AHeader.Objects.AccessoryObject <> nil then

    AHeader.Objects.AccessoryObject.Visible := False;
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
  //Canvas.TextOut(AItem, FormatDateTime('dd-mmm', ATran.Date), 4, 0, 50);

  Canvas.TextOut(AItem,
                 AName,
                 4,
                 0,
                 150);

  Canvas.TextColor := claDodgerblue;
  with Canvas.TextOut(AItem, AValue, 4, 0, 160, TTextTrimming.Character) do
  begin
    PlaceOffset.X := -20;
    TextAlign := TTextAlign.Trailing;
    Align := TListItemAlign.Trailing;
  end;
  AItem.Objects.AccessoryObject.Visible := AClickId <> '';

  {Canvas.TextOut(AItem,
                 AValue,
                 Round(Width - ItemAppearanceObjects.ItemObjects.Detail.PlaceOffset.X),
                 0,
                 Round(ItemAppearanceObjects.ItemObjects.Detail.Width),
                 TTextTrimming.Character);//.Align := TListItemAlign.Trailing;   }
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
begin
  inherited;
  Application.ProcessMessages;
  Sleep(100);
  ItemIndex := -1;
end;

procedure TksListView.EndUpdate;
begin
  inherited;
  CacheObjects;
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
  //Result := AItem.Objects.FindObject(ID) as TListItemImage;
  //if Result = nil then
  //begin
    Result := TListItemImage.Create(AItem);
    //Result.Name := ID;
    Result.VertAlign := TListItemAlign.Center;
    Result.PlaceOffset.X := X;
    Result.PlaceOffset.Y := Y;
    Result.Width := AWidth;
    Result.Height := AHeight;
    Result.Bitmap := ABmp;
  //end;
end;

function TksListViewCanvas.TextOut(AItem: TListViewItem; AText: string; X, Y, AWidth: single; const ATrimming: TTextTrimming = TTextTrimming.None): TksListItemText;
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
  ALbl.Width := AWidth;
  if X <> 0 then ALbl.PlaceOffset.X := X;
  if Y <> 0 then ALbl.PlaceOffset.Y := Y;
  Result := ALbl;

end;

end.

