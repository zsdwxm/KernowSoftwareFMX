unit ksListViewTypes;

interface

uses FMX.ListView, FMX.ListView.Types, FMX.Graphics, Types, FMX.Types, FMX.TextLayout;

type
  TksListItemText = class(TListItemText)
    private
      FCached: TBitmap;
      FTextLayout: TTextLayout;
      //LocalRect: TRectF;
      FItemRect: TRectF;
      procedure Cache(ARect: TRectF); overload;
    protected
    public
      constructor Create(const AOwner: TListItem); override;
      destructor Destroy; override;
      procedure Cache; overload;
      procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
        const SubPassNo: Integer = 0); override;
    end;

implementation

uses System.UIConsts, FMX.Dialogs, SysUtils, FMX.Platform;

{ TksListItemText }

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

    //FCached.Canvas.BeginScene;
    //FCached.Canvas.Font.Assign(Font);

    CalculateLocalRect(ARect, GetScreenScale, []);

    //FCached.Canvas.EndScene;
    FItemRect := FLocalRect;


    //FLocalRect := RectF(0, 0, FLocalRect.Width, FLocalRect.Height);

    FCached.BitmapScale := GetScreenScale;
    FCached.Width := Round(FItemRect.Width * GetScreenScale);
    FCached.Height := Round(FItemRect.Height * GetScreenScale)
    ;
    FCached.Clear(claNull);
    //FCached.Canvas.Stroke.Color := claRed;
    FCached.Canvas.BeginScene;

    //FCached.Canvas.DrawRect(RectF(0,0,FItemRect.Width-0, FItemRect.Bottom-0), 0,0,[],1);

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
    FTextLayout.TopLeft := PointF(0, 0);//FLocalRect.TopLeft;
    FTextLayout.EndUpdate;

    FTextLayout.RenderLayout(FCached.Canvas);
     // FCached.Canvas.DrawRect(

    //OffsetRect(FLocalRect, 0-FLocalRect.Left, 0-FLocalRect.Top);
    //FLocalRect := ARect;

    //inherited Render(FCached.Canvas, TListViewItem(Owner).Index, [] ,0);
    //FCached.Canvas.Fill.Color := claRed;
    FCached.Canvas.EndScene;
    //FCached.SaveToFile('c:\cached.png');
end;

procedure TksListItemText.Cache;
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
  //Cache(FLocalRect);
  Canvas.DrawBitmap(FCached, RectF(0, 0, FCached.Width, FCached.Height),
                    FLocalRect,
                    //FItemRect,
                    1, True);
end;

end.
