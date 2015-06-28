unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, ksSlideMenu, FMX.Layouts, FMX.Objects,
  FMX.TabControl, FMX.ListBox, FMX.ListView.Types, FMX.ListView, ksListView;

type
  TForm6 = class(TForm)
    ToolBar1: TToolBar;
    btnLeftMenu: TButton;
    btnRightMenu: TButton;
    Label1: TLabel;
    imgHome: TImage;
    imgSearch: TImage;
    imgCalendar: TImage;
    imgMenu: TImage;
    imgContact: TImage;
    layoutImages: TLayout;
    SlideMenu1: TksSlideMenu;
    SlideMenu2: TksSlideMenu;
    imgAbout: TImage;
    TabControl1: TTabControl;
    tabAbout: TTabItem;
    tabContact: TTabItem;
    ksListView1: TksListView;
    procedure FormCreate(Sender: TObject);
    procedure btnRightMenuClick(Sender: TObject);
    procedure btnLeftMenuClick(Sender: TObject);
    procedure SlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
  private
    procedure BuildAboutListView;
    { Private declarations }
  protected
    procedure DoShow; override;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

uses System.UIConsts;

{$R *.fmx}

procedure TForm6.btnLeftMenuClick(Sender: TObject);
begin
  SlideMenu1.ToggleMenu;
end;

procedure TForm6.btnRightMenuClick(Sender: TObject);
begin
  SlideMenu2.ToggleMenu;
end;

procedure TForm6.BuildAboutListView;
var
  ICount: integer;
  AItem: TListViewItem;
begin
  ksListView1.Items.BeginUpdate;
  try
    for ICount := 0 to 100 do
    begin
      AItem := ksListView1.Items.Add;
      ksListView1.Canvas.DrawBitmap(AItem, imgHome.Bitmap, 0, 0, 24, 24);
      ksListView1.Canvas.TextColor := claDimgray;
      ksListView1.Canvas.TextOut(AItem, 'Line '+InttoStr(ICount), 30, 0, 0);
      ksListView1.Canvas.TextColor := claDodgerblue;
      ksListView1.Canvas.TextOutRight(AItem, 'cached scrolling :-)', 0, 0, 0);
    end;
  finally
    ksListView1.Items.EndUpdate;
  end;
end;

procedure TForm6.DoShow;
begin
  inherited;
  BuildAboutListView;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  TabControl1.TabPosition := TTabPosition.None;

  SlideMenu1.AddMenuItem('LISTVIEW', 'Cached ListView', imgHome.Bitmap);
  SlideMenu1.AddMenuItem('CONTACT', 'Contact Us', imgContact.Bitmap);
  SlideMenu1.ItemIndex := 0;

  SlideMenu2.AddMenuItem('ANOTHER', 'Another Menu', imgHome.Bitmap);
  SlideMenu2.AddMenuItem('ABOUT', 'About', imgSearch.Bitmap);

  SlideMenu2.ItemIndex := 0;
  layoutImages.Visible := False;
end;

procedure TForm6.SlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
begin
  if AId = 'LISTVIEW' then TabControl1.ActiveTab := tabAbout;
  if AId = 'CONTACT' then TabControl1.ActiveTab := tabContact;
end;

end.
