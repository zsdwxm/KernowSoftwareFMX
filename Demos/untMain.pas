unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, ksSlideMenu, FMX.Layouts, FMX.Objects,
  FMX.TabControl, FMX.ListBox, FMX.ListView.Types, FMX.ListView, ksListView,
  ksSegmentButtons;

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
    tabListView: TTabItem;
    tabSegmentButtons: TTabItem;
    ksListView1: TksListView;
    ksSegmentButtons1: TksSegmentButtons;
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
begin
  ksListView1.Items.BeginUpdate;
  try
    for ICount := 0 to 100 do
    begin
      with ksListView1.AddRow do
      begin
        TextColor := claBlack;
        DrawBitmap(imgHome.Bitmap, 0, 24, 24);
        TextOut('Line '+InttoStr(ICount), 40, 150);
        TextColor := claDodgerblue;
        TextOutRight('Cached scrolling :-)');

      end;
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
  SlideMenu1.AddMenuItem('SEGMENT_BUTTONS', 'Segment Buttons', imgContact.Bitmap);
  SlideMenu1.ItemIndex := 0;

  SlideMenu2.AddMenuItem('ANOTHER', 'Another Menu', imgHome.Bitmap);

  SlideMenu2.ItemIndex := 0;
  layoutImages.Visible := False;

  ksSegmentButtons1.AddButton('Button 1', '1');
  ksSegmentButtons1.AddButton('Button 2', '2');
  ksSegmentButtons1.AddButton('Button 3', '3');
end;

procedure TForm6.SlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
begin
  if AId = 'LISTVIEW' then TabControl1.ActiveTab := tabListView;
  if AId = 'SEGMENT_BUTTONS' then TabControl1.ActiveTab := tabSegmentButtons;
end;

end.
