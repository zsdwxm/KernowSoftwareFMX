unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.ImageList, FMX.ImgList, ksSlideMenu;

type
  TForm6 = class(TForm)
    SlideMenu1: TksSlideMenu;
    menuImages: TImageList;
    SlideMenu2: TksSlideMenu;
    ToolBar1: TToolBar;
    btnLeftMenu: TButton;
    btnRightMenu: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnRightMenuClick(Sender: TObject);
    procedure btnLeftMenuClick(Sender: TObject);
    procedure SlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
    procedure SlideMenu2SelectMenuItemEvent(Sender: TObject; AId: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

procedure TForm6.btnLeftMenuClick(Sender: TObject);
begin
  SlideMenu1.ToggleMenu;
end;

procedure TForm6.btnRightMenuClick(Sender: TObject);
begin
  SlideMenu2.ToggleMenu;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  SlideMenu1.AddMenuItem('ABOUT', 'About Us', 4);
  SlideMenu1.AddMenuItem('RESERVE', 'Make a Booking', 2);
  SlideMenu1.AddMenuItem('MY BOOKINGS', 'My Bookings', 0);
  SlideMenu1.AddMenuItem('MENU', 'View Menus', 3);
  SlideMenu1.AddMenuItem('CONTACT', 'Contact Us', 1);
  SlideMenu1.ItemIndex := 0;

  SlideMenu2.AddMenuItem('ANOTHER', 'Another Menu', 4);
  SlideMenu2.AddMenuItem('ABOUT', 'About', 2);

  SlideMenu2.ItemIndex := 0;
end;

procedure TForm6.SlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
begin
  Label2.Text := 'Menu item clicked: '+AId;
end;

procedure TForm6.SlideMenu2SelectMenuItemEvent(Sender: TObject; AId: string);
begin
  Label2.Text := 'Menu item clicked: '+AId;
end;

end.
