unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, ksSlideMenu, System.ImageList, FMX.ImgList,
  FMX.Effects, FMX.Objects;

type
  TForm6 = class(TForm)
    SlideMenu1: TksSlideMenu;
    SlideMenu2: TksSlideMenu;
    ToolBar1: TToolBar;
    btnLeftMenu: TButton;
    btnRightMenu: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ImageList1: TImageList;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure btnRightMenuClick(Sender: TObject);
    procedure btnLeftMenuClick(Sender: TObject);
    procedure SlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
    procedure SlideMenu2SelectMenuItemEvent(Sender: TObject; AId: string);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure RadioButton3Change(Sender: TObject);
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
  SlideMenu1.MenuStyle := msReveal;
  SlideMenu1.AddMenuItem('ABOUT', 'About Us', 0);
  SlideMenu1.AddMenuItem('RESERVE', 'Make a Booking', 1);
  SlideMenu1.AddMenuItem('MY BOOKINGS', 'My Bookings', 2);
  SlideMenu1.AddMenuItem('MENU', 'View Menus', 3);
  SlideMenu1.AddMenuItem('CONTACT', 'Contact Us', 4);
  SlideMenu1.ItemIndex := 0;

  SlideMenu2.AddMenuItem('ANOTHER', 'Another Menu', 0);
  SlideMenu2.AddMenuItem('ABOUT', 'About', 5);

  SlideMenu2.ItemIndex := 0;
end;

procedure TForm6.RadioButton1Change(Sender: TObject);
begin
  SlideMenu1.MenuStyle := msPush;
end;

procedure TForm6.RadioButton2Change(Sender: TObject);
begin
  SlideMenu1.MenuStyle := msReveal;
end;

procedure TForm6.RadioButton3Change(Sender: TObject);
begin
  SlideMenu1.MenuStyle := msOverlap;
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
