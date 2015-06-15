unit ksFormTransition;

interface

{$IFDEF VER290}
  {$DEFINE XE8_OR_NEWER}
{$ENDIF}


uses System.UITypes, FMX.Controls, FMX.Layouts, FMX.Objects, System.Classes,
  FMX.Types, Generics.Collections, FMX.Graphics, System.UIConsts, FMX.Effects,
  FMX.Forms
  {$IFDEF XE8_OR_NEWER}
  ,FMX.ImgList
  {$ENDIF}
  ;

type

  TksFormTransitionType = (ksSlideIn, ksSlideOut, ksFadeIn, ksFadeOut);
  TksFormTransitionDirection = (ksLeftToRight, ksRightToLeft, ksTopToBottom, ksBottomToTop);

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or  pidiOSDevice)]
  TksFormTransition = class(TFmxObject)
  strict private

  public
    procedure ShowForm(ACurrentForm, ADestinationForm: TForm; AType: TksFormTransitionType; ADirection: TksFormTransitionDirection);
  end;


  procedure Register;

implementation

uses System.Types, FMX.Platform, SysUtils, FMX.Utils;

procedure Register;
begin
  RegisterComponents('Kernow Software', [TksFormTransition]);
end;

function GetScreenScale: Single;
var
   Service : IFMXScreenService;
begin
   Service := IFMXScreenService(
      TPlatformServices.Current.GetPlatformService(IFMXScreenService));
   Result := Service .GetScreenScale;
end;

{ TksFormTransition }


{ TksFormTransition }


{ TksFormTransition }

procedure TksFormTransition.ShowForm(ACurrentForm, ADestinationForm: TForm;
  AType: TksFormTransitionType; ADirection: TksFormTransitionDirection);
begin

end;

end.
