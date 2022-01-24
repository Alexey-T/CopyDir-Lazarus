program copyfiles;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, copyfiles_unit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='CopyFiles';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFRM_copyfiles, FRM_copyfiles);
  Application.Run;
end.

