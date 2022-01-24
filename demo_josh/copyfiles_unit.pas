unit copyfiles_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, copydir;

const AppVer='0.7.2';
      KB=1024;
      MB=KB*KB;
      GB=MB*KB;
      TB=GB*KB;

type

  { TFRM_copyfiles }

  TFRM_copyfiles = class(TForm)
    AppVersion_Lab: TLabel;
    ActualBytesCopied_Lab: TLabel;
    BufferSize_Lab: TLabel;
    ProgressBarFileCopy: TShape;
    read_led: TShape;
    ProgressBar: TShape;
    ProgressBarBorder: TShape;
    write_led: TShape;
    UseCopyBuffer_Lab: TLabel;
    CopyIfContentChanged_Lab: TLabel;
    Image_Options_UseCopyBuffer: TImage;
    image_options_copyIfContentChanged: TImage;
    PreserveDates_Lab: TLabel;
    PreserveAttrib_Lab: TLabel;
    CopyReadOnly_Lab: TLabel;
    CopyHidden_Lab: TLabel;
    CopySystem_Lab: TLabel;
    CopyArchive_Lab: TLabel;
    Abort_copy_Image: TImage;
    CopyAllFiles_Lab: TLabel;
    CopyOnlyIfExists_Lab: TLabel;
    CopyIfNewer_Lab: TLabel;
    CopyIfFileSizeChanged_Lab: TLabel;
    image_options_preserveAttributes: TImage;
    image_options_copyReadOnly: TImage;
    image_options_copyHidden: TImage;
    image_options_copySystem: TImage;
    image_options_copyArchive: TImage;
    image_options_CopyAllFiles: TImage;
    image_options_CopyOnlyIfExists: TImage;
    image_options_copyIfNewer: TImage;
    image_options_copyIfSizeChanged: TImage;
    Image_yes: TImage;
    image_no: TImage;
    image_options_preserveFileDates: TImage;
    Shape1: TShape;
    Shape2: TShape;
    speedlab: TLabel;
    SetDestLab: TLabel;
    ProgressBar_Lab: TLabel;
    StartBut: TButton;
    CopyFromLab: TLabel;
    CopyFromTitle: TLabel;
    BytesCopyLab: TLabel;
    ElapsedTimeLab: TLabel;
    SetSourceLab: TLabel;
    SourceDialog: TSelectDirectoryDialog;
    DestDialog: TSelectDirectoryDialog;
    TotalBytesLab: TLabel;
    MainPanel: TPanel;
    Timer1: TTimer;
    BufferSizeTrackBar: TTrackBar;
    procedure Abort_copy_ImageClick(Sender: TObject);
    procedure BufferSizeTrackBarChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure image_options_preserveFileDatesClick(Sender: TObject);
    procedure SetDestLabClick(Sender: TObject);
    procedure StartButClick(Sender: TObject);
    procedure SetSourceLabClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure displayandswapoptions(avalue:integer;aswapit:boolean);
    function GetMyImage(AValue:boolean):TBitmap;
  public

  end;

var
  FRM_copyfiles: TFRM_copyfiles;
  Old_CopyDirProcessedFile:String;
  cf:TCopyDir;
  starttime:TDateTime;
  SourceFolder:String='';
  DestFolder:String='';
  copyrunning:boolean=false;
  OldTotalBytesToCopy:QWord;

  options_preserveFileDates: Boolean=true;
  options_preserveAttributes: Boolean=true;
  options_copyReadOnly: Boolean=true;
  options_copyHidden: Boolean=false;
  options_copySystem: Boolean=false;
  options_copyArchive: Boolean=true;
  options_copyAllFiles: Boolean=true;
  options_CopyOnlyIfExists:boolean=false;
  options_copyIfNewer:boolean=false;
  options_copyIfSizeChanged:boolean=false;
  options_copyIfContentChanged:boolean=false;

  options_copybuffersize:LongInt=(8*KB)-1;

  options_usecopybuffer:boolean=true;


implementation

{$R *.lfm}

{ TFRM_copyfiles }


function TFRM_copyfiles.GetMyImage(AValue:boolean):TBitmap;
begin
  if Avalue=true then result:=image_Yes.Picture.bitmap
  else result:=image_No.picture.bitmap;
end;

procedure TFRM_copyfiles.displayandswapoptions(avalue:integer;aswapit:boolean);

procedure updateitemsdata(var aimage:timage;
                          var alabel:tlabel;
                          var aboolean:boolean);
begin
  if aswapit then aboolean:=not aboolean;
  aimage.Picture.Bitmap:=GetMyImage(aboolean);
  aimage.Enabled:=not copyrunning;
  alabel.Enabled:=not copyrunning;
end;

begin
  case avalue of
    1:updateitemsdata(image_options_preserveFileDates,PreserveDates_Lab,options_preserveFileDates);
    2:updateitemsdata(image_options_preserveAttributes,PreserveAttrib_Lab,options_preserveAttributes);
    3:updateitemsdata(image_options_copyReadOnly,CopyReadOnly_Lab,options_copyReadOnly);
    4:updateitemsdata(image_options_copyHidden,CopyHidden_Lab,options_copyHidden);
    5:updateitemsdata(image_options_copySystem,CopySystem_Lab,options_copySystem);
    6:updateitemsdata(image_options_copyArchive,CopyArchive_Lab,options_copyArchive);
    10:begin
         updateitemsdata(image_options_CopyAllFiles,CopyAllFiles_Lab,options_CopyAllFiles);
         image_options_CopyOnlyIfExists.visible:=not options_copyAllFiles;
         image_options_copyIfNewer.visible:=not options_copyAllFiles;
         image_options_copyIfSizeChanged.visible:=not options_copyAllFiles;
         image_options_copyIfContentChanged.visible:=not options_copyAllFiles;
         CopyOnlyIfExists_Lab.Visible:=not options_copyAllFiles;
         CopyIfNewer_Lab.Visible:=not options_copyAllFiles;
         CopyIfFileSizeChanged_Lab.Visible:=not options_copyAllFiles;
         CopyIfContentChanged_Lab.visible:=not options_copyAllFiles;
       end;
    11:updateitemsdata(image_options_CopyOnlyIfExists,CopyOnlyIfExists_Lab,options_CopyOnlyIfExists);
    12:updateitemsdata(image_options_copyIfNewer,CopyIfNewer_Lab,options_copyIfNewer);
    13:updateitemsdata(image_options_copyIfSizeChanged,CopyIfFileSizeChanged_Lab,options_copyIfSizeChanged);
    14:updateitemsdata(image_options_copyIfContentChanged,CopyIfContentChanged_Lab,options_copyIfContentChanged);
    16:begin
         updateitemsdata(Image_Options_UseCopyBuffer,UseCopyBuffer_Lab,options_usecopybuffer);
         BufferSizeTrackBar.Visible:=options_usecopybuffer;
         BufferSize_Lab.Visible:=options_usecopybuffer;
       end;
  end;
end;

function expandbytes(AValue:QWord;showdecimals,abbrev:boolean):String;
var suf:string='';
    v:string='';
    d:integer;
begin
  if AValue>=TB Then
  begin
    suf:='TBytes';
    if showdecimals then d:=4 else d:=0;
    v:=FloatToStrF(AValue/TB, fffixed, 8, d)
  end
  else
  if AValue>=GB Then
  begin
    if showdecimals then d:=3 else d:=0;
    suf:='GBytes';
    v:=FloatToStrF(AValue/GB, fffixed, 8, d)
  end
  else
  if AValue>=MB then
  begin
    suf:='MBytes';
    if showdecimals then d:=2 else d:=0;
    v:=FloatToStrF(AValue/MB, fffixed, 8, d)
  end
  else
  if AValue>=KB then
  begin
    suf:='KBytes';
    if showdecimals then d:=2 else d:=0;
    v:=FloatToStrF(AValue/KB, fffixed, 8, d)
  end
  else
  begin
    suf:='Bytes';
    v:=inttostr(AValue);
  end;
  if abbrev then result:=v+' '+leftstr(suf,2)
  else result:=v+' '+suf;
end;

procedure TFRM_copyfiles.StartButClick(Sender: TObject);
var i:integer;
begin
  StartBut.Enabled:=false;
  StartBut.Caption:='Now Copying.. Please wait ...';
  BufferSizeTrackBar.Enabled:=False;
  Abort_copy_Image.Visible:=true;
  application.ProcessMessages;
  starttime:=now();
  copyrunning:=true;
  for i:=1 to 14 do displayandswapoptions(i,false);
  OldTotalBytesToCopy:=0;
  cf:=tcopydir.Create(SourceFolder,DestFolder);
  cf.KeepALive:=true;
  cf.Language:='EN';
  cf.copycomparefiles:=options_copyIfContentChanged;
  cf.AppProcessMessagesCounterInterval:=20;     // processmessages called at a minimum of 20ms  use with keepalive setting.
  cf.CopyReadOnlyFiles := options_copyReadOnly;  // copy files with attribut "ReadOnly"
  cf.CopyHiddenFiles := options_copyHidden;   // don't copy files with attribut "Hidden"
  cf.CopySystemFiles := options_copySystem;   // don't copy files with attribut "SystemFile"
  cf.CopyArchiveFiles := options_copyArchive;   // copy files with attribut "Archive"

  cf.PreserverFileDates := options_preserveFileDates; // preserve dates of copied files

  cf.copyAllFiles:=options_copyAllFiles;
  cf.copyOnlyIfExists:=options_CopyOnlyIfExists;
  cf.copyIfNewer:=options_copyIfNewer;
  cf.copyIfSizeChanged:=options_copyIfSizeChanged;
  {$IFNDEF Unix}
  // TCopyDir.PreserveAttributes is not available for Unix systems
  // (Unit "FileUtil" is not able to set attributes on Unix system)
  cf.PreserveAttributes := options_preserveAttributes; // preserve attributes of copied files
  {$ENDIF}
  ProgressBar.width:=0;
  //cf.Enumerate; // this is optional; files added to directory after enumeration won't be copied
  cf.AbortCopy:=false;
  cf.copybuffersize:=options_copybuffersize;
  cf.comparebuffersize:=cf.copybuffersize;
  cf.usebufferedcopy:=options_usecopybuffer;

  read_led.Visible:=true;
  write_led.Visible:=false;
  cf.Start;
//  Log := Cf.GetLog;
  if assigned(cf) then cf.Free;
  read_led.Visible:=false;
  write_led.Visible:=false;
  StartBut.Caption:='Click to Start Copy';
  StartBut.Enabled:=true;
  BufferSizeTrackBar.Enabled:=True;
  TotalBytesLab.Caption:='';
  ProgressBAr.width:=0;
  ProgressBarFileCopy.Width:=0;
  ProgressBar_Lab.Caption:='';
  ProgressBAr.visible:=false;
  CopyFromLab.Caption:='';
  Abort_copy_Image.Visible:=false;
  application.ProcessMessages;
  copyrunning:=false;
  for i:=1 to 16 do displayandswapoptions(i,false);
end;

procedure TFRM_copyfiles.SetDestLabClick(Sender: TObject);
begin
  if DestDialog.Execute then
  begin
    DestFolder := IncludeTrailingPathDelimiter(DestDialog.FileName);
    SetDestLab.caption:=DestFolder;
  end;
end;

procedure TFRM_copyfiles.FormActivate(Sender: TObject);
var i:integer;
begin
  for i:=1 to 16 do displayandswapoptions(i,false);
  AppVersion_Lab.caption:='Version : '+AppVer;
end;

procedure TFRM_copyfiles.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if assigned(cf) then cf.AbortCopy:=true;
end;

procedure TFRM_copyfiles.Abort_copy_ImageClick(Sender: TObject);
begin
  if assigned(cf) then cf.AbortCopy:=true;
end;

procedure TFRM_copyfiles.BufferSizeTrackBarChange(Sender: TObject);

function setvals(avalue:LongInt):string;
begin
  result:=expandbytes(avalue,false,true)+' Buffer Size';
  options_copybuffersize:=avalue;
end;

function calcnth(st:longint;iter:integer):LongInt;
begin
  result:=st;
  while iter<>0 do
  begin
    result:=result+result;
    dec(iter);
  end;
end;


begin
  BufferSize_Lab.Caption:=setvals(calcnth(1*KB,BufferSizeTrackBar.Position)-1);
end;


procedure TFRM_copyfiles.image_options_preserveFileDatesClick(Sender: TObject);
begin
  if sender is timage then displayandswapoptions(timage(sender).Tag,true)
  else
    if sender is tlabel then displayandswapoptions(tlabel(sender).Tag,true)
end;

procedure TFRM_copyfiles.SetSourceLabClick(Sender: TObject);
begin
  if SourceDialog.Execute then
  begin
    SourceFolder := IncludeTrailingPathDelimiter(SourceDialog.FileName);
    SetSourceLab.caption:=SourceFolder;
  end;
end;

procedure TFRM_copyfiles.Timer1Timer(Sender: TObject);
var e:boolean;
    t:integer;
begin
  e:=not((SourceFolder='') or (DestFolder=''));
  if ((e<>StartBut.Enabled) and (not copyrunning)) then
  begin
  //  ProgressBar.Visible:=e;
    startbut.Enabled:=e;
    if e then startbut.Caption:='Click to Start Copy'
    else startbut.caption:='Set Source and Destination Folders';
    application.ProcessMessages;
  end;
  if (assigned(cf) and (cf.AbortCopy=false)) then    // extra check as variables not accessable if abortcopy is used.
  begin
    FRM_copyfiles.caption:='Copy Folder  -  '+cf.ProgressText;
    if Old_CopyDirProcessedFile<>cf.CopyDirProcessedFromFile then
    begin
      old_CopyDirProcessedFile:=cf.CopyDirProcessedFromFile;
      if  old_CopyDirProcessedFile<>'' then
      begin
        if pos('(Comparing)',old_CopyDirProcessedFile)>0 then CopyFromLab.Font.Color:=clYellow else CopyFromLab.Font.Color:=clWhite;
        CopyFromLab.Caption:=old_CopyDirProcessedFile+' ('+expandbytes(cf.CopyDirProcessedFromFileSize,true,false)+')';
      end;
      BytesCopyLab.caption:=expandbytes(cf.TotalByteProcessed,true,false);//v+' '+suf;
      // check for any zero's to avoid DIV by ZERO and other spurious results
      if ((cf.TotalByteProcessed>0) and (cf.TotalBytesToCopy>0)) then
      begin
        ProgressBar.Visible:=true;
        ProgressBar.Width:=Trunc((ProgressBarBorder.Width-2)*(cf.TotalByteProcessed/cf.TotalBytesToCopy));
        // calculate %
        ProgressBAr_Lab.caption:=FloatToStrF((cf.TotalByteProcessed/cf.TotalBytesToCopy)*100, fffixed, 5, 2)+'%';
      end
      else
      begin
        ProgressBar.Visible:=true;
        ProgressBar.width:=0;
        ProgressBar_Lab.Caption:='';
      end;
    end;
    if copyrunning then
    begin
      t:=SecondsBetween(now(),starttime);
      ElapsedTimeLab.caption:='Elapsed Time : '+timetostr(starttime-now());
      if t>0 then speedlab.caption:=expandbytes(trunc(cf.TotalByteProcessed/t),true,false)+' per Second';
      ActualBytesCopied_Lab.caption:=expandbytes(cf.ActutalBytesCopied,true,false)+' Copied';
      case cf.progress of
        0:begin          // idle
            read_led.Visible:=false;
            write_led.Visible:=false;
          end;
        1:read_led.Visible:=not read_led.Visible;    // reading folders
        2:write_led.Visible:=not write_led.Visible;   // creating folders
        3:begin                                       // copying
            //write_led.Visible:=not write_led.Visible;
            read_led.Visible:=not read_led.Visible;
            write_led.Visible:=not read_led.Visible;
            ProgressBarFileCopy.Visible:=((cf.fileprogress>0) and (cf.fileprogress<100));
            if cf.fileprogress>0 then
            begin
              ProgressBarFileCopy.Width:=Trunc((cf.fileprogress/100)*(ProgressBarBorder.Width-2));
            end;
          end;
        4:begin                     // not used
          end;
        5:begin   // completed
            read_led.Visible:=false;
            write_led.Visible:=false;
          end;
      end;
    end
    else FRM_copyfiles.caption:='Copy Folder';
    if cf.TotalBytesToCopy<>OldTotalBytesToCopy then
    begin
      OldTotalBytesToCopy:=cf.TotalBytesToCopy;
      TotalBytesLab.caption:='Total Bytes to Copy : '+ExpandBytes(cf.TotalBytesToCopy,true,false);
    end;
  end;
end;

end.

