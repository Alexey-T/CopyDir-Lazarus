(**
*                               TCOPYDIR
*                               ========
*                                 v2.1
*
*  This unit contains "TCopyDir" class wich copies entire directories (incl. its
*  subdirectories)
*
*  Author: bastla (@ Supernature-Forum / @ Lazarus Forum)
*  Contact: send me a PM via Lazarus Forum
*      (http://forum.lazarus.freepascal.org/index.php?action=pm;sa=send;u=49100)
*  License: Free Domain
*  Website: http://forum.lazarus.freepascal.org/index.php/topic,20759.0.html
*
*  How TCopyDir works:
*  -------------------
*  TCopyDir uses TFileSearcher to enumerate a whole directory and copies its
*  content file by file.
*  Because of using LCL-components only, this class should work on all available
*  platforms supported by Lazarus and LCL.
*
*  Disclaimer:
*  -----------
*  THIS SOFTWARE IS PROVIDED BY BASTLA "AS IS" AND ANY
*  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL BASTLA BE LIABLE FOR ANY
*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

unit CopyDir;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef lcl}Forms,{$endif}Classes, Dialogs, SysUtils,
  LazFileUtils, FIleUtil, LCLIntf;


const NumberOfLanguages=3;
type
  TDirsArray = array of String;
  Str2=String[2];
  { TCopyDir }

  TCopyDir = class
  private
    _fileprogress:integer;
    _Language:Str2;
    _LanguageOptions:Array[0..NumberOfLanguages] of Str2;
    _LanguageText:Array[0..NumberOfLanguages,0..5] of String;
    _ProgressText:String;
    _Progress:Integer;

    _dirSource, _dirTarget: String;

    _fs: TFileSearcher;
    _log: TStringList;

    _files: TDirsArray;
    _directories: TDirsArray;

    _copied: Boolean;
    _dirsCreated: Boolean;
    _enumerated: Boolean;

    _copyingDone: TDirsArray;
    _copyingFailed: TDirsArray;
    _creatingDone: TDirsArray;
    _creatingFailed: TDirsArray;

    _preserveFileDates: Boolean;
    _preserveAttributes: Boolean;

    _copyReadOnly: Boolean;
    _copyHidden: Boolean;
    _copySystem: Boolean;
    _copyArchive: Boolean;

    _copyAllFiles:boolean;
    _copyOnlyIfExists:boolean;
    _copyIfNewer:Boolean;
    _copyIfSizeChanged:Boolean;
    _abortcopy:boolean;
    _copycomparefiles:boolean;

    _AppProcessMessagesCounterInterval:integer;


    _printToTerminal: Boolean;

    _keepalive:boolean;

    _usebufferedcopy:boolean;
    _comparebuffersize:LongInt;

    _CopyDirProcessedFromFile:String;
    _CopyDirProcessedFromFileSize:int64;
    _TotalBytesToCopy:QWOrd;
    _TotalByteProcessed:QWord;
    _ActutalBytesCopied:QWord;
    _appproctickcounter:Qword;

    _CopyBuffer,_CompBuffer1,_CompBuffer2:Array of Byte;
    _copybuffersize:LongInt;

    procedure _AddToList(const aString: String; var aList: TDirsArray);
    procedure _AddToLog(const aNote: String);
    procedure _CopyFile(const aFile: String);
    procedure _CopyFiles;
    procedure _CreateDir(const aDir: String);
    procedure _CreateDirs;
    procedure _DirFound(FileIterator: TFileIterator);
    procedure _FileFound(FileIterator: TFileIterator);

    function _CanCopy(const aFile: String): Boolean;
    function _SourceToTarget(const aTarget: String): String;

    procedure _CopyDirProcessMessages(ForceUpdate:Boolean);
    function _Compare2Files(const SrcFilename, DestFilename: string): boolean;
    function _CopyFile(const SrcFilename, DestFilename: string; PreserveTime: boolean): boolean;
    procedure _SetProgressText(AValue:integer);
  public
    constructor Create(const aSourceDir, aTargetDir: String);
    destructor Destroy; override;

    property PreserverFileDates: Boolean
      read _preserveFileDates
      write _preserveFileDates;
    {$IFNDEF Unix} property PreserveAttributes: Boolean
      read _preserveAttributes
      write _preserveAttributes; {$ENDIF}

    property CopyReadOnlyFiles: Boolean
      read _copyReadOnly
      write _copyReadOnly;
    property CopyHiddenFiles: Boolean
      read _copyHidden
      write _copyHidden;
    property CopySystemFiles: Boolean
      read _copySystem
      write _copySystem;
    property CopyArchiveFiles: Boolean
      read _copyArchive
      write _copyArchive;

    property PrintToTerminal: Boolean
      read _printToTerminal
      write _printToTerminal;

    property KeepALive: Boolean
      read _keepalive
      write _keepalive;

    property AbortCopy: Boolean
      read _abortcopy
      write _abortcopy;

    property copyAllFiles: Boolean
      read _copyAllFiles
      write _copyAllFiles;

    property copyOnlyIfExists: Boolean
      read _copyOnlyIfExists
      write _copyOnlyIfExists;

    property copyIfNewer: Boolean
      read _copyIfNewer
      write _copyIfNewer;

    property copyIfSizeChanged: Boolean
      read _copyIfSizeChanged
      write _copyIfSizeChanged;

    property AppProcessMessagesCounterInterval:Integer
      read _AppProcessMessagesCounterInterval
      write _AppProcessMessagesCounterInterval;

    property CopyDirProcessedFromFile:String
      read _CopyDirProcessedFromFile
      write _CopyDirProcessedFromFile;

    property CopyDirProcessedFromFileSize:int64
      read _CopyDirProcessedFromFileSize
      write _CopyDirProcessedFromFileSize;

    property TotalBytesToCopy:QwOrd
      read _TotalBytesToCopy
      write _TotalBytesToCopy;

    property TotalByteProcessed:QWord
      read _TotalByteProcessed
      write _TotalByteProcessed;

    property ActutalBytesCopied:QWord
      read _ActutalBytesCopied
      write _ActutalBytesCopied;

    property appproctickcounter:Qword
      read _appproctickcounter
      write _appproctickcounter;

    property copycomparefiles:boolean
      read _copycomparefiles
      write _copycomparefiles;

    property comparebuffersize:LongInt
      read _comparebuffersize
      write _comparebuffersize;

    property copybuffersize:LongInt
      read _copybuffersize
      write _copybuffersize;

    property Language:Str2
      read _Language
      write _Language;

    property ProgressText:String
      read _ProgressText
      write _ProgressText;

    property progress:integer
       read _Progress
       write _Progress;

    property fileprogress:integer
      read _fileprogress
      write _fileprogress;

    property usebufferedcopy:boolean
      read _usebufferedcopy
      write _usebufferedcopy;

    procedure Start;
    procedure Enumerate;

    function GetLog: TStringList;
  end;

implementation

// ************************************************************************** \\
// ***************************** PRIVATE SECTION **************************** \\
// ************************************************************************** \\

procedure TCopyDir._SetProgressText(AValue:integer);
var l,i:integer;
begin
  self.ProgressText:='';
  //find lang
  L:=0; // def to english
  for i:=0 to NumberOfLanguages do
  begin
    if upcase(_LanguageOptions[i])=upcase(_Language) then
    begin
      L:=i;
      break
    end;
  end;
  self.ProgressText:=_LanguageText[L,AValue];
  self.Progress:=Avalue;
end;

function TCopyDir._CopyFile(const SrcFilename, DestFilename: string; PreserveTime: boolean): boolean;

Var SHandle,DHandle:THandle;
    BytesRead:Int64=1; // default to 1 to force first loop
    SOpened:Boolean=False;
    DOpened:Boolean=False;
    fz,fr:int64;
Begin
  fz:=0;
  fr:=0;
  if not self._usebufferedcopy then
  begin
    result:=CopyFile(SrcFilename, DestFilename, self._preserveFileDates);
    self._CopyDirProcessMessages(False);
    exit;
  end;
  result:=false;
  If FileExists(SrcFilename) Then
  Begin
    result:=true;
    fz:=filesize(SrcFilename);
    Try
      SHandle :=FileOpen(SrcFilename, fmOpenRead + fmShareDenyNone);
      result:=(SHandle <> THandle(-1));
    Except
      Result:=False;
    end;
    If Result then
    Try
      SOpened:=True;
      DHandle :=FileCreate(DestFilename, fmCreate or fmShareExclusive);
      result:=(DHandle <> THandle(-1));
    Except
      result:=False;
    end;
    If Result then
    Begin
      DOpened:=True;
      self._CopyDirProcessMessages(False);
      BytesRead:=1;
      while bytesread<>0 do
      begin
        if self._AbortCopy then break;
        BytesRead:=fileread(shandle,self._CopyBuffer[0],self._copybuffersize);
        if BytesRead<>0 then
        begin
          filewrite(DHandle,self._CopyBuffer[0],BytesRead);
          fr:=fr+Bytesread;
          if ((fr>0) and (fz>0)) then self._fileprogress:=trunc((fr/fz)*100);  // check for filesize =0
          self._ActutalBytesCopied:=self._ActutalBytesCopied+BytesRead;
        end;
        self._CopyDirProcessMessages(False);
      end;
    end;
    If SOPened then FileClose(SHandle);
    If DOPened then FileClose(DHandle);
  end;
  if ((result) and (PreserveTime)) then FileSetDate(DestFilename,FileAge(SrcFilename));
end;


function TCopyDir._Compare2Files(const SrcFilename, DestFilename: string): boolean;
var
  SHandle1, SHandle2: THandle;
  BytesRead1: Int64=1;
  BytesRead2: Int64;
  fr,fz:int64;
  ILoop: Int64;
  BlockCount: Int64;
  SOpened1:Boolean=False;
  SOpened2:Boolean=false;
begin
  self._CopyDirProcessedFromFile:=SrcFilename+' (Comparing)';
  result:=True;
  //check if both file exists
  result:=((fileexists(SrcFilename)) and (fileexists(DestFilename))); // check if both files exists
  if result then result:=filesize(SrcFilename)=filesize(DestFilename);//  then result:=false;// check if file sizes different
  if result then
  begin
    SHandle1 :=FileOpen(SrcFilename, fmOpenRead + fmShareDenyNone);
    result:=(SHandle1 <> THandle(-1));
    if result then
    begin
      SOpened1:=True;
      SHandle2:=FileOpen(SrcFilename, fmOpenRead + fmShareDenyNone);
      result:=(SHandle2 <> THandle(-1));
      SOpened2:=Result;
    end;
    if result then
    begin
      self._CopyDirProcessMessages(False);
      fz:=filesize(SrcFilename);
      fr:=0;
      while bytesread1<>0 do
      begin
        BytesRead1:=fileread(shandle1,self._CompBuffer1[0],self._comparebuffersize);
        BytesRead2:=fileread(shandle2,self._CompBuffer2[0],self._comparebuffersize);
        fr:=fr+Bytesread1;
        if ((fr>0) and (fz>0)) then self._fileprogress:=trunc((fr/fz)*100);   // check for filesize =0
        if BytesRead1 = 0 then Break;
        if BytesRead1<>BytesRead2 then
        begin
          result:=false;
          break;
        end;
        BlockCount := BytesRead1;
        self._CopyDirProcessMessages(False);
        // was using a for loop but get ordinal expected on win32 client
        ILoop:=0;
        repeat
          if _CompBuffer1[ILoop] <> _CompBuffer2[ILoop] then
          begin
            result:=false;
            break;
          end;
          if self._AbortCopy then break;
          Inc(ILoop);
        Until ILoop>=BlockCount;
        self._CopyDirProcessMessages(False);
        // exit from while loop
        if self._AbortCopy then break;
        //self._TotalByteProcessed:=self._TotalByteProcessed+BlockCount;
      end;
    end;
    If SOpened1 then FileClose(SHandle1);
    If SOpened2 then FileClose(SHandle2);
  end;
  self._CopyDirProcessedFromFile:=SrcFilename;
  self._CopyDirProcessMessages(False);
end;

procedure TCopyDir._CopyDirProcessMessages(ForceUpdate:Boolean);
begin
  {$ifdef lcl}
  // if using lcl gui; then use routine to update UI
  // forceupdate will bypass the app proc counter interval for immediate update;
  if self.KeepALive then
  begin
    if (( (gettickcount64-self._appproctickcounter)>self._AppProcessMessagesCounterInterval) or (ForceUpdate)) then
    // using abs for when the tickcount rolls over range of dword,
    // GetTickCount64 has a range of qword; but last i checked had issues with win32
    begin
      application.ProcessMessages;
      self._appproctickcounter:=gettickcount64;
    end;
  end;
  {$endif}
end;

procedure TCopyDir._AddToList(const aString: String; var aList: TDirsArray);
begin
  SetLength(aList, Length(aList) + 1);
  aList[High(aList)] := aString;
  self._CopyDirProcessMessages(False);
end;

procedure TCopyDir._AddToLog(const aNote: String);
begin
  //Alexey
  {
  self._log.Append(aNote);
  if self._printToTerminal then WriteLn(aNote);
  }
end;

procedure TCopyDir._CopyFile(const aFile: String);
var OkToCopy:boolean;
    DestFileExists:boolean;
begin
  self._SetProgressText(3);
  self._CopyDirProcessedFromFile:=AFile;
  self._CopyDirProcessedFromFileSize:=filesize(aFile);
 // self._TotalByteProcessed:=self._TotalByteProcessed+self._CopyDirProcessedFromFileSize;
  OkToCopy:=self._copyAllFiles;
  if not OkToCopy then
  begin
    // NOt All files so check for options
    DestFileExists:=FileExists(self._SourceToTarget(aFile));
    OkToCopy:=true;
    if self._copyOnlyIfExists then oktocopy:=DestFileExists;
    if OkToCopy then if ((self._copyIfNewer) and (DestFileExists)) then OkToCopy:=FileAge(aFile)>FileAge(self._SourceToTarget(aFile));
    if OkToCopy then if ((self._copyIfSizeChanged) and (DestFileExists)) then OkToCopy:=Filesize(aFile)<>FileSize(self._SourceToTarget(aFile));
    if OktoCopy then if self._copycomparefiles then OkToCopy:=self._Compare2Files(aFile,self._SourceToTarget(aFile))=False; // copy only if Contents are different.
  end;
  if OkToCopy then
  begin
    self._CopyDirProcessMessages(True);
    if Self._CopyFile(aFile, self._SourceToTarget(aFile), self._preserveFileDates) then
    begin
      if self._preserveAttributes then
      begin
        if FileSetAttrUTF8(self._SourceToTarget(aFile),
          FileGetAttrUTF8(aFile)) <> -1 then
        begin
          self._AddToList(aFile, self._copyingDone);
        end;
      end else
      begin
        showmessage('fun failed');
        self._AddToList(aFile, self._copyingDone);
      end;
      self._CopyDirProcessMessages(True);
    end else
    begin
      self._AddToList(aFile, self._copyingFailed);
    end;
  end;
  self._TotalByteProcessed:=self._TotalByteProcessed+self._CopyDirProcessedFromFileSize;
end;

procedure TCopyDir._CopyFiles;
var
  i: LongWord;
begin
  if self._AbortCopy then exit;
  if (self._enumerated) and ((self._dirsCreated) or
    (Length(self._directories) = 0)) and (Length(self._files) > 0) then
  begin
    for i := Low(self._files) to High(self._files) do
    begin
      if self._AbortCopy then  break;
      if self._files[i] <> '' then
      begin
        self._CopyFile(self._files[i]);
      end;
    end;
    self._copied := true;
  end;
end;

procedure TCopyDir._CreateDir(const aDir: String);
begin
  if self._AbortCopy then exit;
  self._SetProgressText(2);
  if ForceDirectoriesUTF8(aDir) then
  begin
    self._AddToList(aDir, self._creatingDone);
  end else
  begin
    self._AddToList(aDir, self._creatingFailed);
  end;
end;

procedure TCopyDir._CreateDirs;
var
  i: LongWord;
begin
  self._SetProgressText(2);
  if self._AbortCopy then exit;
  if (self._enumerated) and (Length(self._directories) > 0) then
  begin
    for i := Low(self._directories) to High(self._directories) do
    begin
      if self._AbortCopy then break;
      if self._directories[i] <> '' then
      begin
        self._CreateDir(self._SourceToTarget(self._directories[i]));
      end;
      self._CopyDirProcessMessages(False);
    end;
    self._dirsCreated := true;
  end;
end;

procedure TCopyDir._DirFound(FileIterator: TFileIterator);
begin
  self._SetProgressText(1);
  self._AddToList(FileIterator.FileName, self._directories);
end;

procedure TCopyDir._FileFound(FileIterator: TFileIterator);
begin
  self._SetProgressText(1);
  if self._CanCopy(FileIterator.FileName) then
  begin
    self._AddToList(FileIterator.FileName, self._files);
  end;
end;

function TCopyDir._CanCopy(const aFile: String): Boolean;
var
  __fileAttributes: LongInt;
begin
  __fileAttributes := FileGetAttrUTF8(aFile);
  self._TotalBytesToCopy:=self._TotalBytesToCopy+FileSize(AFIle);

  if (__fileAttributes and faReadOnly <> 0) and not self._copyReadOnly then
  begin
    Result := false;
    Exit;
  end;
  if (__fileAttributes and faHidden <> 0) and not self._copyHidden then
  begin
    Result := false;
    Exit;
  end;
  if (__fileAttributes and faSysFile <> 0) and not self._copySystem then
  begin
    Result := false;
    Exit;
  end;
  if (__fileAttributes and faArchive <> 0) and not self._copyArchive then
  begin
    Result := false;
    Exit;
  end;

  Result := true;
end;

function TCopyDir._SourceToTarget(const aTarget: String): String;
begin
  Result := IncludeTrailingPathDelimiter(self._dirTarget) + Copy(aTarget,
    Length(self._dirSource) + 1, Length(aTarget));
end;


// ************************************************************************** \\
// ***************************** PUBLIC SECTION ***************************** \\
// ************************************************************************** \\

constructor TCopyDir.Create(const aSourceDir, aTargetDir: String);

procedure Fill_Lang(avalue:integer;lc,s0,s1,s2,s3,s4,s5:string);
begin

  self._LanguageOptions[avalue]:=lc;

  self._LanguageText[avalue,0]:=s0; // idle
  self._LanguageText[avalue,1]:=s1; // reading folder structure
  self._LanguageText[avalue,2]:=s2; // creating folder structure
  self._LanguageText[avalue,3]:=s3; // copying files
  self._LanguageText[avalue,4]:=s4;
  self._LanguageText[avalue,5]:=s5; // copy complete
end;

begin
  // create language files
  // 0=english (EN); 1=french (FR); 2=German; 3=Russian
  Fill_Lang(0,'EN','idle','reading folder structure','creating folder structure','copying files','','copy complete');
  Fill_Lang(1,'FR','Inactif','structure de dossier de lecture','création d’une structure de dossier','copie de fichiers','','copier complet');
  Fill_Lang(2,'DE','Im leerlauf','Leseordnerstruktur','Erstellen der Ordnerstruktur','Kopieren von Dateien','','Kopie vollständig');
  Fill_Lang(3,'RU','Простоя','структура чтения папок','создание структуры папок','копирование файлов','','копировать полный');

  self._Language:='EN';
  self._progress:=0;

  self._CopyDirProcessedFromFile:='';
  self._TotalByteProcessed:=0;
  self._TotalBytesToCopy:=0;
  self._fs := TFileSearcher.Create;
  self._fs.OnDirectoryFound := @self._DirFound;
  self._fs.OnFileFound := @self._FileFound;
  self._log := TStringList.Create;

  self._copied := false;
  self._dirsCreated := false;
  self._enumerated := false;

  self._preserveFileDates := true;

  {$IFDEF Unix} self._preserveAttributes := false;
    {$ELSE} self._preserveAttributes := true {$ENDIF};
  self._copyReadOnly := true;
  self._copyHidden := true;
  self._copySystem := true;
  self._copyArchive := true;
  self._copyAllFiles:=True;
  self._copyOnlyIfExists:=False;
  self._copyIfNewer:=True;
  self._copyIfSizeChanged:=False;
  self._copycomparefiles:=false;
  self._ActutalBytesCopied:=0;
  self._AppProcessMessagesCounterInterval:=20;

  self._abortcopy:=true;
  self._comparebuffersize:=(16*1024)-1; // default 16k buffersize
  self._copybuffersize:=(16*1024)-1;

  self._printToTerminal := false;


  self._AddToLog('Initializing...');

  self._dirSource := aSourceDir;
  if DirectoryExistsUTF8(self._dirSource) then
  begin
    self._AddToLog('Source: ' + self._dirSource + ' - exists!');
  end else
  begin
    if ForceDirectoriesUTF8(self._dirSource) then
    begin
      self._AddToLog('Source: ' + self._dirSource + ' - doesn''t exist, ' +
        'created!');
    end else
    begin
      self._AddToLog('Source: ' + self._dirSource + ' - doesn''t exist, ' +
        'creating failed!');
    end;
  end;

  self._dirTarget := aTargetDir;
  if DirectoryExistsUTF8(self._dirTarget) then
  begin
    self._AddToLog('Target: ' + self._dirTarget + ' - exists!');
  end else
  begin
    if ForceDirectoriesUTF8(self._dirTarget) then
    begin
      self._AddToLog('Target: ' + self._dirTarget + ' - doesn''t exist, ' +
        'created!');
    end else
    begin
      self._AddToLog('Target: ' + self._dirTarget + ' - doesn''t exist, ' +
        'creating failed!');
    end;
  end;

  self._AddToLog('');
end;

destructor TCopyDir.Destroy;
begin
  self._fs.Free;
  self._log.Free;
  inherited;
end;

procedure TCopyDir.Start;
var
  __startTime: QWord;
  i: Integer;
begin
  self._TotalBytesToCopy:=0;
  if not self._enumerated then self.Enumerate;

  __startTime := GetTickCount64;
  self._AddToLog('');
  self._AddToLog('COPYING STARTED');
  self._AddToLog('===========================================================');
  if self._preserveFileDates and self._preserveAttributes then
  begin
    self._AddToLog('(Preserving dates and attributes)');
  end;
  if self._preserveFileDates and not self._preserveAttributes then
  begin
    self._AddToLog('(Preserving dates)');
  end;
  if not self._preserveFileDates and self._preserveAttributes then
  begin
    self._AddToLog('(Preserving attributes)');
  end;
  self._AddToLog('');


  self._AddToLog('1. Creating directory structure...');
  self._AddToLog('----------------------------------');
  self._AddToLog('');

  self._CreateDirs;
  self._AddToLog(IntToStr(Length(self._creatingDone)) + ' directories ' +
    'successfully created:');
  for i := Low(self._creatingDone) to High(self._creatingDone) do
  begin
    self._AddToLog(self._creatingDone[i]);
  end;
  self._AddToLog('');
  self._AddToLog(IntToStr(Length(self._creatingFailed)) + ' directories ' +
    'failed creating:');
  for i := Low(self._creatingFailed) to High(self._creatingFailed)do
  begin
    self._AddToLog(self._creatingFailed[i]);
  end;
  self._AddToLog('');


  self._AddToLog('2. Copying files...');
  self._AddToLog('-------------------');
  self._AddToLog('');

  if self._usebufferedcopy then
  begin
    setlength(self._CopyBuffer,self._copybuffersize);
  end;

  if self._copycomparefiles then
  begin
    setlength(self._CompBuffer1,self._comparebuffersize);
    setlength(self._CompBuffer2,self._comparebuffersize);
  end;



  self._CopyFiles;

  self._AddToLog(IntToStr(Length(self._copyingDone)) + ' files ' +
    'successfully copied:');
  for i := Low(self._copyingDone) to High(self._copyingDone) do
  begin
    self._AddToLog(self._copyingDone[i]);
  end;
  self._AddToLog('');
  self._AddToLog(IntToStr(Length(self._copyingFailed)) + ' files failed ' +
    'copying:');
  for i := Low(self._copyingFailed) to High(self._copyingFailed) do
  begin
    self._AddToLog(self._copyingFailed[i]);
  end;

  self._AddToLog('');
  self._AddToLog('Overall ' + IntToStr(Length(self._copyingDone)) + ' files ' +
    'in ' + IntToStr(Length(self._creatingDone)) + ' directories copied!');
  self._AddToLog('===========================================================');
  self._AddToLog('COPYING DONE (in ' + IntToStr(GetTickCount64 - __startTime) +
    ' ms)');
  self._AddToLog('');
end;

procedure TCopyDir.Enumerate;
begin
  self._AddToLog('Enumerating source directory...');
  if self._copyReadOnly then self._AddToLog('Including files with "ReadOnly" ' +
    'attribute');
  if self._copyHidden then self._AddToLog('Including files with "Hidden" ' +
    'attribute');
  if self._copySystem then self._AddToLog('Including files with "System" ' +
    'attribute');
  if self._copyArchive then self._AddToLog('Including files with "Archive" ' +
    'attribute');

  self._fs.Search(self._dirSource, '', true);

  self._AddToLog('Found ' + IntToStr(Length(self._files)) + ' files to copy ' +
    'in ' + IntToStr(Length(self._directories)) + ' directories.');
  self._enumerated := true;
  self._AddToLog('');
end;

function TCopyDir.GetLog: TStringList;
begin
  { //Alexey
  self._log.Append('');
  self._log.Append('Greetings,');
  self._log.Append('bastla (@ Lazarus Forum)');
  }
  Result := self._log;
end;


end.

