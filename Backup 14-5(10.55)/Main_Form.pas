unit Main_Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, Buttons, ShellApi, Variants, ScktComp, Menus, ComCtrls, WinSock;

const
  TH_MESSAGE = WM_USER + 1; //Thread message
  TH_FINISHED = 1; //Thread SubMessage : End of thread  WParam = ThreadID
  TH_NEWFILE = 2; //Thread Submessage : Started new file
  TH_FOUND = 3; //Thread Submessage : Found searchString in File
  TH_ERROR = 4; //Thread SubMessage : Error WParam = Error
  MAX_THREADS = 4; //Max number of threads
  SERVER_NAME = 'server';

  ERROR_COULD_NOT_OPEN_FILE = 1; //Error file could not be opened

type
  //Records for communication between Threads and MainForm, the pointer for the

  //Record for found items, will occur when LParam = TH_FOUND, WParam will be PFoundRecord
  PFoundRecord = ^TFoundRecord;
  TFoundRecord = record
    ThreadID: Cardinal;
    Filename: string;
    Project: string;
  end;

  //Record for new files, will occur when LParam = TH_NEWFILE, WParam will be PNewFileRecord
  PNewFileRecord = ^TNewFileRecord;
  TNewFileRecord = record
    ThreadID: Cardinal;
    Filename: string;
    Project: string;
  end;

  //Record to hold the information from one thread
  TThreadInfo = record
    Active: Boolean;
    ThreadHandle: integer;
    ThreadId: Cardinal;
    CurrentFile: string;
  end;

  //The Main form of the application
  TMainForm = class(TForm)
    OpenDialog: TOpenDialog;
    StringGrid: TStringGrid;
    Label2: TLabel;
    Label3: TLabel;
    append_btn: TButton;
    Button2: TButton;
    mvup_btn: TButton;
    mvdwn_btn: TButton;
    add_btn: TButton;
    rmv_btn: TButton;
    inlist_sg: TStringGrid;
    outlist_sg: TStringGrid;
    SpeedButton2: TSpeedButton;
    Button3: TButton;
    Label1: TLabel;
    Listen: TCheckBox;
    ip_lbl: TLabel;
    MemoChat: TMemo;
    ServerSocket: TServerSocket;
    Label5: TLabel;
    procedure btSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure append_btnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure InsertRow(Selected_SG: TObject; i: Integer; s: TStrings);
    procedure add_btnClick(Sender: TObject);
    procedure rmv_btnClick(Sender: TObject);
    procedure mvup_btnClick(Sender: TObject);
    procedure mvdwn_btnClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure StringGridDblClick(Sender: TObject);
    procedure GoClick(Sender: TObject);
    procedure PauseResumeClick(Sender: TObject);
    procedure inlist_sgDblClick(Sender: TObject);
    procedure outlist_sgDblClick(Sender: TObject);
    procedure ServerSocketClientDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocketClientRead(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocketClientConnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ListenClick(Sender: TObject);
    function SendToAllClients(s: string): boolean;


  private
    ThreadInfo: array[0..4] of TThreadInfo; //Holds the information of the threads
    procedure ThreadMessage(var Message: TMessage); message TH_MESSAGE;
    function ThreadIDToIndex(ThreadID: Cardinal): integer; //MessageHandler
  public

  end;
function GetIPAddress: Integer;
function GetIPAddressAsString: string;


var
  MainForm: TMainForm;

implementation

var CriticalSection: TRTLCriticalSection; //Critical section protects the filelist
  FileList: array[1..2] of TStrings; //List of filenames to be searched
  InList_Header: TStrings;
  SearchString: string; //String to be searched in every file
  PauseThread: boolean;
  FileListAccess: boolean;
  LocalHost: string;

{$R *.DFM}

//The threadFunction, this is actually the thread

function FindInFile(data: Pointer): Integer;
var FileStream: TFileStream;
  Ch: char;
  Current, Len: Integer;
  FoundRecord: PFoundRecord;
  NewFileRecord: PNewFileRecord;
  Filename: string;
  Project: string;
  Search: string;
  FilesDone: Boolean;
//========================================
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  ExecuteFile: string;
  ParamString: string;
  StartInString: string;
//========================================
begin
  Result := 0;
  FilesDone := False;
  while not FilesDone do //while there are still files
  begin
    Current := 1;
    EnterCriticalSection(CriticalSection); //Try to catch the critical section
    Search := SearchString; //Access the shared variables
    //Are there still files available
    if FileList[2].Count = 0 then
    begin
      //Leave the critical section, when there are no files left
      LeaveCriticalSection(CriticalSection);
      //Leave the while loop
      break;
    end
    else
    begin
      while not (FileListAccess) do
      begin
          //nothing
      end;
      FileListAccess := false;
      //Read the filename
      Filename := FileList[2].Strings[0];
      Project := FileList[1].Strings[0];
      ExecuteFile := 'cmd.exe'; //Don't change
      ParamString := '/C Title ' + Project + ' & cd "' + ExtractFilePath(Filename) + '" & "' + ExtractFileName(Filename) + '" | tee "' + Project + '.txt"'; //title, filepath, filename, echofile

      //Delete the file from the list
      FileList[1].Delete(0);
      FileList[2].Delete(0);

      //Leave the critical section
      LeaveCriticalSection(CriticalSection);
      FileListAccess := true;
      //Inform MainForm of New File
      New(NewFileRecord);
      NewFileRecord^.Filename := Filename;
      NewFileRecord^.ThreadID := GetCurrentThreadID;
      NewFileRecord^.Project := Project;
      PostMessage(MainForm.Handle, TH_MESSAGE, TH_NEWFILE, Integer(NewFileRecord));

      FillChar(SEInfo, SizeOf(SEInfo), 0);
      SEInfo.cbSize := SizeOf(TShellExecuteInfo);
      with SEInfo do begin
        fMask := SEE_MASK_NOCLOSEPROCESS;
        Wnd := Application.Handle;
        lpFile := PChar(ExecuteFile);
 {
 ParamString can contain the
 application parameters.
 }
        lpParameters := PChar(ParamString);
 {
 StartInString specifies the
 name of the working directory.
 If ommited, the current directory is used.
 }
 // lpDirectory := PChar(StartInString) ;
        nShow := SW_SHOWNORMAL;
      end;
      if ShellExecuteEx(@SEInfo) then begin
        repeat
          Application.ProcessMessages;
          GetExitCodeProcess(SEInfo.hProcess, ExitCode);
        until (ExitCode <> STILL_ACTIVE) or
          Application.Terminated;
          //All done with file, move on to next
        New(FoundRecord);
        FoundRecord^.Filename := FileName;
        FoundRecord^.ThreadID := GetCurrentthreadID;
        FoundRecord^.Project := Project;
        PostMessage(MainForm.Handle, TH_MESSAGE, TH_FOUND, Integer(FoundRecord));
      end
      else PostMessage(MainForm.Handle, TH_MESSAGE, TH_ERROR, ERROR_COULD_NOT_OPEN_FILE);

    end;
  end;
  //All done inform MainForm of ending
  PostMessage(MainForm.Handle, TH_MESSAGE, TH_FINISHED, GetCurrentThreadID);
//============================================================================================================================================
end;


function TMainForm.ThreadIDToIndex(ThreadID: Cardinal): integer;
var Counter: integer;
begin
  Result := -1;
  for Counter := 0 to MAX_THREADS - 1 do //Search for the index of the thread
    if ThreadInfo[Counter].ThreadID = ThreadID then
    begin
      Result := Counter;
      break;
    end;
end;


//functie voor het opvangen van de threadmessages

procedure TMainForm.ThreadMessage(var Message: TMessage);
var FoundRecord: PFoundRecord;
  NewFileRecord: PNewFileRecord;
  ThreadIndex: integer;
  Counter: integer;
  Ended: boolean;
begin
  case Message.WParam of

    TH_FINISHED:
      begin
        ThreadIndex := ThreadIDToIndex(Message.LParam);
        if ThreadIndex = -1 then Exit; //Invalid threadID should never appear
        CloseHandle(ThreadInfo[ThreadIndex].ThreadHandle); //Free the thread memoryspace
        StringGrid.Cells[3, ThreadIndex + 1] := 'False'; //Update the stringgrid
        Threadinfo[ThreadIndex].Active := False; //Update the ThreadInfo array
        Ended := True;
        for counter := 0 to MAX_THREADS - 1 do
          if ThreadInfo[ThreadIndex].Active = True then
          begin
            Ended := False;
            break;
          end;
        Button2.Enabled := false;
        Button3.Enabled := false;
        Button2.Caption := 'Start All Runs';
        append_btn.Enabled := true;
        add_btn.Enabled := true;
        rmv_btn.Enabled := true;
        mvup_btn.Enabled := true;
        mvdwn_btn.Enabled := true;
      end;

    TH_NEWFILE:
      begin
        NewFileRecord := PNewFileRecord(Message.LParam);
        ThreadIndex := ThreadIDToIndex(NewFileRecord^.ThreadID);
        if ThreadIndex = -1 then Exit; //Invalid threadID should never appear
        StringGrid.Cells[1, ThreadIndex + 1] := NewFileRecord^.Project;
        StringGrid.Cells[2, ThreadIndex + 1] := ExtractFileName(NewFileRecord^.Filename) + ' (' + ExtractShortPathName(ExtractFilePath(NewFileRecord^.Filename)) + ')'; //Update StringGrid
        ThreadInfo[ThreadIndex].CurrentFile := NewFileRecord^.Filename; //Update the ThreadInfo
        while not (FileListAccess) do
        begin
          //Wait
        end;
        FileListAccess := false;
        if FileList[1].Count > 0 then
        begin
          inlist_sg.RowCount := FileList[1].Count;
          inlist_sg.Cols[0] := FileList[1];
          inlist_sg.Cols[1] := FileList[2];
          InsertRow(inlist_sg, 0, Inlist_header);
          inlist_sg.FixedRows := 1;
        end
        else
        begin
          inlist_sg.RowCount := 2;
          inlist_sg.Rows[1].Clear;
          inlist_sg.Rows[0] := Inlist_header;
          inlist_sg.FixedRows := 1;
        end;
        FileListAccess := true;
        Dispose(NewFileRecord); //All information is used now free the pointer
      end;

    TH_FOUND:
      begin
        FoundRecord := PFoundRecord(Message.LParam);
        ThreadIndex := ThreadIDToIndex(FoundRecord^.ThreadID);
        if ThreadIndex = -1 then Exit; //Invalid threadID should never appear
        if outlist_sg.RowCount = 2 then
        begin
          if outlist_sg.Cells[3, 1] <> '' then
            outlist_sg.RowCount := outlist_sg.RowCount + 1;
        end
        else
          outlist_sg.RowCount := outlist_sg.RowCount + 1;
        outlist_sg.Cells[0, outlist_sg.RowCount - 1] := FoundRecord^.Project;
        outlist_sg.Cells[1, outlist_sg.RowCount - 1] := FoundRecord^.Filename;
        outlist_sg.Cells[2, outlist_sg.RowCount - 1] := ExtractFilePath(FoundRecord^.Filename) + FoundRecord^.Project + '.txt';
        outlist_sg.Cells[3, outlist_sg.RowCount - 1] := 'Completed';

        StringGrid.Cells[1, ThreadIndex + 1] := '';
        StringGrid.Cells[2, ThreadIndex + 1] := ''; //Update StringGrid

        //if job came from over network
        if Listen.Checked and (pos('(From: ', FoundRecord^.Project) <> 0) then
          SendToAllClients(LocalHost + ': Project:' + FoundRecord^.Project + ', File:' + FoundRecord^.Filename + ' has successfully run.');


        Dispose(FoundRecord); //All information is used now free the pointer
      end;

    TH_ERROR:
      begin
        ThreadIndex := ThreadIDToIndex(Message.LParam);
        if ThreadIndex = -1 then Exit; //Invalid threadID should never appear
        if outlist_sg.RowCount = 2 then
        begin
          if outlist_sg.Cells[3, 1] <> '' then
            outlist_sg.RowCount := outlist_sg.RowCount + 1;
        end
        else
          outlist_sg.RowCount := outlist_sg.RowCount + 1;
        outlist_sg.Cells[0, outlist_sg.RowCount - 1] := 'Project Name Here';
        outlist_sg.Cells[1, outlist_sg.RowCount - 1] := FoundRecord^.Filename;
        outlist_sg.Cells[3, outlist_sg.RowCount - 1] := 'Error';

         //if job came from over network
        if Listen.Checked and (pos('(From: ', FoundRecord^.Project) <> 0) then
          SendToAllClients(LocalHost + ': Project:' + FoundRecord^.Project + ', File:' + FoundRecord^.Filename + ' has run into an error, please check the echo file.');

      end;
  end;
end;


procedure TMainForm.btSearchClick(Sender: TObject);
var Counter: Integer;
begin
  inlist_sg.RowCount := 2;
  outlist_sg.RowCount := 2;
  inlist_sg.Rows[1].Clear;
  outlist_sg.Rows[1].Clear;
  PauseThread := false;

  if Opendialog.Execute then
  begin
    FileList[1].Clear;
    FileList[2].Clear;
    if OpenDialog.Files.Count > 0 then
    begin
      for Counter := 0 to OpenDialog.Files.Count - 1 do
      begin
        FileList[1].Add(Inttostr(Counter));
        FileList[2].Add(OpenDialog.Files[Counter]);
      end;
      //Start all Threads
      for Counter := 0 to MAX_THREADS - 1 do
      begin
        ThreadInfo[Counter].ThreadHandle :=
          BeginThread(nil,
          0,
          @FindInFile,
          nil,
          0,
          Threadinfo[Counter].ThreadId);
        if ThreadInfo[Counter].ThreadHandle <> 0 then //Everything ok
        begin
          //StringGrid.Cells[1, Counter + 1] := IntToStr(ThreadInfo[Counter].ThreadID);
          StringGrid.Cells[3, Counter + 1] := 'True';
          ThreadInfo[Counter].Active := True;
        end;
      end;

    end;
  end;
end;


procedure TMainForm.GoClick(Sender: TObject);
var Counter: Integer;
  delta: Integer; //Variable to compensate for when the number of files is less than the number of available threads
begin
  Button2.Caption := 'Pause Threads';
  Button3.Enabled := true;
  append_btn.Enabled := false;
  add_btn.Enabled := false;
  rmv_btn.Enabled := false;
  mvup_btn.Enabled := false;
  mvdwn_btn.Enabled := false;
  inlist_sg.RowCount := 2;
  outlist_sg.RowCount := 2;
  inlist_sg.Rows[1].Clear;
  outlist_sg.Rows[1].Clear;
  PauseThread := false;

  if FileList[1].Count < MAX_THREADS then delta := MAX_THREADS - FileList[1].Count else delta := 0;

  //Start all Threads
  for Counter := 0 to MAX_THREADS - delta - 1 do
  begin
    ThreadInfo[Counter].ThreadHandle :=
      BeginThread(nil,
      0,
      @FindInFile,
      nil,
      0,
      Threadinfo[Counter].ThreadId);
    if ThreadInfo[Counter].ThreadHandle <> 0 then //Everything ok
    begin
      StringGrid.Cells[3, Counter + 1] := 'True';
      ThreadInfo[Counter].Active := True;
    end;
  end;
end;

procedure TMainForm.PauseResumeClick(Sender: TObject);
var Counter: Integer;
  FileCount: Integer;
  AvailTh: array[0..MAX_THREADS - 1] of Integer;
  AvailThCount: Integer;
begin


  if PauseThread = false then
  begin
    PauseThread := true;
 //Checkthreads and suspend
    for Counter := 0 to MAX_THREADS - 1 do
    begin
      if ThreadInfo[Counter].Active then
      begin
        SuspendThread(ThreadInfo[Counter].ThreadHandle);
        StringGrid.Cells[3, Counter + 1] := 'Suspended';
      end;
    end;
    Button2.Caption := 'Resume Threads';
    Button3.Enabled := false;
    append_btn.Enabled := true;
    add_btn.Enabled := true;
    rmv_btn.Enabled := true;
    mvup_btn.Enabled := true;
    mvdwn_btn.Enabled := true;
  end
  else
  begin
    PauseThread := false;
    FileCount := FileList[1].Count;
    AvailThCount := 0;
    for Counter := 0 to MAX_THREADS - 1 do
    begin
      if not (ThreadInfo[Counter].Active) then
      begin
        AvailTh[AvailThCount] := Counter;
        AvailThCount := AvailThCount + 1;
      end
    end;

    if (FileCount > 0) and (AvailThCount > 0) then
    begin
     //======================================================================
      if AvailThCount > FileCount then AvailThCount := FileCount;
      //Start all Threads
      for Counter := 0 to AvailThCount - 1 do
      begin
        ThreadInfo[AvailTh[Counter]].ThreadHandle :=
          BeginThread(nil,
          0,
          @FindInFile,
          nil,
          0,
          Threadinfo[AvailTh[Counter]].ThreadId);
        if ThreadInfo[AvailTh[Counter]].ThreadHandle <> 0 then //Everything ok
        begin
          //StringGrid.Cells[1, AvailTh[Counter] + 1] := IntToStr(ThreadInfo[AvailTh[Counter]].ThreadID);
          StringGrid.Cells[3, AvailTh[Counter] + 1] := 'True';
          ThreadInfo[AvailTh[Counter]].Active := True;
        end;
      end;
     //=======================================================================
    end;
 //Checkthreads and resume old threads
    for Counter := 0 to MAX_THREADS - 1 do
    begin
      if ThreadInfo[Counter].Active then
      begin
        ResumeThread(ThreadInfo[Counter].ThreadHandle);
        StringGrid.Cells[3, Counter + 1] := 'True';
      end;
    end;
    Button2.Caption := 'Pause Threads';
    Button3.Enabled := true;
    append_btn.Enabled := false;
    add_btn.Enabled := false;
    rmv_btn.Enabled := false;
    mvup_btn.Enabled := false;
    mvdwn_btn.Enabled := false;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var x: byte;
begin
  InitializeCriticalSection(CriticalSection);
  FileList[1] := TStringList.Create;
  FileList[2] := TStringList.Create;
  Inlist_header := TStringList.Create;
  StringGrid.Cells[1, 0] := 'Title';
  StringGrid.Cells[2, 0] := 'Handling File';
  StringGrid.Cells[3, 0] := 'Active';
  StringGrid.RowCount := MAX_THREADS + 1;
  for x := 1 to MAX_THREADS do
  begin
    StringGrid.Cells[0, x] := 'Thread ' + inttostr(x);
    StringGrid.Cells[3, x] := 'False'
  end;
  FileListAccess := true;
  inlist_sg.Cells[0, 0] := 'Title';
  inlist_sg.Cells[1, 0] := 'File Path';
  outlist_sg.Cells[0, 0] := 'Title';
  outlist_sg.Cells[1, 0] := 'File Path';
  outlist_sg.Cells[2, 0] := 'Echo File';
  outlist_sg.Cells[3, 0] := 'Status';
  Inlist_header.Add('Title');
  Inlist_header.Add('File Path');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FileList[1].Free;
  FileList[2].Free;
  DeleteCriticalSection(CriticalSection);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  if Button2.Caption = 'Start All Runs' then
    GoClick(Sender)
  else
    PauseResumeClick(Sender);

end;

procedure TMainForm.InsertRow(Selected_SG: TObject; i: Integer; s: TStrings);
var r: Integer;
begin
  with (Selected_SG as TStringGrid) do
  begin
    RowCount := RowCount + 1;
    for r := RowCount downto i + 1 do begin
      Rows[r] := Rows[r - 1];
    end;
    Rows[i] := s;
  end;
end;

procedure TMainForm.append_btnClick(Sender: TObject);
var Counter: Integer;
  DefaultTitle: string;
  Response: boolean;
begin

  if Opendialog.Execute then
  begin
    if OpenDialog.Files.Count > 0 then
    begin
      DefaultTitle := 'Project';
      Response := InputQuery('Project Title', 'Please enter a project or analysis title', DefaultTitle);
      if not (Response) then exit;
      for Counter := 0 to OpenDialog.Files.Count - 1 do
      begin
        if OpenDialog.Files.Count > 1 then
          FileList[1].Add(DefaultTitle + ' #' + Inttostr(Counter + 1))
        else
          FileList[1].Add(DefaultTitle);
        FileList[2].Add(OpenDialog.Files[Counter]);
      end;
    end;
    inlist_sg.RowCount := FileList[1].Count;
    inlist_sg.Cols[0] := FileList[1];
    inlist_sg.Cols[1] := FileList[2];
    InsertRow(inlist_sg, 0, Inlist_header);
    inlist_sg.FixedRows := 1;
    Button2.Enabled := true;
  end;
end;


procedure TMainForm.add_btnClick(Sender: TObject);
var Counter: Integer;
  DefaultTitle: string;
  Response: boolean;
begin

  if Opendialog.Execute then
  begin
    if OpenDialog.Files.Count > 0 then
    begin



      DefaultTitle := 'Project';
      Response := InputQuery('Project Title', 'Please enter a project or analysis title', DefaultTitle);
      if not (Response) then exit;



      for Counter := OpenDialog.Files.Count - 1 downto 0 do
      begin
        if OpenDialog.Files.Count > 1 then
          FileList[1].Insert(inlist_sg.Row - 1, DefaultTitle + ' #' + Inttostr(Counter + 1))
        else
          FileList[1].Insert(inlist_sg.Row - 1, DefaultTitle);
        FileList[2].Insert(inlist_sg.Row - 1, OpenDialog.Files[Counter]);
      end;
    end;


    inlist_sg.RowCount := FileList[1].Count;
    inlist_sg.Cols[0] := FileList[1];
    inlist_sg.Cols[1] := FileList[2];
    InsertRow(inlist_sg, 0, Inlist_header);
    inlist_sg.FixedRows := 1;

    Button2.Enabled := true;
  end;
end;

procedure TMainForm.rmv_btnClick(Sender: TObject);
var LastRowsSelected: boolean;
begin
  if FileList[1].Count = 0 then exit;
  LastRowsSelected := (inlist_sg.Row = inlist_sg.RowCount - 1) or (inlist_sg.Row = inlist_sg.RowCount - 2);
  FileList[1].Delete(inlist_sg.Row - 1);
  FileList[2].Delete(inlist_sg.Row - 1);
  if FileList[1].Count > 0 then
  begin
    inlist_sg.RowCount := FileList[1].Count;
    inlist_sg.Cols[0] := FileList[1];
    inlist_sg.Cols[1] := FileList[2];
    InsertRow(inlist_sg, 0, Inlist_header);
    inlist_sg.FixedRows := 1;
  end
  else
  begin
    inlist_sg.RowCount := 2;
    inlist_sg.Rows[1].Clear;
    inlist_sg.Rows[0] := Inlist_header;
    inlist_sg.FixedRows := 1;
  end;
  if LastRowsSelected and (inlist_sg.RowCount > 2) then
    inlist_sg.Row := inlist_sg.Row + 1;

  if (inlist_sg.RowCount = 2) and (FileList[1].count = 0) then
    Button2.Enabled := false;

end;

procedure TMainForm.mvup_btnClick(Sender: TObject);
var LastRowsSelected: boolean;
begin
  if inlist_sg.Row = 1 then exit;
  LastRowsSelected := (inlist_sg.Row = inlist_sg.RowCount - 1);
  FileList[1].Exchange(inlist_sg.Row - 1, inlist_sg.Row - 2);
  FileList[2].Exchange(inlist_sg.Row - 1, inlist_sg.Row - 2);
  inlist_sg.RowCount := FileList[1].Count;
  inlist_sg.Cols[0] := FileList[1];
  inlist_sg.Cols[1] := FileList[2];
  InsertRow(inlist_sg, 0, Inlist_header);
  inlist_sg.FixedRows := 1;
  if not (LastRowsSelected) then
    inlist_sg.Row := inlist_sg.row - 1;
end;

procedure TMainForm.mvdwn_btnClick(Sender: TObject);
begin
  if inlist_sg.Row = inlist_sg.RowCount - 1 then exit;
  FileList[1].Exchange(inlist_sg.Row - 1, inlist_sg.Row);
  FileList[2].Exchange(inlist_sg.Row - 1, inlist_sg.Row);
  inlist_sg.RowCount := FileList[1].Count;
  inlist_sg.Cols[0] := FileList[1];
  inlist_sg.Cols[1] := FileList[2];
  InsertRow(inlist_sg, 0, Inlist_header);
  inlist_sg.FixedRows := 1;
  inlist_sg.Row := inlist_sg.Row + 1;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var Counter: Integer;
begin
//Stop all Threads
  for Counter := 0 to MAX_THREADS - 1 do
  begin
    TerminateThread(ThreadInfo[Counter].ThreadHandle, 0);
    StringGrid.Cells[3, Counter + 1] := 'Terminated by user';
    ThreadInfo[Counter].Active := False;
  end;
  Button3.Enabled := false;
  Button2.Caption := 'Start All Runs';
  append_btn.Enabled := true;
  add_btn.Enabled := true;
  rmv_btn.Enabled := true;
  mvup_btn.Enabled := true;
  mvdwn_btn.Enabled := true;

  if FileList[1].Count > 0 then
    Button2.Enabled := true;


end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
var i: integer;
begin
  for i := 1 to outlist_sg.RowCount - 1
    do
  begin
    outlist_sg.Rows[i].Clear;
    outlist_sg.RowCount := 2;
  end;
end;

function FindWindowExtd(partialTitle: string): HWND;
var
  hWndTemp: hWnd;
  iLenText: Integer;
  cTitletemp: array[0..254] of Char;
  sTitleTemp: string;
begin
  hWndTemp := FindWindow(nil, nil);
  while hWndTemp <> 0 do begin
    iLenText := GetWindowText(hWndTemp, cTitletemp, 255);
    sTitleTemp := cTitletemp;
    sTitleTemp := UpperCase(copy(sTitleTemp, 1, iLenText));
    partialTitle := UpperCase(partialTitle);
    if pos(partialTitle, sTitleTemp) <> 0 then
      Break;
    hWndTemp := GetWindow(hWndTemp, GW_HWNDNEXT);
  end;
  result := hWndTemp;
end;



procedure TMainForm.StringGridDblClick(Sender: TObject);
var wnd: DWORD;
begin
  if Stringgrid.Cells[3, Stringgrid.Row] = 'True' then
  begin
    wnd := FindWindowExtd(Stringgrid.Cells[1, Stringgrid.Row]);
    if wnd <> 0 then
    begin
      ShowWindow(wnd, 1);
      SetForegroundWindow(wnd);
    end;
  end;

end;




procedure TMainForm.inlist_sgDblClick(Sender: TObject);
begin
  if FileList[1].Count > 0 then
  begin
    ShellExecute(Application.Handle, 'open', 'explorer.exe',
      PChar('/select,"' + inlist_sg.Cells[1, inlist_sg.Row] + '"'), nil, SW_NORMAL);
  end;
end;

procedure TMainForm.outlist_sgDblClick(Sender: TObject);
begin
  if (outlist_sg.Cells[3, 2] <> '') and ((outlist_sg.Col = 1) or (outlist_sg.Col = 2)) then
  begin
    ShellExecute(Application.Handle, 'open', 'explorer.exe',
      PChar('/select,"' + outlist_sg.Cells[outlist_sg.Col, outlist_sg.Row] + '"'), nil, SW_NORMAL);
  end;
end;

//=========================NETWORK QUEUING SYSTEM===============================================================================================================================

function TMainForm.SendToAllClients(s: string): boolean;
var
  i: integer;
begin
  if ServerSocket.Socket.ActiveConnections = 0 then
  begin
    ShowMessage('There are no connected clients.');
    Exit;
  end;
  for i := 0 to (ServerSocket.Socket.ActiveConnections - 1) do
  begin
    ServerSocket.Socket.Connections[i].SendText(s);
  end;
end;

procedure TMainForm.ListenClick(Sender: TObject);
var
  StrPort: string;
  IntPort: Integer;
begin

  if Listen.Checked then
  begin
    InputQuery('What port?', 'Please enter the port you want to listen. Make sure it''s a valid integer number between 1 and 65535.', StrPort);
    if StrPort = '' then Exit;
    if length(StrPort) > 6 then Exit;
    try
      IntPort := StrToInt(StrPort);
    except
      begin
        MessageBox(handle, 'Stupid you. What did I just tell you? You didn''t give me a valid port!!!', 'Duh!', Mb_ok + Mb_IconError);
        Exit;
      end;
    end;
    if (IntPort < 0) or (IntPort > 65535) then
    begin
      MessageBox(handle, 'Stupid you. What did I just tell you? You didn''t give me a valid port!!!', 'Duh!', Mb_ok + Mb_IconError);
      Exit;
    end;
    //Listen.Checked := True;
    ServerSocket.Port := IntPort;
    ServerSocket.Active := True;
    ip_lbl.caption := 'IP Address: ' + GetIPAddressAsString + ':' + inttostr(IntPort);
  end
  else
  begin //if i am alredy connected
    //Listen.Checked := False;
    ServerSocket.Active := False;

  end;

end;



procedure TMainForm.ServerSocketClientConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  SendToAllClients('<' + Socket.RemoteHost + ' just connected.>');
  MemoChat.Lines.Add('<' + Socket.RemoteHost + ' just connected.>');
  LocalHost := Socket.LocalHost;
end;

procedure TMainForm.ServerSocketClientRead(Sender: TObject;
  Socket: TCustomWinSocket);
var
  received: string;
  projpos: integer;
  filepos: integer;
  filename: string;
  project: string;
  autostart: boolean;
  startclean: boolean;
begin
  Button2.Enabled := false;
  Button3.Enabled := false;
  received := Socket.ReceiveText;
  SendToAllClients(received);
  MemoChat.Lines.Add(received);

  projpos := pos('proj=', received);
  filepos := pos('file=', received);
  autostart := (pos('/autostart', received) <> 0);

  if (projpos <> 0) and (filepos <> 0) then
  begin
    project := copy(received, projpos + 5, filepos - (projpos + 5)) + ' (From: ' + Socket.RemoteHost + ')';
    filename := copy(received, filepos + 5, Length(received) - (filepos + 5) + 1);
    //queue the filename
    if Button2.Caption = 'Start All Runs' then
    begin
      startclean := true;
      FileList[1].Add(project);
      FileList[2].Add(filename);
      inlist_sg.RowCount := FileList[1].Count;
      inlist_sg.Cols[0] := FileList[1];
      inlist_sg.Cols[1] := FileList[2];
      InsertRow(inlist_sg, 0, Inlist_header);
      inlist_sg.FixedRows := 1;
      Button2.Enabled := true;
      if autostart then Button2.Click;
    end
    else
    begin
      startclean := false;
      PauseResumeClick(Sender); //pause then append to list
      FileList[1].Add(project);
      FileList[2].Add(filename);
      inlist_sg.RowCount := FileList[1].Count;
      inlist_sg.Cols[0] := FileList[1];
      inlist_sg.Cols[1] := FileList[2];
      InsertRow(inlist_sg, 0, Inlist_header);
      inlist_sg.FixedRows := 1;
      Button2.Enabled := true;
      PauseResumeClick(Sender);
    end;

    //acknowledge the queuing
    if startclean then
      SendToAllClients(Socket.LocalHost + ': Project:' + project + ', File:' + filename + ' successfully running.')
    else
      SendToAllClients(Socket.LocalHost + ': Project:' + project + ', File:' + filename + ' successfully queued.');

  end
  else
    SendToAllClients(Socket.LocalHost + ': Error! Project:' + project + ', File:' + filename + ' failed to be queued.');

end;

procedure TMainForm.ServerSocketClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  SendToAllClients('<' + Socket.RemoteHost + ' just disconnected.>');
  MemoChat.Lines.Add('<' + Socket.RemoteHost + ' just disconnected.>');
end;

function GetIPAddress: Integer;
var
  Buffer: array[0..255] of Char;
  RemoteHost: PHostEnt;
begin
  Winsock.GetHostName(@Buffer, 255);
  RemoteHost := Winsock.GetHostByName(Buffer);
  if RemoteHost = nil then
    Result := winsock.htonl($07000001) { 127.0.0.1 }
  else
    Result := longint(pointer(RemoteHost^.h_addr_list^)^);
  Result := Winsock.ntohl(Result);
end; // function GetIPAddress: Integer;

function GetIPAddressAsString: string;
var
  tempAddress: Integer;
  Buffer: array[0..3] of Byte absolute tempAddress;
begin
  tempAddress := GetIPAddress;
  Result := Format('%d.%d.%d.%d', [Buffer[3], Buffer[2], Buffer[1], Buffer[0]]);
end;



//===========================================================================================================================================================================



end.

