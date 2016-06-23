unit Main_Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, Buttons;

const
  TH_MESSAGE = WM_USER + 1; //Thread message
  TH_FINISHED = 1; //Thread SubMessage : End of thread  WParam = ThreadID
  TH_NEWFILE = 2; //Thread Submessage : Started new file
  TH_FOUND = 3; //Thread Submessage : Found searchString in File
  TH_ERROR = 4; //Thread SubMessage : Error WParam = Error
  MAX_THREADS = 4; //Max number of threads

  ERROR_COULD_NOT_OPEN_FILE = 1; //Error file could not be opened

type
  //Records for communication between Threads and MainForm, the pointer for the

  //Record for found items, will occur when LParam = TH_FOUND, WParam will be PFoundRecord
  PFoundRecord = ^TFoundRecord;
  TFoundRecord = record
    ThreadID: Cardinal;
    Filename: string;
    Position: Cardinal;
  end;

  //Record for new files, will occur when LParam = TH_NEWFILE, WParam will be PNewFileRecord
  PNewFileRecord = ^TNewFileRecord;
  TNewFileRecord = record
    ThreadID: Cardinal;
    Filename: string;
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
    btSearch: TButton;
    OpenDialog: TOpenDialog;
    edSearch: TEdit;
    StringGrid: TStringGrid;
    Label1: TLabel;
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
    procedure btSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure append_btnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure InsertRow(Selected_SG: TObject; i: Integer; s: TStrings);
    procedure add_btnClick(Sender: TObject);
    procedure rmv_btnClick(Sender: TObject);
    procedure mvup_btnClick(Sender: TObject);
    procedure mvdwn_btnClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    ThreadInfo: array[0..4] of TThreadInfo; //Holds the information of the threads
    procedure ThreadMessage(var Message: TMessage); message TH_MESSAGE;
    function ThreadIDToIndex(ThreadID: Cardinal): integer; //MessageHandler
  public

  end;



var
  MainForm: TMainForm;

implementation

var CriticalSection: TRTLCriticalSection; //Critical section protects the filelist
  FileList: array[1..2] of TStrings; //List of filenames to be searched
  InList_Header: TStrings;
  SearchString: string; //String to be searched in every file
  PauseThread: boolean;
  FileListAccess: boolean;

{$R *.DFM}

//The threadFunction, this is actually the thread

function FindInFile(data: Pointer): Integer;
var FileStream: TFileStream;
  Ch: char;
  Current, Len: Integer;
  FoundRecord: PFoundRecord;
  NewFileRecord: PNewFileRecord;
  Filename: string;
  Search: string;
  FilesDone: Boolean;
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
      PostMessage(MainForm.Handle, TH_MESSAGE, TH_NEWFILE, Integer(NewFileRecord));
      Len := Length(Search);
      try
        FileStream := TFileStream.Create(Filename, fmOpenRead or fmShareExclusive);
      except
        PostMessage(MainForm.Handle, TH_MESSAGE, TH_ERROR, ERROR_COULD_NOT_OPEN_FILE);
        continue;
      end;
      //The search algorithm, pretty simple, the example is not about searching
      while FileStream.Read(Ch, 1) = 1 do
      begin
        if Ch = Search[Current] then
        begin
          Inc(Current);
          if Current > Len then
          begin
            //Found the search string, inform MainForm of our success
            New(FoundRecord);
            FoundRecord^.Filename := Filename;
            FoundRecord^.Position := FileStream.Position;
            FoundRecord^.ThreadID := GetCurrentThreadID;
            PostMessage(MainForm.Handle, TH_MESSAGE, TH_FOUND, Integer(FoundRecord));
          end;
        end
        else
        begin
          FileStream.Position := FileStream.Position - (Current - 1);
          Current := 1;
        end;
      end;
      FileStream.Free;
    end;
  end;
  //All done inform MainForm of ending
  PostMessage(MainForm.Handle, TH_MESSAGE, TH_FINISHED, GetCurrentThreadID);
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
        if Ended then btSearch.Enabled := True;
      end;

    TH_NEWFILE:
      begin
        NewFileRecord := PNewFileRecord(Message.LParam);
        ThreadIndex := ThreadIDToIndex(NewFileRecord^.ThreadID);
        if ThreadIndex = -1 then Exit; //Invalid threadID should never appear
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
        outlist_sg.Cells[0, outlist_sg.RowCount - 1] := 'Project Name Here';
        outlist_sg.Cells[1, outlist_sg.RowCount - 1] := FoundRecord^.Filename;
        outlist_sg.Cells[3, outlist_sg.RowCount - 1] := 'Completed';

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
  SearchString := EdSearch.Text;
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
          StringGrid.Cells[1, Counter + 1] := IntToStr(ThreadInfo[Counter].ThreadID);
          StringGrid.Cells[3, Counter + 1] := 'True';
          ThreadInfo[Counter].Active := True;
        end;
      end;
      btSearch.Enabled := False;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var x: byte;
begin
  InitializeCriticalSection(CriticalSection);
  FileList[1] := TStringList.Create;
  FileList[2] := TStringList.Create;
  Inlist_header := TStringList.Create;
  StringGrid.Cells[1, 0] := 'ThreadID';
  StringGrid.Cells[2, 0] := 'Handling File';
  StringGrid.Cells[3, 0] := 'Active';
  StringGrid.RowCount := MAX_THREADS + 1;
  for x := 1 to MAX_THREADS do
    StringGrid.Cells[0, x] := 'Thread ' + inttostr(x);
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

procedure TMainForm.edSearchChange(Sender: TObject);
begin
  btSearch.Enabled := Length((Sender as TEdit).Text) > 0;
end;



procedure TMainForm.Button2Click(Sender: TObject);
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
          StringGrid.Cells[1, AvailTh[Counter] + 1] := IntToStr(ThreadInfo[AvailTh[Counter]].ThreadID);
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
    append_btn.Enabled := false;
    add_btn.Enabled := false;
    rmv_btn.Enabled := false;
    mvup_btn.Enabled := false;
    mvdwn_btn.Enabled := false;
  end;
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
begin
  FileList[1].Append('3.1415');
  FileList[2].Append('C:\Users\gary.namestnik\Desktop\test\test.txt');
  inlist_sg.RowCount := FileList[1].Count;
  inlist_sg.Cols[0] := FileList[1];
  inlist_sg.Cols[1] := FileList[2];
  InsertRow(inlist_sg, 0, Inlist_header);
  inlist_sg.FixedRows := 1;
end;


procedure TMainForm.add_btnClick(Sender: TObject);
begin
  FileList[1].Insert(inlist_sg.Row - 1, 'hello');
  FileList[2].Insert(inlist_sg.Row - 1, 'C:\Users\gary.namestnik\Desktop\test\test.txt');
  inlist_sg.RowCount := FileList[1].Count;
  inlist_sg.Cols[0] := FileList[1];
  inlist_sg.Cols[1] := FileList[2];
  InsertRow(inlist_sg, 0, Inlist_header);
  inlist_sg.FixedRows := 1;
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

end.

