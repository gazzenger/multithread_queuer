//ThreadExample written for Borland Community as an example for multi-threading with
//the standard window functions and synchronizing with the VCL through messages.

unit Main_Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls;

const
  TH_MESSAGE  = WM_USER + 1;          //Thread message
  TH_FINISHED = 1;                    //Thread SubMessage : End of thread  WParam = ThreadID
  TH_NEWFILE  = 2;                    //Thread Submessage : Started new file
  TH_FOUND    = 3;                    //Thread Submessage : Found searchString in File
  TH_ERROR    = 4;                    //Thread SubMessage : Error WParam = Error
  TH_EDITLIST = 5;                    //Thread SubMessage : Editing the FileList :)
  MAX_THREADS = 4;                    //Max number of threads

  ERROR_COULD_NOT_OPEN_FILE = 1;      //Error file could not be opened

type
  //Records for communication between Threads and MainForm, the pointer for the

  //Record for found items, will occur when LParam = TH_FOUND, WParam will be PFoundRecord
  PFoundRecord = ^TFoundRecord;
  TFoundRecord  = record
                    ThreadID : Cardinal;
                    Filename : string;
                    Position : Cardinal;
                  end;

  //Record for new files, will occur when LParam = TH_NEWFILE, WParam will be PNewFileRecord
  PNewFileRecord = ^TNewFileRecord;
  TNewFileRecord = record
                      ThreadID : Cardinal;
                      Filename : string;
                   end;

  //Record to hold the information from one thread
  TThreadInfo = record
                  Active       : Boolean;
                  ThreadHandle : integer;
                  ThreadId     : Cardinal;
                  CurrentFile  : string;
                end;

  //The Main form of the application
  TMainForm = class(TForm)
    btSearch: TButton;
    Memo: TMemo;
    OpenDialog: TOpenDialog;
    edSearch: TEdit;
    StringGrid: TStringGrid;
    Label1: TLabel;
    Memo1: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    append_btn: TButton;
    Button2: TButton;
    mvup_btn: TButton;
    mvdwn_btn: TButton;
    add_btn: TButton;
    rmv_btn: TButton;
    procedure btSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure append_btnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure add_btnClick(Sender: TObject);
  private
    ThreadInfo : array[0..4] of TThreadInfo;  //Holds the information of the threads
    procedure ThreadMessage( var Message : TMessage ); message TH_MESSAGE;
    function ThreadIDToIndex(ThreadID: Cardinal): integer;  //MessageHandler
  public

  end;

var
  MainForm: TMainForm;

implementation

var CriticalSection : TRTLCriticalSection;  //Critical section protects the filelist
    FileList : TStringList;                 //List of filenames to be searched
    SearchString : string;                  //String to be searched in every file
    PauseThread: boolean;
    FileListAccess: boolean;

{$R *.DFM}

//The threadFunction, this is actually the thread

function FindInFile( data : Pointer) : Integer;
var FileStream    : TFileStream;
    Ch            : char;
    Current,Len   : Integer;
    FoundRecord   : PFoundRecord;
    NewFileRecord : PNewFileRecord;
    Filename      : string;
    Search        : string;
    FilesDone     : Boolean;
begin
  Result := 0;
  FilesDone := False;
  while not FilesDone do                     //while there are still files
  begin
    Current := 1;
    EnterCriticalSection(CriticalSection);   //Try to catch the critical section
    Search := SearchString;                  //Access the shared variables
    //Are there still files available
    if FileList.Count = 0 then
    begin
      //Leave the critical section, when there are no files left
      LeaveCriticalSection(CriticalSection);
      //Leave the while loop
      break;
    end
    else
    begin
      while not(FileListAccess) do
      begin
          //nothing
      end;
      FileListAccess := false;
      //Read the filename
      Filename := FileList.Strings[0];
      //Delete the file from the list
      FileList.Delete(0);
      //Leave the critical section
      LeaveCriticalSection(CriticalSection);
      FileListAccess := true;


      //Inform MainForm of New File
      New(NewFileRecord);
      NewFileRecord^.Filename := Filename;
      NewFileRecord^.ThreadID := GetCurrentThreadID;
      PostMessage( MainForm.Handle, TH_MESSAGE, TH_NEWFILE, Integer(NewFileRecord));

      Len := Length(Search);

      try
        FileStream := TFileStream.Create( Filename, fmOpenRead or fmShareExclusive);
      except
        PostMessage( MainForm.Handle, TH_MESSAGE, TH_ERROR, ERROR_COULD_NOT_OPEN_FILE );
        continue;
      end;

      //The search algorithm, pretty simple, the example is not about searching
      while FileStream.Read(Ch,1) = 1 do
      begin
        If Ch = Search[Current] then
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
      while PauseThread do
        begin
               //Do nothing and wait for program to be unpaused
        end;
  end;
  //All done inform MainForm of ending
  PostMessage( MainForm.Handle, TH_MESSAGE, TH_FINISHED, GetCurrentThreadID );
end;


function TMainForm.ThreadIDToIndex( ThreadID : Cardinal ) : integer;
var Counter : integer;
begin
  Result := -1;
  for Counter := 0 to MAX_THREADS-1 do   //Search for the index of the thread
    if ThreadInfo[Counter].ThreadID = ThreadID then
    begin
      Result := Counter;
      break;
    end;
end;


//functie voor het opvangen van de threadmessages
procedure TMainForm.ThreadMessage( var Message : TMessage );
var FoundRecord   : PFoundRecord;
    NewFileRecord : PNewFileRecord;
    ThreadIndex   : integer;
    Counter       : integer;
    Ended         : boolean;
begin
  case Message.WParam of

    TH_FINISHED :
      begin
        ThreadIndex := ThreadIDToIndex( Message.LParam );
        if ThreadIndex = -1 then Exit;                           //Invalid threadID should never appear
        CloseHandle( ThreadInfo[ThreadIndex].ThreadHandle ); //Free the thread memoryspace
        StringGrid.Cells[3,ThreadIndex+1] := 'False';             //Update the stringgrid
        Threadinfo[ThreadIndex].Active := False;                 //Update the ThreadInfo array
        Ended := True;
        for counter := 0 to MAX_THREADS-1 do
          if ThreadInfo[ThreadIndex].Active = True then
          begin
            Ended := False;
            break;
          end;
        if Ended then btSearch.Enabled := True;
      end;

    TH_NEWFILE  :
      begin
        NewFileRecord := PNewFileRecord(Message.LParam);
        ThreadIndex := ThreadIDToIndex( NewFileRecord^.ThreadID );
        if ThreadIndex = -1 then Exit;                           //Invalid threadID should never appear
        StringGrid.Cells[2,ThreadIndex+1]   := NewFileRecord^.Filename;  //Update StringGrid
        ThreadInfo[ThreadIndex].CurrentFile := NewFileRecord^.Filename;  //Update the ThreadInfo
        while PauseThread do
        begin
            //Do nothing until resumed
        end;

        while not(FileListAccess) do
        begin

        end;

        FileListAccess := false;
        Memo1.Lines.Clear;
        Memo1.Lines.AddStrings(FileList);
        FileListAccess := true;

        Dispose(NewFileRecord);  //All information is used now free the pointer

      end;

    TH_FOUND    :
      begin
        FoundRecord := PFoundRecord(Message.LParam);
        ThreadIndex := ThreadIDToIndex( FoundRecord^.ThreadID );
        if ThreadIndex = -1 then Exit;                           //Invalid threadID should never appear
        Memo.Lines.Add(FoundRecord^.Filename + ' Position :' + IntToStr( FoundRecord^.Position) );
        Dispose(FoundRecord);  //All information is used now free the pointer
      end;

    TH_ERROR    :
      begin
        ThreadIndex := ThreadIDToIndex( Message.LParam );
        if ThreadIndex = -1 then Exit;                          //Invalid threadID should never appear
        Memo.Lines.Add( 'Error : Could not open file ' + ThreadInfo[ThreadIndex].CurrentFile );
      end;



  end;
end;


procedure TMainForm.btSearchClick(Sender: TObject);
var Counter : Integer;
begin
  Memo.Clear;
  Memo1.Clear;
  PauseThread := false;
  SearchString := EdSearch.Text;
  if Opendialog.Execute then
  begin
    FileList.Clear;
    if OpenDialog.Files.Count > 0 then
    begin
      for Counter := 0 to OpenDialog.Files.Count - 1 do
      begin

        FileList.Add(OpenDialog.Files[Counter]);
      end;

      //Start all Threads
      for Counter := 0 to MAX_THREADS-1 do
      begin
        ThreadInfo[Counter].ThreadHandle :=
                            BeginThread(nil,
                            0,
                            @FindInFile,
                            nil,
                            0,
                            Threadinfo[Counter].ThreadId);
        if ThreadInfo[Counter].ThreadHandle <> 0 then       //Everything ok
        begin
          StringGrid.Cells[1,Counter + 1] := IntToStr(ThreadInfo[Counter].ThreadID);
          StringGrid.Cells[3,Counter + 1] := 'True';
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
  FileList := TStringList.Create;
  StringGrid.Cells[1,0] := 'ThreadID';
  StringGrid.Cells[2,0] := 'Handling File';
  StringGrid.Cells[3,0] := 'Active';
  for x := 1 to MAX_THREADS do
    StringGrid.Cells[0,x] := 'Thread ' + inttostr(x);
  FileListAccess := true;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FileList.Free;
  DeleteCriticalSection(CriticalSection);
end;

procedure TMainForm.edSearchChange(Sender: TObject);
begin
  btSearch.Enabled := Length((Sender as TEdit).Text) > 0;
end;



procedure TMainForm.Button2Click(Sender: TObject);
begin
 if PauseThread = false then
 begin
  PauseThread := true;
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
  Button2.Caption := 'Pause Threads';
  append_btn.Enabled := false;
  add_btn.Enabled := false;
  rmv_btn.Enabled := false;
  mvup_btn.Enabled := false;
  mvdwn_btn.Enabled := false;
 end
end;

procedure TMainForm.add_btnClick(Sender: TObject);
begin

Memo1.Lines.Insert(Memo1.SelStart,'C:\Users\gary.namestnik\Desktop\test\test.txt');

        Memo1.Lines.Clear;
        Memo1.Lines.AddStrings(FileList);

end;

procedure TMainForm.append_btnClick(Sender: TObject);
begin

FileList.Append('C:\Users\gary.namestnik\Desktop\test\test.txt');

        Memo1.Lines.Clear;
        Memo1.Lines.AddStrings(FileList);


end;
end.
