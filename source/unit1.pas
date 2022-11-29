unit Unit1;

{$mode objfpc}{$H+}


{ Small app to create or edit Lazarus Examples Metadata Files.
  Uses a coupld of units I wrote to the new Lazarus Example subsystem so could
  be trimmed down but thats what we have smart-linking for.
  When run under the debugger, you will see a lot of (handled) Exceptions,
  so, Not recommended.  Try Ctrl-Shift-F9 if you must run in the IDE.
    David Bannon, Nov 2022.
}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
    ExtCtrls, ComCtrls, fpjson, jsonparser, uexampledata;

type

    { TFormExMetaFile }

    TFormExMetaFile = class(TForm)
        BitBtnSpell: TBitBtn;
        BitBtnNew: TBitBtn;
        BitBtnOpen: TBitBtn;
        BitBtnSave: TBitBtn;
        BitBtnClose: TBitBtn;
        ComboCategory: TComboBox;
        EditName: TEdit;
        EditKeyWords: TEdit;
        LabelDirty: TLabel;
        LabelFileName: TLabel;
        LabelDescription: TLabel;
        LabelKeyWords: TLabel;
        LabelCategory: TLabel;
        LabelProjectName: TLabel;
        Memo1: TMemo;
        OpenDialog1: TOpenDialog;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
        SpeedHelpName: TSpeedButton;
        SpeedHelpCat: TSpeedButton;
        SpeedHelpKeyWords: TSpeedButton;
        SpeedHelpDesc: TSpeedButton;
        StatusBar1: TStatusBar;
        procedure BitBtnNewClick(Sender: TObject);
        procedure BitBtnOpenClick(Sender: TObject);
        procedure BitBtnSaveClick(Sender: TObject);
        procedure BitBtnSpellClick(Sender: TObject);
        procedure EditNameChange(Sender: TObject);
        procedure EditNameEditingDone(Sender: TObject);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure FormCreate(Sender: TObject);
        procedure SpeedHelpCatClick(Sender: TObject);
        procedure SpeedHelpDescClick(Sender: TObject);
        procedure SpeedHelpKeyWordsClick(Sender: TObject);
        procedure SpeedHelpNameClick(Sender: TObject);
    private
        ReadyV : boolean;            // To prevent JSON checks while, eg, loading
        NoFileNameYet : boolean;
        function BuildJSON: string;
        procedure ClearFields();
        function TestJSON: boolean;
        procedure FReady(SetTo : boolean);
        property Ready : boolean read ReadyV write FReady;

    public

    end;

var
    FormExMetaFile: TFormExMetaFile;

implementation

{$R *.lfm}

uses LazFileUtils, LazLogger, uDebSpell, uShowSpell;

const VERSION='v0.01';

{ TFormExMetaFile }

procedure TFormExMetaFile.FReady(SetTo : boolean);
var
    Ctrl : TControl;
begin
    ReadyV := SetTo;
    for Ctrl in [EditName, EditKeyWords, ComboCategory, Memo1] do
           Ctrl.Enabled := ReadyV;
end;

procedure   TFormExMetaFile.ClearFields();
var
    Ctrl : TControl;
begin
         EditName.Text := '';
         EditKeyWords.Text := '';
         ComboCategory.Text := '';
         Memo1.Clear;
         NoFileNameYet := True;
end;

procedure TFormExMetaFile.BitBtnNewClick(Sender: TObject);
begin
    Ready := False;
    if (LabelDirty.caption = '*') and
        (QuestionDlg('Unsaved Content', 'Proceed without saving ?', mtConfirmation, [mrYes, mrNo], '') = mrNo)
         then  begin
             Ready := True;
             exit;
         end;
     SelectDirectoryDialog1.Title := 'Directory for Metadata File';
     if SelectDirectoryDialog1.Execute then begin
         ClearFields();
         LabelFileName.Caption := AppendPathDelim(SelectDirectoryDialog1.Filename);
     end;
     StatusBar1.SimpleText := '';
     Ready := True;
end;

procedure TFormExMetaFile.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   Ready := False;
   if (LabelDirty.caption = '*') and
       (QuestionDlg('Unsaved Content', 'Close without saving ?', mtConfirmation, [mrYes, mrNo], '') = mrNo)
        then  CanClose := False;
   Ready := True;
end;
procedure TFormExMetaFile.BitBtnOpenClick(Sender: TObject);
var
    STL : TStringList;
    Error, Desc, Keys, Cat, ExName : string;
    EXD : TExampleData;
begin
    OpenDialog1.Filter := 'Example Metadata |' + '*' + MetaFileExt;
    if OpenDialog1.Execute then begin
        EXD := TExampleData.Create;
        STL := TStringList.Create;
        Ready := False;
        try
            STL.LoadFromFile(OpenDialog1.FileName);
            if not EXD.ExtractFieldsFromJSON(STL.Text, ExName, Cat
                                    , Keys , Desc, Error) then begin
                showmessage(Error); // We will proceed, but its all blank.
                StatusBar1.SimpleText := Error;
            end else StatusBar1.SimpleText := '';
            Memo1.Text := Desc;
            //FormDump.InString := Desc;      // Thats in case user presses the dump button later on
            EditKeywords.Text := Keys;
            ComboCategory.Text := Cat;
            EditName.Text := ExName;
            LabelFileName.Caption := OpenDialog1.FileName;
            LabelDirty.Caption := ' ';
            BitBtnSave.Enabled := False;
            NoFileNameYet := false;
        finally
            STL.Free;
            EXD.Free;
            Ready := True;
        end;
    end;
end;

procedure TFormExMetaFile.BitBtnSaveClick(Sender: TObject);
var
    ErrorLine : string = '';
    JContent : string;

    function SaveString(FFName : string) : boolean;
    var
        MemBuffer      : TMemoryStream;
    begin
        if fileexistsUTF8(FFName) then
            deleteFile(FFName);
        if fileexists(FFName) then begin
            ErrorLine := 'Cannot replace ' + FFName;             // probably in read only space
            exit(False);
        end;
        MemBuffer := TMemoryStream.Create;                   // ToDo : Must check for errors here
        MemBuffer.Write(JContent[1], JContent.length);
        MemBuffer.SaveToFile(FFName);
        MemBuffer.Free;
        result := fileexists(FFName);
    end;

begin
    JContent := BuildJSON();                                // Assuming content is OK if button enabled ??
    if (ExtractFileNameOnly(LabelFileName.Caption) <> EditName.Text) and
        (QuestionDlg('Project Filename Mismatch', 'Make the Filename match the project ?', mtConfirmation, [mrYes, mrNo], '') = mrYes)
         then begin
             LabelFileName.Caption := AppendPathDelim(ExtractFileDir(LabelFileName.Caption)) + EditName.Text + MetaFileExt;
         end;
    if SaveString(LabelFileName.caption) then begin
        StatusBar1.SimpleText := 'Updated ' + LabelFileName.caption;
        // DisplayMetaFile(FPathName);
        BitBtnSave.Enabled := false;
        LabelDirty.Caption := ' ';
    end else Showmessage(ErrorLine);
end;

procedure TFormExMetaFile.BitBtnSpellClick(Sender: TObject);
var
    DebSpell : TDebSpell;
    Index : integer;
    STL, CorrectList : TStringList;
    St, Correction : String;
begin
    if FileExists('/usr/share/lintian/data/spelling/corrections') then begin
        DebSpell := TDebSpell.create;
        CorrectList := TStringList.create;         // should use a dynamic array, cheaper
        STL := TStringList.Create();
        St := EditKeyWords.Text;
        St.Replace('"', '', [rfReplaceAll, rfIgnoreCase]);
        St.Replace(',', ' ', [rfReplaceAll, rfIgnoreCase]);
        STL.AddDelimitedtext(St);
        St := EditName.Text;
        St.Replace('"', '', [rfReplaceAll, rfIgnoreCase]);
        St.Replace(',', ' ', [rfReplaceAll, rfIgnoreCase]);
        STL.AddDelimitedtext(St);
        St := Memo1.Text;
        St.Replace('"', '', [rfReplaceAll, rfIgnoreCase]);
        St.Replace(',', ' ', [rfReplaceAll, rfIgnoreCase]);
        STL.AddDelimitedtext(St);
        try
            for St in Stl do begin
                Index := DebSpell.CheckThis(lowercase(St), Correction);
                if Index > 0 then
                    CorrectList.Add(St +'  >>>  ' + Correction);
            end;
            FormSpellCorrections.ShowSpelling(CorrectList);
            FormSpellCorrections.show;
            StatusBar1.SimpleText := inttostr(CorrectList.Count) + ' possible spelling issues';
        finally
            CorrectList.free;
            STL.Free;
            DebSpell.free;
        end;
    end else showmessage('Sorry, Spell Check depends on having Lintian Installed');
end;

procedure TFormExMetaFile.EditNameChange(Sender: TObject);     // manages change in all i/p fields
begin
    if not Ready then exit;
    LabelDirty.Caption := '*';
    BitBtnSave.Enabled := TestJson();
end;

procedure TFormExMetaFile.EditNameEditingDone(Sender: TObject);
begin
    //debugln('TFormExMetaFile.EditNameEditingDone called');
    if not Ready then exit;
    TestJSON;
    if Sender.Equals(TObject(EditName))
        and (length(EditName.Text) > 2)
        and NoFileNameYet then begin
            LabelFileName.Caption := LabelFileName.Caption + EditName.Text + MetaFileExt;
            StatusBar1.SimpleText := 'New Metadata Filename determined';
        end;
end;



function TFormExMetaFile.BuildJSON : string;
begin
    result := '{ "' + TExampleData.EscJSON(EditName.Text) + '" : {'
             +     #10'   "Category" : "' + TExampleData.EscJSON(ComboCategory.text)
             + '",'#10'   "Keywords" : [' + EditKeyWords.Text + ']'
             +  ','#10'   "Description" : "' + TExampleData.EscJSON(Memo1.Text)
             + '"}'#10'}';                                    // Now contains a JSON File
end;

function TFormExMetaFile.TestJSON : boolean;
var
    EXD : TExampleData;
    JContent, ErrorLine, Category  : string;
begin
    result := true;
    JContent := BuildJSON;
    EXD := TExampleData.Create();
    try
        if EXD.TestJSON(JContent, ErrorLine, Category) then begin    // TestJSON is not a class method
            StatusBar1.SimpleText := 'Good JSON';
            StatusBar1.hint := '';
        end
        else  begin
             StatusBar1.SimpleText := 'Bad JSON';
             StatusBar1.hint := ErrorLine;
            //debugln('found some bad JSON');
            //debugln(JContent);
            result := false;
        end;
    finally
        EXD.Free;
    end;
end;

procedure TFormExMetaFile.FormCreate(Sender: TObject);
begin
    Caption := 'Exampe Metafile Editor ' + VERSION;
   LabelDirty.Caption := ' ';
   BitBtnSave.Enabled := False;
   Ready := False;
   LabelFileName.Caption := '';
   StatusBar1.SimpleText := 'Probably a good idea to click New or Open';
end;

procedure TFormExMetaFile.SpeedHelpCatClick(Sender: TObject);
begin
    showmessage('Use an existing Category if possible');
end;

procedure TFormExMetaFile.SpeedHelpDescClick(Sender: TObject);
begin
    showmessage('Anything likely to be useful for user, as many lines as you think necessary');
end;

procedure TFormExMetaFile.SpeedHelpKeyWordsClick(Sender: TObject);
begin
    showmessage('eg "ComboBox", "load from file", "needs work"');
end;

procedure TFormExMetaFile.SpeedHelpNameClick(Sender: TObject);
begin
    showmessage('A short but meaningful name for this project, no spaces please');
end;


end.

