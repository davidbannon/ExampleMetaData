unit Unit1;

{$mode objfpc}{$H+}


{ Small app to create or edit Lazarus Examples Metadata Files.
  Uses a coupld of units I wrote to the new Lazarus Example subsystem so could
  be trimmed down but thats what we have smart-linking for.
  When run under the debugger, you will see a lot of (handled) Exceptions,
  so, Not recommended.  Try Ctrl-Shift-F9 if you must run in the IDE.
    David Bannon, Nov 2022.

  History -
    2022-11-30 Initial Release
    2023-04-10 Parse the keywords so user does not have to put in double inverted commas
    2023-04-24 Edit PackageFile for user, command line parameter of ex-metadata file.
    2023-04-25 Add Save As, allow dir above multiple example dir.
    2023-04-25 Check cat is in list, if not, nag user on EVERY save.
}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
    ExtCtrls, ComCtrls, fpjson, jsonparser, uexampledata;

type

    { TFormExMetaFile }

    TFormExMetaFile = class(TForm)
        BitBtnSaveAs: TBitBtn;
        BitBtnCheckLPK: TBitBtn;
        BitBtnSpell: TBitBtn;
        BitBtnNew: TBitBtn;
        BitBtnOpen: TBitBtn;
        BitBtnSave: TBitBtn;
        BitBtnClose: TBitBtn;
        ComboCategory: TComboBox;
        EditName: TEdit;
        EditKeyWords: TEdit;
        Label1: TLabel;
        LabelKeyWordJSON: TLabel;
        LabelKeyRender: TLabel;
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
        procedure BitBtnCheckLPKClick(Sender: TObject);
        procedure BitBtnNewClick(Sender: TObject);
        procedure BitBtnOpenClick(Sender: TObject);
        procedure BitBtnSaveAsClick(Sender: TObject);
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
        function OpenExistingExample(FFName: string): boolean;
        function RenderKeyWords: boolean;
        function SuggestRelativeExDir(FLPKFile, FMetadataFile: string): string;
        function TestJSON: boolean;
        procedure FReady(SetTo : boolean);
//        function TestLPKFile(FullPkgFileName: string): string;
        property Ready : boolean read ReadyV write FReady;

    public

    end;

var
    FormExMetaFile: TFormExMetaFile;

implementation

{$R *.lfm}

uses LazFileUtils, FileUtil, LazLogger, uDebSpell, uShowSpell{, Laz2_XMLRead, Laz2_DOM}, uSetExDir;

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
     BitBtnCheckLPK.Enabled := False;
end;



procedure TFormExMetaFile.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   Ready := False;
   if (LabelDirty.caption = '*') and
       (QuestionDlg('Unsaved Content', 'Close without saving ?', mtConfirmation, [mrYes, mrNo], '') = mrNo)
        then  CanClose := False;
   Ready := True;
end;

function  TFormExMetaFile.OpenExistingExample(FFName : string) : boolean;
var
    STL : TStringList;
    Error, Desc, Keys, Cat, ExName : string;
    EXD : TExampleData;
begin
   if not FileExists(FFName) then exit(False);
   Result := True;
    EXD := TExampleData.Create;
    STL := TStringList.Create;
    Ready := False;
    try
        STL.LoadFromFile(FFName);
        if not EXD.ExtractFieldsFromJSON(STL.Text, ExName, Cat
                                , Keys , Desc, Error) then begin
            showmessage(Error); // We will proceed, but its all blank.
            StatusBar1.SimpleText := Error;
        end else StatusBar1.SimpleText := '';
        Memo1.Text := Desc;
        //FormDump.InString := Desc;      // Thats in case user presses the dump button later on
        EditKeywords.Text := Keys.Replace('"', '', [rfReplaceAll]);
        ComboCategory.Text := Cat;
        EditName.Text := ExName;
        LabelFileName.Caption := FFName;
        LabelDirty.Caption := ' ';
        BitBtnSave.Enabled := False;
        BitBtnSaveAs.Enabled := True;
        NoFileNameYet := false;
        TestJSON();
    finally
        STL.Free;
        EXD.Free;
        Ready := True;
        BitBtnCheckLPK.Enabled := True;
    end;
    StatusBar1.SimpleText := 'Loaded ' + FFName;
end;

procedure TFormExMetaFile.BitBtnOpenClick(Sender: TObject);
begin
    OpenDialog1.Title := 'Select the Package Metadata File';
    OpenDialog1.Filter := 'Example Metadata |' + '*' + MetaFileExt;
    if OpenDialog1.Execute then begin
        OpenExistingExample(OpenDialog1.FileName);
    end;
end;

// We offere a dirSelectDialog, clear the title. If chosen dir already has a ex-meta
// we ignore, SEP.
procedure TFormExMetaFile.BitBtnSaveAsClick(Sender: TObject);
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
         EditName.Hint := 'was ' + EditName.Text;         // Saving will reset to default
         EditName.Text := '';
         LabelFileName.Caption := AppendPathDelim(SelectDirectoryDialog1.Filename);
         NoFileNameYet := True;
     end;
     StatusBar1.SimpleText := '';
     Ready := True;
     BitBtnCheckLPK.Enabled := False;
end;

procedure TFormExMetaFile.BitBtnSaveClick(Sender: TObject);
var
    ErrorLine : string = '';
    JContent : string;
    OldFFileName : string;

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
    if ComboCategory.Items.IndexOf(ComboCategory.Text) < 0 then
        if (QuestionDlg('Warning "' + ComboCategory.Text + '"', 'That is not a std category, are you sure ?', mtConfirmation, [mrYes, mrNo], '') = mrNo) then
            exit;
    JContent := BuildJSON();                                // Assuming content is OK if button enabled ??
    if (ExtractFileNameOnly(LabelFileName.Caption) <> EditName.Text) and
        (QuestionDlg('Project Filename Mismatch', 'Make the Filename match the project ?', mtConfirmation, [mrYes, mrNo], '') = mrYes)
         then begin
             OldFFileName := LabelFileName.Caption;
             LabelFileName.Caption := AppendPathDelim(ExtractFileDir(LabelFileName.Caption))
                + lowercase(EditName.Text).Replace(' ', '_', [rfReplaceAll]) + MetaFileExt;
             if FileExists(OldFFileName) and
                (QuestionDlg('Remove old metadata file ?', OldFFileName
                        , mtConfirmation, [mrYes, mrNo], '') = mrYes) then
                DeleteFileUTF8(OldfFileName);
         end;
    if SaveString(LabelFileName.caption) then begin
        EditName.Hint := 'Short, no spaces please';             // Because Save As may have set it to old name
        StatusBar1.SimpleText := 'Updated ' + LabelFileName.caption;
        // DisplayMetaFile(FPathName);
        BitBtnSave.Enabled := false;
        LabelDirty.Caption := ' ';
        BitBtnCheckLPK.Enabled := True;
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

procedure TFormExMetaFile.EditNameEditingDone(Sender: TObject);  // manages change in all i/p fields
begin
    //debugln('TFormExMetaFile.EditNameEditingDone called');
    if not Ready then exit;

    TestJSON;
    if Sender.Equals(TObject(EditName))
        and (length(EditName.Text) > 2)
        and NoFileNameYet then begin
            LabelFileName.Caption := LabelFileName.Caption
                + lowercase(EditName.Text).Replace(' ', '_', [rfReplaceAll]) + MetaFileExt;
            StatusBar1.SimpleText := 'New Metadata Filename determined';
            NoFileNameYet := false;
        end;
    if Sender.Equals(TObject(ComboCategory)) then
        if ComboCategory.Items.IndexOf(ComboCategory.Text) < 0 then
            StatusBar1.SimpleText := 'WARNING - non standard Category in use';
            // Save will further give user a hard time.
end;



function TFormExMetaFile.BuildJSON : string;
begin
    result := '{ "' + TExampleData.EscJSON(EditName.Text) + '" : {'
             +     #10'   "Category" : "' + TExampleData.EscJSON(ComboCategory.text)
             + '",'#10'   "Keywords" : [' + LabelKeyRender.Caption + ']'
//             + '",'#10'   "Keywords" : [' + EditKeyWords.Text + ']'
             +  ','#10'   "Description" : "' + TExampleData.EscJSON(Memo1.Text)
             + '"}'#10'}';                                    // Now contains a JSON File
end;

{ A term can contain one or more words, comma seperate terms.}

function TFormExMetaFile.RenderKeyWords : boolean;
var
    i : integer;
    St : string;
begin
    Result := false;
    // Clean up, remove ", remove spaces after comma, trailing spaces, trailing commas
    St := EditKeyWords.Caption;
    St := St.Replace('"', '', [rfReplaceAll]);
    St := St.Replace(', ', ',', [rfReplaceAll]);
    if St = '' then exit;
    while St[length(St)] = ' ' do
        delete(St, length(St), 1);
    if St = '' then exit;
    if St[length(St)] = ',' then
        delete(St, length(St), 1);
    LabelKeyRender.Caption := '';
    if length(St) < 2 then
        exit;
    LabelKeyRender.Caption := '"';
    for i := 1 to length(St) do begin
        if St[i] = ',' then begin    // allow for two possibilities, comma at end or mid sentance
            if i = length(St) then
                LabelKeyRender.Caption := LabelKeyRender.Caption + '"'
            else
                LabelKeyRender.Caption := LabelKeyRender.Caption + '", "';
        end else
           LabelKeyRender.Caption := LabelKeyRender.Caption + St[i];
    end;
    if LabelKeyRender.Caption[length(LabelKeyRender.Caption)] <> '"' then
        LabelKeyRender.Caption := LabelKeyRender.Caption + '"';
    result := (length(LabelKeyRender.Caption) > 4);
end;

function TFormExMetaFile.TestJSON : boolean;
var
    EXD : TExampleData;
    JContent, ErrorLine, Category  : string;
begin
    result := true;
    if not RenderKeyWords then begin
        StatusBar1.SimpleText := 'Invalid JSON in Key Words';
        exit(false);
    end;
    JContent := BuildJSON;
    EXD := TExampleData.Create();
    try
        if EXD.TestJSON(JContent, ErrorLine, Category) then begin    // TestJSON is not a class method
            StatusBar1.SimpleText := 'Good JSON';
            StatusBar1.hint := '';
            BitBtnSaveAs.Enabled := True;         // once enabled (because user set a Name) it stays enabled.
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
var
    Params : TStringArray;
    STL : TStringList = nil;
    None : Array of String = nil;
begin
    Caption := 'Exampe Metafile Editor ' + VERSION;
    LabelDirty.Caption := ' ';
    BitBtnSave.Enabled := False;
    BitBtnSaveAs.Enabled := False;
    Ready := False;
    LabelFileName.Caption := '';
    StatusBar1.SimpleText := 'Probably a good idea to click New or Open';
    LabelKeyRender.Caption := '';
    BitBtnCheckLPK.Enabled := False;
    Params := Application.GetNonOptions('', None);
    if length(Params) = 1 then begin
        if FileExists(Params[0]) and string(Params[0]).EndsWith(MetaFileExt) then begin
            if FileNameIsAbsolute(Params[0]) then
                OpenExistingExample(Params[0])
            else
                OpenExistingExample(Resolvedots(AppendPathDelim(GetCurrentDir()) + Params[0]));
       end;
    end else begin
        try
            STL := FindAllFiles('', '*' + MetaFileExt, False);
            if Stl.Count = 1 then
                OpenExistingExample(AppendPathDelim(GetCurrentDir()) + Stl[0]);
        finally
            Stl.Free;
        end;
    end;
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


// ============================== L P K    F I L E =============================

function TFormExMetaFile.SuggestRelativeExDir(FLPKFile, FMetaDataFile : string) : string;
begin
    Result := CreateRelativePath(FMetaDataFile, extractFilePath(FLPKFile), True);
    Result := ExtractFilePath(Result);
end;

procedure TFormExMetaFile.BitBtnCheckLPKClick(Sender: TObject);
var
    St : string = '';     STL : TStringList = nil;
begin
    OpenDialog1.Title := 'Select the Package File';
    OpenDialog1.Filter := 'Package File |' + '*.lpk';
    if OpenDialog1.Execute then begin
        StatusBar1.SimpleText := '';
        FormSetExDir.MetadataFile := ExtractFileName(LabelFileName.Caption);
        FormSetExDir.LabelLPKFile.Caption := OpenDialog1.FileName;
        St := FormSetExDir.TestLPKFile();
        if St <> SuggestRelativeExDir(OpenDialog1.FileName, LabelFileName.Caption) then begin
            if St = '' then ShowMessage('ExamplesDirectory not set')
            else begin                           // its possible there are several example directories below here.....
                STL := FindAllFiles(St, '*' + MetaFileExt, true);
                if Stl.Count < 2 then begin
                    ShowMessage('ExamplesDirectory appears invalid ' + St);
                    FormSetExDir.Edit1.Text := SuggestRelativeExDir(OpenDialog1.FileName, LabelFileName.Caption);
                    FormSetExDir.ShowModal;                                             // mrOK never works for me !
                    if FormSetExDir.LPKFileUpdated then
                        StatusBar1.SimpleText := 'Package file ExamplesDirectory element updated';
                end else StatusBar1.SimpleText := 'Package file ExamplesDirectory seems OK, multiple examples.';
            end;
        end else StatusBar1.SimpleText := 'Package file ExamplesDirectory seems OK';
    end;
end;

end.

