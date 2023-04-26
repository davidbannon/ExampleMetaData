unit uSetExDir;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
    ExtCtrls, Laz2_XMLRead, Laz2_XMLWrite, Laz2_DOM;

type

    { TFormSetExDir }

    TFormSetExDir = class(TForm)
        BitBtnOK: TBitBtn;
        BitBtnCancel: TBitBtn;
        Edit1: TEdit;
        ImageTick: TImage;
        ImageCross: TImage;
        Label1: TLabel;
        LabelLPKFile: TLabel;
        LabelPrompt: TLabel;
        LabelXML: TLabel;
        procedure BitBtnCancelClick(Sender: TObject);
        procedure BitBtnOKClick(Sender: TObject);
        procedure Edit1Change(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private

        function updateScreenXML: boolean;
    public
        MetadataFile : string;
        LPKFileUpdated : boolean;
        // Returns the value of the XML ExamplesDirectory element, '' if blank or not found
        function TestLPKFile(): string;
        // Sets the ExamplesDirectory Element in PackageFile to passed value, happy to create it if necessary.
        function SetExDirXML(LPKFileName, ValueSt: String): boolean;
    end;

var
    FormSetExDir: TFormSetExDir;

implementation

{$R *.lfm}
uses LazLogger, LazFileUtils, FileUtil;

{ TFormSetExDir }

// True if the metadata file is where relative path points to.
function TFormSetExDir.updateScreenXML : boolean;
var
    St : string;
    STL : TStringList = nil;
begin
    St := Edit1.Text;
    if St = '' then
        LabelXML.Caption := '.' + PathDelim
    else begin
        if St.EndsWith(PathDelim) then
            delete(St, length(St), 1);
        LabelXML.Caption := '<ExamplesDirectory Value="' + St + '"/>';
    end;
    STL := FindAllFiles(extractFilePath(LabelLPKFile.Caption) + St, MetaDataFile, True);
    try
        if STL.Count = 1 then
            exit(True);
        result := false;
    finally
        STL.Free;
        ImageTick.Visible := Result;
        ImageCross.Visible := Not Result;
    end;
end;

function TFormSetExDir.SetExDirXML(LPKFileName, ValueSt : String) : boolean;
var
    doc: TXMLDocument;
    NodeA, NodeB: TDOMNode;
begin
    Result := false;
    try
        ReadXMLFile(doc, LPKFileName);
    except on E: Exception do begin
            showmessage('ERROR :  [TFormSetExDir.SetExDirXML] XML Error : ' + E.Message);
            debugln('ERROR :  [TFormSetExDir.SetExDirXML] XML Error : ' + E.Message);
            if assigned(doc) then
                doc.free;
            exit;
        end;
    end;
    try
        NodeB := doc.DocumentElement.FindNode('Package');
        if NodeB = nil then exit;
        NodeA := NodeB.FindNode('ExamplesDirectory');
        if NodeA = Nil then begin
            NodeA :=  Doc.CreateElement('ExamplesDirectory');
        end;
        TDOMElement(NodeA).SetAttribute('Value', ValueSt);
        NodeB.AppendChild(NodeA);
        try
            writeXMLFile(Doc,LPKFileName);
        except on E: Exception do begin
                debugln('ERROR - failed to write back to ' + LPKFileName + ' : ' + E.Message);
                exit;
            end;
        end;
    finally
        doc.free;
    end;
    Result := true;
end;

procedure TFormSetExDir.FormShow(Sender: TObject);
begin
    updateScreenXML;
end;

procedure TFormSetExDir.BitBtnCancelClick(Sender: TObject);
begin
    close;
end;

procedure TFormSetExDir.BitBtnOKClick(Sender: TObject);
begin
    updateScreenXML();                 // we do enforce a valid entry here, user decides !
    if SetExDirXML(LabelLPKFile.Caption, Edit1.Text) then begin
        ModalResult := mrOK;
        LPKFileUpdated := True;
    end else
        Showmessage('Failed to write to ' + LabelLPKFile.Caption);
    close;
end;

procedure TFormSetExDir.Edit1Change(Sender: TObject);
begin
    updateScreenXML();
end;

procedure TFormSetExDir.FormCreate(Sender: TObject);
begin
    ImageTick.Left := ImageCross.Left;
    ImageTick.Visible := True;
    ImageCross.Visible := False;
end;

function TFormSetExDir.TestLPKFile: string;
var
    doc: TXMLDocument;
    NodeA, NodeB: TDOMNode;
begin
    Result := '';
    try
        ReadXMLFile(doc, LabelLPKFile.Caption);
    except on E: Exception do begin
            showmessage('ERROR :  [TExampleData.GetThirdPartyDir] XML Error : ' + E.Message);
            debugln('ERROR :  [TExampleData.GetThirdPartyDir] XML Error : ' + E.Message);
            if assigned(doc) then
                doc.free;
            exit;
        end;
    end;
    try
        NodeB := doc.DocumentElement.FindNode('Package');
        if NodeB = nil then exit;
        NodeA := NodeB.FindNode('ExamplesDirectory');
        if NodeA <> nil then begin
            NodeB := NodeA.Attributes.GetNamedItem('Value');
            if NodeB <> nil then                            // Leave existing path in FullPkgFileName, ie assumes LPK file is level or above examples
                //ADir := NodeB.NodeValue;                    // maybe something like eg ../../Examples
                result := NodeB.NodeValue;
        end;
    finally
        doc.free;
    end;
end;

end.

