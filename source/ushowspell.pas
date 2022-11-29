unit uShowSpell;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    ComCtrls;

type

    { TFormSpellCorrections }

    TFormSpellCorrections = class(TForm)
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        LabelSpellResults: TLabel;
        ListView1: TListView;
        Panel1: TPanel;
        Panel2: TPanel;
    private

    public
        procedure ShowSpelling(const Spell : TStringList);

    end;

var
    FormSpellCorrections: TFormSpellCorrections;

implementation

{$R *.lfm}

uses lazlogger;
{ TFormSpellCorrections }

procedure TFormSpellCorrections.ShowSpelling(const Spell: TStringList);
var
    SpellLength : integer;
    St : string;
begin
    SpellLength := Spell.count;
    LabelSpellResults.Caption := inttostr(SpellLength) + ' suggested corrections found';
    ListView1.Clear;
    for St in Spell do begin
        ListView1.AddItem(St, nil);
        // debugln('LISTVIEWADD ' + St);
    end;

end;

end.

