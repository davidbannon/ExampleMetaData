program Project1;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}
    cthreads,
    {$ENDIF}
    {$IFDEF HASAMIGA}
    athreads,
    {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, Unit1, uexampledata, uDebSpell, uShowSpell;

{$R *.res}

begin
    RequireDerivedFormResource := True;
    Application.Scaled := True;
    Application.Initialize;
    Application.CreateForm(TFormExMetaFile, FormExMetaFile);
    Application.CreateForm(TFormSpellCorrections, FormSpellCorrections);
    Application.Run;
end.

