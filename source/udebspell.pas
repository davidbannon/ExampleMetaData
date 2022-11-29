unit uDebSpell;

// This unit will do some spell checking but not in a conventional sense.
// It uses the Debian Lintian list of "often misspelt" words rather than looking
// to match each word against a dictionary entry.  As such it requires Lintian
// to be installed, or, maybe, the 'corrections' file from Lintian be available.
//
// This model is useful if the text is likely to contain a lot of words that are
// not in the dictionaries, such as source code or documentation for source code.
// The Lintian list has over 10K words so is far from comprehensive and English only !
// https://salsa.debian.org/lintian/lintian

// In use, create the object, pass a string and you get back the index of the first
// misspelt word in that string. It also passes back what it consideres the
// appropriate spelling. Its not particularly fast, needs to run repeatedly
// until clean but solved my problem.

// Copyright : David Bannon, April 25, 2022.
// License :
//    This code is licensed under BSD 3-Clause Clear License
//         https://spdx.org/licenses/BSD-3-Clause-Clear.html
//    with the exception that anyone can change that license to any other license
//    to suit their own needs.


{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils;

type
    { TDebSpell }
    TDebSpell = class
        private
            WordList : TstringList;
            function FirstWord(const Index : integer) : string;
            function SecondWord(const Index : integer) : string;
        public
            ErrorString : String;
            constructor Create;
            destructor Destroy; override;
            function CheckThis(const InString: string; out OutWord: string): integer;
        end;

implementation

{ TDebSpell }

function TDebSpell.FirstWord(const Index: integer): string;
begin
    result := copy(WordList[Index], 1, pos('||', WordList[Index])-1);
end;

function TDebSpell.SecondWord(const Index: integer): string;
begin
    result := copy(WordList[Index], pos('||', WordList[Index])+2, 1000);
end;

constructor TDebSpell.Create;
begin
    WordList := TStringList.create;
    WordList.LoadFromFile('/usr/share/lintian/data/spelling/corrections');
    while WordList.Count > 1 do begin
        if (WordList[0] = '') or (WordList[0][1] = '#') then
            WordList.Delete(0)
        else
            break;
    end;
    if WordList.Count < 10 then                        // 10 ??
        ErrorString := 'Err, something seems to have gone wrong with loading';
end;

destructor TDebSpell.Destroy;
begin
    if Assigned(WordList) then
        WordList.Free;
    inherited Destroy;
end;

// Pass a multiword string. Reports ONLY the first error found in that string.
// If it returns > 0 then there is a spelling mistake, its at result and
// replacement is in OutWord. OutWord undefined otherwise.
function TDebSpell.CheckThis(const InString: string; out OutWord: string): integer;
var
  i : integer = 0;
  STL : TStringList;
  InWord : string;
begin
    result := 0;
    STL  := TStringList.create;
    try
        STL.AddDelimitedtext(InString);
        for InWord in STL do begin
            i := 0;
            while i < WordList.Count do begin
                if InWord = FirstWord(i) then begin
                    OutWord := SecondWord(i);
                    exit(pos(InWord, InString));
                end;
                inc(i);
            end;
        end;
    finally
        STL.free;
    end;
end;

end.

