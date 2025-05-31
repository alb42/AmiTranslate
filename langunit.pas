unit langunit;

{$mode ObjFPC}{$H+}

interface

uses
  AmiTranslatelocale;

var
  SrcLanguages, DestLanguages: array of string;

function LangShort(ID: Integer): string; inline;

implementation

const
  SrcTemplate: array of Integer =
    (
      LANG_BULGARIAN,
      LANG_CZECH,
      LANG_DANISH,
      LANG_GERMAN,
      LANG_GREEK,
      LANG_ENGLISH,
      LANG_SPANISH,
      LANG_ESTONIAN,
      LANG_FINNISH,
      LANG_FRENCH,
      LANG_HUNGARIAN,
      LANG_INDONESIAN,
      LANG_ITALIAN,
      LANG_LITHUANIAN,
      LANG_LATVIAN,
      LANG_NORWEGIAN,
      LANG_DUTCH,
      LANG_POLISH,
      LANG_PORTUGESE,
      LANG_ROMANIAN,
      LANG_RUSSIAN,
      LANG_SLOVAK,
      LANG_SLOVENIAN,
      LANG_SWEDISH,
      LANG_TURKISH,
      LANG_UKRAINIAN
    );
  DestTemplate: array of Integer =
    (
      LANG_BULGARIAN,
      LANG_CZECH,
      LANG_DANISH,
      LANG_GERMAN,
      LANG_GREEK,
      LANG_ENGLISH_GB,
      LANG_ENGLISH_US,
      LANG_SPANISH,
      LANG_ESTONIAN,
      LANG_FINNISH,
      LANG_FRENCH,
      LANG_HUNGARIAN,
      LANG_INDONESIAN,
      LANG_ITALIAN,
      LANG_JAPANESE,
      LANG_LITHUANIAN,
      LANG_LATVIAN,
      LANG_NORWEGIAN,
      LANG_DUTCH,
      LANG_POLISH,
      LANG_PORTUGESE_BR,
      LANG_PORTUGESE_PT,
      LANG_ROMANIAN,
      LANG_RUSSIAN,
      LANG_SLOVAK,
      LANG_SLOVENIAN,
      LANG_SWEDISH,
      LANG_TURKISH,
      LANG_UKRAINIAN
    );


procedure InitSrcLangs;
var
  i: Integer;
begin
  SetLength(SrcLanguages, Length(SrcTemplate) + 1);
  SrcLanguages[0] := GetLocString(LANG_AUTO);
  for i := 0 to High(SrcTemplate) do
  begin
    SrcLanguages[i + 1] := LangShort(SrcTemplate[i]) + ' - ' + GetLocString(SrcTemplate[i]);
  end;
end;

procedure InitDestLangs;
var
  i: Integer;
begin
  SetLength(DestLanguages, Length(DestTemplate));
  for i := 0 to High(DestTemplate) do
    DestLanguages[i] := LangShort(DestTemplate[i]) + ' - ' + GetLocString(DestTemplate[i]);
end;

function LangShort(ID: Integer): string;
begin
  Result := '';
  case ID of
    LANG_BULGARIAN: Result := 'BG';
    LANG_CZECH: Result := 'CS';
    LANG_DANISH: Result := 'DA';
    LANG_GERMAN: Result := 'DE';
    LANG_GREEK: Result := 'EL';
    LANG_ENGLISH: Result := 'EN';
    LANG_ENGLISH_GB: Result := 'EN-GB';
    LANG_ENGLISH_US: Result := 'EN-US';
    LANG_SPANISH: Result := 'ES';
    LANG_ESTONIAN: Result := 'ET';
    LANG_FINNISH: Result := 'FI';
    LANG_FRENCH: Result := 'FR';
    LANG_HUNGARIAN: Result := 'HU';
    LANG_INDONESIAN: Result := 'ID';
    LANG_ITALIAN: Result := 'IT';
    LANG_JAPANESE: Result := 'JA';
    LANG_KOREAN: Result := 'KO';
    LANG_LITHUANIAN: Result := 'LT';
    LANG_LATVIAN: Result := 'LV';
    LANG_NORWEGIAN: Result := 'NB';
    LANG_DUTCH: Result := 'NL';
    LANG_POLISH: Result := 'PL';
    LANG_PORTUGESE: Result := 'PT';
    LANG_PORTUGESE_BR: Result := 'PT-BR';
    LANG_PORTUGESE_PT: Result := 'PT-PT';
    LANG_ROMANIAN: Result := 'RO';
    LANG_RUSSIAN: Result := 'RU';
    LANG_SLOVAK: Result := 'SK';
    LANG_SLOVENIAN: Result := 'SL';
    LANG_SWEDISH: Result := 'SV';
    LANG_TURKISH: Result := 'TR';
    LANG_UKRAINIAN: Result := 'UK';
    LANG_CHINESE: Result := 'ZH';
  end;
end;

initialization
  InitSrcLangs;
  InitDestLangs;

end.

