unit AmiTranslateLocale;
{$mode objfpc}{$H+}
interface

{****************************************************

   This file was created automatically by 'FlexCat 2.18'
   from "locale/AmiTranslate.cd".

   Do NOT edit by hand!

****************************************************}

uses
  Exec , Locale , Utility ;

const
  APP_DESC = 1000 ;
  APP_DESC_STR = 'Translate Texts using DeepL'#0;

  ERROR_SWITCH_AUTO = 2000 ;
  ERROR_SWITCH_AUTO_STR = 'Automatic Source language selected, but no translation done.'#0;

  ERROR_SWITCH_SRC = 2002 ;
  ERROR_SWITCH_SRC_STR = 'Input langauge not found in possible output languages.'#0;

  ERROR_SWITCH_DEST = 2003 ;
  ERROR_SWITCH_DEST_STR = 'Output langauge not found in possible input languages.'#0;

  GUI_INPUT_LANG = 10000 ;
  GUI_INPUT_LANG_STR = 'Input Language'#0;

  GUI_OUTPUT_LANG = 10001 ;
  GUI_OUTPUT_LANG_STR = 'Output Language'#0;

  GUI_TRANSLATE = 10003 ;
  GUI_TRANSLATE_STR = 'Translate'#0;

  GUI_PLAIN = 10004 ;
  GUI_PLAIN_STR = 'Plain'#0;

  GUI_FANCY = 10005 ;
  GUI_FANCY_STR = 'Fancy'#0;

  GUI_CLEAR = 10006 ;
  GUI_CLEAR_STR = 'Clear'#0;

  GUI_INSERT_TEXT = 10007 ;
  GUI_INSERT_TEXT_STR = 'Insert Text'#0;

  GUI_COPYTEXT = 10008 ;
  GUI_COPYTEXT_STR = 'Copy Text'#0;

  GUI_SAVEASUTF8 = 10009 ;
  GUI_SAVEASUTF8_STR = 'Save as UTF-8'#0;

  GUI_SAVEASPDF = 10010 ;
  GUI_SAVEASPDF_STR = 'Save as PDF'#0;

  GUI_SWITCH = 10011 ;
  GUI_SWITCH_STR = 'Switch'#0;

  MENU_TEXT = 11000 ;
  MENU_TEXT_STR = 'Text'#0;

  MENU_PASTE = 11001 ;
  MENU_PASTE_STR = 'Paste'#0;

  MENU_CLEAR = 11002 ;
  MENU_CLEAR_STR = 'Text'#0;

  MENU_PASTEINPUT = 11003 ;
  MENU_PASTEINPUT_STR = 'Paste to Source'#0;

  MENU_PASTEINPUT_KEY = 11004 ;
  MENU_PASTEINPUT_KEY_STR = 'V'#0;

  MENU_COPY = 11005 ;
  MENU_COPY_STR = 'Copy'#0;

  MENU_COPYRESULT = 11006 ;
  MENU_COPYRESULT_STR = 'Copy Result'#0;

  MENU_COPYRESULT_KEY = 11007 ;
  MENU_COPYRESULT_KEY_STR = 'C'#0;

  MENU_CLEARALL = 11008 ;
  MENU_CLEARALL_STR = 'Clear All'#0;

  MENU_CLEARALL_KEY = 11009 ;
  MENU_CLEARALL_KEY_STR = 'D'#0;

  LANG_AUTO = 20000 ;
  LANG_AUTO_STR = 'Automatic'#0;

  LANG_BULGARIAN = 20001 ;
  LANG_BULGARIAN_STR = 'Bulgarian'#0;

  LANG_CZECH = 20002 ;
  LANG_CZECH_STR = 'Czech'#0;

  LANG_DANISH = 20003 ;
  LANG_DANISH_STR = 'Danish'#0;

  LANG_GERMAN = 20004 ;
  LANG_GERMAN_STR = 'German'#0;

  LANG_GREEK = 20005 ;
  LANG_GREEK_STR = 'Greek'#0;

  LANG_ENGLISH = 20006 ;
  LANG_ENGLISH_STR = 'English'#0;

  LANG_ENGLISH_GB = 20007 ;
  LANG_ENGLISH_GB_STR = 'English (British)'#0;

  LANG_ENGLISH_US = 20008 ;
  LANG_ENGLISH_US_STR = 'English (American)'#0;

  LANG_SPANISH = 20009 ;
  LANG_SPANISH_STR = 'Spanish'#0;

  LANG_ESTONIAN = 20010 ;
  LANG_ESTONIAN_STR = 'Estonian'#0;

  LANG_FINNISH = 20011 ;
  LANG_FINNISH_STR = 'Finnish'#0;

  LANG_FRENCH = 20012 ;
  LANG_FRENCH_STR = 'French'#0;

  LANG_HUNGARIAN = 20013 ;
  LANG_HUNGARIAN_STR = 'Hungarian'#0;

  LANG_INDONESIAN = 20014 ;
  LANG_INDONESIAN_STR = 'Indonesian'#0;

  LANG_ITALIAN = 20015 ;
  LANG_ITALIAN_STR = 'Italian'#0;

  LANG_JAPANESE = 20016 ;
  LANG_JAPANESE_STR = 'Japanese'#0;

  LANG_KOREAN = 20017 ;
  LANG_KOREAN_STR = 'Korean'#0;

  LANG_LITHUANIAN = 20018 ;
  LANG_LITHUANIAN_STR = 'Lithunanian'#0;

  LANG_LATVIAN = 20019 ;
  LANG_LATVIAN_STR = 'Latvian'#0;

  LANG_NORWEGIAN = 20020 ;
  LANG_NORWEGIAN_STR = 'Norwegian (Bokmal)'#0;

  LANG_DUTCH = 20021 ;
  LANG_DUTCH_STR = 'Dutch'#0;

  LANG_POLISH = 20022 ;
  LANG_POLISH_STR = 'Polish'#0;

  LANG_PORTUGESE = 20023 ;
  LANG_PORTUGESE_STR = 'Portugese'#0;

  LANG_PORTUGESE_BR = 20024 ;
  LANG_PORTUGESE_BR_STR = 'Portugese (Brazilian)'#0;

  LANG_PORTUGESE_PT = 20025 ;
  LANG_PORTUGESE_PT_STR = 'Portugese (Other)'#0;

  LANG_ROMANIAN = 20026 ;
  LANG_ROMANIAN_STR = 'Romanian'#0;

  LANG_RUSSIAN = 20027 ;
  LANG_RUSSIAN_STR = 'Russian'#0;

  LANG_SLOVAK = 20028 ;
  LANG_SLOVAK_STR = 'Slovak'#0;

  LANG_SLOVENIAN = 20029 ;
  LANG_SLOVENIAN_STR = 'Solvenian'#0;

  LANG_SWEDISH = 20030 ;
  LANG_SWEDISH_STR = 'Swedish'#0;

  LANG_TURKISH = 20031 ;
  LANG_TURKISH_STR = 'Turkish'#0;

  LANG_UKRAINIAN = 20032 ;
  LANG_UKRAINIAN_STR = 'Ukrainian'#0;

  LANG_CHINESE = 20033 ;
  LANG_CHINESE_STR = 'Chinese'#0;


procedure CloseCatalog;
procedure OpenCatalog(Loc: PLocale);
function GetLocString(Num: LongInt): STRPTR;

implementation

const
  Builtinlanguage = 'english'#0;
  Version = 0 ;
  Catalog: PCatalog = NIL ;

type

  TAppString = record
     id: LongInt;
     str: string;
  end;

  TAppStringArray = array[0..59] of TAppString;

const
  AppStrings: TAppStringArray = (
    (id: APP_DESC ; str: APP_DESC_STR ),
    (id: ERROR_SWITCH_AUTO ; str: ERROR_SWITCH_AUTO_STR ),
    (id: ERROR_SWITCH_SRC ; str: ERROR_SWITCH_SRC_STR ),
    (id: ERROR_SWITCH_DEST ; str: ERROR_SWITCH_DEST_STR ),
    (id: GUI_INPUT_LANG ; str: GUI_INPUT_LANG_STR ),
    (id: GUI_OUTPUT_LANG ; str: GUI_OUTPUT_LANG_STR ),
    (id: GUI_TRANSLATE ; str: GUI_TRANSLATE_STR ),
    (id: GUI_PLAIN ; str: GUI_PLAIN_STR ),
    (id: GUI_FANCY ; str: GUI_FANCY_STR ),
    (id: GUI_CLEAR ; str: GUI_CLEAR_STR ),
    (id: GUI_INSERT_TEXT ; str: GUI_INSERT_TEXT_STR ),
    (id: GUI_COPYTEXT ; str: GUI_COPYTEXT_STR ),
    (id: GUI_SAVEASUTF8 ; str: GUI_SAVEASUTF8_STR ),
    (id: GUI_SAVEASPDF ; str: GUI_SAVEASPDF_STR ),
    (id: GUI_SWITCH ; str: GUI_SWITCH_STR ),
    (id: MENU_TEXT ; str: MENU_TEXT_STR ),
    (id: MENU_PASTE ; str: MENU_PASTE_STR ),
    (id: MENU_CLEAR ; str: MENU_CLEAR_STR ),
    (id: MENU_PASTEINPUT ; str: MENU_PASTEINPUT_STR ),
    (id: MENU_PASTEINPUT_KEY ; str: MENU_PASTEINPUT_KEY_STR ),
    (id: MENU_COPY ; str: MENU_COPY_STR ),
    (id: MENU_COPYRESULT ; str: MENU_COPYRESULT_STR ),
    (id: MENU_COPYRESULT_KEY ; str: MENU_COPYRESULT_KEY_STR ),
    (id: MENU_CLEARALL ; str: MENU_CLEARALL_STR ),
    (id: MENU_CLEARALL_KEY ; str: MENU_CLEARALL_KEY_STR ),
    (id: LANG_AUTO ; str: LANG_AUTO_STR ),
    (id: LANG_BULGARIAN ; str: LANG_BULGARIAN_STR ),
    (id: LANG_CZECH ; str: LANG_CZECH_STR ),
    (id: LANG_DANISH ; str: LANG_DANISH_STR ),
    (id: LANG_GERMAN ; str: LANG_GERMAN_STR ),
    (id: LANG_GREEK ; str: LANG_GREEK_STR ),
    (id: LANG_ENGLISH ; str: LANG_ENGLISH_STR ),
    (id: LANG_ENGLISH_GB ; str: LANG_ENGLISH_GB_STR ),
    (id: LANG_ENGLISH_US ; str: LANG_ENGLISH_US_STR ),
    (id: LANG_SPANISH ; str: LANG_SPANISH_STR ),
    (id: LANG_ESTONIAN ; str: LANG_ESTONIAN_STR ),
    (id: LANG_FINNISH ; str: LANG_FINNISH_STR ),
    (id: LANG_FRENCH ; str: LANG_FRENCH_STR ),
    (id: LANG_HUNGARIAN ; str: LANG_HUNGARIAN_STR ),
    (id: LANG_INDONESIAN ; str: LANG_INDONESIAN_STR ),
    (id: LANG_ITALIAN ; str: LANG_ITALIAN_STR ),
    (id: LANG_JAPANESE ; str: LANG_JAPANESE_STR ),
    (id: LANG_KOREAN ; str: LANG_KOREAN_STR ),
    (id: LANG_LITHUANIAN ; str: LANG_LITHUANIAN_STR ),
    (id: LANG_LATVIAN ; str: LANG_LATVIAN_STR ),
    (id: LANG_NORWEGIAN ; str: LANG_NORWEGIAN_STR ),
    (id: LANG_DUTCH ; str: LANG_DUTCH_STR ),
    (id: LANG_POLISH ; str: LANG_POLISH_STR ),
    (id: LANG_PORTUGESE ; str: LANG_PORTUGESE_STR ),
    (id: LANG_PORTUGESE_BR ; str: LANG_PORTUGESE_BR_STR ),
    (id: LANG_PORTUGESE_PT ; str: LANG_PORTUGESE_PT_STR ),
    (id: LANG_ROMANIAN ; str: LANG_ROMANIAN_STR ),
    (id: LANG_RUSSIAN ; str: LANG_RUSSIAN_STR ),
    (id: LANG_SLOVAK ; str: LANG_SLOVAK_STR ),
    (id: LANG_SLOVENIAN ; str: LANG_SLOVENIAN_STR ),
    (id: LANG_SWEDISH ; str: LANG_SWEDISH_STR ),
    (id: LANG_TURKISH ; str: LANG_TURKISH_STR ),
    (id: LANG_UKRAINIAN ; str: LANG_UKRAINIAN_STR ),
    (id: LANG_CHINESE ; str: LANG_CHINESE_STR ),
    (id: 0 ; str: '' )
    );

procedure CloseCatalog;
begin
  if Assigned(Catalog) then
  begin
    Locale.CloseCatalog(Catalog) ;
    Catalog := nil;
  end;
end;

procedure OpenCatalog(loc: PLocale);
var
   tags: array[0..7] of PtrUInt;
begin
  CloseCatalog;
  if (Catalog = nil) and (LocaleBase <> NIL) then
  begin
    tags[0] := OC_BuiltInLanguage; tags[1] := 0; //AsTag(PChar(builtinlanguage));
    tags[2] := OC_Version;         tags[3] := Version;
    tags[4] := TAG_END;
  end;
  Catalog := Locale.OpenCatalogA(loc, PChar('AmiTranslate.catalog'#0), @tags);
end;

function GetLocString(Num: LongInt): STRPTR;
var
  i: LongInt;
  Idx: Integer;
  Default: STRPTR;
begin
  Idx := 0;

  for i := 1 to High(Appstrings) do
  begin
    if AppStrings[i].id = Num then
    begin
      Idx := i;
      Break;
    end;
  end;

  if Idx > 0 then
    Default := PChar(AppStrings[i].str)
  else
    Default := nil;

  if Assigned(Catalog) then
    GetLocString := Locale.GetCatalogStr(Catalog, Num, Default)
  else
    GetLocString := Default
end;

initialization
  OpenCatalog(nil);
finalization
  CloseCatalog;
end.
