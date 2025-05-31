program AmiTranslate;

{$mode objfpc}{$H+}
uses
  Classes, SysUtils,
  MUI, fphttpclient,
  {$ifdef MorphOS}
  morphossslsockets, openssl3,
  {$else}
  amisslsockets, amissl,
  {$endif}
  icon, workbench,
  MUIClass.Base, MUIClass.Area, MUIClass.Dialog, IniFiles,
  MUIClass.Window, MUIClass.Group, MUIClass.TextEdit,
  fpjson, jsonparser, FPPDF, AmiTranslatelocale,
  MUIClass.Menu, TTFPanel, langunit, muihelper;

const
  VERSION = '$VER: AmiTranslate 0.5 (12.01.2025)';

  {
  SrcLanguages: array of string = (
    'Automatic',
    'BG - Bulgarian',
    'CS - Czech',
    'DA - Danish',
    'DE - German',
    'EL - Greek',
    'EN - English',
    'ES - Spanish',
    'ET - Estonian',
    'FI - Finnish',
    'FR - French',
    'HU - Hungarian',
    'ID - Indonesian',
    'IT - Italian',
    //'JA - Japanese',
    //'KO - Korean',
    'LT - Lithuanian',
    'LV - Latvian',
    'NB - Norwegian (Bokmal)',
    'NL - Dutch',
    'PL - Polish',
    'PT - Portuguese',
    'RO - Romanian',
    'RU - Russian',
    'SK - Slovak',
    'SL - Slovenian',
    'SV - Swedish',
    'TR - Turkish',
    'UK - Ukrainian'//,
    //'ZH - Chinese'
    ); }

  //JapaneseLang = 'JA - Japanese';
  //KoreanLan = 'KO - Korean';
  //ChineseLang = 'ZH - Chinese (simplified)';
  {
  DestLanguages: array of string = (
    'BG - Bulgarian',
    'CS - Czech',
    'DA - Danish',
    'DE - German',
    'EL - Greek',
    'EN-GB - English (British)',
    'EN-US - English (American)',
    'ES - Spanish',
    'ET - Estonian',
    'FI - Finnish',
    'FR - French',
    'HU - Hungarian',
    'ID - Indonesian',
    'IT - Italian',
    JapaneseLang,
    //KoreanLan,
    'LT - Lithuanian',
    'LV - Latvian',
    'NB - Norwegian (Bokmal)',
    'NL - Dutch',
    'PL - Polish',
    'PT-BR - Portuguese (Brazilian)',
    'PT-PT - Portuguese (other)',
    'RO - Romanian',
    'RU - Russian',
    'SK - Slovak',
    'SL - Slovenian',
    'SV - Swedish',
    'TR - Turkish',
    'UK - Ukrainian'//,
    //ChineseLang
  );
  }

const
  BaseURL = 'https://api-free.deepl.com/v2/translate';

// we need an API Key
// if you get an error message here, you most likely forget to create this
// file with your DeepL API Key, see also deeplkey.inc.template for an example
{$I deeplkey.inc}



type
  { TMainWindow }

  TMainWindow = class(TMUIWindow)
  private
    InputTxt: TMUITextEdit;
    OutputTxt: TMUITextEdit;
    InLang: TMUICycle;
    OutLang: TMUICycle;
    SL: TStringList;
    TranslateBtn: TMUIButton;
    DetLang: TMUIText;
    TransText: UTF8String;
    RawStr: RawByteString;
    TargetLang: string;
    Ini: TIniFile;
    DeepLKey: string;
    NiceText: TTTFPanel;
    DetSrcLang: string;

    procedure ClearAll(Sender: TObject);
    procedure CloseWindow(Sender: TObject; var CloseAction: TCloseAction);
    procedure InputClear(Sender: TObject);
    procedure InputPaste(Sender: TObject);
    procedure OutputCopy(Sender: TObject);
    procedure SavePDF(Sender: TObject);
    procedure SaveUTF8(Sender: TObject);
    procedure StartTranslate(Sender: TObject);
    procedure SwitchLanguages(Sender: TObject);
    function Translate(Txt: string): Boolean;
    procedure WinShow(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{ TMainWindow }

function TMainWindow.Translate(Txt: string): Boolean;
var
  NTxt, SrcLang, DestLang, FinalString: String;
  FromIdx, DestIdx, i: Integer;
  HP: TFPHTTPClient;
  c: Char;
  FormText: string;
  JData: TJSONData;
  JObject: TJSONObject;
begin
  Result := True;
  SL.Clear;
  NTxt := AnsiToUtf8(Txt);
  DetLang.Contents := '';

  FromIdx := InLang.Active;
  SrcLang := '';
  if (FromIdx >= 0) and (FromIdx <= High(SrcLanguages)) then
  begin
    SrcLang := SrcLanguages[FromIdx];
    if Pos(' - ', SrcLang) > 0 then
      SrcLang := Copy(SrcLang, 1, Pos(' - ', SrcLang) - 1)
    else
      SrcLang := '';
  end;

  DestIdx := OutLang.Active;
  DestLang := '';
  if (DestIdx >= 0) and (DestIdx <= High(DestLanguages)) then
  begin
    DestLang := DestLanguages[DestIdx];
    if Pos(' - ', DestLang) > 0 then
      DestLang := Copy(DestLang, 1, Pos(' - ', DestLang) - 1)
    else
      DestLang := '';
  end;

  FinalString := '';
  for i := 1 to Length(NTxt) do
  begin
    c := NTxt[i];
    if c in ['a'..'z', 'A'..'Z','0'..'9'] then
      FinalString := FinalString + c
    else
      FinalString := FinalString + '%' + IntToHex(Ord(c), 2);
  end;
  Ntxt := stringreplace(NTxt, '&', '', [rfReplaceAll]);
  Ntxt := stringreplace(NTxt, '=', '', [rfReplaceAll]);
  FormText := 'text=' + NTxt + '&target_lang=' + DestLang;
  if SrcLang <> '' then
    FormText := FormText + '&source_lang=' + SrcLang;
  //
  HP := TFPHTTPClient.Create(nil);
  try
    HP.AllowRedirect := True;
    Hp.AddHeader('User-Agent', Copy(Version, 7) + ' ' + {$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%});
    hp.AddHeader('Authorization', 'DeepL-Auth-Key ' + Key);
    RawStr := hp.FormPost(BaseURL, FormText);

    JData := GetJSON(RawStr, False);
    JObject := JData as TJSONObject;
    JData := JObject.FindPath('translations[0]');
    if JData is TJSONObject then
    begin
      JObject := JData as TJSONObject;
      if SrcLang = '' then
        SrcLang := JObject.Get('detected_source_language', '');
      TransText := JObject.Get('text', '');
      OutputTxt.Text := Utf8ToAnsi(TransText);
      NiceText.Text := TransText;
      DetLang.Contents := SrcLang;
      DetSrcLang := SrcLang;
      TargetLang := DestLang;
    end
    else
    begin
      ShowMessage('Answer:' + RawStr);
      SL.Clear;
    end;
  finally
    hp.Free;
  end;

end;

procedure TMainWindow.WinShow(Sender: TObject);
begin
  InputTxt.Clear;
end;

procedure TMainWindow.StartTranslate(Sender: TObject);
begin
  try
    TranslateBtn.Disabled := True;
    Translate(InputTxt.Text);
  finally
    TranslateBtn.Disabled := False;
  end;
end;

procedure TMainWindow.SwitchLanguages(Sender: TObject);
var
  Src, Dest, s: string;
  OutIdx, InIdx, i: Integer;
begin
  //
  if InLang.Active = 0 then
  begin
    // Automatic Active
    if DetSrcLang = '' then
    begin
      ShowMessage(GetLocString(ERROR_SWITCH_AUTO));//'Automatic Source language selected and no translation done.');
      Exit;
    end;
    Src := Copy(DetSrcLang, 1, 2);
  end
  else
  begin
    Src := Copy(InLang.Entries[InLang.Active], 1, 2);
  end;
  Dest :=  Copy(OutLang.Entries[OutLang.Active], 1, 2);

  // Search in Lang in output
  OutIdx := -1;
  for i := 0 to High(OutLang.Entries) do
  begin
    s := Copy(OutLang.Entries[i], 1, 2);
    if s = Src then
    begin
      OutIdx := i;
      Break;
    end;
  end;
  if OutIdx < 0 then
  begin
    ShowMessage(GetLocString(ERROR_SWITCH_SRC) + ' "' + Src + '"'); //'Input langauge not found in possible output languages, "' + Src + '"');
    Exit;
  end;

  InIdx := -1;
  for i := 0 to High(InLang.Entries) do
  begin
    s := Copy(InLang.Entries[i], 1, 2);
    if s = Dest then
    begin
      InIdx := i;
      Break;
    end;
  end;
  if InIdx < 0 then
  begin
    ShowMessage(GetLocString(ERROR_SWITCH_DEST) + ' "' + Dest + '"'); //'Output langauge not found in possible input languages, "' + Dest + '"');
    Exit;
  end;

  InLang.Active := InIdx;
  OutLang.Active := OutIdx;
end;

procedure TMainWindow.SaveUTF8(Sender: TObject);
var
  s: RawByteString;
  FS: TFileStream;
  FD: TFileDialog;
begin
  FS := nil;
  FD := TFileDialog.Create;
  try
    s := TransText;
    FD.Directory := Ini.ReadString('Path', 'TXT', 'Ram:');
    FD.TitleText := 'Choose name for UTF8 export';
    if FD.Execute then
    begin
      FS := TFileStream.Create(FD.FileName, fmCreate);
      FS.Write(s[1], Length(s));
      Ini.WriteString('Path', 'TXT', ExtractFileDir(FD.FileName));
    end;
  finally
    FD.Free;
    FS.Free;
  end;
end;

procedure TMainWindow.SavePDF(Sender: TObject);
var
  PDF: TPDFDocument;
  StdFtHelvetica, y, i: Integer;
  P: TPDFPage;
  s: string;
  FD: TFileDialog;
  Lines: TStringArray;
  p1: SizeInt;
  IsJapanese: Boolean;
begin
  PDF := nil;
  FD := TFileDialog.Create;
  try
    FD.Directory := Ini.ReadString('Path', 'PDF', 'Ram:');
    FD.Pattern := '#?.pdf';
    FD.TitleText := 'Choose name for PDF export';
    IsJapanese := (Pos('JA', TargetLang) = 1) or (Pos('KO', TargetLang) = 1) or (Pos('ZH', TargetLang) = 1);
    if FD.Execute then
    begin
      PDF := TPDFDocument.Create(nil);
      PDF.Infos.Producer := 'Test';
      PDF.Infos.CreationDate := Now;
      PDF.Options := [poPageOriginAtTop{, poCompressFonts, poCompressImages}];
      if not IsJapanese then
        PDF.Options := PDF.Options + [poSubsetFont];
      PDF.DefaultOrientation := ppoPortrait;
      PDF.DefaultPaperType := ptA4;
      PDF.DefaultUnitOfMeasure := uomMillimeters;
      PDF.StartDocument;
      PDF.Sections.AddSection;
      PDF.Sections[0].AddPage(PDF.Pages.AddPage);
      if IsJapanese then
        StdFtHelvetica := PDF.AddFont('PROGDIR:NotoSansJP-Regular.ttf', 'NotoSans')
      else
        StdFtHelvetica := PDF.AddFont('PROGDIR:FreeSans.ttf', 'FreeSans');
      P:=PDF.Pages[0];
      P.SetFont(StdFtHelvetica, 14);
      s := TransText;
      Lines := s.split([#10]);
      y := 10;
      for i := 0 to High(Lines) do
      begin
        //writeln(i, '. "', Lines[i] , '" length: ', Length(Lines[i]));
        if Length(Lines[i]) > 80 then
        begin
          //writeln('wrap needed');
          s := Lines[i];
          while Length(s) > 80 do
          begin
            p1 := s.Substring(0, 80).LastDelimiter(' ');
            if p1 <= 0 then
              p1 := 80;
            //writeln('   @',y,' "', s.Substring(0, p1));
            P.WriteText(10, y, s.Substring(0, p1), 0, False, False);
            s := s.Remove(0, p1);
            y := y + 6;
          end;
          if s <> '' then
          begin
            //writeln('   @',y,' "', s);
            P.WriteText(10, y, s, 0, False, False);
            y := y + 6;
          end;
          y := y + 4;
        end
        else
        begin
          //writeln('no wrap needed');
          P.WriteText(10, y, Lines[i], 0, False, False);
          y := y + 10;
        end;
      end;
      PDF.SaveToFile(FD.FileName);
      Ini.WriteString('Path', 'PDF', ExtractFileDir(FD.FileName));
    end;
  finally
   PDF.Free;
   FD.Free;
  end;
end;

procedure TMainWindow.CloseWindow(Sender: TObject; var CloseAction: TCloseAction);
begin
  Ini.WriteInteger('GUI', 'InLang', InLang.Active);
  Ini.WriteInteger('GUI', 'OutLang', OutLang.Active);
end;

procedure TMainWindow.InputClear(Sender: TObject);
begin
  InputTxt.Clear;
end;

procedure TMainWindow.ClearAll(Sender: TObject);
begin
  InputTxt.Clear;
  OutputTxt.Clear;
end;

procedure TMainWindow.InputPaste(Sender: TObject);
begin
  InputTxt.Clear;
  InputTxt.Paste;
end;

procedure TMainWindow.OutputCopy(Sender: TObject);
begin
  OutputTxt.Copy;
end;

{ load a string tooltype from icon, if not found, return the "Default" string}
function GetStrToolType(DObj: PDiskObject; Entry: string; Default: string): string;
var
  Res: PChar;
begin
  Result := Default;
  // easier here to check if icon is found
  if not assigned(Dobj) then
    Exit;
  // and if there are tooltypes at all
  if not Assigned(Dobj^.do_Tooltypes) then
    Exit;
  // the actual search
  Res := FindToolType(Dobj^.do_Tooltypes, PChar(Entry));
  // check if found
  if Assigned(Res) then
    Result := Res;
end;

constructor TMainWindow.Create;
var
  Grp: TMUIGroup;
  SaveUTF8Btn, SavePDFBtn: TMUIButton;
  DObj: PDiskObject;
  AMenu, OMenu, MMenu: TMUIMenu;
  Pages: TMUIRegister;
begin
  inherited Create;

  self.ID := MAKE_ID('A','M','T','R');
  OnShow  := @WinShow;
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));

  OnCloseRequest  := @CloseWindow;

  DObj := GetDiskObject(PChar(ParamStr(0)));
  ScreenTitle := copy(Version, 7);
  if Assigned(DObj) then
    DeepLKey := GetStrToolType(DObj, 'DEEPLKEY', '');
  if DeepLKey <> '' then
  begin
    Title := 'AmiTranslate <custom API Key>';
  end
  else
  begin
    DeepLKey := Key;
    Title := 'AmiTranslate';
  end;

  SL := TStringList.Create;

  Horizontal := False;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz := True;
    Frame := MUIV_Frame_None;
    Parent := Self;
  end;

  with TMUIText.Create(GetLocString(GUI_INPUT_LANG)) do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;

  InputTxt := TMUITextEdit.Create;
  with InputTxt do
  begin
    Parent := Self;
  end;

  InLang := TMUICycle.Create;
  with InLang do
  begin
    Entries := SrcLanguages;
    Active := Ini.ReadInteger('GUI', 'InLang', 0);
    Parent := Grp;
  end;

  DetLang := TMUIText.Create('  ');
  DetLang.Frame := MUIV_Frame_None;
  DetLang.FixWidthTxt := ' WW ';
  DetLang.Parent := Grp;

  with TMUIButton.Create(GetLocString(GUI_SWITCH)) do
  begin
    FixWidthTxt := GetLocString(GUI_SWITCH); //'Switch';
    OnClick  := @SwitchLanguages;
    Parent := Grp;
  end;


  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz := True;
    Frame := MUIV_Frame_None;
    Parent := Self;
  end;

  with TMUIText.Create(GetLocString(GUI_OUTPUT_LANG)) do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;

  OutLang := TMUICycle.Create;
  with OutLang do
  begin
    Entries := DestLanguages;
    Active := Ini.ReadInteger('GUI', 'OutLang', 5);
    Parent := Grp;
  end;

  TranslateBtn := TMUIButton.Create(GetLocString(GUI_TRANSLATE));
  with TranslateBtn do
  begin
    FixWidthTxt := '  ' + GetLocString(GUI_TRANSLATE) + '  ';//' Translate ';
    OnClick  := @StartTranslate;
    Parent := Grp;
  end;

  Pages := TMUIRegister.Create;
  with Pages do
  begin
    Titles := [GetLocString(GUI_PLAIN), GetLocString(GUI_FANCY)];//['Plain', 'Fancy'];
    Parent := Self;
  end;


  OutputTxt := TMUITextEdit.Create;
  with OutputTxt do
  begin
    Parent := Pages;
  end;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz := True;
    Frame := MUIV_Frame_None;
    Parent := Self;
  end;

  with TMUIButton.Create(GetLocString(GUI_CLEAR)) do
  begin
    FixWidthTxt := GetLocString(GUI_CLEAR); //'Clear';
    OnClick   := @ClearAll;
    Parent := Grp;
  end;
  with TMUIButton.Create(GetLocString(GUI_INSERT_TEXT)) do
  begin
    FixWidthTxt := GetLocString(GUI_INSERT_TEXT); //'Insert Text';
    OnClick   := @InputPaste;
    Parent := Grp;
  end;

  TMUIRectangle.Create.Parent := Grp;

  with TMUIButton.Create(GetLocString(GUI_COPYTEXT)) do
  begin
    FixWidthTxt := GetLocString(GUI_COPYTEXT); //'Copy Text';
    OnClick   := @OutputCopy;
    Parent := Grp;
  end;

  SaveUTF8Btn := TMUIButton.Create(GetLocString(GUI_SAVEASUTF8));
  with SaveUTF8Btn do
  begin
    FixWidthTxt := GetLocString(GUI_SAVEASUTF8); //'Save as UTF-8';
    OnClick   := @SaveUTF8;
    Parent := Grp;
  end;

  SavePDFBtn := TMUIButton.Create(GetLocString(GUI_SAVEASPDF));
  with SavePDFBtn do
  begin
    FixWidthTxt := GetLocString(GUI_SAVEASPDF); //'Save as PDF';
    OnClick    := @SavePDF;
    Parent := Grp;
  end;

  // Menus
  InputTxt.ContextMenu := TMUIMenuStrip.Create;
  Menustrip := TMUIMenustrip.Create;

  MMenu := TMUIMenu.Create;
  MMenu.Title := GetLocString(MENU_TEXT); //'Text';
  MMenu.Parent := Menustrip;

  AMenu := TMUIMenu.Create;
  AMenu.Parent := InputTxt.ContextMenu;
  with TMUIMenuItem.Create do
  begin
    Title := GetLocString(MENU_PASTE); //'Paste';
    OnTrigger  := @InputPaste;
    Parent := AMenu;
  end;

  with TMUIMenuItem.Create do
  begin
    Title := GetLocString(MENU_CLEAR); //'Clear';
    OnTrigger   := @InputClear;
    Parent := AMenu;
  end;

  with TMUIMenuItem.Create do
  begin
    Title := GetLocString(MENU_PASTEINPUT); //'Paste to Source';
    ShortCut := GetLocString(MENU_PASTEINPUT_KEY);// 'V';
    OnTrigger  := @InputPaste;
    Parent := MMenu;
  end;

  OutputTxt.ContextMenu := TMUIMenuStrip.Create;
  OMenu := TMUIMenu.Create;
  OMenu.Parent := OutputTxt.ContextMenu;
  with TMUIMenuItem.Create do
  begin
    Title := GetLocString(MENU_COPY); //'Copy';
    OnTrigger  := @OutputCopy;
    Parent := OMenu;
  end;

  with TMUIMenuItem.Create do
  begin
    Title := GetLocString(MENU_COPYRESULT); //'Copy Result';
    ShortCut := GetLocString(MENU_COPYRESULT_KEY); //'C';
    OnTrigger  := @OutputCopy;
    Parent := MMenu;
  end;

  with TMUIMenuItem.Create do
  begin
    Title := GetLocString(MENU_CLEARALL); //'Clear All';
    ShortCut := GetLocString(MENU_CLEARALL_KEY); //'D';
    OnTrigger   := @ClearAll;
    Parent := MMenu;
  end;


  NiceText := TTTFPanel.Create;
  with NiceText do
  begin
    Parent := Pages;
  end;

end;

destructor TMainWindow.Destroy;
begin
  Ini.UpdateFile;
  Ini.Free;
  SL.Free;
  inherited Destroy;
end;

begin
  TMainWindow.Create;

  MUIApp.Author := 'Marcus "ALB42" Sackrow';
  MUIApp.Description := GetLocString(APP_DESC);//'Translates Texts with DeepL';
  MUIApp.Run;

end.

