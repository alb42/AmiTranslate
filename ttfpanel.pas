unit TTFPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Utility,
  {$ifdef AROS}
  ttenginearos,
  {$endif}
  {$ifdef MorphOS}
  ttenginemorphos,
  {$endif}
  {$ifdef Amiga68k}
  ttengine,
  {$endif}
  tb_UTF8,
  AGraphics, Intuition, MUI,
  MUIClass.DrawPanel, MUIClass.Group, MUIClass.Gadget;

type

  { TTTFPanel }

  TTTFPanel = class(TMUIGroup)
  private
    FDrawPanel: TMUIDrawPanel;
    FScrollbar: TMUIScrollbar;
    FText: string;
    jpfont: Pointer;
    procedure DrawObject(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
    procedure ScrollBarEvent(Sender: TObject);
    procedure SetFText(AValue: string);
    procedure UpdateScrollbar;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Text: string read FText write SetFText;
  end;

implementation



{ TTTFPanel }

procedure TTTFPanel.DrawObject(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
var
  Lines: TStringArray;
  y, i: Integer;
  s: tb_UTF8.TIbUtf8;
  s2: string;
  todraw: LongWord;
  s1: IbUtf8;
begin
  SetAPen(rp, 1);
  GfxMove(RP, DrawRect.Left + 10, DrawRect.Top + 16);
  y := 0;
  if Assigned(jpfont) then
  begin
    //GfxText(rp, 'has font', 8);
    TT_SetFont(RP, jpfont);
    TT_SetAttrs(RP, [
      TT_Screen, AsTag(IntuitionBase^.ActiveScreen),
      TT_Encoding, TT_Encoding_UTF8
      ]);
    Lines := FText.split([#10]);
    y := 16 - FScrollbar.First;
    s := TIbUtf8.Create;
    for i := 0 to High(Lines) do
    begin
      s.Text := Lines[i];
      repeat
        todraw := 18;
        if toDraw > s.Length then
          toDraw := s.Length;
        //writeln('todraw ', todraw, ' length char ', s.length, ' "', s.ToString, '"');
        GfxMove(RP, DrawRect.Left + 10, DrawRect.Top + y);
        if todraw = 0 then
        begin
          s.Text := '';
          break;
        end
        else
        begin
          s1 := s.Copy(1, todraw);
          //writeln('  to print '  + s.Text);
          TT_Text(RP, PChar(s1.Text), s1.NumberOfChars);
          S1 := nil;
          s.Delete(1, todraw);
        end;
        y := y + 16;
      until s.Length = 0;
      y := y + 16;
    end;
    S.Free;

    //s := FText;
    //writeln(s.length);
    //TT_Text(RP, PChar(FText), FText.Length);//Length(Utf8ToAnsi(FText)));

    TT_DoneRastPort(RP);
  end
  else
  begin
    y := 0;
    if Assigned(TTEngineBase) then
      s2 := 'Font not found'
    else
      s2 := 'TTEngine not found';
    GfxText(rp, PChar(s2), Length(s2));
  end;

  FScrollbar.Entries := Y;
  FScrollbar.Visible := FDrawPanel.Height;

end;

procedure TTTFPanel.ScrollBarEvent(Sender: TObject);
begin
  FDrawPanel.RedrawObject;
end;

procedure TTTFPanel.SetFText(AValue: string);
begin
  if FText = AValue then Exit;
  FText := AValue;
  UpdateScrollbar;
  FDrawPanel.RedrawObject;
end;

procedure TTTFPanel.UpdateScrollbar;
var
  Lines: TStringArray;
  s: TIbUtf8;
  y, i, todraw: Integer;
  //s1: IbUtf8;
begin
  Lines := FText.split([#10]);
  y := 0;
  s := TIbUtf8.Create;
  for i := 0 to High(Lines) do
  begin
    s.Text := Lines[i];
    repeat
      todraw := 18;
      if toDraw > s.Length then
        toDraw := s.Length;
      //writeln('todraw ', todraw, ' length char ', s.length, ' "', s.ToString, '"');
      if todraw = 0 then
      begin
        s.Text := '';
        break;
      end
      else
      begin
        //s1 := s.Copy(1, todraw);
        //writeln('  to print '  + s.Text);
        //TT_Text(RP, PChar(s1.Text), s1.NumberOfChars);
        //S1 := nil;
        s.Delete(1, todraw);
      end;
      y := y + 16;
    until s.Length = 0;
    y := y + 16;
  end;
  S.Free;

  if y < FDrawPanel.Height then
    y := FDrawPanel.Height;
  FScrollbar.First := 0;
  FScrollbar.Entries := Y;
  FScrollbar.Visible := FDrawPanel.Height;
end;

constructor TTTFPanel.Create;
begin
  inherited Create;
  Spacing := 0;
  FDrawPanel := TMUIDrawPanel.Create;
  Horiz := True;
  with FDrawPanel do
  begin
    OnDrawObject  := @DrawObject;
    DefHeight := 100;
    DefWidth := 100;
    MinHeight := 100;
    MinWidth := 100;
    Parent := Self;
  end;

  FScrollbar := TMUIScrollbar.Create;
  with FScrollbar do
  begin
    Frame := MUIV_Frame_None;
    Parent := Self;
  end;
  FScrollbar.OnFirstChange := @ScrollBarEvent;
  jpfont := nil;
  if Assigned(TTEngineBase) then
  begin
    jpfont := TT_OpenFont([
      TT_FontFile, AsTag('PROGDIR:NotoSansJP-Regular.ttf'),
      //TT_FontFile, AsTag('PROGDIR:FreeSans.ttf'),
      TT_FontSize, 16
      ]);
  end;
end;

destructor TTTFPanel.Destroy;
begin
  if Assigned(JPfont) then
    TT_CloseFont(jpFont);
  inherited Destroy;
end;

end.

