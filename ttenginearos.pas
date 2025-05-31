{
  This file is part of the Free Pascal run time library.

  A file in Amiga system run time library.
  Copyright (c) 2003 by Nils Sjöholm.
  member of the Amiga RTL development team.
  AROS Version (c) 2023 by Marcus Sackrow

  This is a unit for ttengine.library

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
UNIT ttenginearos;
{$ENDIF FPC_DOTTEDUNITS}

INTERFACE
{$IFDEF FPC_DOTTEDUNITS}
USES Amiga.Core.Exec,Amiga.Core.Utility,Amiga.Core.Agraphics;
{$ELSE FPC_DOTTEDUNITS}
USES Exec,utility,agraphics;
{$ENDIF FPC_DOTTEDUNITS}

var
  TTEngineBase: PLibrary = nil;

const
  TTENGINENAME: PAnsiChar = 'ttengine.library';

  { $VER: ttengine.h 7.2 (5.4.2005) (c) by Grzegorz Kraszewski 2002 - 2005. }

  const
     TTENGINEVERSION = 7;
     TTENGINEMINVERSION = 2;
  { Tags  }
  { Tags applicability legend:  }
  { O - TT_OpenFont()  }
  { G - TT_GetAttrs()  }
  { S - TT_SetAttrs()  }
  { P - TT_GetPixmap()  }
  { ---- name -------------------- value ----- applicability  }
  
    TT_FontFile = $6EDA0000;     { OG..  }
  
    TT_FontStyle = $6EDA0001;    { OG..  }
    TT_FontStyle_Regular = 0;
    TT_FontStyle_Italic = 1;
  
    TT_FamilyTable = $6EDA0002;  { O...  }
  
    TT_FontSize = $6EDA0003;     { OG..  }
  
    TT_FontWeight = $6EDA0004;   { OG..  }
    TT_FontWeight_Normal = 400;
    TT_FontWeight_Bold = 700;
  
    TT_ColorMap = $6EDA0005;     { O...  }
  
    TT_Screen = $6EDA0006;       { O...  }
  
    TT_Window = $6EDA0007;       { O...  }
  
    TT_FontAscender = $6EDA0008; { .G..  }
  
    TT_FontDescender = $6EDA0009; { .G..  }
  
    TT_Antialias = $6EDA000F;    { .GSP  }
    TT_Antialias_Auto = 0;
    TT_Antialias_Off = 1;
    TT_Antialias_On = 2;
  
     TT_Encoding = $6EDA0010;    { .GSP  }
  {  All encoding numbers (excluding TT_Encoding_Default) are equal to IANA 
     registered encoding numbers }
    TT_Encoding_Default = 0;   { use ENV:ttfcodepage or ISO-8859-1 if not found  }  
    TT_Encoding_ISO8859_1 = 4; { Western Europe and US  }  
    TT_Encoding_ISO8859_2 = 5; { Eastern Europe  }
    TT_Encoding_ISO8859_3 = 6;
    TT_Encoding_ISO8859_4 = 7;
    TT_Encoding_ISO8859_5 = 8;
    TT_Encoding_ISO8859_6 = 9;
    TT_Encoding_ISO8859_7 = 10;
    TT_Encoding_ISO8859_8 = 11;
    TT_Encoding_ISO8859_9 = 12;
    TT_Encoding_ISO8859_10 = 13;
    TT_Encoding_ISO8859_11 = 14;
    TT_Encoding_ISO8859_13 = 109;
    TT_Encoding_ISO8859_14 = 110;
    TT_Encoding_ISO8859_15 = 111;
    TT_Encoding_ISO8859_16 = 112;
    TT_Encoding_UTF16_BE = 1013;
    TT_Encoding_UTF32_BE = 1018;
    TT_Encoding_UTF8 = 106;
    TT_Encoding_UTF16_LE = 1014;
    TT_Encoding_UTF32_LE = 1019;
    TT_Encoding_UTF16 = 1015;
    TT_Encoding_UTF32 = 1017;
  
    TT_FontName = $6EDA0011;      { .G..  }  
    TT_FamilyName = $6EDA0012;    { .G..  }  
    TT_SubfamilyName = $6EDA0013; { .G..  }
    TT_Transparency = $6EDA0014;  { .GS.  from 0 to 255  }  
    TT_ScaleX = $6EDA0015;        { O.SP  single precision floating point +- 0.01 to 100  }  
    TT_ScaleY = $6EDA0016;        { O.SP  single precision floating point +- 0.01 to 100  }
  
    TT_SoftStyle = $6EDA0017;     { ..SP (V5)  }
    TT_SoftStyle_None = $0000;
    TT_SoftStyle_Underlined = $0001;
    TT_SoftStyle_DblUnderlined = $0002;
    TT_SoftStyle_Overstriked = $0004;
    TT_SoftStyle_DblOverstriked = $0008;
  
    TT_Foreground = $6EDA0018;        { ..S.  foreground RGB value }
    TT_Foreground_UseRastPort = -(1);
  
    TT_Background = $6EDA0019;        { ..S.  background RGB value }
    TT_Background_UseRastPort = -(1);
  
    TT_FontMaxTop = $6EDA001E;        { .G..  } 
    TT_FontMaxBottom = $6EDA001F;     { .G..  }
    TT_FontDesignHeight = $6EDA0020;  { .G..  }
    TT_FontRealAscender = $6EDA0021;  { .G..  }
    TT_FontRealDescender = $6EDA0022; { .G..  }
    TT_FontAccentedAscender = $6EDA0023;  { .G..  }

    TT_CustomEncoding = $6EDA0024;  { ..SP  }
    TT_Gamma = $6EDA0025;          { .GS.  gettable from V7.2 }
    TT_FontBaseline = TT_FontMaxTop;            {  V6.7 }
    TT_FontFixedWidth = $6EDA0026;  { OG.. V6.7 }
    TT_FontHeight = $6EDA0027;  { .G..  V6.7 }
    TT_FontWidth = $6EDA0028;  { .G..  V6.7 }
    TT_DiskFontMetrics = $6EDA0029;  { ..SP  V6.7 }
    TT_ForceFixedWidth = $6EDA0030;  { ..SP  V7.2 }

  { Structure returned by TT_GetPixmap() (V5) }
type
  PTT_Pixmap = ^TTT_Pixmap;
  TTT_Pixmap = record
    ttp_Size: ULONG;      { size of the structure inculdung this field  }
    ttp_Width: ULONG;     { also equal to bytes per row  }
    ttp_Height: ULONG;    { number of rows  }
    ttp_Data: PByte;      { grayscale pixmap data  }
  end;

  { font requester attributes (V6)  }

const
  TTRQ_Window = $6EDA2000;          { struct Window ,   NULL               }  
  TTRQ_PubScreenName = $6EDA2001;   { STRPTR,           NULL [Workbench]   }
  TTRQ_Screen = $6EDA2002;          { struct Screen ,   NULL               }  
  TTRQ_SleepWindow = $6EDA2003;     { BOOL,             FALSE              }  
  TTRQ_TitleText = $6EDA2004;       { STRPTR,           "Select TrueType font" or localized  } 
  TTRQ_PositiveText = $6EDA2005;    { STRPTR,           "OK" or localized  }
  TTRQ_NegativeText = $6EDA2006;    { STRPTR,           "Cancel" or localized  }
  TTRQ_InitialLeftEdge = $6EDA2007; { WORD,             centered on screen  }
  TTRQ_InitialTopEdge = $6EDA2008;  { WORD,             centered on screen  }
  TTRQ_InitialWidth = $6EDA2009;    { WORD,             max(200, 25% of sceeen width)  }
  TTRQ_InitialHeight = $6EDA200A;   { WORD,             max(200, 50% of screen height)  }
  TTRQ_DoSizes = $6EDA2000;         { BOOL,             TRUE               }
  TTRQ_DoWeight = $6EDA200C;        { BOOL,             FALSE             }
  TTRQ_DoStyle = $6EDA200D;         { BOOL,             FALSE             }
  TTRQ_Activate = $6EDA200E;        { BOOL,             TRUE              }
  TTRQ_InitialSize = $6EDA200F;     { LONG,             0 [no size]       }
  TTRQ_InitialName = $6EDA2010;     { STRPTR,           NULL [no name]    }
  TTRQ_InitialStyle = $6EDA2011;    { LONG,             TT_FontStyle_Regular }
  TTRQ_DoPreview = $6EDA2012;       { BOOL,             FALSE             }
  TTRQ_FixedWidthOnly = $6EDA2013;  { BOOL,             FALSE             }


function TT_AllocRequest: APTR; syscall TTEngineBase 17;
procedure TT_CloseFont(Font: APTR); syscall TTEngineBase 7;
procedure TT_DoneRastPort(Rp: PRastPort); syscall TTEngineBase 16;
procedure TT_FreePixmap(Pixmap: PTT_Pixmap); syscall TTEngineBase 15;
procedure TT_FreeRequest(Request: APTR); syscall TTEngineBase 19;
function TT_GetAttrsA(Rp: PRastPort; Taglist: PTagItem): LongWord; syscall TTEngineBase 10;
function TT_GetPixmapA(Font: APTR; Text: PChar; Count: LongWord; Taglist: PTagItem): PTT_Pixmap; syscall TTEngineBase 14;
function TT_OpenFontA(Taglist: PTagItem): APTR; syscall TTEngineBase 5;
function TT_RequestA(Request: APTR; Taglist: PTagItem): PTagItem; syscall TTEngineBase 18;
function TT_SetAttrsA(Rp: PRastPort; Taglist: PTagItem): LongWord; syscall TTEngineBase 9;
function TT_SetFont(Rp: PRastPort; Font: APTR): WordBool; syscall TTEngineBase 6;
procedure TT_Text(Rp: PRastPort; Text: PChar; Count: LongWord); syscall TTEngineBase 8;
procedure TT_TextExtent(Rp: PRastPort; Text: PChar; Count: LongInt; Te: PTextExtent); syscall TTEngineBase 12;
function TT_TextFit(Rp: PRastPort; Text: PChar; Count: LongWord; Te: PTextExtent; Tec: PTextExtent; Dir: LongInt; CWidth: LongWord; CHeight: LongWord): LongWord; syscall TTEngineBase 13;
function TT_TextLength(Rp: PRastPort; Text: PChar; Count: LongWord): LongWord; syscall TTEngineBase 11;
{
 Functions and procedures with array of PtrUInt go here
}
function TT_GetAttrs(Rp: PRastPort; const Taglist: array of PtrUInt): LongWord;
function TT_GetPixmap(Font: APTR; Text: PChar; Count: LongWord; const Taglist: array of PtrUInt): PTT_Pixmap;
function TT_OpenFont(const Taglist: array of PtrUInt): APTR;
function TT_Request(Request: APTR; const Taglist: array of PtrUInt): PTagItem;
function TT_SetAttrs(Rp: PRastPort; const Taglist: array of PtrUInt): LongWord;

IMPLEMENTATION

{
 Functions and procedures with array of PtrUInt go here
}
function TT_GetAttrs(Rp: pRastPort; const Taglist: array of PtrUInt): LongWord;
begin
  TT_GetAttrs := TT_GetAttrsA(Rp, @Taglist);
end;

function TT_GetPixmap(Font: APTR; Text: PChar; Count: LongWord; const Taglist: array of PtrUInt): PTT_Pixmap;
begin
  TT_GetPixmap := TT_GetPixmapA(Font, Text, Count, @Taglist);
end;

function TT_OpenFont(const Taglist: array of PtrUInt): APTR;
begin
  TT_OpenFont := TT_OpenFontA(@Taglist);
end;

function TT_Request(Request: APTR; const Taglist: array of PtrUInt): PTagItem;
begin
  TT_Request := TT_RequestA(Request, @Taglist);
end;

function TT_SetAttrs(Rp: PRastPort; const Taglist: array of PtrUInt): LongWord;
begin
  TT_SetAttrs := TT_SetAttrsA(Rp, @Taglist);
end;

const
  { Change VERSION and LIBVERSION to proper values }
  VERSION: string[2] = '0';
  LIBVERSION: longword = 0;

initialization
  TTEngineBase := OpenLibrary(TTENGINENAME, LIBVERSION);
finalization
  if Assigned(TTEngineBase) then
    CloseLibrary(TTEngineBase);
end.



