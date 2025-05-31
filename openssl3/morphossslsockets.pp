unit morphossslsockets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets, ssockets, sslsockets, sslbase, openssl3;

Type

  { TAmiSSLSocketHandler }

  TAmiSSLSocketHandler = Class(TSSLSocketHandler)
  Private
    Ctx: PSSL_CTX;
    Web: PBIO;
    //FSSL: TSSL;
    //FCTX : TSSLContext;
    FSSLLastErrorString: string;
    FSSLLastError : Integer;
  Protected
    procedure SetSSLLastErrorString(AValue: string);
    Function FetchErrorInfo: Boolean;
    function CheckSSL(SSLResult: Integer): Boolean;
    function CheckSSL(SSLResult: Pointer): Boolean;
    function InitContext(NeedCertificate: Boolean): Boolean; virtual;
    function DoneContext: Boolean; virtual;
    function InitSslKeys: boolean;virtual;
  Public
    Constructor create; override;
    destructor destroy; override;
    function CreateCertGenerator: TX509Certificate; override;
    function Connect : Boolean; override;
    function Close : Boolean; override;
    function Accept : Boolean; override;
    function Shutdown(BiDirectional : Boolean): boolean; override;
    function Send(Const Buffer; Count: Integer): Integer; override;
    function Recv(Const Buffer; Count: Integer): Integer; override;
    function BytesAvailable: Integer; override;
    // Result of last CheckSSL call.
    Function SSLLastError: integer;
    property SSLLastErrorString: string read FSSLLastErrorString write SetSSLLastErrorString;
    //property SSL: TSSL read FSSL; // allow more lower level info and control
  end;

  TOpenSSLX509Certificate = Class (TX509Certificate)
  protected
    function CreateKey: PEVP_PKEY; virtual;
    procedure SetNameData(x: PX509); virtual;
    procedure SetTimes(x: PX509); virtual;
  public
    Function CreateCertificateAndKey : TCertAndKey; override;
  end;

implementation
uses
  Exec, Utility;

var
  SSLErrNo: Integer;

{ TSocketHandler }
Resourcestring
  SErrNoLibraryInit = 'Could not initialize OpenSSL library';

Procedure MaybeInitSSLInterface;
begin
  //if not IsSSLloaded then
  //  if not InitSSLInterface then
  if not Assigned(OpenSSL3Base) then
    Raise EInOutError.Create(SErrNoLibraryInit);
end;

function TAmiSSLSocketHandler.CreateCertGenerator: TX509Certificate;
begin
  Result:=TOpenSSLX509Certificate.Create;
end;

procedure TAmiSSLSocketHandler.SetSSLLastErrorString(AValue: string);
begin
  if FSSLLastErrorString=AValue then Exit;
  FSSLLastErrorString:=AValue;
end;

function TAmiSSLSocketHandler.Connect: Boolean;
begin

  Sleep(1); // not sure why this is needed, but without, no connection
  Result := InitContext(False);
  if Result then
  begin
    if Result then
    begin
      web := BIO_new_ssl_connect(ctx);
      if SendHostAsSNI  and (Socket is TInetSocket) then
      begin
        BIO_ctrl(web, BIO_C_DO_STATE_MACHINE,0, nil);
        BIO_ctrl(web, BIO_C_SET_CONNECT,0, PAnsiChar(AnsiString((Socket as TInetSocket).Host + ':' + IntToStr((Socket as TInetSocket).Port))));
      end;
     BIO_ctrl(web, BIO_C_DO_STATE_MACHINE,0, nil);
     if Result then
       Result:= DoVerifyCert;
     if Result then
       SetSSLActive(True);
     end;
    end;
end;

function TAmiSSLSocketHandler.Close: Boolean;
begin
  Result:=Shutdown(False);
end;

Function TAmiSSLSocketHandler.FetchErrorInfo : Boolean;

var
  S : AnsiString;

begin
  FSSLLastErrorString:='';
  FSSLLastError:=ERR_get_error;
  Err_Clear_Error;
  Result:=(FSSLLastError<>0);
  if Result then
    begin
    S:=StringOfChar(#0,256);
    ErrErrorString(FSSLLastError,S,256);
    FSSLLastErrorString:=s;
    end;
end;

function TAmiSSLSocketHandler.CheckSSL(SSLResult : Integer) : Boolean;

begin
  Result:=SSLResult>=1;
  if Not Result then
     begin
     FSSLLastError:=SSLResult;
     FetchErrorInfo;
     end;
end;

function TAmiSSLSocketHandler.CheckSSL(SSLResult: Pointer): Boolean;
begin
  Result:=(SSLResult<>Nil);
  if not Result then
    Result:=FetchErrorInfo;
end;

function TAmiSSLSocketHandler.DoneContext: Boolean;

begin
  //FreeAndNil(FSSL);
  //FreeAndNil(FCTX);
  if assigned(Web) then
    BIO_free_all(web);
  if Assigned(Ctx) then
    SSL_CTX_free(Ctx);
  //ErrRemoveState(0); // not in AmiSSL?
  SetSSLActive(False);
  Result:=True;
end;

Function HandleSSLPwd(buf : PAnsiChar; len:Integer; flags:Integer; UD : Pointer):Integer; cdecl;

var
  Pwd: AnsiString;
  H :  TAmiSSLSocketHandler;

begin
  if Not Assigned(UD) then
    PWD:=''
  else
    begin
    H:=TAmiSSLSocketHandler(UD);
    Pwd:=H.CertificateData.KeyPassword;
    end;
  if (len<Length(Pwd)+1) then
    SetLength(Pwd,len-1);
  pwd:=pwd+#0;
  Result:=Length(Pwd);
  Move(Pointer(Pwd)^,Buf^,Result);
end;

function TAmiSSLSocketHandler.InitSslKeys: boolean;

begin
  Result:=(CTX<>Nil);
  if not Result then
    Exit;
  {
  if not CertificateData.Certificate.Empty then
    Result:=CheckSSL(FCTX.UseCertificate(CertificateData.Certificate));
  if Result and not CertificateData.PrivateKey.Empty then
    Result:=CheckSSL(FCTX.UsePrivateKey(CertificateData.PrivateKey));
  if Result and (CertificateData.CertCA.FileName<>'') then
    Result:=CheckSSL(FCTX.LoadVerifyLocations(CertificateData.CertCA.FileName,''));
  if Result and not CertificateData.PFX.Empty then
    Result:=CheckSSL(FCTX.LoadPFX(CertificateData.PFX,CertificateData.KeyPassword));}
end;

constructor TAmiSSLSocketHandler.create;
begin
  inherited create;
  MaybeInitSSLInterface;
end;

destructor TAmiSSLSocketHandler.destroy;
begin
  //FreeAndNil(FCTX);
  //FreeAndNil(FSSL);
  if assigned(Web) then
    BIO_free_all(web);
  if Assigned(Ctx) then
    SSL_CTX_free(Ctx);
  inherited destroy;
end;

function TAmiSSLSocketHandler.InitContext(NeedCertificate:Boolean): Boolean;
{Const
  VO : Array[Boolean] of Integer = (SSL_VERIFY_NONE,SSL_VERIFY_PEER);
var
  s: AnsiString;}
begin
  Result:=DoneContext;
  if Not Result then
    Exit;
  SSLeay_add_ssl_algorithms();
  SSL_load_error_strings();
  Ctx := SSL_CTX_new(SSLv23_client_method());
  Result := Assigned(Ctx);
  {try
    FCTX:=TSSLContext.Create(SSLType);
  Except
    CheckSSL(Nil);
    Result:=False;
    Exit;
  end;
  S:=CertificateData.CipherList;
  FCTX.SetCipherList(S);
  FCTX.SetVerify(VO[VerifypeerCert],Nil);
  FCTX.SetDefaultPasswdCb(@HandleSSLPwd);
  FCTX.SetDefaultPasswdCbUserdata(self);
  If NeedCertificate and CertificateData.NeedCertificateData  then
    if Not CreateSelfSignedCertificate then
      begin
      DoneContext;
      Exit(False);
      end;
   if Not InitSSLKeys then
     begin
     DoneContext;
     Exit(False);
     end;
   try
     FSSL:=TSSL.Create(FCTX);
     Result:=True;
   Except
     CheckSSL(Nil);
     DoneContext;
     Result:=False;
   end;}
end;

function TAmiSSLSocketHandler.Accept: Boolean;

begin
  writeln('Accept not implemented');
  Result := False;
  {
  Result:=InitContext(True);
  if Result then
    begin
    Result:=CheckSSL(FSSL.setfd(Socket.Handle));
    if Result then
      Result:=CheckSSL(FSSL.Accept);
    end;
  SetSSLActive(Result);}
end;


function TAmiSSLSocketHandler.Shutdown(BiDirectional : Boolean): boolean;

var
  r : integer;

begin
  Result := True;
  {
  Result:=assigned(FSsl);
  if Result then
    If Not BiDirectional then
      Result:=CheckSSL(FSSL.Shutdown)
    else
      begin
      r:=FSSL.Shutdown;
      if r<>0 then
        Result:=CheckSSL(r)
      else
        begin
        Result:=fpShutdown(Socket.Handle,1)=0;
        if Result then
          Result:=CheckSSL(FSsl.Shutdown);
        end
      end;
  If Result then
    Result:=DoneContext;}
end;

function TAmiSSLSocketHandler.Send(Const Buffer; Count: Integer): Integer;
var
  e: integer;
begin
  FSSLLastError := 0;
  FSSLLastErrorString:='';
  Result := BIO_write(web, @Buffer, Count);
  {repeat
    Result:=FSsl.Write(@Buffer,Count);
    e:=FSsl.GetError(Result);
  until Not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]);
  if (E=SSL_ERROR_ZERO_RETURN) then
    Result:=0
  else if (e<>0) then
    FSSLLastError:=e;
  writeln('<-- send');}
end;

function TAmiSSLSocketHandler.Recv(Const Buffer; Count: Integer): Integer;

var
  e: integer;
begin
  //writeln('--> Recv');
  FSSLLastError:=0;
  FSSLLastErrorString:= '';
  Result := BIO_read(web, @(Buffer), Count);
  {
  repeat
    Result:=FSSL.Read(@Buffer ,Count);
    e:=FSSL.GetError(Result);
    if (e=SSL_ERROR_WANT_READ) and (Socket.IOTimeout>0) then
      e:=SSL_ERROR_ZERO_RETURN;
  until Not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]);
  if (E=SSL_ERROR_ZERO_RETURN) then
    Result:=0
  else if (e<>0) then
    FSSLLastError:=e;}
  //writeln('<-- recv');
end;

function TAmiSSLSocketHandler.BytesAvailable: Integer;
begin
  Result:= BIO_ctrl_pending(web);
end;

Function TAmiSSLSocketHandler.SSLLastError: integer;
begin
  Result:=FSSLLastError;
end;

function BioToString(B: PBIO; FreeBIO: Boolean = False): AnsiString;
var
  L, RL: Integer;
begin
  l := BIO_ctrl_pending(B);
  Result := StringOfChar(#0, l);
  RL := BIOread(B, Result, L);
  if RL > 0 then
    SetLength(Result, RL)
  else
    SetLength(Result, 0);
  if FreeBio then
    BIO_free_all(B);
end;

function BioToTBytes(B: PBIO; FreeBIO: Boolean = False): TBytes;
var
  L, RL: Integer;
begin
  l := BIO_ctrl_pending(B);
  SetLength(Result, l);
  FillChar(Result[0], L, 0);
  RL := BioRead(B, Result, L);
  if RL > 0 then
    SetLength(Result, RL)
  else
    SetLength(Result, 0);
  if FreeBio then
    BIO_free_all(B);
end;

{ TOpenSSLX509Certificate }


procedure TOpenSSLX509Certificate.SetNameData(x: PX509);
var
  ND: PX509_NAME;
  S: AnsiString;

  procedure SetEntry(ACode, AValue: AnsiString);
  begin
    if AValue <> '' then
      X509NameAddEntryByTxt(ND, ACode, $1001, AValue, -1, -1, 0);
  end;

begin
  ND := X509_get_subject_name(x);
  S := Country;
  if S = '' then
    S := 'BE';
  SetEntry('C', S);
  S := HostName;
  if S = '' then
    S := 'localhost';
  SetEntry('CN', S);
  SetEntry('O', Organization);
  X509_set_issuer_name(x, ND);
end;

procedure TOpenSSLX509Certificate.SetTimes(x : PX509);
var
  Utc: PASN1_UTCTIME;
begin
  Utc := Asn1UtctimeNew;
  try
    ASN1_UTCTIME_set_string(Utc, PAnsiChar(FormatDateTime('YYMMDDHHNNSS', ValidFrom)));
    X509_set1_notBefore(x, Utc);
    ASN1_UTCTIME_set_string(Utc, PAnsiChar(FormatDateTime('YYMMDDHHNNSS', ValidTo)));
    X509_set1_notAfter(x, Utc);
  finally
    ASN1_UTCTIME_free(Utc);
  end;
end;


function TOpenSSLX509Certificate.CreateKey: PEVP_PKEY;
Var
  rsa: PRSA;
begin
  Result := EVP_PKEY_new;
  rsa := RSA_generate_key(KeySize, $10001, nil, nil);
  EVP_PKEY_assign(Result, EVP_PKEY_RSA, rsa);
end;

function TOpenSSLX509Certificate.CreateCertificateAndKey: TCertAndKey;
var
  pk: PEVP_PKEY;
  x: PX509;
  b: PBIO;
{$IFDEF DUMPCERT}
  s : string;
{$ENDIF}
begin
  SetLength(Result.Certificate, 0);
  SetLength(Result.PrivateKey, 0);
  pk := nil;
  x := X509_New;
  try
    X509_set_version(x, Version);
    ASN1_INTEGER_set(X509_get_serialNumber(x), GetRealSerial);
    SetTimes(X);
    pk:=CreateKey;
    X509_set_pubkey(x, pk);
    SetNameData(x);
    X509_sign(x,pk,EvpGetDigestByName('SHA1'));
    // Certificate
    b := Bio_New(BIO_s_mem);
    i2d_X509_bio(b, x);
    Result.Certificate:=BioToTbytes(B, True);
    // Private key
    b := Bio_New(BIO_s_mem);
    i2d_PrivateKey_bio(b, pk);
    Result.PrivateKey:=BioToTbytes(B, True);
{$IFDEF DUMPCERT}
    b := BIO_new(BIO_s_mem);
    PEM_write_bio_X509(b, x);
    S:=BioToString(B, True);
    With TStringList.Create do
      try
        Add(S);
        SaveToFile(DumpCertFile);
      finally
        Free;
      end;
{$ENDIF}
  finally
    X509_free(x);
    EVP_PKEY_free(pk);
  end;
end;

initialization
  if Assigned(OpenSSL3Base) then
    TSSLSocketHandler.SetDefaultHandlerClass(TAmiSSLSocketHandler);
finalization
  
end.

