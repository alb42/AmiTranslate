unit amissl;
{$mode ObjFPC}{$H+}
interface
uses
  exec, utility, ctypes, Sysutils;

var
  AmiSSLMasterBase: PLibrary = nil;
  AmiSSLBase: PLibrary = nil;

// AmiSSLMaster Constants:
const
  AMISSLMASTER_NAME = 'amisslmaster.library';
  AMISSLMASTER_MIN_VERSION  = 4;

  // Constants for InitAmiSSLMaster()
  AMISSL_V2    = $01; // OBSOLETE NAME
  AMISSL_V096g = $01; // AmiSSL v2
  AMISSL_V097g = $02; // AmiSSL v3.7
  AMISSL_V097m = $03; // unreleased version
  AMISSL_V098y = $04; // unreleased version
  AMISSL_V10x  = $05; // AmiSSL/OpenSSL 1.0.x compatible versions
  AMISSL_V11x  = $06; // AmiSSL/OpenSSL 1.1.x compatible versions

  AMISSL_V303 = 22;
  AMISSL_CURRENT_VERSION =  AMISSL_V11x;

// AmiSSLMaster functions:
{$ifdef AROS}
function InitAmiSSLMaster(APIVersion: LongInt; UsesOpenSSLStructs: LongInt): LongInt; syscall AmiSSLMasterBase 5;
function OpenAmiSSL(): PLibrary; syscall AmiSSLMasterBase 6;
procedure CloseAmiSSL(); syscall AmiSSLMasterBase 7;
function OpenAmiSSLCipher(Cipher: LongInt): PLibrary; syscall AmiSSLMasterBase 8;
procedure CloseAmiSSLCipher(CipherBase: PLibrary); syscall AmiSSLMasterBase 9;
{$endif}
{$if defined(Amiga68k) or defined(MorphOS)}
function InitAmiSSLMaster(APIVersion: LongInt location 'd0'; UsesOpenSSLStructs: LongInt location 'd1'): LongInt; syscall AmiSSLMasterBase 30;
function OpenAmiSSL(): PLibrary; syscall AmiSSLMasterBase 36;
procedure CloseAmiSSL(); syscall AmiSSLMasterBase 42;
function OpenAmiSSLCipher(Cipher: LongInt location 'd0'): PLibrary; syscall AmiSSLMasterBase 48;
procedure CloseAmiSSLCipher(CipherBase: PLibrary location 'a0'); syscall AmiSSLMasterBase 54;
{$endif}
const
  // EVP.h Constants

  EVP_MAX_MD_SIZE       = 64; //* longest known is SHA512 */
  EVP_MAX_KEY_LENGTH    = 32;
  EVP_MAX_IV_LENGTH     = 16;
  EVP_MAX_BLOCK_LENGTH  = 32;
  SHA_DIGEST_LENGTH = 20;

// AmiSSL types:
type
  PFunction = procedure;

  SSL_CTX = record end;
  PSSL_CTX = ^SSL_CTX;

  SslPtr = Pointer;
  PEVP_MD = SslPtr;
  PBIO_METHOD = SslPtr;
  PBIO = SslPtr;
  PSSL = SslPtr;
  PSSL_METHOD = SslPtr;
  PRSA = SslPtr;
  PASN1_cInt = SslPtr;
  PPasswdCb = SslPtr;
  PCallbackCb = SslPtr;

  PDH = pointer;
  PSTACK_OFX509 = pointer;

  PASN1_UTCTIME = SslPtr;
  PASN1_INTEGER = SSlPtr;
  POPENSSL_INIT_SETTINGS = SSLPtr;

  PX509_STORE_CTX = SslPtr;
  TSSLCTXVerifyCallback = function (ok : cInt; ctx : PX509_STORE_CTX) : Cint; cdecl;

  ASN1_STRING = record
  length: integer;
  asn1_type: integer;
  data: pointer;
  flags: longint;
  end;
  PASN1_STRING = ^ASN1_STRING;
  PASN1_TIME = PASN1_STRING;

  X509_NAME = record
    entries: pointer;
    modified: integer;
    bytes: pointer;
    hash: cardinal;
  end;
  PX509_NAME = ^X509_NAME;
  PDN = ^X509_NAME;

  X509_VAL = record
  notBefore: PASN1_TIME;
    notAfter: PASN1_TIME;
  end;
  PX509_VAL = ^X509_VAL;

  X509_CINF = record
    version: pointer;
    serialNumber: pointer;
    signature: pointer;
    issuer: pointer;
    validity: PX509_VAL;
    subject: pointer;
    key: pointer;
    issuerUID: pointer;
    subjectUID: pointer;
    extensions: pointer;
  end;
  PX509_CINF = ^X509_CINF;

  CRYPTO_EX_DATA = record
    sk: pointer;
    dummy: integer;
  end;

  X509 = record
    cert_info: PX509_CINF;
    sig_alg: pointer;  // ^X509_ALGOR
    signature: pointer;  // ^ASN1_BIT_STRING
    valid: integer;
    references: integer;
    name: PChar;
    ex_data: CRYPTO_EX_DATA;
    ex_pathlen: integer;
    ex_flags: integer;
    ex_kusage: integer;
    ex_xkusage: integer;
    ex_nscert: integer;
    skid: pointer;  // ^ASN1_OCTET_STRING
    akid: pointer;  // ?
    sha1_hash: array [0..SHA_DIGEST_LENGTH-1] of char;
    aux: pointer;  // ^X509_CERT_AUX
  end;
  pX509 = ^X509;
  PPX509 = ^PX509;

  DSA = record
    pad: integer;
    version: integer;
    write_params: integer;
    p: pointer;
    q: pointer;
    g: pointer;
    pub_key: pointer;
    priv_key: pointer;
    kinv: pointer;
    r: pointer;
    flags: integer;
    method_mont_p: PChar;
    references: integer;
    ex_data: record
        sk: pointer;
        dummy: integer;
      end;
    meth: pointer;
  end;
  pDSA = ^DSA;

  EVP_PKEY_PKEY = record
    case integer of
      0: (ptr: PChar);
      1: (rsa: pRSA);
      2: (dsa: pDSA);
      3: (dh: pDH);
   end;

  EVP_PKEY = record
    ktype: integer;
    save_type: integer;
    references: integer;
    pkey: EVP_PKEY_PKEY;
    save_parameters: integer;
    attributes: PSTACK_OFX509;
  end;
  PEVP_PKEY = ^EVP_PKEY;
  PPEVP_PKEY = ^PEVP_PKEY;


const
  SSL_ERROR_NONE = 0;
  SSL_ERROR_SSL = 1;
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_WANT_X509_LOOKUP = 4;
  SSL_ERROR_SYSCALL = 5; //look at error stack/return value/errno
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_ERROR_WANT_CONNECT = 7;
  SSL_ERROR_WANT_ACCEPT = 8;
  SSL_ERROR_WANT_CHANNEL_ID_LOOKUP = 9;
  SSL_ERROR_PENDING_SESSION = 11;

  // BIO

  BIO_NOCLOSE         = $00;
  BIO_CLOSE           = $01;

  //* modifiers */
  BIO_FP_READ   = $02;
  BIO_FP_WRITE    = $04;
  BIO_FP_APPEND   = $08;
  BIO_FP_TEXT   = $10;

  BIO_C_SET_CONNECT                 = 100;
  BIO_C_DO_STATE_MACHINE            = 101;
  BIO_C_SET_NBIO              = 102;
  BIO_C_SET_PROXY_PARAM             = 103;
  BIO_C_SET_FD                      = 104;
  BIO_C_GET_FD                = 105;
  BIO_C_SET_FILE_PTR              = 106;
  BIO_C_GET_FILE_PTR              = 107;
  BIO_C_SET_FILENAME              = 108;
  BIO_C_SET_SSL               = 109;
  BIO_C_GET_SSL               = 110;
  BIO_C_SET_MD                = 111;
  BIO_C_GET_MD                      = 112;
  BIO_C_GET_CIPHER_STATUS           = 113;
  BIO_C_SET_BUF_MEM               = 114;
  BIO_C_GET_BUF_MEM_PTR       = 115;
  BIO_C_GET_BUFF_NUM_LINES          = 116;
  BIO_C_SET_BUFF_SIZE             = 117;
  BIO_C_SET_ACCEPT              = 118;
  BIO_C_SSL_MODE              = 119;
  BIO_C_GET_MD_CTX              = 120;
  BIO_C_GET_PROXY_PARAM             = 121;
  BIO_C_SET_BUFF_READ_DATA      = 122; // data to read first */
  BIO_C_GET_CONNECT       = 123;
  BIO_C_GET_ACCEPT        = 124;
  BIO_C_SET_SSL_RENEGOTIATE_BYTES   = 125;
  BIO_C_GET_SSL_NUM_RENEGOTIATES    = 126;
  BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT = 127;
  BIO_C_FILE_SEEK       = 128;
  BIO_C_GET_CIPHER_CTX        = 129;
  BIO_C_SET_BUF_MEM_EOF_RETURN  = 130;//*return end of input value*/
  BIO_C_SET_BIND_MODE   = 131;
  BIO_C_GET_BIND_MODE   = 132;
  BIO_C_FILE_TELL   = 133;
  BIO_C_GET_SOCKS   = 134;
  BIO_C_SET_SOCKS   = 135;

  BIO_C_SET_WRITE_BUF_SIZE  = 136;//* for BIO_s_bio */
  BIO_C_GET_WRITE_BUF_SIZE  = 137;
  BIO_C_MAKE_BIO_PAIR   = 138;
  BIO_C_DESTROY_BIO_PAIR  = 139;
  BIO_C_GET_WRITE_GUARANTEE = 140;
  BIO_C_GET_READ_REQUEST  = 141;
  BIO_C_SHUTDOWN_WR   = 142;
  BIO_C_NREAD0            = 143;
  BIO_C_NREAD     = 144;
  BIO_C_NWRITE0     = 145;
  BIO_C_NWRITE      = 146;
  BIO_C_RESET_READ_REQUEST  = 147;
  BIO_C_SET_MD_CTX    = 148;

  BIO_C_SET_PREFIX    = 149;
  BIO_C_GET_PREFIX    = 150;
  BIO_C_SET_SUFFIX    = 151;
  BIO_C_GET_SUFFIX    = 152;

  BIO_C_SET_EX_ARG    = 153;
  BIO_C_GET_EX_ARG    = 154;

  BIO_CTRL_RESET  =    1  ; { opt - rewind/zero etc }
  BIO_CTRL_EOF    =    2  ; { opt - are we at the eof }
  BIO_CTRL_INFO   =     3  ; { opt - extra tit-bits }
  BIO_CTRL_SET    =     4  ; { man - set the 'IO' type }
  BIO_CTRL_GET    =     5  ; { man - get the 'IO' type }
  BIO_CTRL_PUSH   =     6  ; { opt - internal, used to signify change }
  BIO_CTRL_POP    =     7  ; { opt - internal, used to signify change }
  BIO_CTRL_GET_CLOSE =  8  ; { man - set the 'close' on free }
  BIO_CTRL_SET_CLOSE =  9  ; { man - set the 'close' on free }
  ABIO_CTRL_PENDING   =  10  ; { opt - is their more data buffered }
  BIO_CTRL_FLUSH     =  11  ; { opt - 'flush' buffered output }
  BIO_CTRL_DUP       =  12  ; { man - extra stuff for 'duped' BIO }
  BIO_CTRL_WPENDING  =  13  ; { opt - number of bytes still to write }
  BIO_CTRL_SET_CALLBACK   = 14  ; { opt - set callback function }
  BIO_CTRL_GET_CALLBACK   = 15  ; { opt - set callback function }
  BIO_CTRL_SET_FILENAME   = 30  ; { BIO_s_file special }
  BIO_CTRL_DGRAM_CONNECT  = 31  ; { BIO dgram special }
  BIO_CTRL_DGRAM_SET_CONNECTED      = 32  ; { allow for an externally }
  BIO_CTRL_DGRAM_SET_RECV_TIMEOUT   = 33 ; { setsockopt, essentially }
  BIO_CTRL_DGRAM_GET_RECV_TIMEOUT   = 34 ; { getsockopt, essentially }
  BIO_CTRL_DGRAM_SET_SEND_TIMEOUT   = 35 ; { setsockopt, essentially }
  BIO_CTRL_DGRAM_GET_SEND_TIMEOUT   = 36 ; { getsockopt, essentially }
  BIO_CTRL_DGRAM_GET_RECV_TIMER_EXP = 37 ; { flag whether the last }
  BIO_CTRL_DGRAM_GET_SEND_TIMER_EXP = 38 ; { I/O operation tiemd out }
  BIO_CTRL_DGRAM_MTU_DISCOVER       = 39 ; { set DF bit on egress packets }
  BIO_CTRL_DGRAM_QUERY_MTU          = 40 ; { as kernel for current MTU }
  BIO_CTRL_DGRAM_GET_FALLBACK_MTU   = 47 ;
  BIO_CTRL_DGRAM_GET_MTU            = 41 ; { get cached value for MTU }
  BIO_CTRL_DGRAM_SET_MTU            = 42 ; { set cached value for }
  BIO_CTRL_DGRAM_MTU_EXCEEDED       = 43 ; { check whether the MTU }
  BIO_CTRL_DGRAM_GET_PEER           = 46 ;
  BIO_CTRL_DGRAM_SET_PEER           = 44 ; { Destination for the data }
  BIO_CTRL_DGRAM_SET_NEXT_TIMEOUT   = 45 ; { Next DTLS handshake timeout to }
  BIO_CTRL_DGRAM_SCTP_SET_IN_HANDSHAKE = 50;
  BIO_CTRL_DGRAM_SCTP_ADD_AUTH_KEY     = 51;
  BIO_CTRL_DGRAM_SCTP_NEXT_AUTH_KEY    = 52;
  BIO_CTRL_DGRAM_SCTP_AUTH_CCS_RCVD    = 53;
  BIO_CTRL_DGRAM_SCTP_GET_SNDINFO      = 60;
  BIO_CTRL_DGRAM_SCTP_SET_SNDINFO      = 61;
  BIO_CTRL_DGRAM_SCTP_GET_RCVINFO      = 62;
  BIO_CTRL_DGRAM_SCTP_SET_RCVINFO      = 63;
  BIO_CTRL_DGRAM_SCTP_GET_PRINFO       = 64;
  BIO_CTRL_DGRAM_SCTP_SET_PRINFO       = 65;
  BIO_CTRL_DGRAM_SCTP_SAVE_SHUTDOWN    = 70;

  ALLBACK = 15;
  SSL_CTRL_SET_MSG_CALLBACK_ARG = 16;
  SSL_CTRL_SET_MTU = 17;
  SSL_CTRL_SESS_NUMBER = 20;
  SSL_CTRL_SESS_CONNECT = 21;
  SSL_CTRL_SESS_CONNECT_GOOD = 22;
  SSL_CTRL_SESS_CONNECT_RENEGOTIATE = 23;
  SSL_CTRL_SESS_ACCEPT = 24;
  SSL_CTRL_SESS_ACCEPT_GOOD = 25;
  SSL_CTRL_SESS_ACCEPT_RENEGOTIATE = 26;
  SSL_CTRL_SESS_HIT = 27;
  SSL_CTRL_SESS_CB_HIT = 28;
  SSL_CTRL_SESS_MISSES = 29;
  SSL_CTRL_SESS_TIMEOUTS = 30;
  SSL_CTRL_SESS_CACHE_FULL = 31;
  SSL_CTRL_OPTIONS = 32;
  SSL_CTRL_MODE = 33;
  SSL_CTRL_GET_READ_AHEAD = 40;
  SSL_CTRL_SET_READ_AHEAD = 41;
  SSL_CTRL_SET_SESS_CACHE_SIZE = 42;
  SSL_CTRL_GET_SESS_CACHE_SIZE = 43;
  SSL_CTRL_SET_SESS_CACHE_MODE = 44;
  SSL_CTRL_GET_SESS_CACHE_MODE = 45;
  SSL_CTRL_GET_MAX_CERT_LIST = 50;
  SSL_CTRL_SET_MAX_CERT_LIST = 51;
  SSL_CTRL_SET_MAX_SEND_FRAGMENT              = 52;
  SSL_CTRL_SET_TLSEXT_SERVERNAME_CB           = 53;
  SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG          = 54;
  SSL_CTRL_SET_TLSEXT_HOSTNAME                = 55;
  SSL_CTRL_SET_TLSEXT_DEBUG_CB                = 56;
  SSL_CTRL_SET_TLSEXT_DEBUG_ARG               = 57;
  SSL_CTRL_GET_TLSEXT_TICKET_KEYS             = 58;
  SSL_CTRL_SET_TLSEXT_TICKET_KEYS             = 59;
  SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT        = 60;
  SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB     = 61;
  SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB_ARG = 62;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB           = 63;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG       = 64;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE         = 65;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_EXTS         = 66;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_EXTS         = 67;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_IDS          = 68;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_IDS          = 69;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP    = 70;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP    = 71;
  SSL_CTRL_SET_TLSEXT_TICKET_KEY_CB           = 72;
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME_CB        = 75;
  SSL_CTRL_SET_SRP_VERIFY_PARAM_CB            = 76;
  SSL_CTRL_SET_SRP_GIVE_CLIENT_PWD_CB         = 77;
  SSL_CTRL_SET_SRP_ARG                        = 78;
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME           = 79;
  SSL_CTRL_SET_TLS_EXT_SRP_STRENGTH           = 80;
  SSL_CTRL_SET_TLS_EXT_SRP_PASSWORD           = 81;
  SSL_CTRL_GET_EXTRA_CHAIN_CERTS              = 82;
  SSL_CTRL_CLEAR_EXTRA_CHAIN_CERTS            = 83;
  SSL_CTRL_TLS_EXT_SEND_HEARTBEAT             = 85;
  SSL_CTRL_GET_TLS_EXT_HEARTBEAT_PENDING      = 86;
  SSL_CTRL_SET_TLS_EXT_HEARTBEAT_NO_REQUESTS  = 87;
  SSL_CTRL_CHAIN                              = 88;
  SSL_CTRL_CHAIN_CERT                         = 89;
  SSL_CTRL_GET_CURVES                         = 90;
  SSL_CTRL_SET_CURVES                         = 91;
  SSL_CTRL_SET_CURVES_LIST                    = 92;
  SSL_CTRL_GET_SHARED_CURVE                   = 93;
  SSL_CTRL_SET_ECDH_AUTO                      = 94;
  SSL_CTRL_SET_SIGALGS                        = 97;
  SSL_CTRL_SET_SIGALGS_LIST                   = 98;
  SSL_CTRL_CERT_FLAGS                         = 99;
  SSL_CTRL_CLEAR_CERT_FLAGS                   = 100;
  SSL_CTRL_SET_CLIENT_SIGALGS                 = 101;
  SSL_CTRL_SET_CLIENT_SIGALGS_LIST            = 102;
  SSL_CTRL_GET_CLIENT_CERT_TYPES              = 103;
  SSL_CTRL_SET_CLIENT_CERT_TYPES              = 104;
  SSL_CTRL_BUILD_CERT_CHAIN                   = 105;
  SSL_CTRL_SET_VERIFY_CERT_STORE              = 106;
  SSL_CTRL_SET_CHAIN_CERT_STORE               = 107;
  SSL_CTRL_GET_PEER_SIGNATURE_NID             = 108;
  SSL_CTRL_GET_SERVER_TMP_KEY                 = 109;
  SSL_CTRL_GET_RAW_CIPHERLIST                 = 110;
  SSL_CTRL_GET_EC_POINT_FORMATS               = 111;
  SSL_CTRL_GET_TLSA_RECORD                    = 112;
  SSL_CTRL_SET_TLSA_RECORD                    = 113;
  SSL_CTRL_PULL_TLSA_RECORD                   = 114;
  SSL_CTRL_GET_CHAIN_CERTS                    = 115;
  SSL_CTRL_SELECT_CURRENT_CERT                = 116;
  SSL_CTRL_CHANNEL_ID                         = 117;
  SSL_CTRL_GET_CHANNEL_ID                     = 118;
  SSL_CTRL_SET_CHANNEL_ID                     = 119;

  TLSEXT_TYPE_server_name = 0;
  TLSEXT_TYPE_max_fragment_length = 1;
  TLSEXT_TYPE_client_certificate_url = 2;
  TLSEXT_TYPE_trusted_ca_keys = 3;
  TLSEXT_TYPE_truncated_hmac = 4;
  TLSEXT_TYPE_status_request = 5;
  TLSEXT_TYPE_user_mapping = 6;
  TLSEXT_TYPE_client_authz = 7;
  TLSEXT_TYPE_server_authz = 8;
  TLSEXT_TYPE_cert_type = 9;
  TLSEXT_TYPE_elliptic_curves = 10;
  TLSEXT_TYPE_ec_point_formats = 11;
  TLSEXT_TYPE_srp = 12;
  TLSEXT_TYPE_signature_algorithms = 13;
  TLSEXT_TYPE_use_srtp = 14;
  TLSEXT_TYPE_heartbeat = 15;
  TLSEXT_TYPE_session_ticket = 35;
  TLSEXT_TYPE_renegotiate = $ff01;
  TLSEXT_TYPE_next_proto_neg = 13172;
  TLSEXT_NAMETYPE_host_name = 0;
  TLSEXT_STATUSTYPE_ocsp = 1;
  TLSEXT_ECPOINTFORMAT_first = 0;
  TLSEXT_ECPOINTFORMAT_uncompressed = 0;
  TLSEXT_ECPOINTFORMAT_ansiX962_compressed_prime = 1;
  TLSEXT_ECPOINTFORMAT_ansiX962_compressed_char2 = 2;
  TLSEXT_ECPOINTFORMAT_last = 2;
  TLSEXT_signature_anonymous = 0;
  TLSEXT_signature_rsa = 1;
  TLSEXT_signature_dsa = 2;
  TLSEXT_signature_ecdsa = 3;
  TLSEXT_hash_none = 0;
  TLSEXT_hash_md5 = 1;
  TLSEXT_hash_sha1 = 2;
  TLSEXT_hash_sha224 = 3;
  TLSEXT_hash_sha256 = 4;
  TLSEXT_hash_sha384 = 5;
  TLSEXT_hash_sha512 = 6;
  TLSEXT_MAXLEN_host_name = 255;

  SSL_TLSEXT_ERR_OK = 0;
  SSL_TLSEXT_ERR_ALERT_WARNING = 1;
  SSL_TLSEXT_ERR_ALERT_FATAL = 2;
  SSL_TLSEXT_ERR_NOACK = 3;

  SSL_FILETYPE_ASN1 = 2;
  SSL_FILETYPE_PEM = 1;
  EVP_PKEY_RSA = 6;

  // ASN1 values
  V_ASN1_EOC                     = 0;
  V_ASN1_BOOLEAN                 = 1;
  V_ASN1_INTEGER                 = 2;
  V_ASN1_BIT_STRING              = 3;
  V_ASN1_OCTET_STRING            = 4;
  V_ASN1_NULL                    = 5;
  V_ASN1_OBJECT                  = 6;
  V_ASN1_OBJECT_DESCRIPTOR       = 7;
  V_ASN1_EXTERNAL                = 8;
  V_ASN1_REAL                    = 9;
  V_ASN1_ENUMERATED              = 10;
  V_ASN1_UTF8STRING              = 12;
  V_ASN1_SEQUENCE                = 16;
  V_ASN1_SET                     = 17;
  V_ASN1_NUMERICSTRING           = 18;
  V_ASN1_PRINTABLESTRING         = 19;
  V_ASN1_T61STRING               = 20;
  V_ASN1_TELETEXSTRING           = 20;
  V_ASN1_VIDEOTEXSTRING          = 21;
  V_ASN1_IA5STRING               = 22;
  V_ASN1_UTCTIME                 = 23;
  V_ASN1_GENERALIZEDTIME         = 24;
  V_ASN1_GRAPHICSTRING           = 25;
  V_ASN1_ISO64STRING             = 26;
  V_ASN1_VISIBLESTRING           = 26;
  V_ASN1_GENERALSTRING           = 27;
  V_ASN1_UNIVERSALSTRING         = 28;
  V_ASN1_BMPSTRING               = 30;

//DES modes
  DES_ENCRYPT = 1;
  DES_DECRYPT = 0;

// Error codes for ECDH Function
  ECDH_F_ECDH_COMPUTE_KEY = 100;
  ECDH_F_ECDH_DATA_NEW_METHOD = 101;

// Error codes for ECDH Reason
  ECDH_R_NO_PRIVATE_VALUE = 100;
  ECDH_R_POINT_ARITHMETIC_FAILURE = 101;
  ECDH_R_KDF_FAILED = 102;

// AmiSSL Constants:
const
// Tags
  AmiSSL_SocketBase      = TAG_USER + $01;
  AmiSSL_Version         = TAG_USER + $02; // OBSOLETE
  AmiSSL_Revision        = TAG_USER + $03; // OBSOLETE
  AmiSSL_VersionOverride = TAG_USER + $04; // OBSOLETE
//   AmiSSL_TCPStack      = TAG_USER + $05;    OBSOLETE
//   AmiSSL_SSLVersionApp = TAG_USER + $06;    OBSOLETE

  AmiSSL_SocketBaseBrand =  TAG_USER + $09;
  AmiSSL_MLinkLock       = TAG_USER + $0a;
  AmiSSL_ErrNoPtr        = TAG_USER + $0b;

  SSL_VERIFY_NONE = $00;
  SSL_VERIFY_PEER = $01;

type
  PASN1_BIT_STRING = Pointer;
  PPASN1_BIT_STRING = ^PASN1_BIT_STRING;


  PASN1_TYPE = Pointer;
  PASN1_ITEM = Pointer;
  PASN1_OBJECT = Pointer;
// SSLv23_client_method = TLS_client_method

// AmiSSL functions:
{$ifdef AROS}
function InitAmiSSLA(TagList: PTagItem): LongInt; syscall AmiSSLBase 6;
function CleanupAmiSSLA(TagList: PTagItem): LongInt; syscall AmiSSLBase 7;

function ASN1_TYPE_new(): PASN1_TYPE; syscall AmiSSLBase 17;
procedure ASN1_TYPE_free(asn1type: PASN1_TYPE); syscall AmiSSLBase 18;
function d2i_ASN1_TYPE(asn1type: PASN1_TYPE; in1: PPChar; Len: LongInt): PASN1_TYPE; syscall AmiSSLBase 19;
function i2d_ASN1_TYPE(asn1type: PASN1_TYPE; out1: PPChar): LongInt; syscall AmiSSLBase 20;
function ASN1_ANY_it(): PASN1_ITEM; syscall AmiSSLBase 21;
function ASN1_TYPE_get(asn1type: PASN1_TYPE): LongInt; syscall AmiSSLBase 22;
procedure ASN1_TYPE_set(asn1type: PASN1_TYPE; Type_: LongInt; Value: Pointer); syscall AmiSSLBase 23;

function ASN1_OBJECT_new(): PASN1_OBJECT; syscall AmiSSLBase 24;
procedure ASN1_OBJECT_free(asn1object: PASN1_OBJECT); syscall AmiSSLBase 25;
function i2d_ASN1_OBJECT(asn1object: PASN1_OBJECT; PP: PChar): LongInt; syscall AmiSSLBase 26;
function d2i_ASN1_OBJECT(asn1object: PASN1_OBJECT; PP: PChar; Len: LongInt): LongInt; syscall AmiSSLBase 28;
function ASN1_OBJECT_it(): PASN1_ITEM; syscall AmiSSLBase 29;
function ASN1_STRING_new(): PASN1_STRING; syscall AmiSSLBase 30;
procedure ASN1_STRING_free(asn1string: PASN1_STRING); syscall AmiSSLBase 31;
function ASN1_STRING_dup(asn1strig: PASN1_STRING): PASN1_STRING; syscall AmiSSLBase 32;
function ASN1_STRING_type_new(Type_: LongInt): PASN1_STRING; syscall AmiSSLBase 33;
function ASN1_STRING_cmp(a: PASN1_STRING; b: PASN1_STRING): LongInt; syscall AmiSSLBase 34;
function ASN1_STRING_set(asn1strig: PASN1_STRING; Data: Pointer; len: LongInt): LongInt; syscall AmiSSLBase 35;
function ASN1_STRING_length(asn1strig: PASN1_STRING): LongInt; syscall AmiSSLBase 36;
procedure ASN1_STRING_length_set(asn1strig: PASN1_STRING; len: LongInt); syscall AmiSSLBase 37;
function ASN1_STRING_type(asn1strig: PASN1_STRING): LongInt; syscall AmiSSLBase 38;
function ASN1_STRING_data(asn1strig: PASN1_STRING): PChar; syscall AmiSSLBase 39;
function ASN1_BIT_STRING_new(): PASN1_BIT_STRING; syscall AmiSSLBase 40;
procedure ASN1_BIT_STRING_free(s: PASN1_BIT_STRING); syscall AmiSSLBase 41;
function d2i_ASN1_BIT_STRING(s: PPASN1_BIT_STRING; in_: PPChar; Len: LongInt): PASN1_BIT_STRING; syscall AmiSSLBase 42;
function i2d_ASN1_BIT_STRING(s: PASN1_BIT_STRING; Out_: PPChar): LongInt; syscall AmiSSLBase 43;
function ASN1_BIT_STRING_it(): PASN1_ITEM; syscall AmiSSLBase 44;

function ASN1_BIT_STRING_set(s: PASN1_BIT_STRING; data: PChar; Len: LongInt): LongInt; syscall AmiSSLBase 47;

function ASN1_UTCTIME_set_string(t : PASN1_UTCTIME; S : PAnsiChar): cint; syscall AmiSSLBase 72;


procedure ASN1_UTCTIME_free(a: PASN1_UTCTIME); syscall AmiSSLBase 148;
function ASN1_INTEGER_set(a: PASN1_INTEGER; v: integer): integer; syscall AmiSSLBase 177;
function ASN1_INTEGER_get(a: PASN1_INTEGER): integer; syscall AmiSSLBase 178;

function BIO_ctrl_pending(b: PBIO): cInt; syscall AmiSSLBase 275;
function BIO_new(b: PBIO_METHOD): PBIO; syscall AmiSSLBase 288;
function BIO_read(bio: PBIO; Data: Pointer; Len: LongInt): LongInt; syscall AmiSSLBase 292;
function BIO_write(b: PBIO; Buf: PChar; Len: cInt): cInt; syscall AmiSSLBase 294;
function BIO_puts(bio: PBIO; Buf: PAnsiChar): LongInt; syscall AmiSSLBase 295;


function BIO_ctrl(bio: PBIO; Cmd: LongInt; larg: LongInt; PArg: Pointer): LongInt syscall AmiSSLBase 297;


procedure BIO_free_all(bio: PBIO); syscall AmiSSLBase 303;
function BIO_s_mem: PBIO_METHOD; syscall AmiSSLBase 314;

function ERR_get_error: cInt; syscall AmiSSLBase 660;

procedure ERR_clear_error; syscall AmiSSLBase 669;
procedure ERR_error_string_n(e: cInt; buf: PChar; len: cInt); syscall AmiSSLBase 671;

function EVP_get_digestbyname(Name: PChar): PEVP_MD; syscall AmiSSLBase 825;
function EVP_PKEY_assign(pkey: PEVP_PKEY; _type: cInt; key: Prsa): cInt; syscall AmiSSLBase 832;

function EVP_PKEY_new: PEVP_PKEY; syscall AmiSSLBase 839;
procedure EVP_PKEY_free(pk: PEVP_PKEY); syscall AmiSSLBase 840;

function PEM_write_bio_X509(bp: pBIO; x: PX509): integer; syscall AmiSSLBase 1137;

procedure PKCS12_free(p12: SslPtr); syscall AmiSSLBase 1214;
function PKCS12_parse(p12: SslPtr; pass: PChar; var pkey: SslPtr;  var cert: SslPtr; var ca: SslPtr): cInt; syscall AmiSSLBase 1236;
function d2i_PKCS12_bio(b:PBIO; Pkcs12: SslPtr): SslPtr; syscall AmiSSLBase 1239;

function BIO_new_ssl_connect(ctx: PSSL_CTX): PBIO; syscall AmiSSLBase 1363;


function SSL_CTX_set_cipher_list(arg0: PSSL_CTX; str: PChar):cInt; syscall AmiSSLBase 1367;
function SSL_CTX_new(Meth: Pointer): PSSL_CTX; syscall AmiSSLBase 1368;
procedure SSL_CTX_free(a: PSSL_CTX); syscall AmiSSLBase 1369;

function SSL_get_current_cipher(s: PSSL):SslPtr; syscall AmiSSLBase 1377;
function SSL_CIPHER_get_bits(c: SslPtr; alg_bits: PcInt):cInt; syscall AmiSSLBase 1378;
function SSL_CIPHER_get_name(c: Sslptr):PChar; syscall AmiSSLBase 1380;

function SSL_pending(ssl: PSSL):cInt; syscall AmiSSLBase 1392;
function SSL_set_fd(s: PSSL; fd: cInt):cInt; syscall AmiSSLBase 1393;

function SSL_CTX_use_RSAPrivateKey_file(ctx: PSSL_CTX; const _file: PChar; _type: cInt):cInt; syscall AmiSSLBase 1415;
function SSL_CTX_use_certificate_file(ctx: PSSL_CTX; const _file: PChar; _type: cInt):cInt; syscall AmiSSLBase 1417;
function SSL_CTX_use_certificate_chain_file(ctx: PSSL_CTX; const _file: PChar):cInt; syscall AmiSSLBase 1418;


function SSL_get_peer_certificate(ssl: PSSL):PX509; syscall AmiSSLBase 1445;

procedure SSL_CTX_set_verify(ctx: PSSL_CTX; mode: LongInt; arg2: Pointer); syscall AmiSSLBase 1450;
function SSL_CTX_use_PrivateKey(ctx: PSSL_CTX; pkey: sslptr):cInt; syscall AmiSSLBase 1455;
function SSL_CTX_use_PrivateKey_ASN1(pk: cInt; ctx: PSSL_CTX; d: sslptr; len: cInt):cInt; syscall AmiSSLBase 1456;
function SSL_CTX_use_certificate(ctx: PSSL_CTX; x: SslPtr):cInt; syscall AmiSSLBase 1457;
function SSL_CTX_use_certificate_ASN1(ctx: PSSL_CTX; len: cInt; d: SslPtr):cInt; syscall AmiSSLBase 1458;

procedure SSL_CTX_set_default_passwd_cb(ctx: PSSL_CTX; cb: SslPtr); syscall AmiSSLBase 1459;
procedure SSL_CTX_set_default_passwd_cb_userdata(ctx: PSSL_CTX; u: SslPtr); syscall AmiSSLBase 1460;
function SSL_new(ctx: PSSL_CTX):PSSL; syscall AmiSSLBase 1464;
procedure SSL_free(ssl: PSSL); syscall AmiSSLBase 1470;
function SSL_accept(ssl: PSSL):cInt; syscall AmiSSLBase 1471;
function SSL_connect(ssl: PSSL):cInt; syscall AmiSSLBase 1472;
function SSL_read(ssl: PSSL; buf: PChar; num: cInt):cInt; syscall AmiSSLBase 1473;
function SSL_peek(ssl: PSSL; buf: PChar; num: cInt):cInt; syscall AmiSSLBase 1474;
function SSL_write(ssl: PSSL; const buf: PChar; num: cInt):cInt; syscall AmiSSLBase 1475;
function SSL_ctrl(ssl: PSSL; cmd: cInt; larg: clong; parg: Pointer): cLong; syscall AmiSSLBase 1476;

function SSL_CTX_ctrl(ctx: PSSL_CTX; cmd: cInt; larg: clong; parg: Pointer): cLong; syscall AmiSSLBase 1478;
procedure SSL_CTX_callback_ctrl(ctx: PSSL_CTX; _type: cInt; cb: PCallbackCb); syscall AmiSSLBase 1479;
function SSL_get_error(s: PSSL; ret_code: cInt):cInt; syscall AmiSSLBase 1480;
function SSL_get_version(ssl: PSSL):PChar; syscall AmiSSLBase 1481;

function TLSv1_method:PSSL_METHOD; syscall AmiSSLBase 1492;
function TLSv1_server_method:PSSL_METHOD; syscall AmiSSLBase 1493;
function TLSv1_client_method:PSSL_METHOD; syscall AmiSSLBase 1494;

function SSL_shutdown(ssl: PSSL):cInt; syscall AmiSSLBase 1499;

function SSL_CTX_load_verify_locations(ctx: PSSL_CTX; const CAfile: PChar; const CApath: PChar):cInt; syscall AmiSSLBase 1529;
function SSL_get_verify_result(ssl: PSSL):cInt; syscall AmiSSLBase 1537;

function X509_sign(x: PX509; pkey: PEVP_PKEY; const md: PEVP_MD): cInt; syscall AmiSSLBase 1649;

function X509_digest(data: PX509; _type: PEVP_MD; md: PChar; len: PcInt):cInt; syscall AmiSSLBase 1654;

function i2d_X509_bio(b: PBIO; x: PX509): cInt; syscall AmiSSLBase 1659;
function i2d_PrivateKey_bio(b: PBIO; pkey: PEVP_PKEY): cInt; syscall AmiSSLBase 1679;

function X509_new: PX509; syscall AmiSSLBase 10614;
procedure X509_free(x: PX509); syscall AmiSSLBase 10620;

function X509_NAME_oneline(a: PX509_NAME; buf: PChar; size: cInt):PChar; syscall AmiSSLBase 1830;

function X509_set_version(x: PX509; version: cInt): cInt; syscall AmiSSLBase 1837;

function X509_get_serialNumber(x: PX509): PASN1_cInt; syscall AmiSSLBase 1839;

function X509_set_issuer_name(x: PX509; name: PX509_NAME): cInt; syscall AmiSSLBase 1840;
function X509_get_issuer_name(a: PX509):PX509_NAME; syscall AmiSSLBase 1841;

function X509_get_subject_name(a: PX509):PX509_NAME; syscall AmiSSLBase 1843;
function X509_set1_notBefore(x: PX509; tm: PASN1_UTCTIME): cInt; syscall AmiSSLBase 1844;
function X509_set1_notAfter(x: PX509; tm: PASN1_UTCTIME): cInt; syscall AmiSSLBase 1845;
function X509_set_pubkey(x: PX509; pkey: PEVP_PKEY): cInt; syscall AmiSSLBase 1846;

function X509_NAME_add_entry_by_txt(name: PX509_NAME; field: PChar; _type: cInt; bytes: PChar; len: cInt; loc: cInt; _set: cInt): cInt; syscall AmiSSLBase 1908;
function X509_NAME_hash(x: PX509_NAME):cuLong; syscall AmiSSLBase 1885;
function X509_print(b: PBIO; a: PX509): cInt; syscall AmiSSLBase 1890;

function RSA_generate_key(bits: cInt; e: cInt; callback: PFunction; cb_arg: SslPtr): PRSA; syscall AmiSSLBase 2449;


function SSL_get_servername(ssl: PSSL; _type: cInt): PChar; syscall AmiSSLBase 2631;
function SSL_set_SSL_CTX(ssl: PSSL; ctx: PSSL_CTX): PSSL; syscall AmiSSLBase 2632;


function OPENSSL_init_ssl(Opts: QWord; Settings: Pointer): LongInt; syscall AmiSSLBase 4428;
function TLS_client_method(): PSSL_METHOD; syscall AmiSSLBase 4489;
function TLS_method():PSSL_METHOD; syscall AmiSSLBase 4490;
{$endif}
/// ##### MORPHOS/Amiga68k
{$if defined(Amiga68k) or defined(MorphOS)}
function InitAmiSSLA(TagList: PTagItem location 'a0'): LongInt; syscall AmiSSLBase 36;
function CleanupAmiSSLA(TagList: PTagItem location 'a0'): LongInt; syscall AmiSSLBase 42;

function ASN1_TYPE_new(): PASN1_TYPE; syscall AmiSSLBase 102;
procedure ASN1_TYPE_free(asn1type: PASN1_TYPE location 'a0'); syscall AmiSSLBase 108;
function d2i_ASN1_TYPE(asn1type: PASN1_TYPE location 'a0'; in1: PPChar location 'a1'; Len: LongInt location 'd0'): PASN1_TYPE; syscall AmiSSLBase 114;
function i2d_ASN1_TYPE(asn1type: PASN1_TYPE location 'a0'; out1: PPChar location 'a1'): LongInt; syscall AmiSSLBase 120;
function ASN1_ANY_it(): PASN1_ITEM; syscall AmiSSLBase 126;
function ASN1_TYPE_get(asn1type: PASN1_TYPE location 'a0'): LongInt; syscall AmiSSLBase 132;
procedure ASN1_TYPE_set(asn1type: PASN1_TYPE location 'a0'; Type_: LongInt location 'd0'; Value: Pointer location 'a1'); syscall AmiSSLBase 138;

function ASN1_OBJECT_new(): PASN1_OBJECT; syscall AmiSSLBase 144;
procedure ASN1_OBJECT_free(asn1object: PASN1_OBJECT location 'a0'); syscall AmiSSLBase 150;
function i2d_ASN1_OBJECT(asn1object: PASN1_OBJECT location 'a0'; PP: PChar location 'a1'): LongInt; syscall AmiSSLBase 156;
function d2i_ASN1_OBJECT(asn1object: PASN1_OBJECT location 'a0'; PP: PChar location 'a1'; Len: LongInt location 'd0'): LongInt; syscall AmiSSLBase 168;
function ASN1_OBJECT_it(): PASN1_ITEM; syscall AmiSSLBase 174;
function ASN1_STRING_new(): PASN1_STRING; syscall AmiSSLBase 180;
procedure ASN1_STRING_free(asn1string: PASN1_STRING location 'a0'); syscall AmiSSLBase 186;
function ASN1_STRING_dup(asn1strig: PASN1_STRING location 'a0'): PASN1_STRING; syscall AmiSSLBase 192;
function ASN1_STRING_type_new(Type_: LongInt location 'd0'): PASN1_STRING; syscall AmiSSLBase 198;
function ASN1_STRING_cmp(a: PASN1_STRING location 'a0'; b: PASN1_STRING location 'a1'): LongInt; syscall AmiSSLBase 204;
function ASN1_STRING_set(asn1strig: PASN1_STRING location 'a0'; Data: Pointer location 'a1'; len: LongInt location 'd0'): LongInt; syscall AmiSSLBase 210;
function ASN1_STRING_length(asn1strig: PASN1_STRING location 'a0'): LongInt; syscall AmiSSLBase 216;
procedure ASN1_STRING_length_set(asn1strig: PASN1_STRING location 'a0'; len: LongInt location 'd0'); syscall AmiSSLBase 222;
function ASN1_STRING_type(asn1strig: PASN1_STRING location 'a0'): LongInt; syscall AmiSSLBase 228;
function ASN1_STRING_data(asn1strig: PASN1_STRING location 'a0'): PChar; syscall AmiSSLBase 234;
function ASN1_BIT_STRING_new(): PASN1_BIT_STRING; syscall AmiSSLBase 240;
procedure ASN1_BIT_STRING_free(s: PASN1_BIT_STRING location 'a0'); syscall AmiSSLBase 246;
function d2i_ASN1_BIT_STRING(s: PPASN1_BIT_STRING location 'a0'; in_: PPChar location 'a1'; Len: LongInt location 'd0'): PASN1_BIT_STRING; syscall AmiSSLBase 252;
function i2d_ASN1_BIT_STRING(s: PASN1_BIT_STRING location 'a0'; Out_: PPChar location 'a1'): LongInt; syscall AmiSSLBase 258;
function ASN1_BIT_STRING_it(): PASN1_ITEM; syscall AmiSSLBase 264;

function ASN1_BIT_STRING_set(s: PASN1_BIT_STRING location 'a0'; data: PChar location 'a1'; Len: LongInt location 'd0'): LongInt; syscall AmiSSLBase 282;

function ASN1_UTCTIME_set_string(t : PASN1_UTCTIME location 'a0'; S : PAnsiChar location 'a1'): cint; syscall AmiSSLBase 432;


procedure ASN1_UTCTIME_free(a: PASN1_UTCTIME); syscall AmiSSLBase 888;
function ASN1_INTEGER_set(a: PASN1_INTEGER location 'a0'; v: integer location 'd0'): integer; syscall AmiSSLBase 1062;
function ASN1_INTEGER_get(a: PASN1_INTEGER location 'a0'): integer; syscall AmiSSLBase 1068;

function BIO_ctrl_pending(b: PBIO location 'a0'): cInt; syscall AmiSSLBase 1650;
function BIO_new(b: PBIO_METHOD location 'a0'): PBIO; syscall AmiSSLBase 1728;
function BIO_read(bio: PBIO location 'a0'; Data: Pointer location 'a1'; Len: LongInt location 'd0'): LongInt; syscall AmiSSLBase 1752;
function BIO_write(b: PBIO location 'a0'; Buf: PChar location 'a1'; Len: cInt location 'd0'): cInt; syscall AmiSSLBase 1764;
function BIO_puts(bio: PBIO location 'a0'; Buf: PAnsiChar location 'a1'): LongInt; syscall AmiSSLBase 1770;


function BIO_ctrl(bio: PBIO location 'a0'; Cmd: LongInt location 'd0'; larg: LongInt location 'd1'; PArg: Pointer location 'a1'): LongInt syscall AmiSSLBase 1782;

procedure BIO_free_all(bio: PBIO location 'a0'); syscall AmiSSLBase 1818;
function BIO_s_mem: PBIO_METHOD; syscall AmiSSLBase 1884;

function ERR_get_error: cInt; syscall AmiSSLBase 3960;

procedure ERR_clear_error; syscall AmiSSLBase 4014;
procedure ERR_error_string_n(e: cInt location 'd0'; buf: PChar location 'a0'; len: cInt location 'd1'); syscall AmiSSLBase 4026;

function EVP_get_digestbyname(Name: PChar location 'a0'): PEVP_MD; syscall AmiSSLBase 4950;
function EVP_PKEY_assign(pkey: PEVP_PKEY location 'a0'; _type: cInt location 'd0'; key: Prsa location 'a1'): cInt; syscall AmiSSLBase 4992;

function EVP_PKEY_new: PEVP_PKEY; syscall AmiSSLBase 5034;
procedure EVP_PKEY_free(pk: PEVP_PKEY location 'a0'); syscall AmiSSLBase 5040;

function PEM_write_bio_X509(bp: pBIO location 'a0'; x: PX509 location 'a1'): integer; syscall AmiSSLBase 6822;

procedure PKCS12_free(p12: SslPtr location 'a0'); syscall AmiSSLBase 7284;
function PKCS12_parse(p12: SslPtr location 'a0'; pass: PChar location 'a1'; var pkey: SslPtr location 'a2';  var cert: SslPtr location 'a3'; var ca: SslPtr location 'a4'): cInt; syscall AmiSSLBase 7416;
function d2i_PKCS12_bio(b:PBIO location 'a0'; Pkcs12: SslPtr location 'a1'): SslPtr; syscall AmiSSLBase 7434;

function BIO_new_ssl_connect(ctx: PSSL_CTX location 'a0'): PBIO; syscall AmiSSLBase 8178;


function SSL_CTX_set_cipher_list(arg0: PSSL_CTX location 'a0'; str: PChar location 'a1'):cInt; syscall AmiSSLBase 8202;
function SSL_CTX_new(Meth: Pointer location 'a0'): PSSL_CTX; syscall AmiSSLBase 8208;
procedure SSL_CTX_free(a: PSSL_CTX location 'a0'); syscall AmiSSLBase 8214;

function SSL_get_current_cipher(s: PSSL location 'a0'):SslPtr; syscall AmiSSLBase 8262;
function SSL_CIPHER_get_bits(c: SslPtr location 'a0'; alg_bits: PcInt location 'a1'):cInt; syscall AmiSSLBase 8268;
function SSL_CIPHER_get_name(c: Sslptr location 'a0'):PChar; syscall AmiSSLBase 8280;

function SSL_pending(ssl: PSSL location 'a0'):cInt; syscall AmiSSLBase 8352;
function SSL_set_fd(s: PSSL location 'a0'; fd: cInt location 'd0'):cInt; syscall AmiSSLBase 8358;

function SSL_CTX_use_RSAPrivateKey_file(ctx: PSSL_CTX location 'a0'; const _file: PChar location 'a1'; _type: cInt location 'd0'):cInt; syscall AmiSSLBase 8490;
function SSL_CTX_use_certificate_file(ctx: PSSL_CTX location 'a0'; const _file: PChar location 'a1'; _type: cInt location 'd0'):cInt; syscall AmiSSLBase 8502;
function SSL_CTX_use_certificate_chain_file(ctx: PSSL_CTX location 'a0'; const _file: PChar location 'a1'):cInt; syscall AmiSSLBase 8508;


function SSL_get_peer_certificate(ssl: PSSL location 'a0'):PX509; syscall AmiSSLBase 8670;

procedure SSL_CTX_set_verify(ctx: PSSL_CTX location 'a0'; mode: LongInt location 'd0'; arg2: Pointer location 'a1'); syscall AmiSSLBase 8700;
function SSL_CTX_use_PrivateKey(ctx: PSSL_CTX location 'a0'; pkey: sslptr location 'a1'):cInt; syscall AmiSSLBase 8730;
function SSL_CTX_use_PrivateKey_ASN1(pk: cInt location 'd0'; ctx: PSSL_CTX location 'a0'; d: sslptr location 'a1'; len: cInt location 'd1'):cInt; syscall AmiSSLBase 8736;
function SSL_CTX_use_certificate(ctx: PSSL_CTX location 'a0'; x: SslPtr location 'a1'):cInt; syscall AmiSSLBase 8742;
function SSL_CTX_use_certificate_ASN1(ctx: PSSL_CTX location 'a0'; len: cInt location 'd0'; d: SslPtr location 'a1'):cInt; syscall AmiSSLBase 8748;

procedure SSL_CTX_set_default_passwd_cb(ctx: PSSL_CTX location 'a0'; cb: SslPtr location 'a1'); syscall AmiSSLBase 8754;
procedure SSL_CTX_set_default_passwd_cb_userdata(ctx: PSSL_CTX location 'a0'; u: SslPtr location 'a1'); syscall AmiSSLBase 8760;
function SSL_new(ctx: PSSL_CTX location 'a0'):PSSL; syscall AmiSSLBase 8784;
procedure SSL_free(ssl: PSSL location 'a0'); syscall AmiSSLBase 8820;
function SSL_accept(ssl: PSSL location 'a0'):cInt; syscall AmiSSLBase 8826;
function SSL_connect(ssl: PSSL location 'a0'):cInt; syscall AmiSSLBase 8832;
function SSL_read(ssl: PSSL location 'a0'; buf: PChar location 'a1'; num: cInt location 'd0'):cInt; syscall AmiSSLBase 8838;
function SSL_peek(ssl: PSSL location 'a0'; buf: PChar location 'a1'; num: cInt location 'd0'):cInt; syscall AmiSSLBase 8844;
function SSL_write(ssl: PSSL location 'a0'; const buf: PChar location 'a1'; num: cInt location 'd0'):cInt; syscall AmiSSLBase 8850;
function SSL_ctrl(ssl: PSSL location 'a0'; cmd: cInt location 'd0'; larg: clong location 'd1'; parg: Pointer location 'a1'): cLong; syscall AmiSSLBase 8856;

function SSL_CTX_ctrl(ctx: PSSL_CTX location 'a0'; cmd: cInt location 'd0'; larg: clong location 'd1'; parg: Pointer location 'a1'): cLong; syscall AmiSSLBase 8868;
procedure SSL_CTX_callback_ctrl(ctx: PSSL_CTX location 'a0'; _type: cInt location 'd0'; cb: PCallbackCb location 'a1'); syscall AmiSSLBase 8874;
function SSL_get_error(s: PSSL location 'a0'; ret_code: cInt location 'd0'):cInt; syscall AmiSSLBase 8880;
function SSL_get_version(ssl: PSSL location 'a0'):PChar; syscall AmiSSLBase 8886;

function TLSv1_method:PSSL_METHOD; syscall AmiSSLBase 8952;
function TLSv1_server_method:PSSL_METHOD; syscall AmiSSLBase 8958;
function TLSv1_client_method:PSSL_METHOD; syscall AmiSSLBase 8964;

function SSL_shutdown(ssl: PSSL location 'a0'):cInt; syscall AmiSSLBase 8994;

function SSL_CTX_load_verify_locations(ctx: PSSL_CTX location 'a0'; const CAfile: PChar location 'a1'; const CApath: PChar location 'a2'):cInt; syscall AmiSSLBase 9174;
function SSL_get_verify_result(ssl: PSSL location 'a0'):cInt; syscall AmiSSLBase 9222;

function X509_sign(x: PX509 location 'a0'; pkey: PEVP_PKEY location 'a1'; const md: PEVP_MD location 'a2'): cInt; syscall AmiSSLBase 9894;

function X509_digest(data: PX509 location 'a0'; _type: PEVP_MD location 'a1'; md: PChar location 'a2'; len: PcInt location 'a3'):cInt; syscall AmiSSLBase 9924;

function i2d_X509_bio(b: PBIO location 'a0'; x: PX509 location 'a1'): cInt; syscall AmiSSLBase 9954;
function i2d_PrivateKey_bio(b: PBIO location 'a0'; pkey: PEVP_PKEY location 'a1'): cInt; syscall AmiSSLBase 10074;

function X509_new: PX509; syscall AmiSSLBase 10614;
procedure X509_free(x: PX509); syscall AmiSSLBase 10620;

function X509_NAME_oneline(a: PX509_NAME location 'a0'; buf: PChar location 'a1'; size: cInt location 'd0'):PChar; syscall AmiSSLBase 10980;

function X509_set_version(x: PX509 location 'a0'; version: cInt location 'd0'): cInt; syscall AmiSSLBase 11022;

function X509_get_serialNumber(x: PX509 location 'a0'): PASN1_cInt; syscall AmiSSLBase 11034;

function X509_set_issuer_name(x: PX509 location 'a0'; name: PX509_NAME location 'a1'): cInt; syscall AmiSSLBase 11040;
function X509_get_issuer_name(a: PX509 location 'a0'):PX509_NAME; syscall AmiSSLBase 11046;

function X509_get_subject_name(a: PX509 location 'a0'):PX509_NAME; syscall AmiSSLBase 11058;
function X509_set1_notBefore(x: PX509 location 'a0'; tm: PASN1_UTCTIME location 'a1'): cInt; syscall AmiSSLBase 11064;
function X509_set1_notAfter(x: PX509 location 'a0'; tm: PASN1_UTCTIME location 'a1'): cInt; syscall AmiSSLBase 11070;
function X509_set_pubkey(x: PX509 location 'a0'; pkey: PEVP_PKEY location 'a1'): cInt; syscall AmiSSLBase 11076;

function X509_NAME_add_entry_by_txt(name: PX509_NAME location 'a0'; field: PChar location 'a1'; _type: cInt location 'd0'; bytes: PChar location 'a2'; len: cInt location 'd1'; loc: cInt location 'd2'; _set: cInt location 'd3'): cInt; syscall AmiSSLBase 11448;
function X509_NAME_hash(x: PX509_NAME location 'a0'):cuLong; syscall AmiSSLBase 11310;
function X509_print(b: PBIO location 'a0'; a: PX509 location 'a1'): cInt; syscall AmiSSLBase 11340;

function RSA_generate_key(bits: cInt location 'd0'; e: cInt location 'd1'; callback: PFunction location 'a0'; cb_arg: SslPtr location 'a1'): PRSA; syscall AmiSSLBase 14694;


function SSL_get_servername(ssl: PSSL location 'a0'; _type: cInt location 'd0'): PChar; syscall AmiSSLBase 15786;
function SSL_set_SSL_CTX(ssl: PSSL location 'a0'; ctx: PSSL_CTX location 'a1'): PSSL; syscall AmiSSLBase 15792;


function OPENSSL_init_ssl(Opts: QWord location 'd0'; Settings: Pointer location 'a0'): LongInt; syscall AmiSSLBase 26568;
function TLS_client_method(): PSSL_METHOD; syscall AmiSSLBase 26934;
function TLS_method():PSSL_METHOD; syscall AmiSSLBase 26940;
{$endif}

// AmiSSL varargs version
function InitAmiSSL(const Tags: array of PtrUInt): LongInt; inline;
function CleanupAmiSSL(const Tags: array of PtrUInt): LongInt; inline;
function SSLv23_client_method(): Pointer; inline;

function Asn1UtctimeNew: PASN1_UTCTIME;

// shortcuts for strings and Tbytes access
function BioRead(b: PBIO; var Buf: String; Len: cInt): cInt; overload; inline;
function BioRead(b: PBIO; Buf: TBytes; Len: cInt): cInt; overload; inline;
function BioWrite(b: PBIO; Buf: String; Len: cInt): cInt; overload; inline;
function BioWrite(b: PBIO; Buf: TBytes; Len: cInt): cInt; overload; inline;

function SSLGetServername(ssl: PSSL; _type: cInt = TLSEXT_NAMETYPE_host_name): string; inline;

function X509NameAddEntryByTxt(name: PX509_NAME; field: string; _type: cInt; bytes: string; len, loc, _set: cInt): cInt; inline;
function X509NameOneline(a: PX509_NAME; var buf: String; size: cInt):String; inline;
function X509Digest(data: PX509; _type: PEVP_MD; md: String; var len: cInt):cInt; inline;

function EvpGetDigestByName(Name: String): PEVP_MD; inline;

function SslGetVersion(ssl: PSSL):String;
function SslCtxSetCipherList(arg0: PSSL_CTX; var str: String):cInt; inline;
procedure SslCtxSetVerify(ctx: PSSL_CTX; mode: cInt; arg2: TSSLCTXVerifyCallback); inline;
function SslCtxUsePrivateKeyASN1(pk: cInt; ctx: PSSL_CTX; d: String; len: cLong):cInt;overload; inline;
function SslCtxUsePrivateKeyASN1(pk: cInt; ctx: PSSL_CTX; b: TBytes; len: cLong):cInt;overload; inline;
function SslCtxUsePrivateKeyFile(ctx: PSSL_CTX; const _file: String; _type: cInt):cInt; inline;

function SslCtxUseCertificateASN1(ctx: PSSL_CTX; len: cLong; d: String):cInt; overload; inline;
function SslCtxUseCertificateASN1(ctx: PSSL_CTX; len: cLong; Buf: TBytes): cInt; overload; inline;
function SslCtxUseCertificateFile(ctx: PSSL_CTX; const _file: String; _type: cInt):cInt; inline;
function SslCtxUseCertificateChainFile(ctx: PSSL_CTX; const _file: String):cInt; inline;
function SslCtxLoadVerifyLocations(ctx: PSSL_CTX; const CAfile: String; const CApath: String):cInt; inline;

function PKCS12parse(p12: SslPtr; pass: string; var pkey, cert, ca: SslPtr): cInt; inline;

function SSLCipherGetName(c: SslPtr):String; inline;
function SSLCipherGetBits(c: SslPtr; var alg_bits: cInt):cInt; inline;

procedure ErrErrorString(e: cInt; var buf: string; len: cInt); inline;

const
// crypto
// Standard initialisation options
  OPENSSL_INIT_NO_LOAD_CRYPTO_STRINGS = $00000001;
  OPENSSL_INIT_LOAD_CRYPTO_STRINGS    = $00000002;
  OPENSSL_INIT_ADD_ALL_CIPHERS        = $00000004;
  OPENSSL_INIT_ADD_ALL_DIGESTS        = $00000008;
  OPENSSL_INIT_NO_ADD_ALL_CIPHERS     = $00000010;
  OPENSSL_INIT_NO_ADD_ALL_DIGESTS     = $00000020;
  OPENSSL_INIT_LOAD_CONFIG            = $00000040;
  OPENSSL_INIT_NO_LOAD_CONFIG         = $00000080;
  OPENSSL_INIT_ASYNC                  = $00000100;
  OPENSSL_INIT_ENGINE_RDRAND          = $00000200;
  OPENSSL_INIT_ENGINE_DYNAMIC         = $00000400;
  OPENSSL_INIT_ENGINE_OPENSSL         = $00000800;
  OPENSSL_INIT_ENGINE_CRYPTODEV       = $00001000;
  OPENSSL_INIT_ENGINE_CAPI            = $00002000;
  OPENSSL_INIT_ENGINE_PADLOCK         = $00004000;
  OPENSSL_INIT_ENGINE_AFALG           = $00008000;
// OPENSSL_INIT flag = $00010000 reserved for internal use
// OPENSSL_INIT flag range = $fff00000 reserved for OPENSSL_init_ssl()
// Max OPENSSL_INIT flag value is = $80000000

// OPENSSL_INIT flag 0x010000 reserved for internal use
  OPENSSL_INIT_NO_LOAD_SSL_STRINGS   = $00100000;
  OPENSSL_INIT_LOAD_SSL_STRINGS      = $00200000;

function SSLeay_add_ssl_algorithms(): LongInt;
function SSL_load_error_strings(): LongInt;

implementation

// AmiSSL varargs version

function InitAmiSSL(const Tags: array of PtrUInt): LongInt;
begin
  InitAmiSSL := InitAmiSSLA(@Tags);
end;

function CleanupAmiSSL(const Tags: array of PtrUInt): LongInt;
begin
  if Assigned(AmiSSLBase) then
    CleanupAmiSSL := CleanupAmiSSLA(@Tags);
end;


function SSLeay_add_ssl_algorithms(): LongInt;
begin
  SSLeay_add_ssl_algorithms := OPENSSL_init_ssl(0, nil);
end;

function SSL_load_error_strings(): LongInt;
begin
  SSL_load_error_strings := OPENSSL_init_SSL(OPENSSL_INIT_LOAD_SSL_STRINGS or OPENSSL_INIT_LOAD_CRYPTO_STRINGS, nil);
end;

function SSLv23_client_method(): Pointer;
begin
  SSLv23_client_method := TLS_client_method;
end;

function BioRead(b: PBIO; var Buf: String; Len: cInt): cInt;
begin
  Result := Bio_Read(b, PChar(Buf), Len)
end;

function BioRead(b: PBIO; Buf: TBytes; Len: cInt): cInt;
begin
  Result := BIO_read(b, PChar(Buf), Len);
end;

function SSLGetServername(ssl: PSSL; _type: cInt = TLSEXT_NAMETYPE_host_name): string;
begin
  Result := PChar(SSL_Get_Servername(ssl, _type));
end;

function X509NameAddEntryByTxt(name: PX509_NAME; field: string; _type: cInt; bytes: string; len, loc, _set: cInt): cInt;
begin
  Result := X509_NAME_add_entry_by_txt(name, PChar(field), _type, PChar(Bytes), len, loc, _set);
end;

function Asn1UtctimeNew: PASN1_UTCTIME;
begin
  Result:=PASN1_UTCTIME(ASN1_STRING_type_new(V_ASN1_UTCTIME));
end;

function EvpGetDigestByName(Name: String): PEVP_MD;
begin
  Result := EVP_get_digestbyname(PChar(Name));
end;

function SslCtxSetCipherList(arg0: PSSL_CTX; var str: String):cInt;
begin
  Result := SSL_CTX_set_cipher_list(arg0, PChar(str))
end;

procedure SslCtxSetVerify(ctx: PSSL_CTX; mode: cInt; arg2: TSSLCTXVerifyCallback);
begin
  SSL_CTX_set_verify(ctx, mode, @arg2);
end;

function SslCtxUsePrivateKeyASN1(pk: cInt; ctx: PSSL_CTX; d: String; len: cLong):cInt; overload;
begin
  Result := SSL_CTX_use_PrivateKey_ASN1(pk, ctx, Sslptr(d), len)
end;

function SslCtxUsePrivateKeyASN1(pk: cInt; ctx: PSSL_CTX; b: TBytes; len: cLong): cInt;overload;
begin
  Result := SSL_CTX_use_PrivateKey_ASN1(pk, ctx, Sslptr(b), len)
end;

function SslCtxUsePrivateKeyFile(ctx: PSSL_CTX; const _file: String; _type: cInt):cInt;
begin
  Result := SSL_CTX_use_RSAPrivateKey_file(ctx, PChar(_file), _type);
end;

function SslCtxUseCertificateASN1(ctx: PSSL_CTX; len: cLong; d: String):cInt;
begin
  Result := SSL_CTX_use_certificate_ASN1(ctx, len, SslPtr(d))
end;

function SslCtxUseCertificateASN1(ctx: PSSL_CTX; len: cLong; Buf: TBytes): cInt;
begin
  Result := SSL_CTX_use_certificate_ASN1(ctx, len, SslPtr(Buf))
end;

function SslCtxUseCertificateFile(ctx: PSSL_CTX; const _file: String; _type: cInt):cInt;
begin
  Result := SSL_CTX_use_certificate_file(ctx, PChar(_file), _type)
end;

function SslCtxUseCertificateChainFile(ctx: PSSL_CTX; const _file: String):cInt;
begin
  Result := SSL_CTX_use_certificate_chain_file(ctx, PChar(_file))
end;

function SslCtxLoadVerifyLocations(ctx: PSSL_CTX; const CAfile: String; const CApath: String):cInt;
begin
  Result := SSL_CTX_load_verify_locations(ctx, SslPtr(CAfile), SslPtr(CApath))
end;

function BioWrite(b: PBIO; Buf: String; Len: cInt): cInt;
begin
  Result := Bio_Write(b, PChar(Buf), Len)
end;

function BioWrite(b: PBIO; Buf: TBytes; Len: cInt): cInt;
begin
  Result := Bio_Write(b, PChar(Buf), Len)
end;

function PKCS12parse(p12: SslPtr; pass: string; var pkey, cert, ca: SslPtr): cInt;
begin
  Result := PKCS12_parse(p12, SslPtr(pass), pkey, cert, ca)
end;

function SslGetVersion(ssl: PSSL):String;
begin
  Result := SSL_get_version(ssl)
end;

function X509NameOneline(a: PX509_NAME; var buf: String; size: cInt):String;
begin
  Result := X509_NAME_oneline(a, PChar(buf),size)
end;

function X509Digest(data: PX509; _type: PEVP_MD; md: String; var len: cInt):cInt;
begin
  Result := X509_digest(data, _type, PChar(md), @len)
end;

function SSLCipherGetName(c: SslPtr):String;
begin
  Result := SSL_CIPHER_get_name(c)
end;

function SSLCipherGetBits(c: SslPtr; var alg_bits: cInt):cInt;
begin
  Result := SSL_CIPHER_get_bits(c, @alg_bits)
end;

procedure ErrErrorString(e: cInt; var buf: string; len: cInt);
begin
  ERR_error_string_n(e, @buf[1], len);
  buf := PChar(Buf);
end;

initialization
  AmiSSLMasterBase := OpenLibrary(AMISSLMASTER_NAME, AMISSLMASTER_MIN_VERSION);
  if Assigned(AmiSSLMasterBase) then
  begin
    AmiSSLBase := nil;
    if InitAmiSSLMaster(AMISSL_V303, 0) <> 0 then
      AmiSSLBase := OpenAmiSSL();
    if not Assigned(AmiSSLBase) then
    begin
      if InitAmiSSLMaster(AMISSL_CURRENT_VERSION, 0) <> 0 then
        AmiSSLBase := OpenAmiSSL()
    end;
  end;
finalization
  if Assigned(AmiSSLBase) then
    CloseAmiSSL();
  if Assigned(AmiSSLMasterBase) then
    CloseLibrary(AmiSSLMasterBase);
end.
