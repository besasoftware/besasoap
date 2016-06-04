(******************************************************)
(*                                                    *)
(*            Besa Software Classes Library           *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// Utility Library 
/// </summary>
unit bsUtil;

interface

/// Base64 encoding of a string
function Base64Encode(const s: String): String;

/// Base64 decoding of a string
function Base64Decode(const s: String): String;


function SHA1(s:String):String;
implementation


// Base64 string encoding
function Base64Encode(const s: String): String;
procedure Encode(rp, sp: PChar; len: integer);
const
  b64: array[0..63] of Char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var i: integer;
    c: cardinal;
begin
  for i := 1 to len div 3 do begin
    c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8 + ord(sp[2]);
    rp[0] := b64[(c shr 18) and $3f];
    rp[1] := b64[(c shr 12) and $3f];
    rp[2] := b64[(c shr 6) and $3f];
    rp[3] := b64[c and $3f];
    inc(rp,4);
    inc(sp,3);
  end;
  case len mod 3 of
    1: begin
      c := ord(sp[0]) shl 16;
      rp[0] := b64[(c shr 18) and $3f];
      rp[1] := b64[(c shr 12) and $3f];
      rp[2] := '=';
      rp[3] := '=';
    end;
    2: begin
      c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8;
      rp[0] := b64[(c shr 18) and $3f];
      rp[1] := b64[(c shr 12) and $3f];
      rp[2] := b64[(c shr 6) and $3f];
      rp[3] := '=';
    end;
  end;
end;
var len: integer;
begin
  result:='';
  len := length(s);
  if len = 0 then exit;
  SetLength(result, ((len + 2) div 3) * 4);
  Encode(pointer(result),pointer(s),len);
end;

function Base64Decode(const s: String): String;
var i, j, len: integer;
    sp, rp: PChar;
    c, ch: integer;
begin
  result:= '';
  len := length(s);
  if (len = 0) or (len mod 4 <> 0) then
    exit;
  len := len shr 2;
  SetLength(result, len * 3);
  sp := pointer(s);
  rp := pointer(result);
  for i := 1 to len do begin
    c := 0;
    j := 0;
    while true do begin
      ch := ord(sp[j]);
      case chr(ch) of
        'A'..'Z': c := c or (ch - ord('A'));
        'a'..'z': c := c or (ch - (ord('a')-26));
        '0'..'9': c := c or (ch - (ord('0')-52));
        '+': c := c or 62;
        '/': c := c or 63;
        else
        if j=3 then begin
          rp[0] := Char(c shr 16);
          rp[1] := Char(c shr 8);
          SetLength(result, len*3-1);
          exit;
        end else begin
          rp[0] := Char(c shr 10);
          SetLength(result, len*3-2);
          exit;
        end;
      end;
      if j=3 then break;
      inc(j);
      c := c shl 6;
    end;
    rp[2] := Char(c);
    c := c shr 8;
    rp[1] := Char(c);
    c := c shr 8;
    rp[0] := Char(c);
    inc(rp,3);
    inc(sp,4);
  end;
end;



{-----------------------------------------------------------------------------}
type
TSHA1Word = Cardinal;
TSHA1Buf = array[0..4] of TSHA1Word;
TSHA1In = array[0..15] of TSHA1Word;
TSHA1WArray = array[0..79] of TSHA1Word;
TSHA1Context = record
buf: TSHA1Buf;
bytes: array[0..1] of TSHA1Word;
in_: TSHA1In;
W: TSHA1WArray;
end;
TSHA1Digest = array[0..19] of Byte;

//procedure SHA1Init(var ctx: TSHA1Context); forward;
//procedure SHA1Update(var ctx: TSHA1Context; const buffer; len: Cardinal); forward;
//function SHA1Final(var ctx: TSHA1Context): TSHA1Digest; forward;

//function SHA1Buf(const Buffer; Len: Cardinal): TSHA1Digest;
//function SHA1DigestsEqual(const A, B: TSHA1Digest): Boolean;
//function SHA1DigestToString(const D: TSHA1Digest): RawByteString; forward;

//procedure HashString(s: RawByteString; var Digest: TSHA1Digest); forward;


(*
* Start SHA-1 accumulation. Set byte count to 0 and buffer to mysterious
* initialization constants.
*)
procedure SHA1Init(var ctx: TSHA1Context);
begin
ctx.buf[0] := TSHA1Word($67452301);
ctx.buf[1] := TSHA1Word($efcdab89);
ctx.buf[2] := TSHA1Word($98badcfe);
ctx.buf[3] := TSHA1Word($10325476);
ctx.buf[4] := TSHA1Word($c3d2e1f0);

ctx.bytes[0] := 0;
ctx.bytes[1] := 0;
end;


procedure SHA1Transform(var buf: TSHA1Buf; const in_: TSHA1In; var W: TSHA1WArray); forward;
(*
* Update context to reflect the concatenation of another buffer full
* of bytes.
*)
procedure SHA1Update(var ctx: TSHA1Context; const buffer; len: Cardinal);
var
buf: ^Byte;
t: TSHA1Word;
begin
buf := @buffer;

{ Update byte count }
t := ctx.bytes[0];
Inc(ctx.bytes[0], len);
if Cardinal(ctx.bytes[0]) < Cardinal(t) then
Inc(ctx.bytes[1]); { Carry from low to high }

t := 64 - (t and $3f); { Space available in ctx.in (at least 1) }
if Cardinal(t) > Cardinal(len) then begin
Move(buf^, Pointer(Cardinal(@ctx.in_) + 64 - t)^, len);
Exit;
end;
{ First chunk is an odd size }
Move(buf^, Pointer(Cardinal(@ctx.in_) + 64 - t)^, t);
SHA1Transform(ctx.buf, ctx.in_, ctx.W);
Inc(buf, t);
Dec(len, t);

{ Process data in 64-byte chunks }
while Cardinal(len) >= Cardinal(64) do begin
Move(buf^, ctx.in_, 64);
SHA1Transform(ctx.buf, ctx.in_, ctx.W);
Inc(buf, 64);
Dec(len, 64);
end;

{ Handle any remaining bytes of data. }
Move(buf^, ctx.in_, len);
end;


function ByteSwap(const X: TSHA1Word): TSHA1Word;
begin
Result :=
(X shl 24) or
((X and $FF00) shl 8) or
((X and $FF0000) shr 8) or
(X shr 24);
end;

(*
* Final wrapup - pad to 64-byte boundary with the bit pattern
* 1 0* (64-bit count of bits processed, MSB-first)
*)
function SHA1Final(var ctx: TSHA1Context): TSHA1Digest;
var
count, i: Integer;
p: ^Byte;
begin
count := ctx.bytes[0] and $3f; { Number of bytes in ctx.in }
p := @ctx.in_;
Inc(p, count);

{ Set the first char of padding to 0x80. There is always room. }
p^ := $80;
Inc(p);

{ Bytes of padding needed to make 56 bytes (-8..55) }
count := 56 - 1 - count;

if count < 0 then begin { Padding forces an extra block }
FillChar(p^, count + 8, 0);
SHA1Transform(ctx.buf, ctx.in_, ctx.W);
p := @ctx.in_;
count := 56;
end;
FillChar(p^, count, 0);

{ Append length in bits and transform }
ctx.in_[15] := ByteSwap(ctx.bytes[0] shl 3);
ctx.in_[14] := ByteSwap((ctx.bytes[1] shl 3) or (ctx.bytes[0] shr 29));
SHA1Transform(ctx.buf, ctx.in_, ctx.W);

for i := 0 to High(ctx.buf) do
ctx.buf[i] := ByteSwap(ctx.buf[i]);
Move(ctx.buf, Result, SizeOf(Result));
FillChar(ctx, SizeOf(ctx), 0); { In case it's sensitive }
end;

procedure HashString(s: String; var Digest: TSHA1Digest);
var
Context: TSHA1Context;
begin
SHA1Init(Context);
SHA1Update(Context, S[1], Length(S));
Digest := SHA1Final(Context);
end;









(*
* The core of the SHA-1 algorithm, this alters an existing SHA-1 hash to
* reflect the addition of 16 longwords of new data. SHA1Update blocks
* the data and converts bytes into longwords for this routine.
*)
procedure SHA1Transform(var buf: TSHA1Buf; const in_: TSHA1In; var W: TSHA1WArray);
const
K1 = $5A827999;
K2 = $6ED9EBA1;
K3 = $8F1BBCDC;
K4 = $CA62C1D6;
var
t: Integer;
temp, A, B, C, D, E: TSHA1Word;
begin
for t := 0 to 15 do begin
{ ByteSwap inlined: }
temp := in_[t];
W[t] := (temp shl 24) or
((temp and $FF00) shl 8) or
((temp and $FF0000) shr 8) or
(temp shr 24);
end;

for t := 16 to 79 do begin
temp := W[t-3] xor W[t-8] xor W[t-14] xor W[t-16];
W[t] := (temp shl 1) or (temp shr (32-1));
end;

A := buf[0];
B := buf[1];
C := buf[2];
D := buf[3];
E := buf[4];

for t := 0 to 19 do begin
temp := ((A shl 5) or (A shr (32-5))) +
(D xor (B and (C xor D))) + E + W[t] + K1;
E := D;
D := C;
C := (B shl 30) or (B shr (32-30));
B := A;
A := temp;
end;

for t := 20 to 39 do begin
temp := ((A shl 5) or (A shr (32-5))) + (B xor C xor D) + E + W[t] + K2;
E := D;
D := C;
C := (B shl 30) or (B shr (32-30));
B := A;
A := temp;
end;

for t := 40 to 59 do begin
temp := ((A shl 5) or (A shr (32-5))) +
((B and C) or (B and D) or (C and D)) + E + W[t] + K3;
E := D;
D := C;
C := (B shl 30) or (B shr (32-30));
B := A;
A := temp;
end;

for t := 60 to 79 do begin
temp := ((A shl 5) or (A shr (32-5))) + (B xor C xor D) + E + W[t] + K4;
E := D;
D := C;
C := (B shl 30) or (B shr (32-30));
B := A;
A := temp;
end;

Inc(buf[0], A);
Inc(buf[1], B);
Inc(buf[2], C);
Inc(buf[3], D);
Inc(buf[4], E);
end;

{ New functions by JR: }

function SHA1Buf(const Buffer; Len: Cardinal): TSHA1Digest;
var
Context: TSHA1Context;
begin
SHA1Init(Context);
SHA1Update(Context, Buffer, Len);
Result := SHA1Final(Context);
end;

function SHA1DigestsEqual(const A, B: TSHA1Digest): Boolean;
var
I: Integer;
begin
for I := Low(TSHA1Digest) to High(TSHA1Digest) do
if A[I] <> B[I] then begin
Result := False;
Exit;
end;
Result := True;
end;

function SHA1DigestToString(const D: TSHA1Digest): String;
const
Digits: array[0..15] of Char = '0123456789abcdef';
var
Buf: array[0..39] of Char;
P: PChar;
I: Integer;
begin
P := @Buf;
for I := 0 to 19 do begin
P^ := Digits[D[I] shr 4];
Inc(P);
P^ := Digits[D[I] and 15];
Inc(P);
end;
SetString(Result, Buf, 40);
end;

function SHA1(s:String):String;
var
Digest: TSHA1Digest;
begin
HashString(S,Digest);
Result := SHA1DigestToString(Digest);
end;

end.
