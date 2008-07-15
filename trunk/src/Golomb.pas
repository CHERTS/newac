(*
 This unit contains routines for signed Golomb codes manipulation.
*)

(* $Id$ *)

unit Golomb;

interface

uses
  SysUtils, Math;


type

  // 2147483646 - the maximum number of bytes Delphi can swallow
  ArrayOfByte = array[0..2147483646] of Byte;
  PArrayOfByte = ^ArrayOfByte;

  // 536870910 (?) - the maximum number of Integers Delphi can swallow
  ArrayOfInteger = array[0..536870910] of Integer;
  PArrayOfInteger = ^ArrayOfInteger;

  (* Encodes a set of Integers assed via Data array using Golomb codes. Codes sre stored in memory area pointed to by Block.
   The Block os created with the routine by GetMem and should be freed by FreeMem when it is no more needed.
   The length of the new block in bytes is returned via BlockLength. M - Golomb parameter. *)
  procedure GolombNewBlock(Data : PArrayOfInteger; DataLength : LongWord; var Block : PArrayOfByte; var BlockLength : LongWord; var M : Word);

  procedure GolombDecodeBlock(Data : PArrayOfInteger; DataLength : LongWord; Block : PArrayOfByte; BlockLength : LongWord; M : Word);

implementation

function ReadBit(var Offset : LongWord; Source : PArrayOfByte) : Byte;
var
  mask : Byte;
begin
  mask := 1 shl (offset mod 8);
  Result := (Source[Offset div 8] and mask) div mask;
  Inc(Offset);
end;

procedure WriteBit(var Offset : LongWord; Dest : PArrayOfByte; Bit : Byte);
var
  mask : Byte;
begin
  mask := 1 shl (offset mod 8);
  if Bit = 0 then
    Dest[Offset div 8] := Dest[Offset div 8] and ($FF - mask)
  else
    Dest[offset div 8] := Dest[offset div 8] or mask;
  Inc(Offset);
end;

procedure WriteBits(var Offset : LongWord; Dest : PArrayOfByte; Bits : LongWord; Count : Byte);
var
  rev : LongWord;
  i : Integer;
  mask : Byte;
begin
  rev := 0;
  for i := 0 to Count -1 do
  begin
    rev := (rev shl 1) + (Bits mod 2);
    Bits := Bits shr 1;
  end;
  for i := 0 to Count -1 do
  begin
    mask := 1 shl (Offset mod 8);
    if rev mod 2 = 0 then
      Dest[offset div 8] := Dest[offset div 8] and ($ff - mask)
    else
      Dest[offset div 8] := Dest[offset div 8] or mask;
    rev := rev shr 1;
    Inc(Offset)
  end;
end;

procedure ReadBits(var Offset : LongWord; Source : PArrayOfByte; var Bits : LongWord; Count : Byte);
var
  i : Integer;
  mask : Byte;
begin
  Bits := 0;
  for i := 0 to Count - 1 do
  begin
    Bits :=  Bits shl 1;
    mask := 1 shl (Offset mod 8);
    Bits := Bits + ((Source[Offset div 8] and mask) div mask);
    Inc(Offset);
  end;
end;

procedure GolombEncode(var Offset : LongWord; Dest : PArrayOfByte; Number : Integer; M, clm : Word);
var
  r, q, bits : LongWord;
begin
  if Number < 0 then
  begin
    WriteBit(Offset, Dest, 1);
    Number := System.Abs(Number);
  end else
    WriteBit(Offset, Dest, 0);
  q := Number div M;
  r := Number mod  M;
  while q >= 32 do
  begin
    WriteBits(Offset, Dest, $ffffffff, 32);
    Dec(q, 32);
  end;
  bits := (1 shl q) - 1;
  WriteBits(Offset, Dest, Bits, q);
  if r < ((1 shl clm) - M) then
    WriteBits(Offset, Dest, r, clm)
  else
    WriteBits(Offset, Dest, r + (1 shl clm) - M, clm + 1);
end;

function GolombDecode(var Offset : LongWord; Source : PArrayOfByte; M, clm : Word) : Integer;
var
  r, q : LongWord;
   sign : ShortInt;
begin
  if ReadBit(Offset, Source) = 0 then
    sign := 1
  else
    sign := -1;
  r := 0;
  q := 0;
  while ReadBit(Offset, Source) = 1 do Inc(q);
  if M > 1 then
  begin
    ReadBits(Offset, Source, r, clm-1);
    if r >= ((1 shl clm) - M) then
    begin
      r := (r shl 1) + ReadBit(Offset, Source);
      r := r - Word((1 shl clm) - M);
    end;
  end;
  Result := (q*M + r)*sign;
end;

function CodeLength(Number : Integer; M, clm : Word) : Word;
var
  ql, rl : Word;
  r : LongWord;
begin
  Number := Abs(Number);
  ql := number div M;
  r := number mod M;
  rl := clm + 1;
  if r < ((1 shl clm) - M) then Dec(rl);
  Result := ql + rl + 1;
end;

procedure GolombNewBlock(Data : PArrayOfInteger; DataLength : LongWord; var Block : PArrayOfByte; var BlockLength : LongWord; var M : Word);
var
  Offset, NewLength : LongWord;
  clm, NewM : Word;
  max : Integer;
  i : LongWord;
begin
  Offset := 0;
  NewLength := 0;
  clm := 0;
  NewM := 0;
  max := 0;
  for i := 0 to DataLength-1 do
    if (Abs(Data[i]) > max) then
      max := Abs(Data[i]);
  if max-1 < 256 then
    NewM := max-1
  else
    NewM := 256;
  if NewM <= 0 then NewM := 1;
  clm := Ceil(Log2(NewM));
  for i := 0 to DataLength -1 do
    NewLength := NewLength + CodeLength(data[i], NewM, clm);
  repeat
    BlockLength := NewLength;
    M := NewM;
    if NewM = 1 then
      Break;
    NewLength := 0;
    Dec(NewM);
    clm := Ceil(Log2(NewM));
    for i := 0 to  DataLength - 1 do
      NewLength := NewLength + CodeLength(data[i], NewM, clm);
  until NewLength > BlockLength;
  clm := Ceil(Log2(M));
  BlockLength := (BlockLength + 7) div 8;
  GetMem(Block,	BlockLength);
  for i := 0 to  DataLength - 1 do
    GolombEncode(Offset, Block, Data[i], M, clm);
end;

procedure GolombDecodeBlock(Data : PArrayOfInteger; DataLength : LongWord; Block : PArrayOfByte; BlockLength : LongWord; M : Word);
var
  Offset : LongWord;
  clm : Word;
  i : LongWord;
begin
  Offset := 0;
  BlockLength := BlockLength*8;
  clm := Ceil(Log2(M));
  for i := 0 to DataLength -1 do
  begin
    if offset > BlockLength then
      raise Exception.Create('Invalid Golomb Block');
    Data[i] := GolombDecode(Offset, Block, M, clm);
  end;
end;

end.
