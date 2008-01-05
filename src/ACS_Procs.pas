(*
  This file is a part of New Audio Components package v 1.0
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Revision: 1.4 $ $Date: 2007/08/19 11:01:16 $ *)

unit ACS_Procs;

{ Title: ACS_Procs
    Utility procedures. }

interface

uses
  SysUtils, ACS_Types, Math;

type

  TFilterWindowType = (fwHamming, fwHann, fwBlackman);

{$IFDEF LINUX}
  function FindLibs(const Pattern : String) : String;
{$ENDIF}


  // Direction = 1 - forward FFT, Direction = -1 - inverse FFT.
  procedure ComplexFFT(Data : PComplexArray; DataSize, Direction : Integer);

  procedure HannWindow(OutData : PDoubleArray; Width : Integer; Symmetric : Boolean);

  procedure HammingWindow(OutData : PDoubleArray; Width : Integer; Symmetric : Boolean);

  procedure BlackmanWindow(OutData : PDoubleArray; Width : Integer; Symmetric : Boolean);

  procedure CalculateSincKernel(OutData : PDoubleArray; CutOff : Double; Width : Integer; WType : TFilterWindowType);

  procedure SmallIntArrayToDouble(InData : PSmallInt; OutData : PDouble; DataSize : Integer);

  procedure SmallIntArrayToComplex(InData : PSmallInt; OutData : PComplex; DataSize : Integer);


  // Computes Op2[i] = Op1[i]*Op2[i], i = [0..DataSize-1]

  procedure MultDoubleArrays(Op1, Op2 : PDouble; DataSize : Integer);

  (*
    Performs calculation of
                   /
                  | Lg(Abs(InData[i])) + Shift, if Lg(Abs(InData[i])) + Shift >= 0
    OutData[i] = <  0, if Lg(Abs(InData[i])) + Shift < 0
                  | 0, if Abs(InData[i]) = 0.
                  \
    i = [0..DataSize-1]
  *)
  procedure LgMagnitude(InData : PComplex; OutData : PDouble; DataSize, Shift : Integer);

  (*
    An important note about following Convert* routines.
    Conversion is performed in-place, input data is passed via InOutBuf
    and output is returned in the same parameter.
    InSize holds the number of meaningful input bytes in InOutBuf,
    but InOutBuf's length itself should be no less than the output size.
    For example, if you convert 8 bps sound to 16 bps with Convert16To8,
    and there are 1024 8-bit samples in the buffer, the InSize value should be
    1024 and the actual buffer length must be no less than 2048.
  *)

  // Convert 24 bit samples to 16 bit

  procedure Convert24To16(InOutBuf : PBuffer16; InSize : Integer);

  // Convert 16 bit samples to 24 bit
  procedure Convert16To24(InOutBuf : PBuffer16; InSize : Integer);

  // Convert 16 bit samples to 8 bit
  procedure Convert16To8(InOutBuf : PBuffer8; InSize : Integer);

  // Convert 16 bit samples to Single
  procedure Convert16ToSingle(InBuf : PBuffer16; OutBuf : PSingleArray; Elements : Integer);

  // Convert 8 bit samples to 16 bit
  procedure Convert8To16(InOutBuf : PBuffer8; InSize : Integer);

  // Convert 8 bit samples to Single
  procedure Convert8ToSingle(InBuf : PBuffer8; var OutBuf : PSingleArray; Elements : Integer);

  // Convert Single to 16 bit
  procedure ConvertSingleTo16(var OutBuf : PSingleArray; InBuf : PBuffer16; Elements : Integer);

  // Convert Single to 8 bit
  procedure ConvertSingleTo8(var OutBuf : PSingleArray; InBuf : PBuffer8; Elements : Integer);

  // Convert mono 16-bit audio stream to stereo
  procedure ConvertMonoToStereo16(InOutBuf : PBuffer16; InSize : Integer; Mode : TMSConverterMode);

  // Convert mono 8-bit audio stream to stereo
  procedure ConvertMonoToStereo8(InOutBuf : PBuffer8; InSize : Integer; Mode : TMSConverterMode);

  // Convert mono 24-bit audio stream to stereo
  procedure ConvertMonoToStereo24(InOutBuf : PBuffer8; InSize : Integer; Mode : TMSConverterMode);

  // Convert stereo 24-bit audio stream to mono
  procedure ConvertStereoToMono24(InOutBuf : PBuffer8; InSize : Integer);

  // Convert stereo 16-bit audio stream to mono
  procedure ConvertStereoToMono16(InOutBuf : PBuffer16; InSize : Integer);

  // Convert stereo 8-bit audio stream to mono
  procedure ConvertStereoToMono8(InOutBuf : PBuffer8; InSize : Integer);

  function GetRightOf(Delim : Char; const S : String) : String;

  function GetLeftOf(Delim : Char; const S : String) : String;

implementation

{$IFDEF LINUX}
  function FindLibs(const Pattern : String) : String;
  var
    Path : String;
    SR : TSearchRec;
  begin
    Path := '/usr/lib/';
    if FindFirst(Path + Pattern, faAnyFile, SR) = 0 then
    begin
      Result := SR.Name;
      FindClose(SR);
      Exit;
    end;
    Path := '/usr/local/lib/';
    if FindFirst(Path + Pattern, faAnyFile, SR) = 0 then
    begin
      Result := SR.Name;
      FindClose(SR);
      Exit;
    end;
    Result := '';
  end;
{$ENDIF}


  procedure Convert8ToSingle(InBuf : PBuffer8; var OutBuf : PSingleArray; Elements : Integer);
  var
    i : Integer;
  begin
    for i := 0 to Elements - 1 do OutBuf[i] := InBuf[i];
  end;

  procedure Convert16ToSingle(InBuf : PBuffer16; OutBuf : PSingleArray; Elements : Integer);
  var
    i : Integer;
  begin
    for i := 0 to Elements - 1 do OutBuf[i] := InBuf[i];
  end;

  procedure ConvertSingleTo8(var OutBuf : PSingleArray; InBuf : PBuffer8; Elements : Integer);
  var
    i : Integer;
  begin
    for i := 0 to Elements - 1 do InBuf[i] := Round(OutBuf[i]);
  end;

  procedure ConvertSingleTo16(var OutBuf : PSingleArray; InBuf : PBuffer16; Elements : Integer);
  var
    i : Integer;
  begin
    for i := 0 to Elements - 1 do InBuf[i] := Round(OutBuf[i]);
  end;

  procedure Convert16To8(InOutBuf : PBuffer8; InSize : Integer);
  var
    i : Integer;
    P : PBuffer16;
  begin
    P := @InOutBuf[0];
    for i := 0 to (Insize shr 1) -1 do
    InOutBuf[i] := Hi(P[i]+$8000);
  end;

  procedure Convert8To16(InOutBuf : PBuffer8; InSize : Integer);
  var
    i : Integer;
    P : PBuffer16;
  begin
    P := @InOutBuf[0];
    for i := Insize - 1 downto 0 do P[i] := (InOutBuf[i] shl 8) - $8000;
  end;

  procedure ConvertStereoToMono16(InOutBuf : PBuffer16; InSize : Integer);
  var
    i : Integer;
  begin
    for i := 0 to (Insize shr 2) - 1 do
    begin
      InOutBuf[i] := (InOutBuf[i shl 1] + InOutBuf[(i shl 1)+1]) div 2;
    end;
  end;

  procedure ConvertStereoToMono8(InOutBuf : PBuffer8; InSize : Integer);
  var
    i : Integer;
  begin
    for i := 0 to (Insize shr 1) - 1 do
    begin
      InOutBuf[i] := (InOutBuf[i shl 1] + InOutBuf[(i shl 1)+1]) div 2;
    end;
  end;

  procedure ConvertMonoToStereo16(InOutBuf : PBuffer16; InSize : Integer; Mode : TMSConverterMode);
  var
    i : Integer;
  begin
    case Mode of
      msmMonoToBoth :
      for i := (Insize shr 1) - 1 downto 0 do
      begin
        InOutBuf[i shl 1] := InOutBuf[i];
        InOutBuf[(i shl 1)+1] := InOutBuf[i];
      end;
      msmMonoToLeft :
      for i := (Insize shr 1) - 1 downto 0 do
      begin
        InOutBuf[i shl 1] := 0;
        InOutBuf[(i shl 1)+1] := InOutBuf[i];
      end;
      msmMonoToRight :
      for i := (Insize shr 1) - 1 downto 0 do
      begin
        InOutBuf[i shl 1] := InOutBuf[i];
        InOutBuf[(i shl 1)+1] := 0;
      end;
    end;
  end;

  procedure ConvertMonoToStereo8(InOutBuf : PBuffer8; InSize : Integer; Mode : TMSConverterMode);
  var
    i : Integer;
  begin
    case Mode of
      msmMonoToBoth :
      for i := Insize - 1 downto 0 do
      begin
        InOutBuf[i shl 1] := InOutBuf[i];
        InOutBuf[(i shl 1)+1] := InOutBuf[i];
      end;
      msmMonoToLeft :
      for i := Insize - 1 downto 0 do
      begin
        InOutBuf[i shl 1] := 0;
        InOutBuf[(i shl 1)+1] := InOutBuf[i];
      end;
      msmMonoToRight :
      for i := Insize - 1 downto 0 do
      begin
        InOutBuf[i shl 1] := InOutBuf[i];
        InOutBuf[(i shl 1)+1] := 0;
      end;
    end;
  end;

  (* This routine is converted from the original C code by P. Burke
   Direction = 1 - forward FFT, Direction = -1 - inverse FFT. *)
  procedure ComplexFFT(Data : PComplexArray; DataSize, Direction : Integer);
  var
    i, i1, j, k, i2, l, l1, l2, Log2n : Integer;
    c1, c2, tx, ty, t1, t2, u1, u2, z  : Double;
  begin
    Log2n := Trunc(Log2(DataSize));
    // Do the bit reversal
    i2 := DataSize shr 1;
    j := 0;
    for i := 0 to DataSize-2 do
    begin
      if i < j then
      begin
        tx := Data[i].Re;
        ty := Data[i].Im;
        Data[i].Re := Data[j].Re;
        Data[i].Im := Data[j].Im;
        Data[j].Re := tx;
        Data[j].Im := ty;
      end;
      k := i2;
      while k <= j do
      begin
        Dec(j, k);
        k := k shr 1;
      end;
      Inc(j, k);
    end;
    // Compute the FFT
    c1 := -1.0;
    c2 := 0.0;
    l2 := 1;
    for l := 0 to Log2n-1 do
    begin
      l1 := l2;
      l2 := l2 shl 1;
      u1 := 1.0;
      u2 := 0.0;
      for j := 0 to l1-1 do
      begin
        i := j;
        while i < DataSize do
        begin
          i1 := i + l1;
          t1 := u1 * Data[i1].Re - u2 * Data[i1].Im;
          t2 := u1 * Data[i1].Im + u2 * Data[i1].Re;
          Data[i1].Re := Data[i].Re - t1;
          Data[i1].Im := Data[i].Im - t2;
          Data[i].Re := Data[i].Re + t1;
          Data[i].Im := Data[i].Im + t2;
          Inc(i, l2);
        end;
        z :=  u1*c1 - u2*c2;
        u2 := u1*c2 + u2*c1;
        u1 := z;
      end;
      c2 := Sqrt((1.0 - c1)/2.0);
      if Direction = 1 then c2 := -c2;
      c1 := Sqrt((1.0 + c1)/2.0);
    end;

    // Scaling for forward transform
    if Direction = 1 then
    for i := 0 to DataSize-1 do
    begin
      Data[i].Re := Data[i].Re/DataSize;
      Data[i].Im := Data[i].Im/DataSize;
    end;
  end;

  procedure HannWindow(OutData : PDoubleArray; Width : Integer; Symmetric : Boolean);
  var
    i, n : Integer;
  begin
    if Symmetric then n := Width-1
    else n := Width;
    for i := 0 to Width-1 do OutData[i] := (1-Cos(TwoPi*i/n))/2;
  end;

  procedure HammingWindow(OutData : PDoubleArray; Width : Integer; Symmetric : Boolean);
  var
    i, n : Integer;
  begin
    if Symmetric then n := Width-1
    else n := Width;
    for i := 0 to Width-1 do OutData[i] := 0.54-0.46*Cos(TwoPi*i/n);
  end;

  procedure BlackmanWindow(OutData : PDoubleArray; Width : Integer; Symmetric : Boolean);
  var
    i, n : Integer;
  begin
    if Symmetric then n := Width-1
    else n := Width;
    for i := 0 to Width-1 do OutData[i] := 0.42-0.5*Cos(TwoPi*i/n) + 0.08*Cos(2*TwoPi*i/n);
  end;

  procedure CalculateSincKernel(OutData : PDoubleArray; CutOff : Double; Width : Integer; WType : TFilterWindowType);
  var
    i : Integer;
    S : Double;
    Window : array of Double;
  begin
//    SetLength(OutData, Width);
    SetLength(Window, Width);
    case WType of
      fwHamming : HammingWindow(@Window[0], Width, False);
      fwHann : HannWindow(@Window[0], Width, False);
      fwBlackman : BlackmanWindow(@Window[0], Width, False);
    end;
    S := 0;
    for i := 0 to Width-1 do
    begin
      if i-(Width shr 1) <> 0 then
      OutData[i] := Sin(TwoPi*CutOff*(i-(Width shr 1)))/(i-(Width shr 1))*Window[i]
      else OutData[i] := TwoPi*CutOff*Window[i];
      S := S + OutData[i];
    end;
    for i := 0 to Width-1 do OutData[i] := OutData[i]/S;
  end;

  procedure SmallIntArrayToDouble(InData : PSmallInt; OutData : PDouble; DataSize : Integer);
  begin
    asm
                MOV EDX, DataSize;
                SHL EDX, 3;
                MOV ECX, OutData;
                ADD EDX, ECX;
                MOV EAX, InData;
      @test:    CMP EDX, ECX;
                JE @out;
                FILD WORD[EAX];
                ADD EAX, 2;
                FSTP QWORD[ECX];
                ADD ECX, 8;
                JMP @test;
      @out:     ;
    end;
  end;

  procedure SmallIntArrayToComplex(InData : PSmallInt; OutData : PComplex; DataSize : Integer);
  begin
    asm
                MOV EDX, DataSize;
                SHR EDX, 4;
                MOV ECX, OutData;
                ADD EDX, ECX;
                MOV EAX, InData;
      @test:    CMP EDX, ECX;
                JE @out;
                FILD WORD[EAX];
                ADD EAX, 2;
                FSTP QWORD[EAX];
                ADD ECX, 16;
                JMP @test;
      @out:     ;
    end;
  end;

  procedure MultDoubleArrays(Op1, Op2 : PDouble; DataSize : Integer);
  begin
    asm
                MOV EDX, DataSize;
                SHL EDX, 3;
                MOV ECX, Op1;
                ADD EDX, ECX;
                MOV EAX, Op2;
      @test:    CMP EDX, ECX;
                JE @out;
                FLD QWORD[ECX];
                FLD QWORD[EAX];
                FMUL;
                FSTP QWORD[EAX];
                ADD ECX, 8;
                ADD EAX, 8;
                JMP @test;
      @out:     ;
    end;
  end;

  procedure LgMagnitude(InData : PComplex; OutData : PDouble; DataSize, Shift : Integer);
  var
    LogBase  : Double;
  begin
    asm
                FLD1;
                FLDL2T;
                FDIVP;
                FSTP LogBase;
                MOV EDX, DataSize;
                SHL EDX, 3;
                MOV ECX, OutData;
                ADD EDX, ECX;
                MOV EAX, InData;
      @test:    CMP EDX, ECX;
                JE @out;
                FLD QWORD[EAX];
                FMUL ST(0), ST(0);
                ADD EAX, 8;
                FLD QWORD[EAX];
                FMUL ST(0), ST(0);
                FADDP;
                FSQRT;
                FTST;
                PUSH EAX;
                FSTSW AX;
                SAHF;
                JE @skip;
                FLD LogBase;
                FXCH;
                FYL2X;
                FIADD Shift;
                FTST;
                FSTSW AX;
                SAHF;
                JAE @skip;
                FSTP QWORD[ECX];
                FLDZ;
      @skip:    POP EAX;
                ADD EAX, 8;
                FSTP QWORD[ECX];
                ADD ECX, 8;
                JMP @test;
      @out:     ;
    end;
  end;

function AbsDouble(X : Double) : Double;
begin
  asm
    FLD X;
    FMUL ST(0), ST(0);
    FSTP Result;
  end;
end;

function GetLeftOf;
var
  i, p : Integer;
begin
  p := 0;
  for i := 1 to Length(S) do
    if S[i] = Delim then
    begin
      p := i - 1;
      break;
    end;
  SetLength(Result, p);
  Move(S[1], Result[1], p);
end;

function GetRightOf;
var
  i, p : Integer;
begin
  p := Length(S);
  for i := 1 to Length(S) do
    if S[i] = Delim then
    begin
      p := i;
      break;
    end;
  SetLength(Result, Length(S) - p);
  Move(S[p + 1], Result[1], Length(S) - p);
end;

procedure Convert24To16(InOutBuf : PBuffer16; InSize : Integer);
var
  i : Integer;
begin
  for i := 0 to (InSize div 3) - 1 do
    InOutBuf^[i] := PSmallInt(@(PBuffer8(InOutBuf)^[i*3 + 1]))^;
end;

procedure Convert16To24(InOutBuf : PBuffer16; InSize : Integer);
var
  Buf : PBuffer8;
  i : Integer;
begin
  Buf := PBuffer8(InOutBuf);
  for i := (InSize shr 1) - 1 downto 0 do
  begin
    Buf^[i*3] := 0;
    PSmallInt(@(Buf^[i*3 + 1]))^ := InOutBuf[i];
  end;
end;

procedure ConvertStereoToMono24(InOutBuf : PBuffer8; InSize : Integer);
var
  i, S1, S2 : Integer;
begin
  for i := 0 to (InSize div 6) - 1 do
  begin
    S1 := (PSmallInt(@InOutBuf[i*6 + 1])^ shl 8) + InOutBuf[i*6];
    S2 := (PSmallInt(@InOutBuf[i*6 + 4])^ shl 8) + InOutBuf[i*6 + 3];
    S1 := (S1 + S2) shr 1;
    Move(S1, InOutBuf^[i*3], 3)
  end;
end;

  procedure ConvertMonoToStereo24(InOutBuf : PBuffer8; InSize : Integer; Mode : TMSConverterMode);
  var
    i : Integer;
  begin
    case Mode of
    msmMonoToBoth :
        for i := (Insize div 3) - 1 downto 0 do
        begin
          Move(InOutBuf[i*3], InOutBuf[i*6], 3);
          Move(InOutBuf[i*3], InOutBuf[i*6 + 3], 3);
        end;
    msmMonoToLeft :
        for i := (Insize div 3) - 1 downto 0 do
        begin
          FillChar(InOutBuf[i*6], 3, 0);
          Move(InOutBuf[i*3], InOutBuf[i*6 + 3], 3);
        end;
    msmMonoToRight :
        for i := (Insize div 3) - 1 downto 0 do
        begin
          FillChar(InOutBuf[i*6 + 3], 3, 0);
          Move(InOutBuf[i*3], InOutBuf[i*6], 3);
        end;
    end;
  end;



end.
