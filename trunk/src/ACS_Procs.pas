(*
  This file is a part of New Audio Components package v 1.8
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_Procs;

(* Title: ACS_Procs
    Utility procedures. *)

interface

uses
  SysUtils, ACS_Types, Math

  {$IFDEF WIN32}
  , Windows
  {$ENDIF}

  ;

type

  TFilterWindowType = (fwHamming, fwHann, fwBlackman);

{$IFDEF LINUX}
  function FindLibs(const Pattern : String) : String;
{$ENDIF}

var

  FastCopyMem : procedure(Dest, Src : Pointer; Count : LongWord);

  // Direction = 1 - forward FFT, Direction = -1 - inverse FFT.
  procedure ComplexFFT(Data : PComplexArray; DataSize, Direction : Integer);

  procedure HannWindow(OutData : PDoubleArray; Width : Integer; Symmetric : Boolean);

  procedure HammingWindow(OutData : PDoubleArray; Width : Integer; Symmetric : Boolean);

  procedure BlackmanWindow(OutData : PDoubleArray; Width : Integer; Symmetric : Boolean);

  procedure HannWindowS(OutData : PSingleArray; Width : Integer; Symmetric : Boolean);

  procedure HammingWindowS(OutData : PSingleArray; Width : Integer; Symmetric : Boolean);

  procedure BlackmanWindowS(OutData : PSingleArray; Width : Integer; Symmetric : Boolean);

  procedure CalculateSincKernel(OutData : PDoubleArray; CutOff : Double; Width : Integer; WType : TFilterWindowType);

  procedure CalculateSincKernelSingle(OutData : PSingleArray; CutOff : Single; Width : Integer; WType : TFilterWindowType);

  procedure CalculateChebyshev(CutOff, Ripple : Single; NumPoles : Integer; HighPass : Boolean; var A, B : array of Single);

  procedure SmallIntArrayToDouble(InData : PSmallInt; OutData : PDouble; DataSize : Integer);

  procedure SmallIntArrayToComplex(InData : PSmallInt; OutData : PComplex; DataSize : Integer);


  // Computes Op2[i] = Op1[i]*Op2[i], i = [0..DataSize-1]

  procedure MultDoubleArrays(Op1, Op2 : PDouble; DataSize : Integer);

  procedure MultSingleArrays(Op1, Op2 : PSingle; DataSize : Integer);

  procedure MultAndSumSingleArrays(Op1, Op2 : PSingle; var Accumulator : Single; DataSize : Integer);
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

  procedure ConvertStereoToMono32(InOutBuf : PBuffer32; InSize : Integer);

  procedure ConvertMonoToStereo32(InOutBuf : PBuffer32; InSize : Integer; Mode : TMSConverterMode);

  procedure Convert32To24(InOutBuf : PBuffer8; InSize : Integer);

  procedure Convert24To32(InOutBuf : PBuffer8; InSize : Integer);

  procedure ConvertIEEEFloatTo32(InOutBuf : PBuffer32; InSize : Integer);

  procedure ConvertShortIEEEFloatTo32(InOutBuf : PBuffer32; InSize : Integer);

  procedure SingleToByte(_in : PBufferSingle; _out : PBuffer8; len : Integer);

  procedure SingleToSmallInt(_in : PBufferSingle; _out : PBuffer16; len : Integer);

  procedure SingleToInt32(_in : PBufferSingle; _out : PBuffer32; len : Integer);

  procedure SingleToInt24(_in : PBufferSingle; _out : PBuffer8; len : Integer);

  procedure ByteToSingle(_in : PBuffer8; _out : PBufferSingle; len : Integer);

  procedure SmallIntToSingle(_in : PBuffer16; _out : PBufferSingle; len : Integer);

  procedure Int24ToSingle(_in : PBuffer8; _out : PBufferSingle; len : Integer);

  procedure Int32ToSingle(_in : PBuffer32; _out : PBufferSingle; len : Integer);

  procedure SingleStereoToMono(_inout : PBufferSingle; frames : Integer);

  procedure SingleMonoToStereo(_inout : PBufferSingle; frames : Integer);

  function GetRightOf(Delim : AnsiChar; const S : AnsiString) : AnsiString;

  function GetLeftOf(Delim : AnsiChar; const S : AnsiString) : AnsiString;

  function GUIDSEqual(const g1, g2 : TGUID) : Boolean;

  function GCD(a, b : Integer) : Integer;

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

  procedure HannWindowS(OutData : PSingleArray; Width : Integer; Symmetric : Boolean);
  var
    i, n : Integer;
  begin
    if Symmetric then n := Width-1
    else n := Width;
    for i := 0 to Width-1 do OutData[i] := (1-Cos(TwoPi*i/n))/2;
  end;

  procedure HammingWindowS(OutData : PSingleArray; Width : Integer; Symmetric : Boolean);
  var
    i, n : Integer;
  begin
    if Symmetric then n := Width-1
    else n := Width;
    for i := 0 to Width-1 do OutData[i] := 0.54-0.46*Cos(TwoPi*i/n);
  end;

  procedure BlackmanWindowS(OutData : PSingleArray; Width : Integer; Symmetric : Boolean);
  var
    i, n : Integer;
  begin
    if Symmetric then n := Width-1
    else n := Width;
    for i := 0 to Width-1 do OutData[i] := 0.42-0.5*Cos(TwoPi*i/n) + 0.08*Cos(2*TwoPi*i/n);
  end;

  procedure CalculateSincKernelSingle(OutData : PSingleArray; CutOff : Single; Width : Integer; WType : TFilterWindowType);
  var
    i : Integer;
    S : Single;
    Window : array of Single;
  begin
//    SetLength(OutData, Width);
    SetLength(Window, Width);
    case WType of
      fwHamming : HammingWindowS(@Window[0], Width, False);
      fwHann : HannWindowS(@Window[0], Width, False);
      fwBlackman : BlackmanWindowS(@Window[0], Width, False);
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

  procedure MultSingleArrays(Op1, Op2 : PSingle; DataSize : Integer);
  var
    P : Pointer;
  begin
    LongWord(P) := LongWord(Op1) + LongWord(DataSize*4);
    while Op1 <> P do
    begin
      Op1^ := Op1^*Op2^;
      Inc(Op1);
      Inc(Op2);
    end;
  end;

  procedure MultAndSumSingleArrays(Op1, Op2 : PSingle; var Accumulator : Single; DataSize : Integer);
  var
    P : Pointer;
  begin
    LongWord(P) := LongWord(Op1) + LongWord(DataSize*4);
    while Op1 <> P do
    begin
      Accumulator := Accumulator + Op1^*Op2^;
      Inc(Op1);
      Inc(Op2);
    end;
  end;

  procedure MultAndSumXSingleArrays(A, X : PSingle; var Accumulator : Single; DataSize : Integer);
  var
    P  : Pointer;
    X2 : PSingle;
  begin
    LongWord(P) := LongWord(X) + LongWord((DataSize shr 1)*4);
    LongWord(X2) := LongWord(P) + 4;
    while X <> P do
    begin
      Accumulator := Accumulator + A^*(X^+X2^);
      Inc(A);
      Inc(X);
      Inc(X2);
    end;
    Accumulator := Accumulator + A^*X^;
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

procedure ConvertStereoToMono32(InOutBuf : PBuffer32; InSize : Integer);
var
  i : Integer;
begin
  for i := 0 to (InSize div 8) - 1 do
  begin
    InOutBuf[i] := (InOutBuf[i*2] + InOutBuf[i*2+1]) div 2;
  end;
end;

  procedure ConvertMonoToStereo32(InOutBuf : PBuffer32; InSize : Integer; Mode : TMSConverterMode);
  var
    i : Integer;
  begin
    case Mode of
    msmMonoToBoth :
        for i := (Insize div 4) - 1 downto 0 do
        begin
          InOutBuf[i*2] := InOutBuf[i];
          InOutBuf[i*2 + 1] := InOutBuf[i];
        end;
    msmMonoToLeft :
        for i := (Insize div 4) - 1 downto 0 do
        begin
          InOutBuf[i*2] := 0;
          InOutBuf[i*2 + 1] := InOutBuf[i];
        end;
    msmMonoToRight :
        for i := (Insize div 4) - 1 downto 0 do
        begin
          InOutBuf[i*2] := InOutBuf[i];
          InOutBuf[i*2 + 1] := 0;
        end;
    end;
  end;

procedure Convert32To24(InOutBuf : PBuffer8; InSize : Integer);
var
  i : Integer;
begin
  for i := 0 to (InSize div 4) - 1 do
  begin
    InOutBuf^[i*3] := InOutBuf^[i*4 + 1];
    PSmallInt(@(InOutBuf[i*3+1]))^ := PSmallInt(@(InOutBuf[i*4 + 2]))^;
  end;
end;

procedure Convert24To32(InOutBuf : PBuffer8; InSize : Integer);
var
  i : Integer;
begin
  for i := (InSize div 3) - 1 downto 0 do
  begin
    InOutBuf[i*4] := 0;
    InOutBuf[i*4+1] := InOutBuf[i*3];
    PSmallInt(@(InOutBuf[i*4+2]))^ := PSmallInt(@(InOutBuf[i*3 + 1]))^;
  end;
end;



procedure ConvertIEEEFloatTo32(InOutBuf : PBuffer32; InSize : Integer);
var
  i : Integer;
begin
  for i := 0 to (InSize div 8) - 1 do
  begin
    if PDouble(@InOutBuf[i*2])^ >= 1 then InOutBuf[i] := High(Integer)
    else
    if PDouble(@InOutBuf[i*2])^ <= -1 then InOutBuf[i] := Low(Integer)
    else
    if PDouble(@InOutBuf[i*2])^ = 0 then InOutBuf[i] := 0
    else
    InOutBuf[i] := Floor(PDouble(@InOutBuf[i*2])^ * High(Integer));
  end;
end;

procedure ConvertShortIEEEFloatTo32(InOutBuf : PBuffer32; InSize : Integer);
var
  i : Integer;
begin
  for i := 0 to (InSize div 4) - 1 do
  begin
    if PSingle(@InOutBuf[i])^ >= 1 then InOutBuf[i] := High(Integer)
    else
    if PSingle(@InOutBuf[i])^ <= -1 then InOutBuf[i] := Low(Integer)
    else
    if PSingle(@InOutBuf[i])^ = 0 then InOutBuf[i] := 0
    else
    InOutBuf[i] := Floor(PSingle(@InOutBuf[i])^ * High(Integer));
  end;
end;

  procedure SingleToByte(_in : PBufferSingle; _out : PBuffer8; len : Integer);
  var
    i : Integer;
  begin
    for i := 0 to len - 1 do
    begin
      if _in[i] >= 1 then
        _out[i] := 255
      else
      if _in[i] <= -1 then
        _out[i] := 0
      else
      _out[i] := Floor((_in[i]+1) * 128);
    end;
  end;

  procedure SingleToSmallInt(_in : PBufferSingle; _out : PBuffer16; len : Integer);
  var
    i : Integer;
  begin
    for i := 0 to len - 1 do
    begin
      if _in[i] >= 1 then
        _out[i] := 32767
      else
      if _in[i] <= -1 then
        _out[i] := -32768
      else
      _out[i] := Floor(_in[i] * $8000);
    end;
  end;

  procedure SingleToInt32(_in : PBufferSingle; _out : PBuffer32; len : Integer);
  var
    i : Integer;
  begin
    for i := 0 to len - 1 do
    begin
      if _in[i] >= 1 then
        _out[i] := High(Integer)
      else
      if _in[i] <= -1 then
        _out[i] := Low(Integer)
      else
      _out[i] := Floor(_in[i] * $80000000);
    end;
  end;

  procedure SingleToInt24(_in : PBufferSingle; _out : PBuffer8; len : Integer);
  var
    i, Sample : Integer;
  begin
    for i := 0 to len - 1 do
    begin
      if _in[i] >= 1 then
        Sample := $800000 - 1
      else
      if _in[i] <= -1 then
        Sample := -$800000
      else
      Sample := Floor(_in[i] * $800000);
      _out[i*3] := Sample and $ff;
      PSmallInt(@(_out[i*3 + 1]))^ := SmallInt(Sample shr 8);
    end;
  end;

  procedure ByteToSingle(_in : PBuffer8; _out : PBufferSingle; len : Integer);
  var
    i : Integer;
  begin
    for i := 0 to len - 1 do
      _out[i] := _in[i]/128 - 1;
  end;
  
  procedure SmallIntToSingle(_in : PBuffer16; _out : PBufferSingle; len : Integer);
  var
    i : Integer;
  begin
    for i := 0 to len - 1 do
      _out[i] := _in[i]/$8000;
  end;

  procedure Int32ToSingle(_in : PBuffer32; _out : PBufferSingle; len : Integer);
  var
    i : Integer;
  begin
    for i := 0 to len - 1 do
      _out[i] := _in[i]/$80000000;
  end;

  procedure Int24ToSingle(_in : PBuffer8; _out : PBufferSingle; len : Integer);
  var
    i : Integer;
  begin
    for i := 0 to len - 1 do
      _out[i] := ((PSmallInt(@(_in[i*3 + 1]))^ shl 8) + ShortInt(_in[i*3]))/$800000;
  end;

  procedure SingleStereoToMono(_inout : PBufferSingle; frames : Integer);
  var
    i : Integer;
  begin
    for i := 0 to frames - 1 do
      _inout[i] := (_inout[i*2] + _inout[i*2 + 1])/2;
  end;

  procedure SingleMonoToStereo(_inout : PBufferSingle; frames : Integer);
  var
    i : Integer;
  begin
    for i := frames - 1 downto 0 do
    begin
      _inout[i*2 + 1] := _inout[i];
      _inout[i*2] := _inout[i];
    end;
  end;


   function GUIDSEqual(const g1, g2 : TGUID) : Boolean;
   begin
     Result := False;
     if g1.D1 <> g2.D1 then Exit;
     if g1.D2 <> g2.D2 then Exit;
     if g1.D3 <> g2.D3 then Exit;
     if g1.D4[0] <> g2.D4[0] then Exit;
     if g1.D4[1] <> g2.D4[1] then Exit;
     if g1.D4[2] <> g2.D4[2] then Exit;
     if g1.D4[3] <> g2.D4[3] then Exit;
     if g1.D4[4] <> g2.D4[4] then Exit;
     if g1.D4[5] <> g2.D4[5] then Exit;
     if g1.D4[6] <> g2.D4[6] then Exit;
     if g1.D4[7] <> g2.D4[7] then Exit;
     Result := True;
   end;

    procedure NewCascade(CutOff : Single; HighPass : Boolean; Ripple : Single; P, NumPoles : Integer; var A, B : array of Single);
    var
      ReP, ImP, ES, VX, KX, T, W, Mag, D, K, X0, X1, X2, Y1, Y2 : Single;
    begin
      ReP := -cos(Pi/(NumPoles*2) + (P-1) * Pi/NumPoles);
      ImP := sin(Pi/(NumPoles*2) + (P-1) * Pi/NumPoles);
      if Ripple <> 0 then
      begin
        ES := Sqrt( Sqr(100 / (100-Ripple)) - 1);
        VX := (1/NumPoles) * Ln((1/ES) + Sqrt(1/Sqr(ES) + 1));
        KX := (1/NumPoles) * Ln((1/ES) + Sqrt(1/Sqr(ES) - 1));
        KX := (Exp(KX) + Exp(-KX))/2;
        ReP := ReP*((Exp(VX) - Exp(-VX))/2)/KX;
        ImP := ImP*((Exp(VX) + Exp(-VX))/2)/ KX;
      end;
      T := 2*Tan(0.5);
      W := 2*Pi*CutOff;
      Mag := Sqr(ReP) + Sqr(ImP);
      D := 4 - 4*ReP*T + Mag*Sqr(T);
      X0 := Sqr(T)/D;
      X1 := 2*Sqr(T)/D;
      X2 := X0;
      Y1 := (8 - 2*Mag*Sqr(T))/D;
      Y2 := (-4 - 4*ReP*T - Mag*Sqr(T))/D;
      if HighPass then
        K := -cos(W/2 + 0.5)/cos(W/2 - 0.5)
      else
        K := sin(0.5 - W/2)/sin(0.5 + W/2);
      D := 1 + Y1*K - Y2*Sqr(K);
      A[0] := (X0 - X1*K + X2*Sqr(K))/D;
      A[1] := (-2*X0*K + X1 + X1*Sqr(K) - 2*X2*K)/D;
      A[2] := (X0*Sqr(K) - X1*K + X2)/D;
      B[0] := (2*K + Y1 + Y1*Sqr(K) - 2*Y2*K)/D;
      B[1] := (-Sqr(K) - Y1*K + Y2)/D;
      if HighPass then
      begin
        A[1] := -A[1];
        B[0] := -B[0];
      end;
    end;

    procedure CalculateChebyshev(CutOff, Ripple : Single; NumPoles : Integer; HighPass : Boolean; var A, B : array of Single);
    var
      i, j, P : Integer;
      TA, TB : array of Single;
      Ax : array[0..2] of Single;
      Bx : array[0..1] of Single;
      SumA, SumB, Gain : Single;
    begin
      SetLength(TA, NumPoles + 4);
      SetLength(TB, NumPoles + 4);
      for i := 0 to NumPoles + 3 do
      begin
        A[i] := 0;
        B[i] := 0
      end;
      A[2] := 1;
      B[2] := 1;
      for P := 1 to NumPoles div 2 do
      begin
        NewCascade(CutOff, HighPass, Ripple, P, NumPoles, Ax, Bx);
        for i := 0 to NumPoles + 3 do
        begin
          TA[i] := A[i];
          TB[i] := B[i];
        end;
        for j := 2 to NumPoles + 3 do
        begin
          A[j] := Ax[0]*TA[j] + Ax[1]*TA[j-1] + Ax[2]*TA[j-2];
          B[j] := TB[j] - Bx[0]*TB[j-1] - Bx[1]*TB[j-2];
        end;
      end;
      B[2] := 0;
      for i := 0 to NumPoles do
      begin
        A[i] := A[i+2];
        B[i] := -1*B[i+3];
      end;
      SumA := 0;
      SumB := 0;
      if HighPass then
      begin
        for i := 0 to NumPoles do
          SumA := SumA + A[i]*(1 - (i mod 2)*2);
        for i := 0 to NumPoles - 1 do
          SumB := SumB + B[i]*(1 - (i mod 2)*2);
        SumB := SumB + 1;  
      end else
      begin
        for i := 0 to NumPoles do
          SumA := SumA + A[i];
        for i := 0 to NumPoles do
          SumB := SumB + B[i];
      end;
      Gain := SumA/(1 - SumB);
      for i := 0 to NumPoles do
        A[i] := A[i]/Gain;
    end;

  function GCD(a, b : Integer) : Integer;
  var
    p, q, r : Integer;
  begin
    p := a;
    q := b;
    r := p mod q;
    while r <> 0 do
    begin
      p := q;
      q := r;
      r := p mod q;
    end;
    Result := q;
  end;

  function SSESupported : Boolean;
  var
    Flag : LongWord;
  begin
    asm
      PUSH EBX;
      MOV EAX, 1;
      CPUID;
      MOV Flag, EDX;
      POP EBX;
    end;
    Result := (Flag and (1 shl 25)) <> 0;
  end;

  procedure CopySSE(Dest, Src : Pointer; Count : LongWord);
  begin
    if ((LongWord(Dest) mod 16) <> 0) or ((LongWord(Src) mod 16) <> 0) then
    begin
      CopyMemory(Dest, Src, Count);
      Exit;
    end;

    asm
        PUSH ESI;
        PUSH EDI;
        MOV EDI, Dest;
        MOV ESI, Src;
        MOV ECX, Count;

        MOV EAX, ECX;
        SUB ECX, EDI;
        SUB ECX, EAX;
        AND ECX, 15;
        SUB EAX, ECX;
        JLE @l2;

        EMMS;
        REP MOVSB;
        MOV ECX, EAX;
        AND EAX, 15
        SHR ECX, 4
        JZ	@l2;
        SUB	EDI, ESI;
   @l1: MOVDQA	XMM0, [ESI];
        MOVDQA	[EDI+ESI], XMM0;
        ADD ESI, 16;
        DEC ECX;
        JNZ @l1;
        EMMS;
        ADD	EDI, ESI;

   @l2: ADD ECX, EAX;
        REP MOVSB;

   @l3: POP EDI;
        POP ESI;
    end;
  end;

initialization
  if SSESupported then
  begin
    FastCopyMem := CopySSE;
  end else
  begin
    FastCopyMem := CopyMemory;
  end;

end.
