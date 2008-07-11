unit acs_reg;

(* Title: ACS_Reg 
    Registers all ACS components. *)

interface

uses
  Classes, ACS_DXAudio,
  ACS_CDROM, ACS_AudioMix, ACS_Converters, NewAC_DSP,
  ACS_Misc, ACS_Vorbis, ACS_Wave, ACS_Filters, ACS_LAME, ACS_smpeg, ACS_MAC, ACS_Streams, ACS_FLAC, AuSampleRate, ACS_WavPack, ACS_WinMedia, ACS_TTA, AudioPass, ACS_OptimFROG, ACS_TAK, ACS_MPC, AudioDMO, NewAC_AVI;

  procedure Register();

implementation

procedure Register();
begin
  RegisterComponents('Audio I/O', [TDXAudioIn, TDXAudioOut, TCDPlayer, TCDIn, TWMStreamedIn, TWMStreamedOut,
  TInputList, TMemoryIn, TVorbisIn, TVorbisOut,
  TWaveIn, TWaveOut, TMP3In, TMP3Out, TMACIn, TMACOut, TStreamIn, TStreamOut, TFLACIn, TFLACOut, TWVIn, TWVOut, TWMIn,
  TWMAOut, TTTAIn, TTTAOut, TOFRIn, TWaveTap, TWMATap, TTAKIn, TMPCIn, TMPCOut, TWMADualPassOut, TAVIIn, TNULLOut]);
  RegisterComponents('Audio Processing', [TAudioMixer, TRealTimeMixer, TAudioConverter, TACMConverter,
  TRateConverter, TAudioProcessor, TMSResampler, TBWFilter, TSincFilter, TStereoBalance, TResampler, TDitherer, TAudioPass, TNormalizer, TVoiceFilter, TFrequencyAnalysis, TConvolver]);

end;


end.
