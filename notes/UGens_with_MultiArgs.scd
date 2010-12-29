AudioControl
BufWr
Control
Demand
DiskOut
EnvGen
IEnvGen
LagControl
LocalOut
(Mix)
Out
PackFFT
Poll
RecordBuf
XOut

DiskOut.ar( buf, SinOsc.ar( 444 :: 555 :: Nil ))
DiskOut.ar( buf1 :: buf2 :: Nil, SinOsc.ar( 444 :: 555 :: Nil )) ?
DiskOut.ar( buf1 :: buf2 :: Nil, expand SinOsc.ar( 444 :: 555 :: Nil )) ?
DiskOut.ar( buf1 :: buf2 :: Nil, Expand( SinOsc.ar( 444 :: 555 :: Nil ))) ?
DiskOut.ar( buf1 :: buf2 :: Nil, Expand( SinOsc.ar( 444 :: 555 :: Nil ), 1 )) ?
DiskOut.ar( buf1 :: buf2 :: Nil, Expand( SinOsc.ar( 444 :: 555 :: 666 :: 777 :: Nil ), 2 )) ?

Out.ar( idx0 :: idx1 :: idx2 :: Nil, Expand( In.ar( 0, 3 )))

[ 1, 2, 3, 4, 5 ].clump( 1 )

Out.ar( idx0 :: idx1 :: idx2 :: Nil, In.ar( 0, 3 ) expand )
Out.ar( idx0 :: idx1 :: idx2 :: Nil, In.ar( 0, 6 ) expand 2 )
Out.ar( idx0 :: idx1 :: idx2 :: Nil, In.ar( 0, 6 ) clump 2 )

object Expand {
   def none( g: GE ) : Expand  // default (implicit)
   def apply( g: GE, step: Int = 1 ) : Expand
   def iterate( g: GE, start: Int = 0 )( f: Int => Int )
   def tabulate( g: GE )( f: Int => Int )
   def fill( g: GE )( elem: => Int )
}

/// oder viel einfacher?

object DiskOut {
   def ar( buf: GE, multi: => GE ) : DiskOut
}

?

//////////////////////////

Implementations of 'checkInputs' :
   AbstractOut:checkInputs :     this.checkInputs
[ class LocalOut, class XOut, class SharedOut, class Out, class OffsetOut, class ReplaceOut ]
   AmpComp:checkInputs :     this.checkInputs
   BiPanB2:checkInputs :     this.checkInputs
   BufRd:checkInputs :     this.checkInputs
   BufWr:checkInputs :     this.checkInputs
   CheckBadValues:checkInputs :     this.checkInputs
   DecodeB2:checkInputs :     this.checkInputs
   Demand:checkInputs :     this.checkInputs
   DiskOut:checkInputs :     this.checkInputs
   Duty:checkInputs :     this.checkInputs
   Filter:checkInputs :     this.checkInputs
[ BPF, LPF, LPZ2, LagUD, Decay2, Decay, OnePole, Resonz, MoogFF, Median, Integrator, DetectSilence, Lag, LPZ1, Ringz, FOS, RLPF, MidEQ, Slew, LeakDC, TwoPole, BEQSuite, Slope, Formlet, SOS, BRF, HPF, HPZ2, BPZ2, BRZ2, Lag2UD, Lag3UD, OneZero, Ramp, Lag3, Lag2, HPZ1, RHPF, TwoZero, APF, BBandStop, BHiPass, BLowShelf, BPeakEQ, BAllPass, BHiShelf, BBandPass, BLowPass ]
   LinExp:checkInputs :     this.checkInputs
   LinLin:checkInputs :     this.checkInputs
   LinXFade2:checkInputs :     this.checkInputs
   Median:checkInputs :     this.checkInputs
   Panner:checkInputs :     this.checkInputs
[ Rotate2, DecodeB2, Pan4, PanAz, Balance2, PanB2, PanB, Pan2, BiPanB2, LinPan2 ]
   Peak:checkInputs :     this.checkInputs
   PitchShift:checkInputs :     this.checkInputs
   Poll:checkInputs :     this.checkInputs
   PulseCount:checkInputs :     this.checkInputs
   Select:checkInputs :     this.checkInputs
   SendTrig:checkInputs :     this.checkInputs
   Stepper:checkInputs :     this.checkInputs
   SynthDef:checkInputs :     this.checkInputs
   T2K:checkInputs :     this.checkInputs
   TDelay:checkInputs :     this.checkInputs
   Timer:checkInputs :     this.checkInputs
//   UGen:checkInputs :     this.checkInputs
   XFade2:checkInputs :     this.checkInputs
   XOut:checkInputs :     this.checkInputs
   ZeroCrossing:checkInputs :     this.checkInputs

MultiOutUGen.allSubclasses.asCompileString
[ /* Panner, */ GrainIn, BeatTrack, /* AbstractIn, */ Demand, Warp1, GrainSin, Pitch, PlayBuf, /* UnpackFFT, */ FreeVerb2, GrainFM, Control, AudioControl, TGrains, Hilbert, StereoConvolution2L, DC, DiskIn, GrainBuf, BufRd, MFCC, GVerb, Silent, BeatTrack2, VDiskIn, Rotate2, DecodeB2, Pan4, PanAz, Balance2, PanB2, PanB, Pan2, BiPanB2, LinPan2, LocalIn, SharedIn, LagIn, InFeedback, InTrig, In, LagControl, TrigControl ]

/////////////////


Changed argument order:
	Demand
	AbstractSeqDemand
	Dswitch1
	Dswitch
	SendTrig
	SendReply


///////////////////////////
sclang probs:

SynthDef( \test2, { SinOsc.kr( 441, mul: LFSaw.ar, add: 4 )}).writeDefFile( "~/Desktop/".standardizePath )
// --> produziert MulAdd.kr mit illegalem a-rate mul argument
// wobei nicht klar ist, ob es wirklich verboten ist:
{ K2A.ar( SinOsc.kr( 441, mul: LFSaw.ar(1), add: 0.0001 ))}.play  // hmmm... sound the same
{ K2A.ar( SinOsc.kr( 441, mul: LFSaw.kr(1), add: 0.0001 ))}.play

