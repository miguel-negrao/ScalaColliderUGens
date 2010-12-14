package de.sciss.synth

import collection.immutable.{IndexedSeq => IIdxSeq}

object TestTypes {
   sealed trait Rate
//   sealed trait AcceptableWith[ R <: Rate ]
   case object scalar   extends Rate // with AcceptableWith[ scalar.type ]
   case object control  extends Rate
   case object audio    extends Rate
   case object demand   extends Rate

   trait Expands[ A ] {
      def expand: IIdxSeq[ A ]
   }

   sealed trait GE[ R <: Rate, U <: UGenIn[ _ <: Rate ]] extends Expands[ U ] {
      type rate = R
   }

   sealed trait HasDoneFlag

//   trait AudioRatedGE extends GE with AudioRated {
//      def expand: IIdxSeq[ UGenIn with AudioRated ]
//   }

   sealed trait UGenIn[ R <: Rate ] extends GE[ R, UGenIn[ R ]] {
      def expand: IIdxSeq[ UGenIn[ R ]] = IIdxSeq( this )
   }

   abstract class SingleOutUGen[ R <: Rate ]( inputs: Seq[ UGenIn[ _ <: Rate ]]) extends UGenIn[ R ]

//   trait ScalarRated { def rate = scalar }
//   trait AudioRated  { def rate = audio }

//   object DiskOut {
//      def ar( buf: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]], multi: GE[ audio.type, _ <: UGenIn[ audio.type ]]) : DiskOut = apply( buf, multi )
//   }
//
//   case class DiskOut( buf: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]], multi: GE[ audio.type, _ <: UGenIn[ audio.type ]])
//   extends GE[ audio.type, DiskOutUGen ] {
//      def rate: audio.type = audio
//      def expand: IIdxSeq[ DiskOutUGen ] = {
//         val bufE :   IIdxSeq[ UGenIn[ _ <: Rate ]] = buf.expand
//         val multiE = multi.expand
//         val numExp  = bufE.size // math.max( bufE.size, multiE.size )
//         IIdxSeq.tabulate( numExp )( i => DiskOutUGen( bufE( i ), multiE ))
//      }
//   }
//
//   case class DiskOutUGen( buf: UGenIn[ _ <: Rate ], multi: Seq[ UGenIn[ audio.type ]])
//   extends SingleOutUGen[ audio.type ]( buf +: multi )

   object SinOsc {
      def ar( freq: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]]) = apply[ audio.type ](   freq )
      def kr( freq: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]]) = apply[ control.type ]( freq )
   }
   case class SinOsc[ R <: Rate ]( freq: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]])
   extends GE[ R, SinOscUGen[ R ]] {
      def expand: IIdxSeq[ SinOscUGen[ R ]] = {
         val freqE : IIdxSeq[ UGenIn[ _ <: Rate ]]   = freq.expand // why the explicit type???
         val numExp  = freqE.size
         IIdxSeq.tabulate( numExp )( i => SinOscUGen[ R ]( freqE( i )))
      }
   }
   case class SinOscUGen[ R <: Rate ]( freq: UGenIn[ _ <: Rate ])
   extends SingleOutUGen[ R ]( freq :: Nil )

   object Line {
      def kr( start: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]], end: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]],
              dur: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]], doneAction: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]]) =
         apply[ control.type ]( start, end, dur, doneAction )
   }
   case class Line[ R <: Rate ]( start: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]], end: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]],
                                 dur: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]], doneAction: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ]])
   extends GE[ R, LineUGen[ R ]] with HasDoneFlag {
      def expand: IIdxSeq[ LineUGen[ R ]] = {
         val startE: IIdxSeq[ UGenIn[ _ <: Rate ]] = start.expand
         val endE: IIdxSeq[ UGenIn[ _ <: Rate ]]   = end.expand
         val durE: IIdxSeq[ UGenIn[ _ <: Rate ]]   = dur.expand
         val doneE: IIdxSeq[ UGenIn[ _ <: Rate ]]  = doneAction.expand
         val numExp  = math.max( math.max( math.max( startE.size, endE.size ), durE.size ), doneE.size )
         IIdxSeq.tabulate( numExp )( i =>
            LineUGen[ R ]( startE( i % numExp ), endE( i % numExp ), durE( i % numExp ), doneE( i % numExp )))
      }
   }
   case class LineUGen[ R <: Rate ]( start: UGenIn[ _ <: Rate ], end: UGenIn[ _ <: Rate ],
                                     dur: UGenIn[ _ <: Rate ], doneAction: UGenIn[ _ <: Rate ])
   extends SingleOutUGen[ R ]( List( start, end,  dur, doneAction )) with HasDoneFlag

//   object ZeroCrossing {
//      def ar( in: GE[ audio.type ])   = apply[ audio.type ](   in )
//      def kr( in: GE[ control.type ]) = apply[ control.type ]( in )
//   }
//   case class ZeroCrossing[ R <: Rate ]( in: GE[ R ])
//   extends GE[ R ] {
//      def expand: IIdxSeq[ ZeroCrossingUGen[ R ]] = {
//         val inE: IIdxSeq[ UGenIn[ R ]] = in.expand
//         val numExp  = inE.size
//         IIdxSeq.tabulate( numExp )( i => ZeroCrossingUGen[ R ]( inE( i % numExp )))
//      }
//   }
//   case class ZeroCrossingUGen[ R <: Rate ]( in: UGenIn[ R ])
//   extends SingleOutUGen[ R ]( in :: Nil )

//   sealed trait EnsureEqualRatesIfAudio[ A, B ]
//   private class EnsureEqualRatesIfAudioImpl[ A, B ] extends EnsureEqualRatesIfAudio[ A, B ]
//   implicit def ensureEqualRateIfAudio1[ B ] : EnsureEqualRatesIfAudio[ scalar.type, B ]        = new EnsureEqualRatesIfAudioImpl[ scalar.type, B ]
//   implicit def ensureEqualRateIfAudio2[ B ] : EnsureEqualRatesIfAudio[ control.type, B ]       = new EnsureEqualRatesIfAudioImpl[ control.type, B ]
//   implicit def ensureEqualRateIfAudio3[ B ] : EnsureEqualRatesIfAudio[ demand.type, B ]        = new EnsureEqualRatesIfAudioImpl[ demand.type, B ]
//   implicit val ensureEqualRateIfAudio4      : EnsureEqualRatesIfAudio[ audio.type, audio.type ]= new EnsureEqualRatesIfAudioImpl[ audio.type, audio.type ]

//   object BufRd {
//      def ar( numChannels: Int, buf: GE[ _ <: Rate ], phase: GE[ audio.type ], loop: GE[ _ <: Rate ], interp: GE[ _ <: Rate ]) =
//         apply[ audio.type ]( numChannels, buf, phase, loop, interp )
//
//      def kr( numChannels: Int, buf: GE[ _ <: Rate ], phase: GE[ _ <: Rate ], loop: GE[ _ <: Rate ], interp: GE[ _ <: Rate ]) =
//         apply[ control.type ]( numChannels, buf, phase, loop, interp )
//   }
//   // XXX can we enforce at this stage that for R == audio.type, phase must be audio.type??
//   case class BufRd[ R <: Rate ]( numChannels: Int, buf: GE[ _ <: Rate ], phase: GE[ _ <: Rate ],
//                                  loop: GE[ _ <: Rate ], interp: GE[ _ <: Rate ])
//   extends GE[ R ] {
//      def expand: IIdxSeq[ BufRdUGen[ R ]] = {
//         val bufE:      IIdxSeq[ UGenIn[ _ <: Rate ]] = buf.expand
//         val phaseE:    IIdxSeq[ UGenIn[ _ <: Rate ]] = phase.expand
//         val loopE:     IIdxSeq[ UGenIn[ _ <: Rate ]] = loop.expand
//         val interpE:   IIdxSeq[ UGenIn[ _ <: Rate ]] = interp.expand
//         val numExp  = math.max( math.max( math.max( bufE.size, phaseE.size ), loopE.size ), interpE.size )
//         IIdxSeq.tabulate( numExp )( i =>
//            BufRdUGen[ R ]( bufE( i % numExp ), phaseE( i % numExp ), loopE( i % numExp ), interpE( i % numExp )))
//      }
//   }
//   case class BufRdUGen[ R <: Rate ]( buf: UGenIn[ _ <: Rate ], phase: UGenIn[ _ <: Rate ],
//                                      loop: UGenIn[ _ <: Rate ], interp: UGenIn[ _ <: Rate ])
//   extends SingleOutUGen[ R ]( List( buf, phase, loop, interp )) // XXX not SingleOut
//
   object Done {
      def kr( src: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ] with HasDoneFlag ]) = apply( src )
   }
   case class Done( src: GE[ _ <: Rate, _ <: UGenIn[ _ <: Rate ] with HasDoneFlag ])
   extends GE[ control.type, DoneUGen ] {
      def expand: IIdxSeq[ DoneUGen ] = {
         val srcE : IIdxSeq[ UGenIn[ _ <: Rate ] with HasDoneFlag ] = src.expand
         val numExp = srcE.size
         IIdxSeq.tabulate( numExp )( i => DoneUGen( srcE( i % numExp )))
      }
   }
   case class DoneUGen( src: UGenIn[ _ <: Rate ] with HasDoneFlag )
   extends SingleOutUGen[ control.type ]( src :: Nil )

   case class Constant( v: Float ) extends UGenIn[ scalar.type ]

   implicit def floatToGE( f: Float ) = Constant( f )

   def test {
//      Done.kr( SinOsc.kr)
//      val zero = ZeroCrossing.kr( SinOsc.ar( 441 ))
//      val disk = DiskOut.ar( 0, ZeroCrossing.ar( SinOsc.ar( 441 )))
//      BufRd.kr[ audio.type ]( 1, 0, SinOsc.ar( 441 ), 0, 1 )   // ugly!!!
//      BufRd.ar( 1, 0, SinOsc.ar( 441 ), 0, 1 )
//      BufRd[ audio.type, audio.type ]( 1, 0, SinOsc.ar( 441 ), 0, 1 )
//      BufRd[ audio.type, control.type ]( 1, 0, SinOsc.kr( 441 ), 0, 1 )
//      disk.rate match {
//         case `audio` => println( "Jo chuck" )
//         case _ => println( "Nooo" )
//      }
   }
}