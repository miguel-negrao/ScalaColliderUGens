package de.sciss.synth

import collection.immutable.{IndexedSeq => IIdxSeq}

object TestTypes {
   sealed trait Rate
//   sealed trait AcceptableWith[ R <: Rate ]
   case object scalar   extends Rate // with AcceptableWith[ scalar.type ]
   case object control  extends Rate
   case object audio    extends Rate
   case object demand   extends Rate

//   trait Expands[ +R ] {
//      def expand: IIdxSeq[ R ]
//   }

   sealed trait GE[ +U <: AnyUGenIn ] /* extends Expands[ U ] */ {
//      type rate = R
      def expand: IIdxSeq[ U ]
   }

   sealed trait MultiGE[ +U <: AnyUGenIn ] {
      def expand: IIdxSeq[ GE[ U ]]
   }

   case class UGenInSeq[ +U <: AnyUGenIn ]( elems: IIdxSeq[ U ]) extends IIdxSeq[ U ] with GE[ U ] {
      def expand = this
      def apply( idx: Int ) = elems( idx )
      def length : Int = elems.length
   }

   sealed trait HasDoneFlag

//   trait AudioRatedGE extends GE with AudioRated {
//      def expand: IIdxSeq[ UGenIn with AudioRated ]
//   }

   sealed trait UGenIn[ R <: Rate ] extends GE[ UGenIn[ R ]] {
      def expand: IIdxSeq[ UGenIn[ R ]] = IIdxSeq( this )
   }

   type AnyUGenIn = UGenIn[ _ <: Rate ]

   abstract class SingleOutUGen[ R <: Rate ]( inputs: Seq[ AnyUGenIn ]) extends UGenIn[ R ]

//   trait ScalarRated { def rate = scalar }
//   trait AudioRated  { def rate = audio }

   object DiskOut {
      def ar( buf: GE[ AnyUGenIn ], multi: MultiGE[ UGenIn[ audio.type ]]) : DiskOut = apply( buf, multi )
   }

   case class DiskOut( buf: GE[ AnyUGenIn ], multi: MultiGE[ UGenIn[ audio.type ]])
   extends GE[ DiskOutUGen ] {
//      def rate: audio.type = audio
      def expand: IIdxSeq[ DiskOutUGen ] = {
         val bufE :   IIdxSeq[ AnyUGenIn ] = buf.expand
         val multiE  = multi.expand
         val numExp  = math.max( bufE.size, multiE.size )
         IIdxSeq.tabulate( numExp )( i => DiskOutUGen( bufE( i % numExp ), multiE( i % numExp ).expand ))
      }
   }

   case class DiskOutUGen( buf: UGenIn[ _ <: Rate ], multi: Seq[ UGenIn[ audio.type ]])
   extends SingleOutUGen[ audio.type ]( buf +: multi )

   object SinOsc {
      def ar( freq: GE[ AnyUGenIn ]) = apply[ audio.type ](   freq )
      def kr( freq: GE[ AnyUGenIn ]) = apply[ control.type ]( freq )
   }
   case class SinOsc[ R <: Rate ]( freq: GE[ AnyUGenIn ])
   extends GE[ SinOscUGen[ R ]] {
      def expand: IIdxSeq[ SinOscUGen[ R ]] = {
         val freqE : IIdxSeq[ AnyUGenIn ]   = freq.expand // why the explicit type???
         val numExp  = freqE.size
         IIdxSeq.tabulate( numExp )( i => SinOscUGen[ R ]( freqE( i )))
      }
   }
   case class SinOscUGen[ R <: Rate ]( freq: AnyUGenIn )
   extends SingleOutUGen[ R ]( freq :: Nil )

   object Line {
      def kr( start: GE[ AnyUGenIn ], end: GE[ AnyUGenIn ], dur: GE[ AnyUGenIn ], doneAction: GE[ AnyUGenIn ]) =
         apply[ control.type ]( start, end, dur, doneAction )
   }
   case class Line[ R <: Rate ]( start: GE[ AnyUGenIn ], end: GE[ AnyUGenIn ],
                                 dur: GE[ AnyUGenIn ], doneAction: GE[ AnyUGenIn ])
   extends GE[ LineUGen[ R ]] with HasDoneFlag {
      def expand: IIdxSeq[ LineUGen[ R ]] = {
         val startE: IIdxSeq[ AnyUGenIn ] = start.expand
         val endE: IIdxSeq[ AnyUGenIn ]   = end.expand
         val durE: IIdxSeq[ AnyUGenIn ]   = dur.expand
         val doneE: IIdxSeq[ AnyUGenIn ]  = doneAction.expand
         val numExp  = math.max( math.max( math.max( startE.size, endE.size ), durE.size ), doneE.size )
         IIdxSeq.tabulate( numExp )( i =>
            LineUGen[ R ]( startE( i % numExp ), endE( i % numExp ), durE( i % numExp ), doneE( i % numExp )))
      }
   }
   case class LineUGen[ R <: Rate ]( start: AnyUGenIn, end: AnyUGenIn, dur: AnyUGenIn, doneAction: AnyUGenIn )
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
      def kr( src: GE[ AnyUGenIn with HasDoneFlag ]) = apply( src )
   }
   case class Done( src: GE[ AnyUGenIn with HasDoneFlag ])
   extends GE[ DoneUGen ] {
      def expand: IIdxSeq[ DoneUGen ] = {
         val srcE : IIdxSeq[ UGenIn[ _ <: Rate ] with HasDoneFlag ] = src.expand
         val numExp = srcE.size
         IIdxSeq.tabulate( numExp )( i => DoneUGen( srcE( i % numExp )))
      }
   }
   case class DoneUGen( src: AnyUGenIn with HasDoneFlag )
   extends SingleOutUGen[ control.type ]( src :: Nil )

   case class Constant( v: Float ) extends UGenIn[ scalar.type ]

   object Expand {
      def none[ U <: AnyUGenIn ]( ge: GE[ U ]) = new MultiGE[ U ] {
         def expand = IIdxSeq( ge )
      }
      def apply[ U <: AnyUGenIn ]( ge: GE[ U ], step: Int = 1 ) = new MultiGE[ U ] {
         def expand = {
            val exp     = ge.expand
            val flatCh  = exp.size
            val numCh   = flatCh / step
            IIdxSeq.tabulate( numCh ) { idx => UGenInSeq( exp.slice( idx * step, math.min( (idx + 1) * step, flatCh )))}
         }
      }
      def iterate[ U <: AnyUGenIn ]( ge: GE[ U ], n: Int )( f: GE[ U ] => GE[ U ]) = new MultiGE[ U ] {
         def expand = IIdxSeq.iterate( ge, n )( f )
      }
      def tabulate[ U <: AnyUGenIn ]( n: Int )( f: Int => GE[ U ]) = new MultiGE[ U ] {
         def expand = IIdxSeq.tabulate( n )( f )
      }
      def fill[ U <: AnyUGenIn ]( n: Int )( elem: => GE[ U ]) = new MultiGE[ U ] {
         def expand = IIdxSeq.fill( n )( elem )
      }
   }

   implicit def floatToGE( f: Float ) = Constant( f )
   implicit def defaultExpand[ U <: AnyUGenIn ]( ge: GE[ U ]) = Expand.none( ge )
//   implicit def seqOfGEToGE[ T <% GE[ AnyUGenIn ]]( seq: Seq[ T ]) = UGenInSeq( seq.toIndexedSeq )
//   implicit def seqOfGEToGE( x: Seq[ GE ]) : GE = {
//      val outputs: IIdxSeq[ UGenIn ] = x.flatMap( _.outputs )( breakOut )
//      outputs match {
//         case IIdxSeq( mono ) => mono
//         case _               => new UGenInSeq( outputs )
//      }
//   }

   def test {
      Done.kr( Line.kr( 0, 1, 2, 3 ))
      DiskOut.ar( 0, SinOsc.ar( 441 ))
      DiskOut.ar( 0, Expand( SinOsc.ar( 441 )))
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