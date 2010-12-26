package de.sciss.synth

import collection.immutable.{IndexedSeq => IIdxSeq}

object TestTypes {
   sealed trait Rate
//   sealed trait AcceptableWith[ R <: Rate ]
   case object scalar   extends Rate // with AcceptableWith[ scalar.type ]
   case object control  extends Rate
   case object audio    extends Rate
   case object demand   extends Rate

   implicit def doubleToGE( d: Double ) = Constant( d.toFloat )
   implicit def floatToGE( f: Float ) = Constant( f )

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
         val bufE    = buf.expand
         val multiE  = multi.expand
         val numExp  = math.max( bufE.size, multiE.size )
         IIdxSeq.tabulate( numExp )( i => DiskOutUGen( bufE( i % numExp ), multiE( i % numExp ).expand ))
      }
   }

   case class DiskOutUGen( buf: UGenIn[ _ <: Rate ], multi: Seq[ UGenIn[ audio.type ]])
   extends SingleOutUGen[ audio.type ]( buf +: multi )

   object SinOsc {
      def ar: SinOsc[audio.type] = ar( )
      def kr: SinOsc[control.type] = kr( )
      def ar(freq: GE[AnyUGenIn] = 440.0, phase: GE[AnyUGenIn] = 0.0) = apply[audio.type](freq, phase)
      def kr(freq: GE[AnyUGenIn] = 440.0, phase: GE[AnyUGenIn] = 0.0) = apply[control.type](freq, phase)
   }
   case class SinOsc[R <: Rate](freq: GE[AnyUGenIn], phase: GE[AnyUGenIn]) extends GE[SinOscUGen[R]] {
      def expand = {
         val _freq = freq.expand
         val _phase = phase.expand
         val _sz_freq = _freq.size
         val _sz_phase = _phase.size
         val _exp_ = max(_sz_freq, _sz_phase)
         IIdxSeq.tabulate(_exp_)(i => SinOscUGen(_freq(i.%(_sz_freq)), _phase(i.%(_sz_phase))))
      }
   }
   case class SinOscUGen[ R <: Rate ]( freq: AnyUGenIn, phase: AnyUGenIn )
   extends SingleOutUGen[ R ]( List( freq, phase ))

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

   def max( i: Int, is: Int* ) : Int = is.foldLeft( i )( math.max( _, _ ))

   case class VDiskIn(rate: Rate, numChannels: Int, buf: GE[AnyUGenIn], speed: GE[AnyUGenIn], loop: GE[AnyUGenIn], sendID: GE[AnyUGenIn]) extends GE[VDiskInUGen] {
      def expand = {
         val _buf = buf.expand
         val _speed = speed.expand
         val _loop = loop.expand
         val _sendID = sendID.expand
         val _sz_buf = _buf.size
         val _sz_speed = _speed.size
         val _sz_loop = _loop.size
         val _sz_sendID = _sendID.size
         val _exp_ = max(_sz_buf, _sz_speed, _sz_loop, _sz_sendID)
         IIdxSeq.tabulate(_exp_)(i => VDiskInUGen(_buf(i.%(_sz_buf)), _speed(i.%(_sz_speed)), _loop(i.%(_sz_loop)), _sendID(i.%(_sz_sendID))))
      }
   }
   case class VDiskInUGen( buf: AnyUGenIn, speed: AnyUGenIn, loop: AnyUGenIn, sendID: AnyUGenIn )
   extends SingleOutUGen[ audio.type ]( List( buf, speed, loop, sendID ))

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
      val sin = SinOsc.ar( 441 )
      val sinUs: IIdxSeq[ SinOscUGen[ _ ]] = sin.expand
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