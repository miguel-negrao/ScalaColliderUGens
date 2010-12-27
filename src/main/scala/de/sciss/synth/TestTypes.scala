package de.sciss.synth

import collection.immutable.{IndexedSeq => IIdxSeq}

object TestTypes {
   sealed trait Rate
//   sealed trait AcceptableWith[ R <: Rate ]
   case object scalar   extends Rate // with AcceptableWith[ scalar.type ]
   case object control  extends Rate
   case object audio    extends Rate
   case object demand   extends Rate

   trait ScalarRated {  def rate = scalar }
   trait ControlRated { def rate = audio }
   trait AudioRated {   def rate = audio }

   implicit def doubleToGE( d: Double ) = Constant( d.toFloat )
   implicit def floatToGE( f: Float ) = Constant( f )

//   trait Expands[ +R ] {
//      def expand: IIdxSeq[ R ]
//   }

   sealed trait GE[ +U ] {
      def expand: IIdxSeq[ U ]
   }

//   sealed trait Multi[ +G ] {
//      def expand: IIdxSeq[ G ]
//   }

   case class UGenInSeq[ +U <: AnyUGenIn ]( elems: IIdxSeq[ U ]) extends IIdxSeq[ U ] with GE[ U ] {
      def expand = this
      def apply( idx: Int ) = elems( idx )
      def length : Int = elems.length
   }

   sealed trait HasSideEffect
   sealed trait HasDoneFlag

//   trait AudioRatedGE extends GE with AudioRated {
//      def expand: IIdxSeq[ UGenIn with AudioRated ]
//   }

   case class UGenOutProxy[ R <: Rate ]( source: UGen, outputIndex: Int, rate: R )
   extends UGenIn[ R ] /* with UGenProxy */ {
      override def toString = source.toString + ".\\(" + outputIndex + ")"
      def displayName = source.displayName + " \\ " + outputIndex
   }

   sealed trait UGenIn[ R <: Rate ] extends GE[ UGenIn[ R ]] {
//      final override def numOutputs = 1
//      final def outputs: IIdxSeq[ AnyUGenIn ] = Vector( this )

      def expand: IIdxSeq[ UGenIn[ R ]] = IIdxSeq( this )
   }

   type AnyUGenIn = UGenIn[ _ <: Rate ]
   type AnyGE     = GE[ AnyUGenIn ]

   abstract class UGen /* extends RatedGE with UGenProxy */ {
//      // ---- constructor ----
//      {
//         SynthGraph.builder.addUGen( this )
//      }

      def rate: Rate
      def name = { val cn = getClass.getName; cn.substring( cn.lastIndexOf( '.' ) + 1 )}
      def displayName = name
      def inputs: IIdxSeq[ AnyUGenIn ]
      def numInputs = inputs.size
      def source = this
      def specialIndex = 0
      def outputIndex = 0

//      override def toString: String = {
//         name + "." + rate.methodName + inputs.mkString( "(", ", ", ")" )
//      }
   }

   abstract class SingleOutUGen[ R <: Rate ]( val inputs: IIdxSeq[ AnyUGenIn ]) extends UGen with UGenIn[ R ]
//   abstract class SingleOutUGen( val inputs: UGenIn* ) extends UGen with UGenIn

   abstract class MultiOutUGen( /* outputRates: IIdxSeq[ Rate ], */ val inputs: IIdxSeq[ AnyUGenIn ]) extends UGen with AnyGE {
      // most multi out ugens use the same rate for all outputs,
      // therefore we have a simpler constructor
//      def this[ R <: Rate ]( /* rate: Rate, */ numOutputs: Int, inputs: UGenIn* ) = this( Vector.fill( numOutputs )( rate ), inputs: _* )

//      final override def numOutputs = outputRates.size
//      final def outputs: IIdxSeq[ UGenIn ] = outputRates.zipWithIndex.map(
//         tup => UGenOutProxy( this, tup._2, tup._1 ))
      def expand = IIdxSeq.tabulate( inputs.size )( UGenOutProxy( this, _, rate ))
   }

   abstract class ZeroOutUGen( val inputs: IIdxSeq[ AnyUGenIn ]) extends UGen with HasSideEffect {
//      final override def numOutputs = 0
//      final def outputs: IIdxSeq[ UGenIn ] = Vector.empty
   }

   object DiskOut {
      def ar(buf: GE[AnyUGenIn], multi: GE[AnyGE]) = apply(buf, multi)
   }
   case class DiskOut(buf: GE[AnyUGenIn], multi: GE[AnyGE]) extends GE[DiskOutUGen] {
      def expand = {
         val _buf = buf.expand
         val _multi = multi.expand
         val _sz_buf = _buf.size
         val _sz_multi = _multi.size
         val _exp_ = max(_sz_buf, _sz_multi)
         IIdxSeq.tabulate(_exp_)(i => DiskOutUGen(_buf(i.%(_sz_buf)), _multi(i.%(_sz_multi))))
      }
   }
   case class DiskOutUGen(buf: AnyUGenIn, multi: AnyGE) extends SingleOutUGen[audio.type](multi.expand.+:(buf)) with AudioRated

   object SinOsc {
      def ar: SinOsc[audio.type] = ar( )
      def kr: SinOsc[control.type] = kr( )
      def ar(freq: GE[AnyUGenIn] = 440.0, phase: GE[AnyUGenIn] = 0.0) = apply[audio.type](audio, freq, phase)
      def kr(freq: GE[AnyUGenIn] = 440.0, phase: GE[AnyUGenIn] = 0.0) = apply[control.type](control, freq, phase)
   }
   case class SinOsc[R <: Rate](rate: R, freq: GE[AnyUGenIn], phase: GE[AnyUGenIn]) extends GE[SinOscUGen[R]] {
      def expand = {
         val _freq = freq.expand
         val _phase = phase.expand
         val _sz_freq = _freq.size
         val _sz_phase = _phase.size
         val _exp_ = max(_sz_freq, _sz_phase)
         IIdxSeq.tabulate(_exp_)(i => SinOscUGen(rate, _freq(i.%(_sz_freq)), _phase(i.%(_sz_phase))))
      }
   }
   case class SinOscUGen[R <: Rate](rate: R, freq: AnyUGenIn, phase: AnyUGenIn) extends SingleOutUGen[R](IIdxSeq(freq, phase))

   object Line {
      def kr( start: GE[ AnyUGenIn ], end: GE[ AnyUGenIn ], dur: GE[ AnyUGenIn ], doneAction: GE[ AnyUGenIn ]) =
         apply[ control.type ]( control, start, end, dur, doneAction )
   }
   case class Line[ R <: Rate ]( rate: R, start: GE[ AnyUGenIn ], end: GE[ AnyUGenIn ],
                                 dur: GE[ AnyUGenIn ], doneAction: GE[ AnyUGenIn ])
   extends GE[ LineUGen[ R ]] with HasDoneFlag {
      def expand: IIdxSeq[ LineUGen[ R ]] = {
         val startE: IIdxSeq[ AnyUGenIn ] = start.expand
         val endE: IIdxSeq[ AnyUGenIn ]   = end.expand
         val durE: IIdxSeq[ AnyUGenIn ]   = dur.expand
         val doneE: IIdxSeq[ AnyUGenIn ]  = doneAction.expand
         val numExp  = math.max( math.max( math.max( startE.size, endE.size ), durE.size ), doneE.size )
         IIdxSeq.tabulate( numExp )( i =>
            LineUGen[ R ]( rate, startE( i % numExp ), endE( i % numExp ), durE( i % numExp ), doneE( i % numExp )))
      }
   }
   case class LineUGen[ R <: Rate ]( rate: R, start: AnyUGenIn, end: AnyUGenIn, dur: AnyUGenIn, doneAction: AnyUGenIn )
   extends SingleOutUGen[ R ]( IIdxSeq( start, end,  dur, doneAction )) with HasDoneFlag

   def max( i: Int, is: Int* ) : Int = is.foldLeft( i )( math.max( _, _ ))

   object VDiskIn {
      def ar(numChannels: Int, buf: GE[AnyUGenIn], speed: GE[AnyUGenIn] = 1.0, loop: GE[AnyUGenIn] = 0.0, sendID: GE[AnyUGenIn] = 0.0) = apply(numChannels, buf, speed, loop, sendID)
   }
   case class VDiskIn(numChannels: Int, buf: GE[AnyUGenIn], speed: GE[AnyUGenIn], loop: GE[AnyUGenIn], sendID: GE[AnyUGenIn]) extends GE[VDiskInUGen] {
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
         IIdxSeq.tabulate(_exp_)(i => VDiskInUGen(numChannels, _buf(i.%(_sz_buf)), _speed(i.%(_sz_speed)), _loop(i.%(_sz_loop)), _sendID(i.%(_sz_sendID))))
      }
   }
   case class VDiskInUGen(numChannels: Int, buf: AnyUGenIn, speed: AnyUGenIn, loop: AnyUGenIn, sendID: AnyUGenIn)
   extends MultiOutUGen(IIdxSeq(buf, speed, loop, sendID)) with AudioRated

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
   extends SingleOutUGen[ control.type ]( IIdxSeq( src )) with ControlRated

   case class Constant( v: Float ) extends UGenIn[ scalar.type ]

   object Expand {
      def none[ G <: GE[ AnyUGenIn ]]( ge: G ) = new GE[ G ] {
         def expand = IIdxSeq( ge )
      }
      def apply[ G <: GE[ AnyUGenIn ]]( ge: G, step: Int = 1 ) = new GE[ GE[ AnyUGenIn ]] {
         def expand = {
            val exp     = ge.expand
            val flatCh  = exp.size
            val numCh   = flatCh / step
            IIdxSeq.tabulate( numCh ) { idx => UGenInSeq( exp.slice( idx * step, math.min( (idx + 1) * step, flatCh )))}
         }
      }
      def iterate[ G <: GE[ AnyUGenIn ]]( ge: G, n: Int )( f: G => G ) = new GE[ G ] {
         def expand = IIdxSeq.iterate( ge, n )( f )
      }
      def tabulate[ G <: GE[ AnyUGenIn ]]( n: Int )( f: Int => G ) = new GE[ G ] {
         def expand = IIdxSeq.tabulate( n )( f )
      }
      def fill[ G <: GE[ AnyUGenIn ]]( n: Int )( elem: => G ) = new GE[ G ] {
         def expand = IIdxSeq.fill( n )( elem )
      }
   }

   implicit def defaultExpand[ G <: GE[ AnyUGenIn ]]( ge: G ) = Expand.none( ge )

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
      DiskOut.ar( 0, VDiskIn.ar( 2, 0 ))
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