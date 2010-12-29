package de.sciss.synth

import collection.immutable.{IndexedSeq => IIdxSeq}

object TestTypes {
   sealed trait Rate
//   sealed trait AcceptableWith[ R <: Rate ]
   sealed trait scalar  extends Rate
   sealed trait control extends Rate
   sealed trait audio   extends Rate
   sealed trait demand  extends Rate

   case object scalar   extends scalar
   case object control  extends control
   case object audio    extends audio
   case object demand   extends demand

   trait ScalarRated {  def rate = scalar }
   trait ControlRated { def rate = control }
   trait AudioRated {   def rate = audio }

   implicit def doubleToGE( d: Double ) = Constant( d.toFloat )
   implicit def floatToGE( f: Float ) = Constant( f )

   trait Expands[ +T ] {
      def expand: IIdxSeq[ T ]
   }

   sealed trait GE[ +R <: Rate, +U <: UGenIn[ R ]] extends Expands[ U ] {
//      def expand: IIdxSeq[ U ]
      def rate : R
   }

//   sealed trait Multi[ +G ] {
//      def expand: IIdxSeq[ G ]
//   }

//   case class UGenInSeq[ R <: Rate, +U <: UGenIn[ R ]]( elems: IIdxSeq[ U ]) extends /* IIdxSeq[ U ] with */ GE[ R, U ] {
//      def expand = elems // this
////      def apply( idx: Int ) = elems( idx )
////      def length : Int = elems.length
//   }

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

   sealed trait UGenIn[ +R <: Rate ] extends GE[ R, UGenIn[ R ]] {
//      final override def numOutputs = 1
//      final def outputs: IIdxSeq[ AnyUGenIn ] = Vector( this )

      def expand: IIdxSeq[ UGenIn[ R ]] = IIdxSeq( this )
   }

   type AnyUGenIn = UGenIn[ _ <: Rate ]
   type AnyGEGaga[ R <: Rate ] = GE[ R, _ <: UGenIn[ R ]]
   type AnyGE = AnyGEGaga[ _ <: Rate ]

   abstract class UGen /* extends RatedGE with UGenProxy */ {
//      // ---- constructor ----
//      {
//         SynthGraph.builder.addUGen( this )
//      }

//      def rate: Rate
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

   abstract class MultiOutUGen[ R <: Rate ]( outputRates: IIdxSeq[ R ], val inputs: IIdxSeq[ AnyUGenIn ]) extends UGen with GE[ R, UGenIn[ R ]] {
      // most multi out ugens use the same rate for all outputs,
      // therefore we have a simpler constructor
//      def this[ R <: Rate ]( /* rate: Rate, */ numOutputs: Int, inputs: UGenIn* ) = this( Vector.fill( numOutputs )( rate ), inputs: _* )

//      final override def numOutputs = outputRates.size
//      final def outputs: IIdxSeq[ UGenIn ] = outputRates.zipWithIndex.map(
//         tup => UGenOutProxy( this, tup._2, tup._1 ))
//      def expand = IIdxSeq.tabulate( inputs.size )( UGenOutProxy( this, _, rate ))
      def expand: IIdxSeq[ UGenIn[ R ]] = outputRates.zipWithIndex.map( tup => UGenOutProxy( this, tup._2, tup._1 ))
   }

   abstract class ZeroOutUGen( val inputs: IIdxSeq[ AnyUGenIn ]) extends UGen with HasSideEffect {
//      final override def numOutputs = 0
//      final def outputs: IIdxSeq[ UGenIn ] = Vector.empty
   }

   sealed abstract class DoneAction( val id: Int )
   case object doNothing         extends DoneAction( 0 )
   case object pauseSelf         extends DoneAction( 1 )
   case object freeSelf          extends DoneAction( 2 )
   case object freeSelfPred      extends DoneAction( 3 )
   case object freeSelfSucc      extends DoneAction( 4 )
   case object freeSelfPredAll   extends DoneAction( 5 )
   case object freeSelfSuccAll   extends DoneAction( 6 )
   case object freeSelfToHead    extends DoneAction( 7 )
   case object freeSelfToTail    extends DoneAction( 8 )
   case object freeSelfPausePred extends DoneAction( 9 )
   case object freeSelfPauseSucc extends DoneAction( 10 )
   case object freeSelfPredDeep  extends DoneAction( 11 )
   case object freeSelfSuccDeep  extends DoneAction( 12 )
   case object freeAllInGroup    extends DoneAction( 13 )
   case object freeGroup         extends DoneAction( 14 )

   case class Constant( v: Float ) extends UGenIn[ scalar ] { def rate = scalar }

   implicit def doneActionToGE( x: DoneAction ) = Constant( x.id )

   def max( i: Int, is: Int* ) : Int = is.foldLeft( i )( math.max( _, _ ))

   private type FuckYou[ R <: Rate ] = GE[ R, UGenIn[ R ] with HasDoneFlag ]
   object Done {
//      def kr(src: GE[_ <: Rate, AnyUGenIn with HasDoneFlag]) = apply(src)
      def kr(src: FuckYou[_ <: Rate ]) = apply(src)
   }
   case class Done(src: FuckYou[_ <: Rate ]) extends GE[control, DoneUGen] with ControlRated {
      def expand = {
         val _src = src.expand
         val _sz_src = _src.size
         val _exp_ = max(_sz_src)
         IIdxSeq.tabulate(_exp_)(i => DoneUGen(_src(i.%(_sz_src))))
      }
   }
   case class DoneUGen(src: AnyUGenIn with HasDoneFlag) extends SingleOutUGen[control](IIdxSeq(src)) with ControlRated with HasSideEffect

   object Line {
      def ar: Line[audio] = ar( )
      def kr: Line[control] = kr( )
      def ar(start: AnyGE = 0.0, end: AnyGE = 1.0, dur: AnyGE = 1.0, doneAction: AnyGE = doNothing) = apply[audio](audio, start, end, dur, doneAction)
      def kr(start: AnyGE = 0.0, end: AnyGE = 1.0, dur: AnyGE = 1.0, doneAction: AnyGE = doNothing) = apply[control](control, start, end, dur, doneAction)
   }
   case class Line[R <: Rate](rate: R, start: AnyGE, end: AnyGE, dur: AnyGE, doneAction: AnyGE) extends GE[R, LineUGen[R]] {
      def expand = {
         val _start = start.expand
         val _end = end.expand
         val _dur = dur.expand
         val _doneAction = doneAction.expand
         val _sz_start = _start.size
         val _sz_end = _end.size
         val _sz_dur = _dur.size
         val _sz_doneAction = _doneAction.size
         val _exp_ = max(_sz_start, _sz_end, _sz_dur, _sz_doneAction)
         IIdxSeq.tabulate(_exp_)(i => LineUGen(rate, _start(i.%(_sz_start)), _end(i.%(_sz_end)), _dur(i.%(_sz_dur)), _doneAction(i.%(_sz_doneAction))))
      }
   }
   case class LineUGen[R <: Rate](rate: R, start: AnyUGenIn, end: AnyUGenIn, dur: AnyUGenIn, doneAction: AnyUGenIn)
   extends SingleOutUGen[R](IIdxSeq(start, end, dur, doneAction)) with HasSideEffect with HasDoneFlag

   object SinOsc {
      def ar: SinOsc[audio] = ar( )
      def kr: SinOsc[control] = kr( )
      def ar(freq: AnyGE = 440.0, phase: AnyGE = 0.0) = apply[audio](audio, freq, phase)
      def kr(freq: AnyGE = 440.0, phase: AnyGE = 0.0) = apply[control](control, freq, phase)
   }
   case class SinOsc[R <: Rate](rate: R, freq: AnyGE, phase: AnyGE) extends GE[R, SinOscUGen[R]] {
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

   object DiskIn {
      def ar(numChannels: Int, buf: AnyGE, loop: AnyGE = 0.0) = apply(numChannels, buf, loop)
   }
   case class DiskIn(numChannels: Int, buf: AnyGE, loop: AnyGE) extends Expands[DiskInUGen] with AudioRated {
      def expand = {
         val _buf = buf.expand
         val _loop = loop.expand
         val _sz_buf = _buf.size
         val _sz_loop = _loop.size
         val _exp_ = max(_sz_buf, _sz_loop)
         IIdxSeq.tabulate(_exp_)(i => DiskInUGen(numChannels, _buf(i.%(_sz_buf)), _loop(i.%(_sz_loop))))
      }
   }
   case class DiskInUGen(numChannels: Int, buf: AnyUGenIn, loop: AnyUGenIn) extends MultiOutUGen(IIdxSeq.fill(numChannels)(audio), IIdxSeq(buf, loop)) with AudioRated with HasSideEffect
   object DiskOut {
      def ar(buf: AnyGE, multi: Expands[GE[audio, UGenIn[audio]]]) = apply(buf, multi)
   }
   case class DiskOut(buf: AnyGE, multi: Expands[GE[audio, UGenIn[audio]]]) extends GE[audio, DiskOutUGen] with AudioRated {
      def expand = {
         val _buf = buf.expand
         val _multi = multi.expand
         val _sz_buf = _buf.size
         val _sz_multi = _multi.size
         val _exp_ = max(_sz_buf, _sz_multi)
         IIdxSeq.tabulate(_exp_)(i => DiskOutUGen(_buf(i.%(_sz_buf)), _multi(i.%(_sz_multi))))
      }
   }
   case class DiskOutUGen(buf: AnyUGenIn, multi: GE[audio, UGenIn[audio]]) extends SingleOutUGen[audio](multi.expand.+:(buf)) with AudioRated with HasSideEffect
   object VDiskIn {
      def ar(numChannels: Int, buf: AnyGE, speed: AnyGE = 1.0, loop: AnyGE = 0.0, sendID: AnyGE = 0.0) = apply(numChannels, buf, speed, loop, sendID)
   }
   case class VDiskIn(numChannels: Int, buf: AnyGE, speed: AnyGE, loop: AnyGE, sendID: AnyGE) extends Expands[VDiskInUGen] with AudioRated {
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
   extends MultiOutUGen(IIdxSeq.fill(numChannels)(audio), IIdxSeq(buf, speed, loop, sendID)) with AudioRated with HasSideEffect

   object Expand {
      def none[ R <: Rate, G <: GE[ R, UGenIn[ R ]]]( ge: G ) = new Expands[ G ] {
         def rate = ge.rate
         def expand = IIdxSeq( ge )
      }
//      def apply[ U <: UGenIn[ _ <: Rate ], G <: GE[ U ]]( ge: G, step: Int = 1 ) = new GE[ GE[ U ]] {
//         def expand = {
//            val exp     = ge.expand
//            val flatCh  = exp.size
//            val numCh   = flatCh / step
//            IIdxSeq.tabulate( numCh ) { idx => UGenInSeq[ U ]( exp.slice( idx * step, math.min( (idx + 1) * step, flatCh )))}
//         }
//      }

//      def apply[ U <: Rate, G <: GE[ UGenIn[ U ]]]( ge: G, step: Int = 1 ) = new GE[ GE[ UGenIn[ U ]]] {
//         def expand = {
//            val exp     = ge.expand
//            val flatCh  = exp.size
//            val numCh   = flatCh / step
//            IIdxSeq.tabulate( numCh ) { idx => UGenInSeq( exp.slice( idx * step, math.min( (idx + 1) * step, flatCh )))}
//         }
//      }

//      def apply[ U <: UGenIn[ _ <: Rate ]]( ge: GE[ U ], step: Int = 1 ) = new GE[ GE[ U ]] {
//         def expand = {
//            val exp     = ge.expand
//            val flatCh  = exp.size
//            val numCh   = flatCh / step
//            IIdxSeq.tabulate( numCh ) { idx => UGenInSeq( exp.slice( idx * step, math.min( (idx + 1) * step, flatCh )))}
//         }
//      }
//      def iterate[ G <: GE[ AnyUGenIn ]]( ge: G, n: Int )( f: G => G ) = new GE[ G ] {
//         def expand = IIdxSeq.iterate( ge, n )( f )
//      }
//      def tabulate[ G <: GE[ AnyUGenIn ]]( n: Int )( f: Int => G ) = new GE[ G ] {
//         def expand = IIdxSeq.tabulate( n )( f )
//      }
//      def fill[ G <: GE[ AnyUGenIn ]]( n: Int )( elem: => G ) = new GE[ G ] {
//         def expand = IIdxSeq.fill( n )( elem )
//      }
   }

   implicit def defaultExpand[ R <: Rate, G <: GE[ R, UGenIn[ R ]]]( ge: G ) = Expand.none[ R, G ]( ge )

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
// :: rightfully rejected by compiler ::
//      Done.kr( SinOsc.kr )
      DiskOut.ar( 0, SinOsc.ar( 441 ))
// :: rightfully rejected by compiler ::
//      DiskOut.ar( 0, SinOsc.kr )

      DiskOut.ar( 0, VDiskIn.ar( 2, 0 ))
      DiskOut.ar( 0, SinOsc.ar( 441 ))
//      DiskOut.ar( 0, Expand.none( SinOsc.ar( 441 )))
      DiskOut.ar( 0, Expand.none[audio,SinOsc[audio]]( SinOsc.ar( 441 )))  // XXX FUCK
// :: rightfully rejected by compiler ::
//      DiskOut.ar( 0, Expand[SinOscUGen[control.type]]( SinOsc.kr( 441 )))  // XXX FUCK

// XXX TODO
//      DiskOut.ar( 0, Expand( SinOsc.ar( 441 )))
      val sin = SinOsc.ar( 441 )
      val sinUs: IIdxSeq[ SinOscUGen[ _ ]] = sin.expand
   }

   def test2( g: AnyGE ) = g match {
      case x @ Line( rate, start, end, dur, doneAction ) => x.copy( dur = 2 )
      case x => x
   }
}