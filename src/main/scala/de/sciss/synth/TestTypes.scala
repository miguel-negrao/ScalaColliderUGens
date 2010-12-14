package de.sciss.synth

import collection.immutable.{IndexedSeq => IIdxSeq}

object TestTypes {
   sealed trait Rate
   case object scalar   extends Rate
   case object control  extends Rate
   case object audio    extends Rate
   case object demand   extends Rate

   sealed trait GE[ R <: Rate ] {
      type rate = R
      def expand: IIdxSeq[ UGenIn[ R ]]
   }

   sealed trait HasDoneFlag

//   trait AudioRatedGE extends GE with AudioRated {
//      def expand: IIdxSeq[ UGenIn with AudioRated ]
//   }

   sealed trait UGenIn[ R <: Rate ] extends GE[ R ] {
      def expand: IIdxSeq[ UGenIn[ R ]] = IIdxSeq( this )
   }

   abstract class SingleOutUGen[ R <: Rate ]( inputs: Seq[ UGenIn[ _ <: Rate ]]) extends UGenIn[ R ]

//   trait ScalarRated { def rate = scalar }
//   trait AudioRated  { def rate = audio }

   object DiskOut {
      def ar( buf: GE[ _ <: Rate ], multi: GE[ audio.type ]) : DiskOut = apply( buf, multi )
   }
   
   case class DiskOut( buf: GE[ _ <: Rate ], multi: GE[ audio.type ])
   extends GE[ audio.type ] {
      def rate: audio.type = audio
      def expand: IIdxSeq[ DiskOutUGen ] = {
         val bufE : IIdxSeq[ UGenIn[ _ <: Rate ]] = buf.expand
         val multiE  = multi.expand
         val numExp  = bufE.size // math.max( bufE.size, multiE.size )
         IIdxSeq.tabulate( numExp )( i => DiskOutUGen( bufE( i ), multiE ))
      }
   }

   case class DiskOutUGen( buf: UGenIn[ _ <: Rate ], multi: Seq[ UGenIn[ audio.type ]])
   extends SingleOutUGen[ audio.type ]( buf +: multi )

   object SinOsc {
      def ar( freq: GE[ _ <: Rate ]) = apply[ audio.type ](   freq )
      def kr( freq: GE[ _ <: Rate ]) = apply[ control.type ]( freq )
   }
   case class SinOsc[ R <: Rate ]( freq: GE[ _ <: Rate ]) extends GE[ R ] {
      def expand: IIdxSeq[ SinOscUGen[ R ]] = {
         val freqE : IIdxSeq[ UGenIn[ _ <: Rate ]]   = freq.expand // why the explicit type???
         val numExp  = freqE.size
         IIdxSeq.tabulate( numExp )( i => SinOscUGen[ R ]( freqE( i )))
      }
   }
   case class SinOscUGen[ R <: Rate ]( freq: UGenIn[ _ <: Rate ])
   extends SingleOutUGen[ R ]( freq :: Nil )

   object Line {
      def kr( start: GE[ _ <: Rate ], end: GE[ _ <: Rate ], dur: GE[ _ <: Rate ], doneAction: GE[ _ <: Rate ]) =
         apply[ control.type ]( start, end, dur, doneAction )
   }
   case class Line[ R <: Rate ]( start: GE[ _ <: Rate ], end: GE[ _ <: Rate ], dur: GE[ _ <: Rate ], doneAction: GE[ _ <: Rate ])
   extends GE[ R ] with HasDoneFlag {
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

   object ZeroCrossing {
      def ar( in: GE[ audio.type ])   = apply[ audio.type ](   in )
      def kr( in: GE[ control.type ]) = apply[ control.type ]( in )
   }
   case class ZeroCrossing[ R <: Rate ]( in: GE[ R ])
   extends GE[ R ] {
      def expand: IIdxSeq[ ZeroCrossingUGen[ R ]] = {
         val inE: IIdxSeq[ UGenIn[ R ]] = in.expand
         val numExp  = inE.size
         IIdxSeq.tabulate( numExp )( i => ZeroCrossingUGen[ R ]( inE( i % numExp )))
      }
   }
   case class ZeroCrossingUGen[ R <: Rate ]( in: UGenIn[ R ])
   extends SingleOutUGen[ R ]( in :: Nil )

   object BufRd {
      def ar( numChannels: Int, buf: GE[ _ <: Rate ], phase: GE[ audio.type ], loop: GE[ _ <: Rate ], interp: GE[ _ <: Rate ]) =
         apply[ audio.type ](   numChannels, buf, phase, loop, interp )

      def kr( numChannels: Int, buf: GE[ _ <: Rate ], phase: GE[ _ <: Rate ], loop: GE[ _ <: Rate ], interp: GE[ _ <: Rate ]) =
         apply[ control.type ]( numChannels, buf, phase, loop, interp )
   }
   // XXX can we enforce at this stage that for R == audio.type, phase must be audio.type??
   case class BufRd[ R <: Rate ]( numChannels: Int, buf: GE[ _ <: Rate ], phase: GE[ _ <: Rate ],
                                  loop: GE[ _ <: Rate ], interp: GE[ _ <: Rate ])
   extends GE[ R ] {
      def expand: IIdxSeq[ BufRdUGen[ R ]] = {
         val bufE:      IIdxSeq[ UGenIn[ _ <: Rate ]] = buf.expand
         val phaseE:    IIdxSeq[ UGenIn[ _ <: Rate ]] = phase.expand
         val loopE:     IIdxSeq[ UGenIn[ _ <: Rate ]] = loop.expand
         val interpE:   IIdxSeq[ UGenIn[ _ <: Rate ]] = interp.expand
         val numExp  = math.max( math.max( math.max( bufE.size, phaseE.size ), loopE.size ), interpE.size )
         IIdxSeq.tabulate( numExp )( i =>
            BufRdUGen[ R ]( bufE( i % numExp ), phaseE( i % numExp ), loopE( i % numExp ), interpE( i % numExp )))
      }
   }
   case class BufRdUGen[ R <: Rate ]( buf: UGenIn[ _ <: Rate ], phase: UGenIn[ _ <: Rate ],
                                      loop: UGenIn[ _ <: Rate ], interp: UGenIn[ _ <: Rate ])
   extends SingleOutUGen[ R ]( List( buf, phase, loop, interp )) // XXX not SingleOut

   case class Constant( v: Float ) extends UGenIn[ scalar.type ]

   implicit def floatToGE( f: Float ) = Constant( f )

   def test {
//      val zero = ZeroCrossing.kr( SinOsc.ar( 441 ))
      val disk = DiskOut.ar( 0, ZeroCrossing.ar( SinOsc.ar( 441 )))
      disk.rate match {
         case `audio` => println( "Jo chuck" )
         case _ => println( "Nooo" )
      }
   }
}