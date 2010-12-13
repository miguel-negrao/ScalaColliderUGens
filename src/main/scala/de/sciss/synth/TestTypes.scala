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
      def ar( freq: GE[ _ <: Rate ]) = apply[ audio.type ]( audio, freq )
   }
   case class SinOsc[ R <: Rate ]( rate: R, freq: GE[ _ <: Rate ]) extends GE[ R ] {
      def expand: IIdxSeq[ SinOscUGen[ R ]] = {
         val freqE : IIdxSeq[ UGenIn[ _ <: Rate ]]   = freq.expand // why the explicit type???
         val numExp  = freqE.size
         IIdxSeq.tabulate( numExp )( i => SinOscUGen( rate, freqE( i )))
      }
   }
   case class SinOscUGen[ R <: Rate ]( rate: R, freq: UGenIn[ _ <: Rate ])
   extends SingleOutUGen[ R ]( freq :: Nil )

   case class Constant( v: Float ) extends UGenIn[ scalar.type ]

   implicit def floatToGE( f: Float ) = Constant( f )

   def test {
      val disk = DiskOut.ar( 0, SinOsc.ar( 441 ))
      disk.rate match {
         case `audio` => println( "Jo chuck" )
         case _ => println( "Nooo" )
      }
   }
}