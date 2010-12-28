/*
 *  UGens.scala
 *  (ScalaCollider-UGens)
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.synth

import xml.{ Node, XML }
import java.io.{FileOutputStream, BufferedWriter, OutputStreamWriter, File}

object UGens {
   def main( args: Array[ String ]) {
      require( args.size == 2 && args( 0 ) == "-d" )
      val dir  = new File( args( 1 ))
      val xml  = XML.load( UGens.getClass.getResourceAsStream( "standard-ugens.xml" ))
//      create( xml, dir )
      val synth = new CodeSynthesizer
      synth.perform( xml, dir )
//TestTypes.test
      System.exit( 0 )
   }

   private def create( xml: Node, dir: File ) {
      (xml \ "file") foreach { node =>
         val name       = (node \ "@name").text
         val fileName   = name + ".scala" 
         val ugens      = (node \ "ugen") map { node =>
            val name          = (node \ "@name").text
            val sideEffect    = getBoolAttr( node, "sideeffect" )
            val rates         = (node \ "rate") map { n =>
               val name       = (n \ "@name").text
               val methodName = name match {
                  case "audio"   => "ar"
                  case "control" => "kr"
                  case "scalar"  => "ir"
                  case "demand"  => "dr"
               }
               val implied    = getBoolAttr( n, "implied" )
               RateInfo( name, methodName, implied )
            }
            val impliedRate   = rates.find( _.implied )
            if( impliedRate.isDefined ) require( rates.size == 1 )
            val args          = (node \ "arg").zipWithIndex map { tup =>
               val (n, idx)   = tup
               val name       = (n \ "@name").text
               val typ        = (n \ "@type").headOption.map( _.text ).getOrElse( "GE" )
               val default    = (n \ "@default").headOption.map( _.text )
               val multi      = getBoolAttr( n, "multi" )
               val doc        = (n \ "doc").headOption.map( _.text ) 
               UGenArg( ScalaMethodArg( name, typ, default, doc ), multi, idx )
            }
            val outputs       = (node \ "outputs").headOption match {
               case Some( n ) => (n \ "@num").text match {
                  case "0" => ZeroOutputs
                  case t   => MultiOutput( t )
               }
               case None      => SingleOutput
            }
            val doc  = (node \ "doc").headOption map { n =>
               val main = (n \ "text").text
               val sees = (n \ "see").map( _.text )
               UGenDoc( main, sees )
            }
            UGenInfo( name, rates, impliedRate, sideEffect, args, outputs, doc )
         }
         val source =
"""/*
 *  """ + fileName + """
 *  (ScalaCollider)
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.synth
package ugen

import SynthGraph._
""" +
         ugens.map( ugen => {
            val (ugenIns, other) = ugen.args.partition( _.arg.typ == "GE" )
            "\n" + makeDoc( ugen.doc ) +
            "object " + ugen.name + " {" +
            ugen.rates.map( rate => "\n   def " + rate.methodName +
               ugen.args.map( arg =>
         //         ( name: String, typ: String, default: Option[ String ], multi: Boolean,
         //           doc: Option[ String ])
               arg.arg.name + ": " + arg.arg.typ + arg.arg. default.map( " = " + _ ).getOrElse( "" )
               ).mkString( "( ", ", ", " ) : GE =\n      make" ) +
                  (if( rate.implied ) ugen.args.map( _.arg.name ) else rate.name +: ugen.args.map( _.arg.name )).mkString( "( ", ", ", " )" ) + "\n"
            ).mkString + "\n   private def make" + {
               val args0   = ugen.args.zipWithIndex.map( tup => tup._1.arg.name + ": " + tup._1.arg.typ )
               val args    = if( ugen.impliedRate.isDefined ) args0 else "rate: Rate" +: args0
               args.mkString( "( ", ", ", " ) : GE =" )
            } + {
               val regExpNames = Vector.tabulate( ugenIns.size )( i => "a" + (i + 1) )
               "\n      for( Seq" + regExpNames.mkString( "( ", ", ", " )" ) + " <- expand" +
                  ugenIns.map( _.arg.name ).mkString( "( ", ", ", " )" ) + ") yield this" + {
                  val outArgs0 = other.map( _.arg.name ) ++ regExpNames
                  (if( ugen.impliedRate.isDefined ) outArgs0 else "rate" +: outArgs0).mkString( "( ", ", ", " )" )
               }
            } + "\n}\ncase class " + ugen.name + {
               val outArgs0 = other.map( a => a.arg.name + ": " + a.arg.typ ) ++ ugenIns.map( a => a.arg.name + ": UGenIn" )
               (if( ugen.impliedRate.isDefined ) outArgs0 else "rate: Rate" +: outArgs0).mkString( "( ", ", ", " )" )
            } + "\nextends " + (ugen.outputs match {
               case SingleOutput       => "SingleOutUGen" + ugenIns.map( _.arg.name ).mkString( "( ", ", ", " )" )
               case ZeroOutputs        => "ZeroOutUGen"   + ugenIns.map( _.arg.name ).mkString( "( ", ", ", " )" )
               case MultiOutput( num ) => "MultiOutUGen( " + ugen.impliedRate.map( _.name ).getOrElse( "rate" ) +
                  ", " + num + ugenIns.map( _.arg.name ).mkString( ", ", ", ", " )" )  
            }) + (if( ugen.impliedRate.isDefined || ugen.sideEffect ) {
               "\n" + ugen.impliedRate.map( r => "with " + r.name.capitalize + "Rated" +
                  (if( ugen.sideEffect) " " else "")).getOrElse( "" ) +
               (if( ugen.sideEffect ) "with SideEffectUGen" else "")
            } else "")
         }).mkString

         val fos  = new FileOutputStream( new File( dir, fileName ))
         val w    = new BufferedWriter( new OutputStreamWriter( fos, "UTF-8" ))
         w.write( source )
         w.close
      }
   }

// case class DiskIn( numChannels: Int, buf: UGenIn, loop: UGenIn )
// extends MultiOutUGen( audio, numChannels, buf, loop ) with AudioRated with SideEffectUGen // side-effect: advancing sf offset

   private def removeWhitespace( s: String ) : String =
      s.dropWhile( _ == ' ' ).reverse.dropWhile( _ == ' ' ).reverse

   private def removeEmptyLines( s: Seq[ String ]) : Seq[ String ] = {
      val s1 = if( s.headOption == Some( "" )) s.tail else s
      val s2 = if( s.lastOption == Some( "" )) s1.dropRight( 1 ) else s1
      s2
   }

   private def makeDoc( doc: Option[ UGenDoc ]) : String = doc.map( d =>
         removeEmptyLines( d.main.lines.map( removeWhitespace( _ )).toSeq ).mkString( "/**\n *  ", "\n *  ", "\n */\n" )
      ).getOrElse( "" )

   private def getBoolAttr( n: Node, name: String, default: Boolean = false ) =
      (n \ ("@" + name)).headOption.map( _.text.toBoolean ).getOrElse( default )
   
   private case class RateInfo( name: String, methodName: String, implied: Boolean )
   private abstract sealed class Outputs
   private case object ZeroOutputs extends Outputs
   private case object SingleOutput extends Outputs
   private case class MultiOutput( num: String ) extends Outputs
   private case class UGenDoc( main: String, sees: Seq[ String ])
   private case class UGenInfo( name: String, rates: Seq[ RateInfo ], impliedRate: Option[ RateInfo ], sideEffect: Boolean,
      args: Seq[ UGenArg ], outputs: Outputs, doc: Option[ UGenDoc ])
   private sealed abstract class Visibility { def mkString: String }
   private case object PublicVisibility extends Visibility { def mkString = "" }
   private case object ProtectedVisibility extends Visibility { def mkString = "protected " }
   private case object PrivateVisibility extends Visibility { def mkString = "private " }
   private case class ScalaMethod( args: Seq[ ScalaMethodArg ], returnTyp: String, body: String,
                              doc: Option[ String ] = None, visibility: Visibility = PublicVisibility )
   private case class ScalaMethodArg( name: String, typ: String, default: Option[ String ], doc: Option[ String ])
   private case class ScalaObject( name: String, methods: Seq[ ScalaMethod ], doc: Option[ String ])
   private case class UGenArg( arg: ScalaMethodArg, multi: Boolean, idx: Int )
}