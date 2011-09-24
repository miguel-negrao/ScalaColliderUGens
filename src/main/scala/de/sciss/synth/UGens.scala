/*
 *  UGens.scala
 *  (ScalaCollider-UGens)
 *
 *  Copyright (c) 2008-2011 Hanns Holger Rutz. All rights reserved.
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

import xml.XML
import scopt.OptionParser
import java.io.{IOException, File}

object UGens {
   val name          = "ScalaCollider-UGens"
   val version       = 0.14
   val copyright     = "(C)opyright 2008-2011 Hanns Holger Rutz"
   val isSnapshot    = true

   def versionString = {
      val s = (version + 0.001).toString.substring( 0, 4 )
      if( isSnapshot ) s + "-SNAPSHOT" else s
   }

   def main( args: Array[ String ]) {
//      var xmlPath    = ""
      var inputs     = IndexedSeq.empty[ String ]
      var docs       = true
      var dirOption  = Option.empty[ String ]
      val parser     = new OptionParser( name ) {
//         opt( "v", "verbose", "Verbose output", verbose = true )
         opt( "d", "dir", "<directory>", "Source output root directory", (s: String) => dirOption = Some( s ))
//         doubleOpt( "in-start", "Punch in begin (secs)", (d: Double) => punchInStart  = Some( d ))
//         intOpt( "num-per-file", "Maximum matches per single file (default 1)", numPerFile = _ )
//         doubleOpt( "spacing", "Minimum spacing between matches within one file (default 0.5)", minSpacing = _ )
//         arg( "input", "UGen description file (XML) to process", (i: String) => xmlPath = i )
         arglistOpt( "inputs...", "List of UGen description files (XML) to process", inputs +:= _ )
         opt( "no-docs", "Do not include scaladoc comments", docs = false )
      }

      if( !parser.parse( args )) sys.exit( 1 )

      val dir  = new File( new File( new File( new File( new File( new File( new File( dirOption.getOrElse( "out" ),
         "src" ), "main" ), "scala" ), "de" ), "sciss" ), "synth" ), "ugen" )
      if( !dir.isDirectory ) if( !dir.mkdirs() ) throw new IOException( "Could not create directory: " + dir.getPath )

//      val xml  = XML.load( UGens.getClass.getResourceAsStream( "standard-ugens.xml" ))
      val synth= new CodeSynthesizer4( docs )
//      synth.perform( xml, dir )
      inputs.foreach { xmlPath =>
         val xml = XML.loadFile( xmlPath )
         synth.perform( xml, dir ) //, (f, u) => f == "TriggerUGens" || f == "FilterUGens" )
      }
      sys.exit( 0 )
   }
}