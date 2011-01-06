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
   val version = 0.10
   def versionString = (version.toString + "0").substring( 0, 4 )

   def main( args: Array[ String ]) {
      require( args.size == 2 && args( 0 ) == "-d" )
      val dir  = new File( args( 1 ))
      val xml  = XML.load( UGens.getClass.getResourceAsStream( "standard-ugens.xml" ))
      val synth= new CodeSynthesizer
//      synth.perform( xml, dir )
      synth.perform( xml, dir, (f, u) => (f == "NoiseUGens") ) // (f, u) => (f == "NoiseUGens" || f == "DelayUGens" || f == "IOUGens" )
      System.exit( 0 )
   }
}