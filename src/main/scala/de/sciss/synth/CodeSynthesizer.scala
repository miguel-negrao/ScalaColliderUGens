package de.sciss.synth

import scala.tools.nsc.symtab.Flags
import tools.refactoring.Refactoring
import tools.refactoring.common.{Tracing, Change}
import tools.refactoring.util.CompilerProvider
import xml.Node
import java.io.File

class CodeSynthesizer extends Refactoring with Tracing with CompilerProvider {

   override val defaultIndentationStep = "   "

   import global._
   
   private def perform( xml: Node, dir: File ) {

      (xml \ "file") foreach { node =>
         val name       = (node \ "@name").text
         val fileName   = name + ".scala"
         val ast        = treeFrom( "package de.sciss.synth.ugen\n" )
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
               val typStr     = (n \ "@type").headOption.map( _.text ).getOrElse( "GE" )
               val default    = (n \ "@default").headOption.map( _.text )
               val multi      = getBoolAttr( n, "multi" )
               val doc        = (n \ "doc").headOption.map( _.text )
//               UGenArg( ScalaMethodArg( name, typ, default, doc ), multi, idx )
               val typ        = 
               val vParam     = ValDef( Modifiers( Flags.PARAM ), name, TypeTree( typ /* selectedValue.tpt.tpe */ ), EmptyTree ) :: Nil
               vParam
            }
//            val outputs       = (node \ "outputs").headOption match {
//               case Some( n ) => (n \ "@num").text match {
//                  case "0" => ZeroOutputs
//                  case t   => MultiOutput( t )
//               }
//               case None      => SingleOutput
//            }
            val trnsAst    = ↓( matchingChildren( trns )) apply ast
            val changes    = refactor( trnsAst.toList )
            val outputText = Change.applyChanges( changes, inputText )

            println( outputText )
         }
      }
   }

//   {
////      val inputText  = "object Schnuck\n"
//      val inputText  = """package de.sciss.synth.ugen
//"""
//      val ast        = treeFrom( inputText )
//
//      val applyDef = DefDef(
//         mods     = Modifiers( Flags.METHOD ) withPosition (Flags.METHOD, NoPosition),
//         name     = "apply",
//         tparams  = Nil,
//         vparamss = List( Nil ),
//         tpt      = EmptyTree,
//         rhs      = Block( Ident( "gugu" ) :: Nil, EmptyTree )
//      )
//
//      val trns       = transform {
//        case tpl: Template =>
//          val body = applyDef :: tpl.body
//          tpl.copy( body = body ) setPos tpl.pos
//      }
//      val trnsAst    = ↓( matchingChildren( trns )) apply ast
//      val changes    = refactor( trnsAst.toList )
//      val outputText = Change.applyChanges( changes, inputText )
//
//      println( outputText )
//   }

   private def getBoolAttr( n: Node, name: String, default: Boolean = false ) =
      (n \ ("@" + name)).headOption.map( _.text.toBoolean ).getOrElse( default )

   private case class RateInfo( name: String, methodName: String, implied: Boolean )
}