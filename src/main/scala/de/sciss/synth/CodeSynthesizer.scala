package de.sciss.synth

import scala.tools.nsc.symtab.Flags
import tools.refactoring.Refactoring
import tools.refactoring.common.{Tracing, Change}
import tools.refactoring.util.CompilerProvider

class CodeSynthesizer extends Refactoring with Tracing with CompilerProvider {

   override val defaultIndentationStep = "   "

   import global._
   //
   {
//      val inputText  = "object Schnuck\n"
      val inputText  = """package de.sciss.synth.ugen
"""
      val ast        = treeFrom( inputText )

      val applyDef = DefDef(
         mods     = Modifiers( Flags.METHOD ) withPosition (Flags.METHOD, NoPosition),
         name     = "apply",
         tparams  = Nil,
         vparamss = List( Nil ),
         tpt      = EmptyTree,
         rhs      = Block( Ident( "gugu" ) :: Nil, EmptyTree )
      )

      val trns       = transform {
        case tpl: Template =>
          val body = applyDef :: tpl.body
          tpl.copy( body = body ) setPos tpl.pos
      }
      val trnsAst    = ↓( matchingChildren( trns )) apply ast
      val changes    = refactor( trnsAst.toList )
      val outputText = Change.applyChanges( changes, inputText )

      println( outputText )
   }
}