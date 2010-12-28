/*
 *  CodeSynthesizer.scala
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

import scala.tools.nsc.symtab.Flags
import tools.refactoring.Refactoring
import tools.refactoring.util.CompilerProvider
import xml.Node
import java.io.File
import collection.breakOut
import net.virtualvoid.string.MyNodePrinter
import tools.refactoring.transformation.TreeFactory
import tools.refactoring.common.{CompilerAccess, Tracing, Change}
import tools.nsc.io.AbstractFile

class CodeSynthesizer extends Refactoring
with Tracing with CompilerProvider with MyNodePrinter with CompilerAccess with TreeFactory {
   override val defaultIndentationStep = "   "

   import global._

   def compilationUnitOfFile( f: AbstractFile ) = global.unitOfFile.get( f )

   def perform( xml: Node, dir: File ) {

//      val testAst = treeFrom( "object Test { def eins { }; def zwei() { }; def drei { eins; zwei() }}" )
//      println( printer( testAst ))
////      println( createText( testAst ))
//
//      ↓(matchingChildren(transform {
//         case t: Template => t.body.foreach {
//            case d: DefDef =>
////               if( d.name == "eins" || d.name == "zwei" ) {
//                  println( "For " + d.name + " --> " + d.vparamss )
////               }
////               println( "Jo, defdef( name = " + d.name+ ", name " + d.hasSymbol + " ) = " + d )
////               println( printer( d ))
//            case _ =>
//         }
//         t
//         case x => x
//      })) apply testAst

//      val traitGE = ClassDef(
//         NoMods withPosition (Flags.TRAIT, NoPosition), // Modifiers( Flags.CASEACCESSOR ),
//         "GE",
//         Nil,
//         Template(
//            EmptyTree :: Nil,
//            emptyValDef,
//            Nil
//         )
//      )

//      val traitGE = TypeDef( Modifiers( Flags.TRAIT ), "GE", Nil, EmptyTree )
      val traitSideEffect  = TypeDef( Modifiers( Flags.TRAIT ), "HasSideEffect", Nil, EmptyTree )
      val traitDoneFlag    = TypeDef( Modifiers( Flags.TRAIT ), "HasDoneFlag",   Nil, EmptyTree )
      val identIIdxSeq     = Ident( "IIdxSeq" )

      (xml \ "file") foreach { node =>
         val name       = (node \ "@name").text
         val fileName   = name + ".scala"
         val ast        = treeFrom( "package de.sciss.synth.ugen\n" )
         val ugens: List[ Tree ] = (node \ "ugen").flatMap( node => {
            val name          = (node \ "@name").text
            val sideEffect    = getBoolAttr( node, "sideeffect" )
            val doneFlag      = getBoolAttr( node, "doneflag" )
            val rates: List[ RateInfo ] = (node \ "rate").map( n => {
               val name       = (n \ "@name").text
               val methodName = name match {
                  case "audio"   => "ar"
                  case "control" => "kr"
                  case "scalar"  => "ir"
                  case "demand"  => "dr"
               }
               val implied    = getBoolAttr( n, "implied" )
               RateInfo( name, methodName, implied )
            })( breakOut )
            val impliedRate   = rates.find( _.implied )
            if( impliedRate.isDefined ) require( rates.size == 1 )
            val args : List[ UGenArgInfo ] = (node \ "arg").zipWithIndex.map( tup => {
               val (n, idx)   = tup
               val name       = (n \ "@name").text
               val multi      = getBoolAttr( n, "multi" )
               val doneFlagArg= getBoolAttr( n, "doneflag" )
               val rate       = (n \ "@rate").text
               if( multi ) require( !doneFlagArg )
               val typInfo    = (n \ "@type").headOption.map( n => TypeInfo( n.text -> Nil ))
//                  .getOrElse( TypeInfo( (if( multi ) "MultiGE" else "GE") -> (TypeInfo( ("AnyUGenIn" -> Nil) ) :: Nil) ))
                  .getOrElse( TypeInfo( "GE" -> {
                     val tup0 = if( doneFlagArg ) (("HasDoneFlag" -> Nil) :: Nil) else Nil
                     val tup1 = ((if( multi ) {
                        rate match {
                           case "" => "AnyGE"
                           case r  => "GE[UGenIn[" + r + /* ".type]]" */ "]]" // XXX dirty
                        }
                     } else {
                        rate match {
                           case "" => "AnyUGenIn"
                           case r  => "UGenIn[" + r + /* ".type]]" */ "]]" // XXX dirty
                        }
                     }) -> Nil) :: tup0
                     TypeInfo( tup1: _* ) :: Nil
                  }))
               val default    = (n \ "@default").headOption.map( _.text )
               val doc        = (n \ "doc").headOption.map( _.text )
               UGenArgInfo( ArgInfo( name, typInfo, default, doc ), multi, /* doneFlag, */ idx )
//               val typ        =
//               val vParam     = ValDef( Modifiers( Flags.PARAM ), name, TypeTree( typ /* selectedValue.tpt.tpe */ ), EmptyTree ) :: Nil
//               vParam
            })( breakOut )

            val outputs       = (node \ "outputs").headOption match {
               case Some( n ) => (n \ "@num").text match {
                  case "0" => ZeroOutputs
                  case t   => MultiOutput( t )
               }
               case None      => SingleOutput
            }

//            val trnsAst    = ↓( matchingChildren( trns )) apply ast
//            val changes    = refactor( trnsAst.toList )
//            val outputText = Change.applyChanges( changes, inputText )

            val ugenName = name + "UGen"

            val objectMethodArgs = args map { uArgInfo =>
               ValDef(
                  Modifiers( Flags.PARAM ),
                  uArgInfo.arg.name,
                  Ident( uArgInfo.arg.typ.toString ),
                  uArgInfo.arg.default.map( s => if( uArgInfo.isGE ) {
                     try {
                        Literal( Constant( s.toFloat ))
                     } catch {
                        case e: NumberFormatException => Ident( s )
                     }
                  } else Ident( s )).getOrElse( EmptyTree )
               )
            }

//            val methodBody = Block( Select( Ident( "freq" ), "toString" ) :: Nil, EmptyTree )
//            val methodBody = Select( Ident( "freq" ), "toString" ))
            val objectMethodDefs0 = rates.map( rateInfo => {
               val args0 = args.map( i => Ident( i.arg.name ))
               val methodBody = if( impliedRate.isDefined ) {
                  Apply( Ident( "apply" ), args0 )
               } else {
                  // the type application is still needed for the compiler not complain about some
                  // audio.type versus audio bullshit...
                  Apply( TypeApply( Ident( "apply" ), Ident( rateInfo.typ ) :: Nil ), Ident( rateInfo.name ) :: args0 )
//                  Apply( Ident( "apply" ), Ident( rateInfo.name ) :: args0 )
               }
               DefDef(
                  NoMods withPosition (Flags.METHOD, NoPosition),
                  rateInfo.methodName,
                  Nil,        // tparams
                  objectMethodArgs :: Nil,    // vparamss
                  TypeTree( NoType ), // tpt -- empty for testing
                  methodBody // rhs
               )
            })

            val allDefaults = args.forall( _.arg.default.isDefined )
            val objectMethodDefs = if( allDefaults ) {
               rates.map( rateInfo => {
//                  val methodBody = Apply( Ident( rateInfo.methodName ), Ident( " " ) :: Nil )  // XXX how to get ar() with the parentheses?
                  val methodBody = Apply( Ident( rateInfo.methodName ), Ident( " " ) :: Nil )  // XXX how to get ar() with the parentheses?
                  DefDef(
                     NoMods withPosition (Flags.METHOD, NoPosition),
                     rateInfo.methodName,
                     Nil,        // tparams
                     Nil,        // vparams
                     TypeDef( NoMods, name, if( impliedRate.isDefined ) Nil else {
                        TypeDef( NoMods, rateInfo.typ, Nil, EmptyTree ) :: Nil
                     }, EmptyTree ),
                     methodBody // rhs
                  )
               }) ++ objectMethodDefs0
            } else objectMethodDefs0

            val objectDef = ModuleDef(
               NoMods,
               name,
               Template(
                  EmptyTree :: Nil, // parents
                  emptyValDef,      // self
                  objectMethodDefs  // body
               )
            )

            val caseClassConstrArgs  = {
               val args0 = args map { uArgInfo => (NoMods, uArgInfo.arg.name + ": " + uArgInfo.arg.typ, EmptyTree) }
               if( impliedRate.isDefined ) args0 else {
                  (NoMods, "rate: R", EmptyTree) :: args0
               }
            }

            val caseClassExpandDef = {
//               val bufE    = buf.expand
//               val multiE  = multi.expand
//               val numExp  = math.max( bufE.size, multiE.size )
//               IIdxSeq.tabulate( numExp )( i => DiskOutUGen( bufE( i % numExp ), multiE( i % numExp ).expand ))
               val expArgs = args.filter( _.isGE )
               val methodBody = Block(
                  (expArgs.map( a => ValDef( NoMods, "_" + a.arg.name, TypeTree( NoType ), Select( Ident( a.arg.name ), "expand" ))) ++
                   expArgs.map( a => ValDef( NoMods, "_sz_" + a.arg.name, TypeTree( NoType ), Select( Ident( "_" + a.arg.name ), "size" ))) ++ (
                     ValDef( NoMods, "_exp_", TypeTree( NoType ), Apply( Ident( "max" ), expArgs.map( a => Ident( "_sz_" + a.arg.name )))) ::
                     Apply( Apply( Select( identIIdxSeq, "tabulate" ), Ident( "_exp_" ) :: Nil ),
                        Function( ValDef( Modifiers( Flags.PARAM ), "i", TypeTree( NoType ), EmptyTree ) :: Nil,
                           Apply( Ident( ugenName ), {
                              val args0 = args.map( a => {
                                 if( a.isGE ) {
                                    val apply = Apply( Ident( "_" + a.arg.name ),
                                       Apply( Select( Ident( "i" ), "%" ), Ident( "_sz_" + a.arg.name ) :: Nil ) :: Nil )
//                                    if( a.multi ) Select( apply, "expand" ) else apply
                                    apply
                                 } else Ident( a.arg.name )
                              })
                              if( impliedRate.isDefined ) args0 else (Ident( "rate" ) :: args0)
                           })
                        ) :: Nil
                     ) :: Nil
                  )): _*
               )
//                  Apply( Ident( "apply" ), Ident( rateInfo.name ) :: args.map( i => Ident( i.arg.name )))
               DefDef(
                  NoMods withPosition (Flags.METHOD, NoPosition),
                  "expand",
                  Nil, // tparams
                  Nil,        // vparamss
                  TypeTree( NoType ), // tpt -- empty for testing
                  methodBody // rhs
               )
            }

            // hmmmm... is this the cleanest way to define R <: Rate?
//            if( impliedRate.isDefined ) Nil else (TypeDef( NoMods, "R", Nil, TypeBoundsTree( EmptyTree, TypeDef( NoMods, "Rate", Nil, EmptyTree ))) :: Nil), // tparams
            val caseClassTypeParam = if( impliedRate.isDefined ) Nil else (TypeDef( NoMods, "R <: Rate", Nil, EmptyTree ) :: Nil)

            val caseClassDef = mkCaseClass(
               NoMods,
               name,

               caseClassTypeParam, // tparams
               caseClassConstrArgs,
               caseClassExpandDef :: Nil,
               TypeDef( NoMods, "GE", TypeDef( NoMods, ugenName,
                  if( impliedRate.isDefined ) Nil else (TypeDef( NoMods, "R", Nil, EmptyTree ) :: Nil),
                  EmptyTree ) :: Nil, EmptyTree ) :: Nil
            )

//            caseClassDef.setSymbol( new tools.nsc.symtab.Symbols.Symbol( NoSymbol, NoPosition, new Name( 0, 0 )))

//            global.docComment( caseClassDef.symbol, "Jo chuck", NoPosition )
//            global.docComments += caseClassDef.symbol -> DocComment( "/** Kuuka */", NoPosition )

//            println( "JUHU " + caseClassConstr.symbol.isConstructor )
//            println( printer( caseClassConstr ))

//            case class SinOscUGen[ R <: Rate ]( freq: AnyUGenIn, phase: AnyUGenIn )
//            extends SingleOutUGen[ R ]( List( freq, phase ))

            val ugenCaseClassConstrArgs = {
               val args0 = args map { uArgInfo =>
                  (NoMods, uArgInfo.arg.name + ": " + (if( uArgInfo.isGE ) {
                     uArgInfo.arg.typ.tuples.head._2.head
                  } else uArgInfo.arg.typ), EmptyTree)
               }
               if( impliedRate.isDefined ) args0 else {
                  (NoMods, "rate: R", EmptyTree) :: args0
               }
            }

            val ugenCaseClassParents: List[ TypeDef ] = {
               val p1 = if( doneFlag )   (traitDoneFlag :: Nil) else Nil
               val p2 = if( sideEffect ) (traitSideEffect :: p1) else p1
               val p3 = impliedRate.map( r => TypeDef( NoMods, r.traitTyp, Nil, EmptyTree ) :: p2 ).getOrElse( p2 )
               TypeDef( NoMods, outputs.typ, if( outputs != SingleOutput ) Nil else {
                  TypeDef( NoMods, impliedRate.map( _.typ ).getOrElse( "R" ): String, Nil, EmptyTree ) :: Nil
               }, EmptyTree ) :: p3
            }
//            val ugenCaseClassParents: List[ TypeDef ] = impliedRate.map( r =>
//               ugenCaseClassParents0 :: TypeDef( NoMods, r.traitTyp, Nil, EmptyTree ) :: Nil
//            ).getOrElse( ugenCaseClassParents0 :: Nil )

            val ugenCaseClassDef = mkCaseClass(
               NoMods,
               ugenName,
               caseClassTypeParam, // tparams
               ugenCaseClassConstrArgs,
               Nil,
               ugenCaseClassParents,
               superArgs = {
                  val geArgs = args.filter( _.isGE )  // XXX TODO: order
                  val args0 = geArgs.lastOption match {
                     case Some( a ) if( a.multi ) =>
                        val rvsArgs = geArgs.dropRight( 1 ).reverse
                        rvsArgs.foldLeft[ Tree ]( Select( Ident( a.arg.name ), "expand" ))( (a, b) => Apply( Select( a, "+:" ), Ident( b.arg.name ) :: Nil )) :: Nil
                     case _ => Apply( identIIdxSeq, geArgs.map( a => Ident( a.arg.name ))) :: Nil
                  }
                  outputs match {
                     case m: MultiOutput =>
                        Apply( Apply( Select( identIIdxSeq, "fill" ), Ident( m.num ) :: Nil ),
                           Ident( impliedRate.map( _.typ ).getOrElse( "rate" )) :: Nil ) :: args0
                     case _ => args0
                  }
               }
            )

            objectDef :: caseClassDef :: ugenCaseClassDef :: Nil  // how to prepend a blank line??

//            println( outputText )
         })( breakOut )

         // XXX add UGen class

         val packageDef = PackageDef( Select( Select( Ident( "de" ), "sciss" ), "synth" ),
            PackageDef( Ident( "ugen" ),
               Import( Select( Ident( "collection" ), "immutable" ), ImportSelector( "IndexedSeq", -1, identIIdxSeq.name, -1 ) :: Nil ) ::
               Import( Ident( "SynthGraph" ), ImportSelector( nme.WILDCARD, -1, nme.WILDCARD, -1 ) :: Nil ) ::
               ugens ) :: Nil )
         println( createText( packageDef ))
         println()
//         println( createText( ugens.head ))
      }
   }

   private def getBoolAttr( n: Node, name: String, default: Boolean = false ) =
      (n \ ("@" + name)).headOption.map( _.text.toBoolean ).getOrElse( default )

   private case class RateInfo( name: String, methodName: String, implied: Boolean ) {
//      def typ = name + ".type"
      def typ = name // + ".type"
      def traitTyp = name.capitalize + "Rated"
   }
//   private object TypeInfo {
//      def toString( typ: Seq[ TypeInfo ]) : String = {
//         typ.map( t => t.name + (t.params match {
//            case Nil  => ""
//            case coll => coll.mkString( "[ ", ", ", " ]" )
//         })).mkString( " with ")
//      }
//   }
//   private case class TypeInfo( name: String, params: List[ TypeInfo ] = Nil )

//   private case class MyFunction(vparams: List[ValOrDefDef], body: Tree) extends TermTree with SymTree
//   private case class ArgDef(mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) extends ValOrDefDef

   // XXX remove in favour of TypeDef
   private case class TypeInfo( tuples: (String, List[ TypeInfo ])* ) {
      override def toString = {
         tuples.map( t => t._1 + (t._2 match {
            case Nil  => ""
            case coll => coll.mkString( "[", ", ", "]" )
         })).mkString( " with ")
      }
   }
   private case class ArgInfo( name: String, typ: TypeInfo, default: Option[ String ], doc: Option[ String ])
   private case class UGenArgInfo( arg: ArgInfo, multi: Boolean, idx: Int ) {
      def isGE = arg.typ.tuples.headOption match {
         case Some( ("GE", _) )        => true
//         case Some( ("MultiGE", _) )   => true
         case _                        => false
      }
   }

   private abstract sealed class Outputs {
      def typ: String
   }
   private case object ZeroOutputs extends Outputs {
      val typ = "ZeroOutUGen"
   }
   private case object SingleOutput extends Outputs {
      val typ = "SingleOutUGen"
   }
   private case class MultiOutput( num: String ) extends Outputs {
      val typ = "MultiOutUGen"
   }
}