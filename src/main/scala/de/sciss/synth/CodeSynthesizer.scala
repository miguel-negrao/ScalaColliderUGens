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
import collection.breakOut
import net.virtualvoid.string.MyNodePrinter
import tools.refactoring.transformation.TreeFactory
import tools.refactoring.common.{CompilerAccess, Tracing, Change}
import tools.nsc.io.AbstractFile
import java.io.{FileOutputStream, OutputStreamWriter, FileWriter, File}

class CodeSynthesizer extends Refactoring
with Tracing with CompilerProvider with MyNodePrinter with CompilerAccess with TreeFactory {
   override val defaultIndentationStep = "   "

   import global._

   def compilationUnitOfFile( f: AbstractFile ) = global.unitOfFile.get( f )

//   def myMkClass(
//      mods: Modifiers = NoMods,
//      name: String,
//      tparams: List[TypeDef] = Nil,
//      args: List[(Modifiers, String, Tree)],
//      body: List[Tree] = Nil,
//      parents: List[Tree] = Nil,
//      superArgs: List[Tree] = Nil) = {
//
//      val constructorArguments0 = args.map {
//         case (mods, name, tpe) =>
//            ValDef(mods | Flags.PARAMACCESSOR, name, tpe, EmptyTree)
//      }
//      val constructorArguments = if( args.nonEmpty ) constructorArguments0 else {
//         ValDef(NoMods | Flags.PARAMACCESSOR, "\t", EmptyTree, EmptyTree) :: constructorArguments0
//      }
//
//      val superArgsCall = superArgs match {
//         case Nil => EmptyTree
//         case args =>
//            mkDefDef(name = nme.CONSTRUCTOR.toString, body = Apply(EmptyTree, args) :: Nil)
//      }
//
//      ClassDef(
//         mods,
//         mkTypeName(name),
//         tparams,
//         Template(
//            parents,
//            emptyValDef,
//            superArgsCall :: constructorArguments ::: body
//         )
//      )
//   }

   def myMkCaseClass(
      mods: Modifiers = NoMods,
      name: String,
      tparams: List[TypeDef] = Nil,
      args: List[(Modifiers, String, Tree)],
      body: List[Tree] = Nil,
      parents: List[Tree] = Nil,
      superArgs: List[Tree] = Nil) = {

//    if (args.size < 1) throw new IllegalArgumentException("Case-class must have at least one argument.")

      mkClass(mods withPosition (Flags.CASE, NoPosition), name, tparams, args, body, parents, superArgs)
   }

   def perform( xml: Node, dir: File ) {

//      val testAst = treeFrom( "class A(); class B" )
//      println( printer( testAst ))
//      println( createText( testAst ))
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

      // bug in scala-refactoring : case class constructor arg list cannot be empty
//      val dummyCaseClassArgs = (NoMods, "", EmptyTree) :: Nil

      (xml \ "file") foreach { node =>
         val name       = (node \ "@name").text
         val fileName   = name + ".scala"
         val ast        = treeFrom( "package de.sciss.synth.ugen\n" )
         val ugens: List[ Tree ] = (node \ "ugen").flatMap( node => {
            val name          = (node \ "@name").text
            val readsBus      = getBoolAttr( node, "readsbus" )
            val writesBus     = getBoolAttr( node, "writesbus" )
            val writesBuffer  = getBoolAttr( node, "writesbuffer" )
            val sideEffect    = getBoolAttr( node, "sideeffect" ) || readsBus || writesBus || writesBuffer
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
               val typInfo    = (n \ "@type").headOption.map( n => TypeInfo( (n.text -> Nil) :: Nil ))
//                  .getOrElse( TypeInfo( (if( multi ) "MultiGE" else "GE") -> (TypeInfo( ("AnyUGenIn" -> Nil) ) :: Nil) ))
                  .getOrElse({
                     val typ0 = rate match {
                        case "" => if( doneFlagArg ) {
                           TypeInfo( ("GE" -> (TypeInfo( ("R" -> Nil) :: Nil ) :: TypeInfo(
                              List( ("UGenIn" -> (TypeInfo( ("R" -> Nil) :: Nil ) :: Nil)),
                                    ("HasDoneFlag" -> Nil))
                           ) :: Nil)) :: Nil, Some( "R" -> "<: Rate" ))
                        } else {
                           TypeInfo( ("AnyGE" -> Nil) :: Nil )
                        }
                        case r  =>
                           val t0 = "UGenIn" -> (TypeInfo( (r -> Nil) :: Nil ) :: Nil)
                           val t1: List[ (String, List[ TypeInfo ])] = if( doneFlagArg ) {
                              List( t0, "HasDoneFlag" -> Nil )
                           } else {
                              t0 :: Nil
                           }
                           TypeInfo( ("GE" -> (TypeInfo( (r -> Nil) :: Nil ) :: TypeInfo( t1 ) :: Nil)) :: Nil )
                     }
                     if( multi ) {
                        TypeInfo( ("Expands" -> (typ0 :: Nil)) :: Nil )
                     } else {
                        typ0
                     }
                  })
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

            val objectMethodArgs = args map { uArgInfo =>
               ValDef(
                  Modifiers( Flags.PARAM ),
                  uArgInfo.arg.name,
                  Ident( uArgInfo.arg.typ.toString ),
                  uArgInfo.arg.default.map( s => if( uArgInfo.isGE ) {
                     try {
//                        Literal( Constant( s.toFloat ))
                        Ident( s.toFloat.toString + "f" )  // XXX workaround for scala-refactoring bug of missing f
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

            val allDefaults = args.nonEmpty && args.forall( _.arg.default.isDefined )
            val objectMethodDefs = if( allDefaults ) {
               rates.map( rateInfo => {
//                  val methodBody = Apply( Ident( rateInfo.methodName ), Ident( " " ) :: Nil )  // XXX how to get ar() with the parentheses?
                  val methodBody = Apply( Ident( rateInfo.methodName ), Ident( " " ) :: Nil )  // XXX how to get ar() with the parentheses?
//                  val methodBody = Apply( Ident( rateInfo.methodName ), Nil )  // fixed in scala-refactoring ; NOT FIXED
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

            // hmmmm... is this the cleanest way to define R <: Rate?
//            if( impliedRate.isDefined ) Nil else (TypeDef( NoMods, "R", Nil, TypeBoundsTree( EmptyTree, TypeDef( NoMods, "Rate", Nil, EmptyTree ))) :: Nil), // tparams
            val caseClassTypeParam = if( impliedRate.isDefined ) Nil else (TypeDef( NoMods, "R <: Rate", Nil, EmptyTree ) :: Nil)

            val expArgs    = args.filter( _.isGE )
            val hasExpArgs = expArgs.size > 0
            val ugenName   = if( hasExpArgs ) name + "UGen" else name
            val classes0   = if( hasExpArgs ) {

            val caseClassExpandDef = {
//               val bufE    = buf.expand
//               val multiE  = multi.expand
//               val numExp  = math.max( bufE.size, multiE.size )
//               IIdxSeq.tabulate( numExp )( i => DiskOutUGen( bufE( i % numExp ), multiE( i % numExp ).expand ))
               val moreThanOneExp = expArgs.size > 1
               val methodBody = Block(
//                  (expArgs.map( a => ValDef( NoMods, "_" + a.arg.name, TypeTree( NoType ), Select( Ident( a.arg.name ), "expand" )))
                  // XXX dirty
                  (expArgs.map( a => ValDef( NoMods, "_" + a.arg.name + ": IIdxSeq[" + a.deriveGE + "]", TypeTree( NoType ), Select( Ident( a.arg.name ), "expand" ))) ++
                   (if( moreThanOneExp ) {
                      expArgs.map( a => ValDef( NoMods, "_sz_" + a.arg.name, TypeTree( NoType ), Select( Ident( "_" + a.arg.name ), "size" )))
                   } else {
                      Nil
                   }) ++ {
//                     if( !moreThanOneExp ) println( name + " " + expArgs + " / " + args )
                     val numId = if( moreThanOneExp ) Ident( "_exp_" ) else Select( Ident( "_" + expArgs.head.arg.name ), "size" )
                     val app0 = Apply( Apply( Select( identIIdxSeq, "tabulate" ), numId :: Nil ),
                        Function( ValDef( Modifiers( Flags.PARAM ), "i", TypeTree( NoType ), EmptyTree ) :: Nil,
                           Apply( Ident( ugenName ), {
                              val args0 = args.map( a => {
                                 if( a.isGE ) {
                                    val apply = Apply( Ident( "_" + a.arg.name ), {
                                       if( moreThanOneExp ) {
                                          Apply( Select( Ident( "i" ), "%" ), Ident( "_sz_" + a.arg.name ) :: Nil )
                                       } else {
                                          Ident( "i" )
                                       }
                                    }  :: Nil )
//                                    if( a.multi ) Select( apply, "expand" ) else apply
                                    apply
                                 } else Ident( a.arg.name )
                              })
                              if( impliedRate.isDefined ) args0 else (Ident( "rate" ) :: args0)
                           })
                        ) :: Nil
                     ) :: Nil
                     if( moreThanOneExp ) {
                        ValDef( NoMods, "_exp_", TypeTree( NoType ), Apply( Ident( "maxInt" ), expArgs.map( a => Ident( "_sz_" + a.arg.name )))) :: app0
                     } else app0
                  }): _*
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

            val caseClassDef = myMkCaseClass(
               NoMods,
               name,

               caseClassTypeParam, // tparams
               caseClassConstrArgs,
               caseClassExpandDef :: Nil,
               {
                  val t0 = TypeDef( NoMods, if( outputs == SingleOutput ) "GE" else "Expands", {
                     val p0 = TypeDef( NoMods, ugenName,
                        if( impliedRate.isDefined ) Nil else (TypeDef( NoMods, "R", Nil, EmptyTree ) :: Nil),
                        EmptyTree ) :: Nil
                     if( outputs == SingleOutput ) {
                        TypeDef( NoMods, impliedRate.map( _.typ ).getOrElse( "R" ): String, Nil, EmptyTree ) :: p0
                     } else {
                        p0
                     }
                  }, EmptyTree )
                  impliedRate.map( r => t0 :: TypeDef( NoMods, r.traitTyp, Nil, EmptyTree ) :: Nil ).getOrElse( t0 :: Nil )
               }
            )

//            caseClassDef.setSymbol( new tools.nsc.symtab.Symbols.Symbol( NoSymbol, NoPosition, new Name( 0, 0 )))

//            global.docComment( caseClassDef.symbol, "Jo chuck", NoPosition )
//            global.docComments += caseClassDef.symbol -> DocComment( "/** Kuuka */", NoPosition )

//            println( "JUHU " + caseClassConstr.symbol.isConstructor )
//            println( printer( caseClassConstr ))

//            case class SinOscUGen[ R <: Rate ]( freq: AnyUGenIn, phase: AnyUGenIn )
//            extends SingleOutUGen[ R ]( List( freq, phase ))
               objectDef :: caseClassDef :: Nil
            } else {
               objectDef :: Nil
            }

            val ugenCaseClassConstrArgs = {
               val args0 = args map { uArgInfo =>
                  (NoMods, uArgInfo.arg.name + ": " + uArgInfo.deriveGE, EmptyTree)
               }
               if( impliedRate.isDefined ) args0 else {
                  (NoMods, "rate: R", EmptyTree) :: args0
               }
            }

            val ugenCaseClassParents: List[ TypeDef ] = {
               val p1 = if( doneFlag )   (traitDoneFlag :: Nil) else Nil
               // note: ZeroOutUGen already extends HasSideEffect
               val p2 = if( sideEffect && (outputs != ZeroOutputs) ) (traitSideEffect :: p1) else p1
               val p3 = impliedRate.map( r => TypeDef( NoMods, r.traitTyp, Nil, EmptyTree ) :: p2 ).getOrElse( p2 )
               TypeDef( NoMods, outputs.typ, if( outputs != SingleOutput ) Nil else {
                  TypeDef( NoMods, impliedRate.map( _.typ ).getOrElse( "R" ): String, Nil, EmptyTree ) :: Nil
               }, EmptyTree ) :: p3
            }
//            val ugenCaseClassParents: List[ TypeDef ] = impliedRate.map( r =>
//               ugenCaseClassParents0 :: TypeDef( NoMods, r.traitTyp, Nil, EmptyTree ) :: Nil
//            ).getOrElse( ugenCaseClassParents0 :: Nil )

            val ugenCaseClassDef = myMkCaseClass(
               NoMods,
               ugenName,
               caseClassTypeParam, // tparams
               /* if( ugenCaseClassConstrArgs.nonEmpty ) */ ugenCaseClassConstrArgs /* else dummyCaseClassArgs */,
               Nil,
               ugenCaseClassParents,
               superArgs = {
                  val geArgs = args.filter( _.isGE )  // XXX TODO: order
                  val args0 = geArgs.lastOption match {
                     case Some( a ) if( a.multi ) =>
                        val rvsArgs = geArgs.dropRight( 1 ).reverse
                        rvsArgs.foldLeft[ Tree ]( Select( Ident( a.arg.name ), "expand" ))( (a, b) => Apply( Select( a, "+:" ), Ident( b.arg.name ) :: Nil )) :: Nil
                     case _ => (if( hasExpArgs ) {
                        Apply( identIIdxSeq, geArgs.map( a => Ident( a.arg.name )))
                     } else {
                        Select( identIIdxSeq, "empty" )
                     }) :: Nil
                  }
                  outputs match {
                     case m: MultiOutput =>
                        Apply( Apply( Select( identIIdxSeq, "fill" ), Ident( m.num ) :: Nil ),
                           Ident( impliedRate.map( _.typ ).getOrElse( "rate" )) :: Nil ) :: args0
                     case _ => args0
                  }
               }
            )

            classes0 ++ List( ugenCaseClassDef ) // how to prepend a blank line??

//            println( outputText )
         })( breakOut )

         // XXX add UGen class

         val packageDef = PackageDef( Select( Select( Ident( "de" ), "sciss" ), "synth" ),
            PackageDef( Ident( "ugen" ),
               Import( Select( Ident( "collection" ), "immutable" ), ImportSelector( "IndexedSeq", -1, identIIdxSeq.name, -1 ) :: Nil ) ::
               Import( Ident( "UGenHelper" ), ImportSelector( nme.WILDCARD, -1, nme.WILDCARD, -1 ) :: Nil ) ::
               ugens ) :: Nil )
//         println( createText( packageDef ))
//         println()
//         println( createText( ugens.head ))
         println( "Writing " + fileName )
         val osw = new OutputStreamWriter( new FileOutputStream( new File( dir, fileName )), "UTF-8" )
         osw.write( """/*
 * """ + fileName + """
 * (ScalaCollider-UGens)
 *
 * This is a synthetically generated file.
 * Created: """ + (new java.util.Date()).toString + """
 * ScalaCollider-UGen version: """ + UGens.versionString + """
 */

""" )
         osw.write( createText( packageDef ))
         osw.close()
      }
      println( "Done.")
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
   private case class TypeInfo( tuples: List[ (String, List[ TypeInfo ])], exist: Option[ (String, String) ] = None ) {
      override def toString = {
         val s0 = tuples.map( t => t._1 + (t._2 match {
            case Nil  => ""
            case coll => coll.mkString( "[", ", ", "]" )
         })).mkString( " with ")
         exist.map( tup => s0 + " forSome { type " + tup._1 + " " + tup._2 + " }" ).getOrElse( s0 )
      }
   }
   private case class ArgInfo( name: String, typ: TypeInfo, default: Option[ String ], doc: Option[ String ])
   private case class UGenArgInfo( arg: ArgInfo, multi: Boolean, idx: Int ) {
      def isGE = arg.typ.tuples.headOption match {
         case Some( ("GE", _) )        => true
         case Some( ("AnyGE", _) )     => true
         case Some( ("Expands", List( TypeInfo( List( (sub, _), _* ), _ ), _* ))) => sub match {
            case "GE"      => true
            case "AnyGE"   => true
            case _         => false
         }
//         case Some( ("MultiGE", _) )   => true
         case _                        => false
      }

      def deriveGE = arg.typ.tuples.headOption match {
         case Some( ("GE", List( r, sub, _* )))    => arg.typ.exist.map( tup => {
//            println( "Jo. derive with existential '" + sub + "'" )
            val res = sub match {
//               case TypeInfo( List( ("UGenIn", List( TypeInfo( List( (tup._1, Nil) ), None )))), None ) => "AnyUGenIn"
               case TypeInfo( List( ("UGenIn", List( TypeInfo( List( (tup._1, Nil) ), None ))), rest @ _* ), None ) =>
                  TypeInfo( ("AnyUGenIn" -> Nil) :: rest.toList, None )
               case _ => Predef.error( "Cannot derive type " + arg.typ.toString )
            }
//            println( "res = " + res )
            res.toString
         }).getOrElse( sub.toString )
         case Some( ("Expands", List( sub, _* )))  => sub.toString
         case Some( ("AnyGE", _) )                 => "AnyUGenIn"
         case _                                    => arg.typ.toString
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