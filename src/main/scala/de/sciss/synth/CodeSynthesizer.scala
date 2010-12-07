package de.sciss.synth

import scala.tools.nsc.symtab.Flags
import tools.refactoring.Refactoring
import tools.refactoring.common.{Tracing, Change}
import tools.refactoring.util.CompilerProvider
import xml.Node
import java.io.File
import collection.breakOut

class CodeSynthesizer extends Refactoring with Tracing with CompilerProvider {

   override val defaultIndentationStep = "   "

   import global._
   
   def perform( xml: Node, dir: File ) {

//      val testAst = treeFrom( "case class Schnucki( hallo: Int )" )
//      ↓(matchingChildren(transform {
//         case t: Template => t.body.foreach {
//            case d: DefDef => println( "Jo, defdef( name = " + d.name+ ", name " + d.name + " ) = " + d )
//            case _ =>
//         }
//         t
//         case x => x
//      })) apply testAst

      (xml \ "file") foreach { node =>
         val name       = (node \ "@name").text
         val fileName   = name + ".scala"
         val ast        = treeFrom( "package de.sciss.synth.ugen\n" )
         val ugens: List[ Tree ] = (node \ "ugen").flatMap( node => {
            val name          = (node \ "@name").text
            val sideEffect    = getBoolAttr( node, "sideeffect" )
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
               val typStr     = (n \ "@type").headOption.map( _.text ).getOrElse( "GE" )
               val default    = (n \ "@default").headOption.map( _.text )
               val multi      = getBoolAttr( n, "multi" )
               val doc        = (n \ "doc").headOption.map( _.text )
               UGenArgInfo( ArgInfo( name, typStr, default, doc ), multi, idx )
//               val typ        = 
//               val vParam     = ValDef( Modifiers( Flags.PARAM ), name, TypeTree( typ /* selectedValue.tpt.tpe */ ), EmptyTree ) :: Nil
//               vParam
            })( breakOut )
//            val outputs       = (node \ "outputs").headOption match {
//               case Some( n ) => (n \ "@num").text match {
//                  case "0" => ZeroOutputs
//                  case t   => MultiOutput( t )
//               }
//               case None      => SingleOutput
//            }

//            val trnsAst    = ↓( matchingChildren( trns )) apply ast
//            val changes    = refactor( trnsAst.toList )
//            val outputText = Change.applyChanges( changes, inputText )

            val objectMethodArgs = args map { uArgInfo =>
               ValDef(
                  Modifiers( Flags.PARAM ),
                  uArgInfo.arg.name,
                  Ident( uArgInfo.arg.typ ),
                  uArgInfo.arg.default.map( s => Literal( Constant( uArgInfo.arg.typ match {
                     case "GE" => s.toFloat 
                  }))).getOrElse( EmptyTree )
               )
            }

//            val methodBody = Block( Select( Ident( "freq" ), "toString" ) :: Nil, EmptyTree )
//            val methodBody = Select( Ident( "freq" ), "toString" ))
            val objectMethodDefs = rates map { rateInfo =>
               val methodBody = Apply( Ident( "apply" ), Ident( rateInfo.name ) :: args.map( i => Ident( i.arg.name )))
               DefDef(
                  NoMods withPosition (Flags.METHOD, NoPosition),
                  rateInfo.methodName,
                  Nil,        // tparams
                  objectMethodArgs :: Nil,    // vparamss
                  TypeTree( methodBody.tpe ), // tpt -- empty for testing
                  methodBody // rhs
               )
            }

            val objectDef = ModuleDef(
               NoMods,
               name,
               Template(
                  EmptyTree :: Nil, // parents
                  emptyValDef,      // self
                  objectMethodDefs  // body
               )
            )

            val caseClassConstrArgs = args map { uArgInfo =>
               ValDef(
                  Modifiers( Flags.PARAM ),
                  uArgInfo.arg.name,
                  Ident( uArgInfo.arg.typ ),
                  EmptyTree
               )
            }

            val caseClassConstr = DefDef(
               NoMods, // withPosition (Flags.PARAMACCESSOR | Flags.CASEACCESSOR, NoPosition),
               nme.CONSTRUCTOR,
               Nil,
               caseClassConstrArgs :: Nil,
               TypeTree(),   // cheeze... how to do this?
               {
                  val superPos = NoPosition // XXX
                  val superRef: Tree = atPos(superPos) {
                    Select(Super(nme.EMPTY.toTypeName, nme.EMPTY.toTypeName), nme.CONSTRUCTOR)
                  }
                  val argss: List[ List[ Tree ]] = Nil // XXX
                  val superCall = (superRef /: argss) (Apply)
                  Block( /*lvdefs ::: */ List(superCall), Literal(()))
               }
            )

            val caseClassDefX = ClassDef(
               NoMods withPosition (Flags.CASE, NoPosition), // Modifiers( Flags.CASEACCESSOR ),
               name,
               Nil,
               Template(
                  EmptyTree :: Nil,       // parents
                  emptyValDef,            // self
                  caseClassConstr :: Nil  // body
               )
            )

            val caseClassDef = ClassDef(
               NoMods withPosition (Flags.CASE, NoPosition), // Modifiers( Flags.CASEACCESSOR ),
               name,
               Nil,
               Template(
                  EmptyTree :: Nil,
                  emptyValDef,
                  Modifiers( Flags.CASE ),
                  caseClassConstrArgs :: Nil,
                  Nil, // argss
                  Nil, // body: List[Tree]
                  NoPosition   // superPos
               )
            )

            println( "JUHU " + caseClassConstr.symbol.isConstructor )

            /* Ident( "\n" ) :: */ objectDef :: caseClassDef :: Nil  // how to prepend a blank line??

//            println( outputText )
         })( breakOut )

         val packageDef = PackageDef( Select( Select( Select( Ident( "de" ), "sciss" ), "synth" ), "ugen" ),
            ugens )
         println( createText( packageDef ))
//         println( createText( ugens.head ))
      }
   }

   private def getBoolAttr( n: Node, name: String, default: Boolean = false ) =
      (n \ ("@" + name)).headOption.map( _.text.toBoolean ).getOrElse( default )

   private case class RateInfo( name: String, methodName: String, implied: Boolean )
   private case class ArgInfo( name: String, typ: String, default: Option[ String ], doc: Option[ String ])
   private case class UGenArgInfo( arg: ArgInfo, multi: Boolean, idx: Int )
}