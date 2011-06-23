///*
// *  CodeSynthesizer.scala
// *  (ScalaCollider-UGens)
// *
// *  Copyright (c) 2008-2011 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is free software; you can redistribute it and/or
// *  modify it under the terms of the GNU General Public License
// *  as published by the Free Software Foundation; either
// *  version 2, june 1991 of the License, or (at your option) any later version.
// *
// *  This software is distributed in the hope that it will be useful,
// *  but WITHOUT ANY WARRANTY; without even the implied warranty of
// *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// *  General Public License for more details.
// *
// *  You should have received a copy of the GNU General Public
// *  License (gpl.txt) along with this software; if not, write to the Free Software
// *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// *
// *
// *  Changelog:
// */
//
//package de.sciss.synth
//
//import scala.tools.nsc.symtab.Flags
//import tools.refactoring.Refactoring
//import tools.refactoring.util.CompilerProvider
//import xml.Node
//import collection.breakOut
//import collection.immutable.{ IndexedSeq => IIdxSeq }
//import net.virtualvoid.string.MyNodePrinter
//import tools.refactoring.transformation.TreeFactory
//import tools.refactoring.common.{CompilerAccess, Tracing, Change}
//import tools.nsc.io.AbstractFile
//import java.io.{FileOutputStream, OutputStreamWriter, FileWriter, File}
//
//object CodeSynthesizer {
//   val nameExpands   = "Multi"
//}
//
//class CodeSynthesizer extends Refactoring
//with Tracing with CompilerProvider with MyNodePrinter with CompilerAccess with TreeFactory {
//   val rateTypes  = false
//   val docs       = true
//
//   override val defaultIndentationStep = "   "
//
//   import CodeSynthesizer._
//   import global._
//
//   def compilationUnitOfFile( f: AbstractFile ) = global.unitOfFile.get( f )
//
////   def myMkClass(
////      mods: Modifiers = NoMods,
////      name: String,
////      tparams: List[TypeDef] = Nil,
////      args: List[(Modifiers, String, Tree)],
////      body: List[Tree] = Nil,
////      parents: List[Tree] = Nil,
////      superArgs: List[Tree] = Nil) = {
////
////      val constructorArguments0 = args.map {
////         case (mods, name, tpe) =>
////            ValDef(mods | Flags.PARAMACCESSOR, name, tpe, EmptyTree)
////      }
////      val constructorArguments = if( args.nonEmpty ) constructorArguments0 else {
////         ValDef(NoMods | Flags.PARAMACCESSOR, "\t", EmptyTree, EmptyTree) :: constructorArguments0
////      }
////
////      val superArgsCall = superArgs match {
////         case Nil => EmptyTree
////         case args =>
////            mkDefDef(name = nme.CONSTRUCTOR.toString, body = Apply(EmptyTree, args) :: Nil)
////      }
////
////      ClassDef(
////         mods,
////         mkTypeName(name),
////         tparams,
////         Template(
////            parents,
////            emptyValDef,
////            superArgsCall :: constructorArguments ::: body
////         )
////      )
////   }
//
////   def myMkCaseClass(
////      mods: Modifiers = NoMods,
////      name: String,
////      tparams: List[TypeDef] = Nil,
////      args: List[(Modifiers, String, Tree)],
////      body: List[Tree] = Nil,
////      parents: List[Tree] = Nil,
////      superArgs: List[Tree] = Nil) = {
////
//////    if (args.size < 1) throw new IllegalArgumentException("Case-class must have at least one argument.")
////
////      mkClass(mods withPosition (Flags.CASE, NoPosition), name, tparams, args, body, parents, superArgs)
////   }
//
//   private def trimDoc( docText: String ) : Seq[ String ] = {
//      val trim0   = docText.lines.map( _.trim ).toIndexedSeq
//      val trim1   = trim0.dropWhile( _.isEmpty )
//      val idx     = trim1.lastIndexWhere( _.isEmpty ) // why the fuck there's no dropRightWhile?
//      if( idx >= 0 ) trim1.dropRight( trim1.size - idx ) else trim1
//   }
//
//   private def ensureEmptyTrail( text: Seq[ String ]) : Seq[ String ] = if( text.nonEmpty ) text :+ "" else text
//
//   private def wrapDoc( tree: Tree, indent: Int, docText: String = "", sees: Seq[ String ] = Nil, docWarnPos: Boolean = false,
//                         methodDocs: Seq[ (String, String) ] = Nil ) : Tree = {
//      val hasAny = docs && (docText.nonEmpty || sees.nonEmpty || docWarnPos || methodDocs.nonEmpty )
//      if( hasAny ) {
//// XXX indentation funzt im moment eh nicht richtig bei scala-refactoring. besser ganz weglassen
////         val ind     = Seq.fill( indent )( "   " ).mkString
//         val txt0    = trimDoc( docText )
//         val txt1    = if( methodDocs.isEmpty ) txt0 else ensureEmptyTrail( txt0 ) ++ methodDocs.flatMap( tup => {
//            trimDoc( tup._2 ).zipWithIndex.map( tup2 => {
//               val (ln, idx) = tup2
//               (if( idx == 0 ) ("@param " + tup._1 + "              ").take( 21 ) + "  " else "                       ") + ln
//            })
//         })
//         val txt2    = if( sees.isEmpty ) txt1 else ensureEmptyTrail( txt1 ) ++ sees.map( "@see [[de.sciss.synth." + _ + "]]" )
//         // note: @warn is not recognized by scaladoc. we use @note instead
//         // fucking scaladoc just ignores @note
////         val txt3    = if( !docWarnPos ) txt2 else ensureEmptyTrail( txt2 ) :+ "@note  The argument order is different from its sclang counterpart."
//         val txt3    = if( !docWarnPos ) txt2 else ensureEmptyTrail( txt2 ) :+ "'''Warning''': The argument order is different from its sclang counterpart."
//
//         DocDef( DocComment(
////            txt3.mkString( "\n" + ind + "/**\n" + ind + " * ", "\n" + ind + " * ", "\n" + ind + " */\n" ), NoPosition ),
//            txt3.mkString( "\n/**\n * ", "\n * ", "\n */\n" ), NoPosition ),
//            tree )
//      } else tree
//   }
//
//   private def collectMethodDocs( args: Seq[ UGenArgInfoLike ], rate: Option[ RateInfo ] = None ) : Seq[ (String, String) ] =
//      args.map( ua => ua.name -> rate.map( ua.arg( _ )).getOrElse( ua.argDefault ).doc ).collect { case (name, Some( doc )) => name -> doc }
//
//   // filter gets applied with filename (without extension) and ugen name
//   def perform( xml: Node, dir: File, filter: (String, String) => Boolean = (_, _) => true ) {
//
////      val testAst = treeFrom( "class A(); class B" )
////      println( printer( testAst ))
////      println( createText( testAst ))
////
////      ↓(matchingChildren(transform {
////         case t: Template => t.body.foreach {
////            case d: DefDef =>
//////               if( d.name == "eins" || d.name == "zwei" ) {
////                  println( "For " + d.name + " --> " + d.vparamss )
//////               }
//////               println( "Jo, defdef( name = " + d.name+ ", name " + d.hasSymbol + " ) = " + d )
//////               println( printer( d ))
////            case _ =>
////         }
////         t
////         case x => x
////      })) apply testAst
//
////      val traitGE = ClassDef(
////         NoMods withPosition (Flags.TRAIT, NoPosition), // Modifiers( Flags.CASEACCESSOR ),
////         "GE",
////         Nil,
////         Template(
////            EmptyTree :: Nil,
////            emptyValDef,
////            Nil
////         )
////      )
//
////      val traitGE = TypeDef( Modifiers( Flags.TRAIT ), "GE", Nil, EmptyTree )
//      val traitSideEffect  = TypeDef( Modifiers( Flags.TRAIT ), "HasSideEffect", Nil, EmptyTree )
//      val traitDoneFlag    = TypeDef( Modifiers( Flags.TRAIT ), "HasDoneFlag",   Nil, EmptyTree )
//      val traitRandom      = TypeDef( Modifiers( Flags.TRAIT ), "UsesRandSeed",  Nil, EmptyTree )
//      val traitIndiv       = TypeDef( Modifiers( Flags.TRAIT ), "IsIndividual",  Nil, EmptyTree )
////      val traitReadsFFT    = TypeDef( Modifiers( Flags.TRAIT ), "ReadsFFT",      Nil, EmptyTree )
//      val traitWritesBuffer= TypeDef( Modifiers( Flags.TRAIT ), "WritesBuffer",  Nil, EmptyTree )
//      val traitWritesFFT   = TypeDef( Modifiers( Flags.TRAIT ), "WritesFFT",     Nil, EmptyTree )
//      val traitWritesBus   = TypeDef( Modifiers( Flags.TRAIT ), "WritesBus",     Nil, EmptyTree )
//      val identIIdxSeq     = Ident( "IIdxSeq" )
//      val identBinOpUGen   = Ident( "BinaryOpUGen" )
//
//      // XXX the extra closing ] bracket is due to a bug in scala-refactoring (05-jan-10)
//      val typExpandBinDf   = TypeDef( NoMods, "S <: Rate", Nil, EmptyTree ) :: TypeDef( NoMods, "T <: Rate", Nil, EmptyTree ) :: Nil
////      val typExpandBinDfBUG= TypeDef( NoMods, "S <: Rate", Nil, EmptyTree ) :: TypeDef( NoMods, "T <: Rate]", Nil, EmptyTree ) :: Nil
//      val typExpandBin     = TypeDef( NoMods, "S", Nil, EmptyTree ) :: TypeDef( NoMods, "T", Nil, EmptyTree ) :: Nil
////      val argIndiv         = SyntheticUGenArgInfo( "_indiv", ArgInfo( TypeInfo( ("Int", Nil) :: Nil ), None, None ))
//      val identApply        = Ident( "apply" )
//      val identRateOrder    = Ident( "rateOrder" )
//      val identRateOrderClass = Ident( "RateOrder" )
//      val identRate        = Ident( "rate" )
//      val dateString       = new java.util.Date().toString
//
////         def *[ S <: Rate, T <: Rate ]( b: GE[ S, UGenIn[ S ]])( implicit r: RateOrder[ R, S, T ]) =
////            Times.make[ R, S, T ]( r.rate, this, b )
//
//      // bug in scala-refactoring : case class constructor arg list cannot be empty
////      val dummyCaseClassArgs = (NoMods, "", EmptyTree) :: Nil
//
//      (xml \ "file") foreach { node =>
//         val name       = (node \ "@name").text
//         val fileName   = name + ".scala"
//         val ast        = treeFrom( "package de.sciss.synth.ugen\n" )
//         val fltNode    = (node \ "ugen").filter( node => filter( name, (node \ "@name").text ))
//         var importFloat = false
//         val ugens: List[ Tree ] = fltNode.flatMap( node => {
//            val name          = (node \ "@name").text
//            val readsBus      = getBoolAttr( node, "readsbus" )
//            val writesBus     = getBoolAttr( node, "writesbus" )
////if( name == "Out" ) println( " OUT : " + writesBus )
//            val readsBuffer   = getBoolAttr( node, "readsbuf" )   // currently unused
//            val readsFFT      = getBoolAttr( node, "readsfft" )   // currently unused
//            val writesBuffer  = getBoolAttr( node, "writesbuf" )
//            val writesFFT     = getBoolAttr( node, "writesfft" )
//            val sideEffect    = getBoolAttr( node, "sideeffect" ) // || writesBus || writesBuffer
//            val indSideEffect = writesBuffer || writesFFT || writesBus
//            val doneFlag      = getBoolAttr( node, "doneflag" )
//            val random        = getBoolAttr( node, "random" )
//            val indiv         = getBoolAttr( node, "indiv" ) // || random
//            val indIndiv      = indSideEffect || random
//            val rates0: List[ RateInfo ] = (node \ "rate").map( n => {
//               val name       = (n \ "@name").text
//               val mName0     = (n \ "@method").text
//               val mName1     = (n \ "@methodalias").text
//               val mName2     = if( mName0 != "" ) mName0 else name match {
//                  case "audio"   => "ar"
//                  case "control" => "kr"
//                  case "scalar"  => "ir"
//                  case "demand"  => "dr"
//               }
//               val methodNames   = if( mName1 != "" ) {
//                  mName1 :: mName2 :: Nil
//               } else {
//                  mName2 :: Nil
//               }
//               val implied    = getBoolAttr( n, "implied" )
//               RateInfo( name, methodNames, implied, n )
//            })( breakOut )
//
////            if( name == "Out" ) println( "for Out, rates = " + rates0 )
//
//            val impliedRate   = rates0.find( _.implied )
//            val (rates: List[ RateInfo ], caseDefaults: Boolean) = impliedRate.map( irate => {
//               require( rates0.size == 1, "Can only have one implied rate (" + name + ")" )
//               val rateInfo   = rates0.head
//               val hasApply   = rateInfo.methodNames.contains( "apply" )
//               ((if( hasApply ) {
//                  rateInfo.copy( methodNames = rateInfo.methodNames.filterNot( _ == "apply" )) :: Nil
//               } else rates0), hasApply)
//            }).getOrElse( (rates0, false) )
//
//            val docNode       = (node \ "doc")
//            val docSees       = (docNode \ "see").map( _.text )
//            val docWarnPos    = docNode.headOption.map( getBoolAttr( _, "warnpos" )).getOrElse( false )
//            val docText       = (docNode \ "text").text
//
//            val argsTup : List[ (UGenArgInfo, Int) ] = (node \ "arg").zipWithIndex.map( tup => {
//               val (n, idx0)  = tup
//               val idx        = getIntAttr( n, "pos", idx0 )
//               val aname      = (n \ "@name").text
//               val multi      = getBoolAttr( n, "multi" )
//               val doneFlagArg= getBoolAttr( n, "doneflag" )
//               val expandBin  = (n \ "@expandbin").text match {
//                  case ""  => None
//                  case "*" => Some( "Times" )
//                  case x   => Predef.error( "Illegal expandbin value '" + x + "' (" + name + ")" )
//               }
////               val rateAttr   = (n \ "@rate").text
//               if( multi ) require( !doneFlagArg, "Multi arguments cannot have done flag (" + name + ")" )
//
//               def createInfo( rateNode: Option[ Node ], rateInfo: Option[ RateInfo ]) : ArgInfo = {
//                  def getEitherNode( name: String ) : Option[ Node ] = {
//                     (n \ name).headOption.orElse( rateNode.flatMap( n => (n \ name).headOption ))
//                  }
//                  def getEitherAttr( name: String ) : Option[ Node ] = getEitherNode( "@" + name )
//                  val typInfo    = getEitherAttr( "type" ).map( n => TypeInfo( (n.text -> Nil) :: Nil ))
//   //                  .getOrElse( TypeInfo( (if( multi ) "MultiGE" else "GE") -> (TypeInfo( ("AnyUGenIn" -> Nil) ) :: Nil) ))
//                     .getOrElse({
//                        val rateAttr = getEitherAttr( "rate" ).map( _.text ) // (n \ "@rate").text
//                        def r0 = if( doneFlagArg || expandBin.isDefined ) {
//                           val typPar = if( expandBin.isDefined ) "S" else "R"
//                           val par0 = "UGenIn" -> (if( rateTypes ) {
//                              TypeInfo( (typPar -> Nil) :: Nil ) :: Nil
//                           } else Nil)
//                           val par1 = TypeInfo( if( doneFlagArg ) par0 :: ("HasDoneFlag" -> Nil) :: Nil else par0 :: Nil ) :: Nil
//                           val par2 = if( rateTypes ) TypeInfo( (typPar -> Nil) :: Nil ) :: par1 else par1
//                           TypeInfo( ("GE" -> par2) :: Nil, if( expandBin.isEmpty && rateTypes ) Some( "R" -> "<: Rate" ) else None )
//                        } else {
//                           TypeInfo( ("AnyGE" -> Nil) :: Nil )
//                        }
//                        def r1( rateName: String ) = {
//                           val t0 = "UGenIn" -> (if( rateTypes ) (TypeInfo( (rateName -> Nil) :: Nil ) :: Nil) else Nil)
//                           val t1: List[ (String, List[ TypeInfo ])] = if( doneFlagArg ) {
//                              List( t0, "HasDoneFlag" -> Nil )
//                           } else {
//                              t0 :: Nil
//                           }
//                           val t2 = TypeInfo( t1 ) :: Nil
//                           val t3 = if( rateTypes ) TypeInfo( (rateName -> Nil) :: Nil ) :: t2 else t2
//                           TypeInfo( ("GE" -> t3) :: Nil )
//                        }
//                        val typ0 = (rateAttr, rateInfo) match {
//                           case (None, _)                            => r0
//                           case (Some( "ugen" ), None)               => r0
//                           case (_, _) if( expandBin.isDefined )     => r0
//                           case (Some( "ugen" ), Some( info ))       => r1( info.name )
//                           case (Some( rateName ), _)                => r1( rateName )
//                        }
//                        if( multi ) {
//                           TypeInfo( (nameExpands -> (typ0 :: Nil)) :: Nil )
//                        } else {
//                           typ0
//                        }
//                     })
//                  val default    = getEitherAttr( "default" ).map( _.text )
//                  if( default == Some( "inf" ) || default == Some( "-inf" )) {
//                     importFloat = true
////                     println( "importFloat is true" )
////                  } else if( name == "Dseries" ) {
////                     println( "NOT INF?? '" + default + "'" )
//                  }
//                  val doc        = getEitherNode( "doc" ).map( _.text )
////if( doc.isDefined ) println( "jo, doc: " + doc )
//                  ArgInfo( typInfo, default, doc )
//               }
//
//               val argDefault = createInfo( None, None )
//               val argMap: Map[ RateInfo, ArgInfo ] = rates.map( rateInfo => {
//                  val rateNode   = (rateInfo.xml \ "arg").find( n => (n \ "@name").text == aname )
////                  if( name == "Out" ) println( "for Out (r = " + rateInfo.name + "), rateNode = " + rateNode )
//                  rateInfo -> createInfo( rateNode, Some( rateInfo ))
//               })( breakOut )
//               UGenArgInfo( aname, argDefault, argMap, multi, expandBin ) -> idx
////               val typ        =
////               val vParam     = ValDef( Modifiers( Flags.PARAM ), name, TypeTree( typ /* selectedValue.tpt.tpe */ ), EmptyTree ) :: Nil
////               vParam
//            })( breakOut )
//
////            val argsPoss   = args0.map( _._2 )
//            val numArgsIn  = argsTup.size
////            require( numArgsIn == argsPoss.toSet.size, "Wrong argument positions (" + name + ")" )
//            val argsOut    = argsTup.map( _._1 ).filter( _.isGE )
//            val argsIn     = List.tabulate( numArgsIn )(
//               idx => argsTup.find( _._2 == idx ).getOrElse( Predef.error( "Wrong argument positions (" + name + ")" ))._1 )
////            val argsInS    = if( indiv ) {
////               argsIn ::: (argIndiv :: Nil)
////            } else argsIn
//            val argsInS = argsIn  // no exceptions at the moment
//
//            val expandBin = argsIn.map( _.expandBin ).collect { case Some( exp ) => exp } match {
//               case exp :: Nil => Some( exp )
//               case Nil => None
//               case _ => Predef.error( "Can only have one expandBin (" + name + ")" )
//            }
//
//            val outputs       = (node \ "outputs").headOption match {
//               case Some( n ) => (n \ "@num").text match {
//                  case "0" => ZeroOutputs
//                  case t   => argsIn.find( _.name == t ).map( a => ArgMultiOutput( a )).getOrElse( FixedMultiOutput( t.toInt ))
//               }
//               case None      => SingleOutput
//            }
//
////            val trnsAst    = ↓( matchingChildren( trns )) apply ast
////            val changes    = refactor( trnsAst.toList )
////            val outputText = Change.applyChanges( changes, inputText )
//
////            val methodBody = Block( Select( Ident( "freq" ), "toString" ) :: Nil, EmptyTree )
////            val methodBody = Select( Ident( "freq" ), "toString" ))
//            val objectMethodDefs = rates.flatMap( rateInfo => {
//               val objectMethodArgs0 : List[ ValDef ] = argsIn.map( uArgInfo => {
//                  val argInfo = uArgInfo.arg( rateInfo )
//                  ValDef(
//                     Modifiers( Flags.PARAM ),
//                     uArgInfo.name,
//                     Ident( argInfo.typ.toString ),
//                     uArgInfo.defaultTree( argInfo )
//                  )
//               })
//               val objectMethodArgs : List[ List[ ValDef ]] = if( expandBin.isDefined && rateTypes ) {
//                  val curry : List[ List[ ValDef ]] = List( ValDef(
//// the next line would produce implicit val... for reasons beyond my imagination
////                     NoMods withPosition (Flags.PARAM, NoPosition) withPosition (Flags.IMPLICIT, NoPosition),
//                     Modifiers( Flags.PARAM ) withPosition (Flags.IMPLICIT, NoPosition),
//                     identRateOrder.toString, // XXX dirty
//// stupid bug... this would result in a closing bracket missing in the method's type parameter...
//                     TypeDef( NoMods, "RateOrder", TypeDef( NoMods, rateInfo.typ, Nil, EmptyTree ) :: typExpandBin, EmptyTree ),
////                     Ident( "RateOrder<" + rateInfo.typ + ",S,T>" ), // XXX dirty
//                     EmptyTree
//                  )) :: Nil
//                  if( objectMethodArgs0.nonEmpty ) objectMethodArgs0 :: curry else curry
//               } else {
//                  if( objectMethodArgs0.nonEmpty ) objectMethodArgs0 :: Nil else Nil
//               }
//
//               val args0 = argsInS.map( i => Ident( i.name ))
//               val methodBody = {
//                  val typApply0  = if( expandBin.isDefined && rateTypes ) typExpandBin else Nil
//                  val typApply1  = if( impliedRate.isEmpty && rateTypes ) TypeDef( NoMods, rateInfo.typ, Nil, EmptyTree ) :: typApply0 else typApply0
//                  val args1      = if( expandBin.isDefined && rateTypes ) {
//                     Select( identRateOrder, "out" ) :: args0
//                  } else if( impliedRate.isEmpty ) {
//                     Ident( rateInfo.name ) :: args0
//                  } else {
//                     args0
//                  }
//// BUG: TypeApply trennt typen nicht durch kommas
////                  val apply0 = Apply( if( typApply1.nonEmpty ) TypeApply( identApply, typApply1 ) else identApply, args1 )
//                  val apply0 = if( typApply1.nonEmpty ) {
////                     Apply( TypeDef( NoMods, "apply", typApply1, EmptyTree ), args1 )  // XXX dirty
//                     Apply( TypeApply( Ident( "apply" ), typApply1 ), args1 )  // XXX dirty
//                  } else Apply( identApply, args1 )
//                  if( expandBin.isDefined && rateTypes ) Apply( apply0, identRateOrder :: Nil ) else apply0
//               }
//               val mdocs = collectMethodDocs( argsIn, Some( rateInfo ))
////if( mdocs.nonEmpty ) println( "GOT SOME MDOCS : " + mdocs )
//               val def0 = rateInfo.methodNames.map { mName =>
//                  val df = DefDef(
//                     NoMods withPosition (Flags.METHOD, NoPosition),
//                     mName,
//                     if( expandBin.isDefined && rateTypes ) typExpandBinDf else Nil,        // tparams
//                     objectMethodArgs,    // vparamss
//                     if( rateTypes || mName != "apply" ) EmptyTree else TypeDef( NoMods, name, Nil, EmptyTree ), // TypeTree( NoType ), // tpt -- empty for testing
//                     methodBody // rhs
//                  )
//                  wrapDoc( df, 1, methodDocs = mdocs )
//               }
//               val allDefaults = argsIn.nonEmpty && argsIn.forall( _.arg( rateInfo ).default.isDefined )
//               if( allDefaults && rateInfo.methodNames.nonEmpty ) {
////                  require( rateInfo.methodNames.nonEmpty, "No method names for rate " + rateInfo.name + " (" + name + ")" )
//                  val mName = rateInfo.methodNames.head
////                  val methodBody = Apply( Ident( mName ), Ident( " " ) :: Nil )  // XXX how to get ar() with the parentheses?
//                  val methodBody = Apply( Ident( mName ), Nil )  // XXX how to get ar() with the parentheses?
//                  DefDef(
//                     NoMods withPosition (Flags.METHOD, NoPosition),
//                     mName,
//                     Nil,        // tparams
//                     Nil,        // vparams
//                     TypeDef( NoMods, name, {
//                        val typ0 = TypeDef( NoMods, rateInfo.typ, Nil, EmptyTree )
//                        val typ1 = if( expandBin.isDefined && rateTypes ) {
//                           TypeDef( NoMods, "scalar", Nil, EmptyTree ) :: typ0 :: Nil
//                        } else Nil
//                        if( impliedRate.isEmpty && rateTypes ) (typ0 :: typ1) else typ1
//                     }, EmptyTree ),
//                     methodBody // rhs
//                  ) :: def0
//               } else def0
//            })
//
//            val objectDef = if( objectMethodDefs.nonEmpty ) {
//               val mod = ModuleDef(
//                  NoMods,
//                  name,
//                  Template(
//                     EmptyTree :: Nil, // parents
//                     emptyValDef,      // self
//                     objectMethodDefs  // body
//                  )
//               )
//               wrapDoc( mod, 0, docText, docSees, docWarnPos ) :: Nil
//            } else Nil
//
//            val caseClassConstrArgs  = {
////               val args0 = argsInS map { uArgInfo => (NoMods, uArgInfo.name + ": " + uArgInfo.argDefault.typ,
////                  if( caseDefaults ) uArgInfo.defaultTree() else EmptyTree) }
//               val args0 = argsInS map { uArgInfo => (NoMods, uArgInfo.name, {
//                  val typ0 = Ident( uArgInfo.argDefault.typ.toString )
//                  if( caseDefaults ) uArgInfo.defaultTree() match {
//                     case EmptyTree => typ0
//                     case t => Assign( typ0, t )
//                  } else typ0
//               }) }
//               if( impliedRate.isDefined ) args0 else {
//                  (NoMods, "rate: " + (if( rateTypes ) (if( expandBin.isDefined ) "T" else "R") else "Rate"), EmptyTree) :: args0
//               }
//            }
//
//            // hmmmm... is this the cleanest way to define R <: Rate?
////            if( impliedRate.isDefined ) Nil else (TypeDef( NoMods, "R", Nil, TypeBoundsTree( EmptyTree, TypeDef( NoMods, "Rate", Nil, EmptyTree ))) :: Nil), // tparams
//            val ugenCaseClassTypeParam = if( impliedRate.isEmpty && rateTypes ) (TypeDef( NoMods, "R <: Rate", Nil, EmptyTree ) :: Nil) else Nil
//            val caseClassTypeParam = if( expandBin.isDefined && rateTypes ) ugenCaseClassTypeParam ::: typExpandBinDf else ugenCaseClassTypeParam
//
////            val expArgs    = argsOut.filter( _.isGE )
////            val hasArgsOut = argsOut.size > 0
////          val ugenName   = if( hasArgsOut ) name + "UGen" else name
//            val ugenName   = name + "UGen"
//
//            val caseCommonParents : List[ TypeDef ] = {
//               val p1 = if( doneFlag )             (traitDoneFlag :: Nil)     else Nil
//               val p2 = if( random )               (traitRandom :: p1)        else p1
//               val p3 = if( writesBus )            (traitWritesBus :: p2)     else p2
//               val p4 = if( writesBuffer )         (traitWritesBuffer :: p3)  else p3
//               val p5 = if( writesFFT )            (traitWritesFFT :: p4)     else p4
//               val p6 = if( indiv && !indIndiv )   (traitIndiv :: p5)         else p5
////               val p6 = if( sideEffect && !indSideEffect ) ...
//               impliedRate.map( r => TypeDef( NoMods, r.traitTyp, Nil, EmptyTree ) :: p6 ).getOrElse( p6 )
//            }
//
//            val caseClassExpandDef = {
////               val bufE    = buf.expand
////               val multiE  = multi.expand
////               val numExp  = math.max( bufE.size, multiE.size )
////               IIdxSeq.tabulate( numExp )( i => DiskOutUGen( bufE( i % numExp ), multiE( i % numExp ).expand ))
//               val moreThanZeroArgOut = argsOut.size > 0
//               val moreThanOneArgOut  = argsOut.size > 1
//               val methodBody : Tree = {
//                  val stat =
////                  (expArgs.map( a => ValDef( NoMods, "_" + a.arg.name, TypeTree( NoType ), Select( Ident( a.arg.name ), "expand" )))
//                  // XXX dirty
//                  (argsOut.map( a => ValDef( NoMods, "_" + a.name + ": IIdxSeq[" + a.deriveGE( false ) + "]", TypeTree( NoType ),
//                     Select( Ident( a.name ), if( a.multi ) "mexpand" else "expand" ))) ++
//                   (if( moreThanOneArgOut ) {
//                      argsOut.map( a => ValDef( NoMods, "_sz_" + a.name, TypeTree( NoType ), Select( Ident( "_" + a.name ), "size" )))
//                   } else {
//                      Nil
//                   }) ++ {
////                     if( !moreThanOneArgOut ) println( name + " " + expArgs + " / " + args )
////                     if( moreThanZeroArgOut ) {
//                     val funBody = {
//                        val app1 = Apply( Ident( ugenName ), {
//                           val args0 = argsInS.filterNot( _.expandBin.isDefined ).map( a => {
//                              if( a.isGE ) {
//                                 val apply = Apply( Ident( "_" + a.name ), {
//                                    if( moreThanOneArgOut ) {
//                                       Apply( Select( Ident( "i" ), "%" ), Ident( "_sz_" + a.name ) :: Nil )
//                                    } else {
//                                       Ident( "i" )
//                                    }
//                                 }  :: Nil )
//                                 if( a.multi ) Select( apply, "expand" ) else apply
////                                    apply
//                              } else Ident( a.name )
//                           })
//                           if( impliedRate.isDefined ) args0 else (if( expandBin.isDefined && rateTypes ) Select( identRateOrder, "in1" ) else identRate) :: args0
//                        })
//                        expandBin.map( binSel => { // XXX should call make1 to collapse multiplication with one
//                           val a = argsInS.find( _.expandBin.isDefined ).get
//                           val u = if( rateTypes ) TypeApply( identBinOpUGen, Ident( "T" ) :: Nil ) else identBinOpUGen
//                           val r = if( rateTypes ) Select( identRateOrder, "out" ) else identRate
//                           Apply( u, r ::
//                              Select( Ident( "BinaryOp" ), binSel ) :: app1 :: Apply( Ident( "_" + a.name ), {
//                                 if( moreThanOneArgOut ) {
//                                    Apply( Select( Ident( "i" ), "%" ), Ident( "_sz_" + a.name ) :: Nil )
//                                 } else {
//                                    Ident( "i" )
//                                 }
//                              } :: Nil ) :: Nil )
//                        }).getOrElse( app1 )
//                     }
//                     if( moreThanZeroArgOut ) {
//                        val numId = if( moreThanOneArgOut ) Ident( "_exp_" ) else Select( Ident( "_" + argsOut.head.name ), "size" )
//                        val app0 = Apply( Apply( Select( identIIdxSeq, "tabulate" ), numId :: Nil ),
//                           Function( ValDef( Modifiers( Flags.PARAM ), "i", TypeTree( NoType ), EmptyTree ) :: Nil, funBody ) :: Nil
//                        ) :: Nil
//                        if( moreThanOneArgOut ) {
//                           ValDef( NoMods, "_exp_", TypeTree( NoType ), Apply( Ident( "maxInt" ), argsOut.map( a => Ident( "_sz_" + a.name )))) :: app0
//                        } else app0
//                     } else {
//                        Apply( identIIdxSeq, funBody :: Nil ) :: Nil
//                     }
//                  }) // : _*
//
//                  stat match {
//                     case single :: Nil => single
//                     case _ => Block( stat: _* )
//                  }
//               }
////                  Apply( Ident( "apply" ), Ident( rateInfo.name ) :: args.map( i => Ident( i.arg.name )))
//               DefDef(
//                  NoMods withPosition (Flags.PROTECTED, NoPosition) withPosition (Flags.METHOD, NoPosition),
//                  "expandUGens",
//                  Nil, // tparams
//                  Nil,        // vparamss
//                  TypeTree( NoType ), // tpt -- empty for testing
//                  methodBody // rhs
//               )
//            }
//
//            val caseClassMethods =
////            if( indiv ) {
////               caseClassExpandDef :: methodOverrideEquals :: methodOverrideHashCode :: Nil
////            } else {
//               caseClassExpandDef :: Nil
////            }
//
//            val caseClassDef0 = mkCaseClass(
//               NoMods,
//               name,
//
//               caseClassTypeParam, // tparams
//               caseClassConstrArgs :: (if( expandBin.isDefined && rateTypes ) {
//                  List( ( // curry
//                     NoMods withPosition (Flags.PARAM, NoPosition) withPosition (Flags.IMPLICIT, NoPosition),
//                     /* "implicit " + */ identRateOrder.toString + ": RateOrder[" + impliedRate.map( _.typ ).getOrElse( "R" ) + ", S, T]",  // XXX dirty
//                     EmptyTree
//                  ) ) :: Nil
//               } else Nil),
//               caseClassMethods,
//               {
//                  val p4 = if( sideEffect && !indSideEffect ) (traitSideEffect :: caseCommonParents) else caseCommonParents
////                  val t0 = TypeDef( NoMods, if( outputs == SingleOutput ) "GE" else "Expands", {
////                     val p0 = TypeDef( NoMods, ugenName,
////                        if( impliedRate.isDefined ) Nil else (TypeDef( NoMods, "R", Nil, EmptyTree ) :: Nil),
////                        EmptyTree ) :: Nil
////                     if( outputs == SingleOutput ) {
////                        TypeDef( NoMods, impliedRate.map( _.typ ).getOrElse( "R" ): String, Nil, EmptyTree ) :: p0
////                     } else {
////                        p0
////                     }
////                  }, EmptyTree )
////                  impliedRate.map( r => t0 :: TypeDef( NoMods, r.traitTyp, Nil, EmptyTree ) :: Nil ).getOrElse( t0 :: Nil )
//                  TypeDef( NoMods, outputs.sourceName, {
//                     val p0 = if( expandBin.isDefined ) {
//                        // XXX should not put BinaryOpUGen here...
//                        val p1 = if( rateTypes ) (TypeDef( NoMods, "T", Nil, EmptyTree ) :: Nil) else Nil
//                        TypeDef( NoMods, "BinaryOpUGen", p1, EmptyTree ) :: Nil
//                     } else {
//                        TypeDef( NoMods, ugenName,
//                           if( impliedRate.isEmpty && rateTypes ) (TypeDef( NoMods, "R", Nil, EmptyTree ) :: Nil) else Nil,
//                           EmptyTree ) :: Nil
//                     }
//                     if( (outputs != ZeroOutputs) && rateTypes ) {
//                        TypeDef( NoMods, (if( expandBin.isDefined ) "T" else impliedRate.map( _.typ ).getOrElse( "R" )): String,
//                           Nil, EmptyTree ) :: p0
//                     } else {
//                        p0
//                     }
//                  }, EmptyTree ) :: p4
//               }
//            )
//            val caseClassDef = wrapDoc( caseClassDef0, 0, docText, docSees, docWarnPos, collectMethodDocs( argsIn ))
//
////            caseClassDef.setSymbol( new tools.nsc.symtab.Symbols.Symbol( NoSymbol, NoPosition, new Name( 0, 0 )))
//
////            global.docComment( caseClassDef.symbol, "Jo chuck", NoPosition )
////            global.docComments += caseClassDef.symbol -> DocComment( "/** Kuuka */", NoPosition )
//
////            println( "JUHU " + caseClassConstr.symbol.isConstructor )
////            println( printer( caseClassConstr ))
//
////            case class SinOscUGen[ R <: Rate ]( freq: AnyUGenIn, phase: AnyUGenIn )
////            extends SingleOutUGen[ R ]( List( freq, phase ))
//            val classes0 = objectDef ::: (caseClassDef :: Nil)
//
//            val ugenCaseClassConstrArgs = {
//               val args0 = argsIn.filterNot( _.expandBin.isDefined ) map { uArgInfo =>
//                  (NoMods, uArgInfo.name + ": " + uArgInfo.deriveGE( true ), EmptyTree)
//               }
//               if( impliedRate.isDefined ) args0 else {
//                  (NoMods, "rate: " + (if( rateTypes ) "R" else "Rate"), EmptyTree) :: args0
//               }
//            }
//
//            val ugenCaseClassParents: List[ TypeDef ] = {
//               // note: ZeroOutUGen already extends HasSideEffect
//               val p4 = if( sideEffect && !indSideEffect && (outputs != ZeroOutputs) ) (traitSideEffect :: caseCommonParents) else caseCommonParents
//               TypeDef( NoMods, outputs.typ, if( outputs == ZeroOutputs || !rateTypes ) Nil else {
//                  TypeDef( NoMods, impliedRate.map( _.typ ).getOrElse( "R" ): String, Nil, EmptyTree ) :: Nil
//               }, EmptyTree ) :: p4
//            }
////            val ugenCaseClassParents: List[ TypeDef ] = impliedRate.map( r =>
////               ugenCaseClassParents0 :: TypeDef( NoMods, r.traitTyp, Nil, EmptyTree ) :: Nil
////            ).getOrElse( ugenCaseClassParents0 :: Nil )
//
//            val ugenCaseClassDef = mkCaseClass(
//               NoMods,
//               ugenName,
//               ugenCaseClassTypeParam, // tparams
//               ugenCaseClassConstrArgs :: Nil,
//               Nil,
//               ugenCaseClassParents,
//               superArgs = {
//                  val geArgs = argsOut.filterNot( _.expandBin.isDefined )
//                  val args0 = geArgs.lastOption match {
//                     case Some( a ) if( a.multi ) =>
////                        val rvsArgs = geArgs.dropRight( 1 ).reverse
////                        rvsArgs.foldLeft[ Tree ]( Select( Ident( a.arg.name ), "expand" ))( (a, b) => Apply( Select( a, "+:" ), Ident( b.arg.name ) :: Nil )) :: Nil
//                        val args1   = geArgs.dropRight( 1 )
////                        val sel     = Select( Ident( a.name ), "expand" )
//                        val sel     = Ident( a.name ) // already expanded to IIdxSeq!
//                        (if( args1.nonEmpty ) {
//                           Apply( Select( Apply( TypeApply( identIIdxSeq, Ident( if( rateTypes ) "AnyUGenIn" else "UGenIn" ) :: Nil ),
//                              geArgs.dropRight( 1 ).map( a => Ident( a.name ))),
//                              "++" ), sel :: Nil )
//                        } else {
//                           sel
//                        }) :: Nil
//                     case _ => (if( geArgs.nonEmpty ) {
//                        Apply( identIIdxSeq, geArgs.map( a => Ident( a.name )))
//                     } else {
//                        Select( identIIdxSeq, "empty" )
//                     }) :: Nil
//                  }
//                  outputs match {
//                     case m: MultiOutputLike =>
//                        Apply( Apply( Select( identIIdxSeq, "fill" ), m.tree :: Nil ),
//                           Ident( impliedRate.map( _.typ ).getOrElse( "rate" )) :: Nil ) :: args0
//                     case _ => args0
//                  }
//               }
//            )
//
//            classes0 ++ List( ugenCaseClassDef ) // how to prepend a blank line??
//
////            println( outputText )
//         })( breakOut )
//
//         // XXX add UGen class
//         if( ugens.nonEmpty ) {
//            val imports0 = if( importFloat ) {
//               Import( Ident( "Float" ), ImportSelector( "PositiveInfinity", -1, "inf", -1 ) :: Nil ) :: Nil
//            } else Nil
//            val imports1 = Import( Select( Ident( "collection" ), "immutable" ), ImportSelector( "IndexedSeq", -1, identIIdxSeq.name, -1 ) :: Nil ) ::
//                  Import( Ident( "UGenHelper" ), ImportSelector( nme.WILDCARD, -1, nme.WILDCARD, -1 ) :: Nil ) :: imports0
//            val packageDef = PackageDef( Select( Select( Ident( "de" ), "sciss" ), "synth" ),
//               PackageDef( Ident( "ugen" ), imports1 ::: ugens ) :: Nil )
//            println( "Writing " + fileName )
//            val osw = new OutputStreamWriter( new FileOutputStream( new File( dir, fileName )), "UTF-8" )
//            osw.write( """/*
// * """ + fileName + """
// * (ScalaCollider-UGens)
// *
// * This is a synthetically generated file.
// * Created: """ + dateString + """
// * ScalaCollider-UGen version: """ + UGens.versionString + """
// */
//
//""" )
//            osw.write( createText( packageDef ))
//            osw.close()
//         }
//      }
//      println( "Done.")
//   }
//
//   private def getBoolAttr( n: Node, name: String, default: Boolean = false ) =
//      (n \ ("@" + name)).headOption.map( _.text.toBoolean ).getOrElse( default )
//
//   private def getIntAttr( n: Node, name: String, default: Int ) =
//      (n \ ("@" + name)).headOption.map( _.text.toInt ).getOrElse( default )
//
//   private case class RateInfo( name: String, methodNames: List[ String ], implied: Boolean, xml: Node ) {
////      def typ = name + ".type"
//      def typ = name // + ".type"
//      def traitTyp = name.capitalize + "Rated"
//   }
////   private object TypeInfo {
////      def toString( typ: Seq[ TypeInfo ]) : String = {
////         typ.map( t => t.name + (t.params match {
////            case Nil  => ""
////            case coll => coll.mkString( "[ ", ", ", " ]" )
////         })).mkString( " with ")
////      }
////   }
////   private case class TypeInfo( name: String, params: List[ TypeInfo ] = Nil )
//
////   private case class MyFunction(vparams: List[ValOrDefDef], body: Tree) extends TermTree with SymTree
////   private case class ArgDef(mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) extends ValOrDefDef
//
//   // XXX remove in favour of TypeDef
//   private case class TypeInfo( tuples: List[ (String, List[ TypeInfo ])], exist: Option[ (String, String) ] = None ) {
//      override def toString = {
//         val s0 = tuples.map( t => t._1 + (t._2 match {
//            case Nil  => ""
//            case coll => coll.mkString( "[", ", ", "]" )
//         })).mkString( " with ")
//         exist.map( tup => s0 + " forSome { type " + tup._1 + " " + tup._2 + " }" ).getOrElse( s0 )
//      }
//   }
//   private case class ArgInfo( typ: TypeInfo, default: Option[ String ], doc: Option[ String ])
//   private trait UGenArgInfoLike {
//      def name : String
//      def argDefault : ArgInfo
//      def multi : Boolean
//      def expandBin: Option[ String ]
//
//      def arg( rate: RateInfo ) : ArgInfo
//
//      def isGE = argDefault.typ.tuples.headOption match {
//         case Some( ("GE", _) )        => true
//         case Some( ("AnyGE", _) )     => true
//         case Some( (`nameExpands`, List( TypeInfo( List( (sub, _), _* ), _ ), _* ))) => sub match {
//            case "GE"      => true
//            case "AnyGE"   => true
//            case _         => false
//         }
////         case Some( ("MultiGE", _) )   => true
//         case _                        => false
//      }
//
//      def isExpands = argDefault.typ.tuples.headOption match {
//         case Some( (`nameExpands`, _ ) ) => true
//         case _ => false
//      }
//
//      def deriveGE( geToSeq: Boolean ) : String = deriveGEFrom( argDefault.typ, geToSeq )
//
//      private def deriveGEFrom( typ: TypeInfo, geToSeq: Boolean ) : String =
//         if( rateTypes ) deriveGEFromR( typ, geToSeq ) else deriveGEFromNR( typ, geToSeq )
//
//      private def deriveGEFromR(  typ: TypeInfo, geToSeq: Boolean ) : String = typ.tuples.headOption match {
//         case Some( ("GE", List( r, sub, _* )))    => argDefault.typ.exist.map( tup => {
////            println( "Jo. derive with existential '" + sub + "'" )
//            val res = sub match {
////               case TypeInfo( List( ("UGenIn", List( TypeInfo( List( (tup._1, Nil) ), None )))), None ) => "AnyUGenIn"
//               case TypeInfo( List( ("UGenIn", List( TypeInfo( List( (tup._1, Nil) ), None ))), rest @ _* ), None ) =>
//                  TypeInfo( ("AnyUGenIn" -> Nil) :: rest.toList, None )
//               case _ => Predef.error( "Cannot derive type " + argDefault.typ.toString )
//            }
////            println( "res = " + res )
//            res.toString
//         }).getOrElse( sub.toString )
//         case Some( (`nameExpands`, List( sub, _* )))  => if( geToSeq ) "IIdxSeq[" + deriveGEFrom( sub, false ) + "]" else sub.toString
//         case Some( ("AnyGE", _) )                 => "AnyUGenIn"
//         case _                                    => argDefault.typ.toString
//      }
//
//      private def deriveGEFromNR(  typ: TypeInfo, geToSeq: Boolean ) : String = typ.tuples.headOption match {
//         case Some( ("GE", List( sub, _* )))    => argDefault.typ.exist.map( tup => {
//            val res = sub match {
//               case TypeInfo( List( ("UGenIn", Nil), rest @ _* ), None ) =>
//                  TypeInfo( ("UGenIn" -> Nil) :: rest.toList, None )
//               case _ => Predef.error( "Cannot derive type " + argDefault.typ.toString )
//            }
//            res.toString
//         }).getOrElse( sub.toString )
//         case Some( (`nameExpands`, List( sub, _* )))  => if( geToSeq ) "IIdxSeq[" + deriveGEFrom( sub, false ) + "]" else sub.toString
//         case Some( ("AnyGE", _) )                 => "UGenIn"
//         case _                                    => argDefault.typ.toString
//      }
//
//      def defaultTree( arg: ArgInfo = argDefault ): Tree = arg.default.map( s => if( isGE ) {
//         try {
//            Ident( s.toFloat.toString + "f" )  // XXX workaround for scala-refactoring bug of missing f
//         } catch {
//            case e: NumberFormatException => Ident( s )
//         }
//      } else Ident( s )).getOrElse( EmptyTree )
//   }
//
//   private case class UGenArgInfo( name: String, argDefault: ArgInfo, argMap: Map[ RateInfo, ArgInfo ], multi: Boolean, expandBin: Option[ String ])
//   extends UGenArgInfoLike {
//      def arg( rate: RateInfo ) : ArgInfo = argMap.getOrElse( rate, Predef.error( "Accessing illegal rate " + rate.name + " (" + name + ")" ))
//   }
//
//   private case class SyntheticUGenArgInfo( name: String, argDefault: ArgInfo )
//   extends UGenArgInfoLike {
//      def multi = false
//      def arg( rate: RateInfo ) : ArgInfo = argDefault
//      def expandBin = None
//   }
//
//   private abstract sealed class Outputs {
//      def typ: String
//      def sourceName: String
//   }
//   private case object ZeroOutputs extends Outputs {
//      val typ        = "ZeroOutUGen"
//      val sourceName = "ZeroOutUGenSource"
//   }
//   private case object SingleOutput extends Outputs {
//      val typ        = "SingleOutUGen"
//      val sourceName = "SingleOutUGenSource"
//   }
//   private trait MultiOutputLike extends Outputs {
//      val typ        = "MultiOutUGen"
//      val sourceName = "MultiOutUGenSource"
//      def tree: Tree
//   }
//   private case class FixedMultiOutput( num: Int ) extends MultiOutputLike {
//      def tree = Literal( Constant( num ))
//   }
//   private case class ArgMultiOutput( arg: UGenArgInfoLike ) extends MultiOutputLike {
//      def tree = if( arg.isGE ) {
//         if( arg.isExpands) {
//            Select( Ident( arg.name ), "size" )
//         } else {
//            Predef.error( "Not yet supported ("+ arg.name + ")" )
//         }
//      } else {
//         Ident( arg.name )
//      }
//   }
//}