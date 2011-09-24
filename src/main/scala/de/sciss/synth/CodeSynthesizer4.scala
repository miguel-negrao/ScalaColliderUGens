/*
 *  CodeSynthesizer4.scala
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

import scala.tools.nsc.symtab.Flags
import tools.refactoring.Refactoring
import tools.refactoring.util.CompilerProvider
import xml.Node
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq }
//import net.virtualvoid.string.MyNodePrinter
import tools.refactoring.transformation.TreeFactory
import tools.refactoring.common.{CompilerAccess, Tracing}
import tools.nsc.io.AbstractFile
import java.io.{FileOutputStream, OutputStreamWriter, File}
import sys.{error => err}

/**
 * Again with rate type removed
 */
class CodeSynthesizer4( docs: Boolean ) extends Refactoring
with Tracing with CompilerProvider /* with MyNodePrinter */ with CompilerAccess with TreeFactory {
//   val docs          = true
   val ugenClass     = false
   val ratedTrait    = true
   val maybeRates    = true
   val displayName   = true
   val multiOutArg   = true   // whether UGenSource.MultiOut has an explicit argument for the number of outputs

   override val defaultIndentationStep = "   "

//   val strMulti         = "Multi"

   import global._

   // fix 2.8.1 --> 2.9.0
   def typeName( name: String ) = newTypeName( name )
//   def trmName( name: String ) = newTermName( name )
   def ident( name: String ) = new Ident( newTermName( name ))

   def compilationUnitOfFile( f: AbstractFile ) = global.unitOfFile.get( f )

   private def trimDoc( docText: String ) : Seq[ String ] = {
      val trim0   = docText.lines.map( _.trim ).toIndexedSeq
      val trim1   = trim0.dropWhile( _.isEmpty )
      val idx     = trim1.lastIndexWhere( _.isEmpty ) // why the fuck there's no dropRightWhile?
      if( idx >= 0 ) trim1.dropRight( trim1.size - idx ) else trim1
   }

   private def ensureEmptyTrail( text: Seq[ String ]) : Seq[ String ] = if( text.nonEmpty ) text :+ "" else text

   private def wrapDoc( tree: Tree, indent: Int, docText: String = "", sees: Seq[ String ] = Nil, docWarnPos: Boolean = false,
                         methodDocs: Seq[ (String, String) ] = Nil ) : Tree = {
      val hasAny = docs && (docText != "" || sees.nonEmpty || docWarnPos || methodDocs.nonEmpty )
      if( hasAny ) {
// XXX indentation funzt im moment eh nicht richtig bei scala-refactoring. besser ganz weglassen
//         val ind     = Seq.fill( indent )( "   " ).mkString
         val txt0    = trimDoc( docText )
         val txt1    = if( !docWarnPos ) txt0 else ensureEmptyTrail( txt0 ) :+ "'''Warning''': The argument order is different from its sclang counterpart."
         val txt2    = if( methodDocs.isEmpty ) txt1 else ensureEmptyTrail( txt1 ) ++ methodDocs.flatMap( tup => {
            trimDoc( tup._2 ).zipWithIndex.map( tup2 => {
               val (ln, idx) = tup2
               (if( idx == 0 ) ("@param " + tup._1 + "              ").take( 21 ) + "  " else "                       ") + ln
            })
         })
         val txt3    = if( sees.isEmpty ) txt2 else ensureEmptyTrail( txt2 ) ++ sees.map( "@see [[de.sciss.synth." + _ + "]]" )
         // note: @warn is not recognized by scaladoc. we use @note instead
         // fucking scaladoc just ignores @note
//         val txt3    = if( !docWarnPos ) txt2 else ensureEmptyTrail( txt2 ) :+ "@note  The argument order is different from its sclang counterpart."
//         val txt3    = if( !docWarnPos ) txt2 else ensureEmptyTrail( txt2 ) :+ "'''Warning''': The argument order is different from its sclang counterpart."

         DocDef( DocComment(
//            txt3.mkString( "\n" + ind + "/**\n" + ind + " * ", "\n" + ind + " * ", "\n" + ind + " */\n" ), NoPosition ),
            txt3.mkString( "\n/**\n * ", "\n * ", "\n */\n" ), NoPosition ),
            tree )
      } else tree
   }

   private def collectMethodDocs( args: Seq[ UGenArgInfoLike ], rate: Option[ RateInfo ] = None ) : Seq[ (String, String) ] =
      args.map( ua => ua.name -> rate.map( ua.arg( _ )).getOrElse( ua.argDefault ).doc ).collect { case (name, Some( doc )) => name -> doc }

   // filter gets applied with filename (without extension) and ugen name
   def perform( xml: Node, dir: File, filter: (String, String) => Boolean = (_, _) => true ) {
//      val traitGE = TypeDef( Modifiers( Flags.TRAIT ), "GE", Nil, EmptyTree )
      val traitSideEffect  = TypeDef( Modifiers( Flags.TRAIT ), typeName( "HasSideEffect" ), Nil, EmptyTree )
      val traitDoneFlag    = TypeDef( Modifiers( Flags.TRAIT ), typeName( "HasDoneFlag" ),   Nil, EmptyTree )
      val traitRandom      = TypeDef( Modifiers( Flags.TRAIT ), typeName( "UsesRandSeed" ),  Nil, EmptyTree )
      val traitIndiv       = TypeDef( Modifiers( Flags.TRAIT ), typeName( "IsIndividual" ),  Nil, EmptyTree )
//      val traitReadsFFT    = TypeDef( Modifiers( Flags.TRAIT ), "ReadsFFT",      Nil, EmptyTree )
      val traitWritesBuffer= TypeDef( Modifiers( Flags.TRAIT ), typeName( "WritesBuffer" ),  Nil, EmptyTree )
      val traitWritesFFT   = TypeDef( Modifiers( Flags.TRAIT ), typeName( "WritesFFT" ),     Nil, EmptyTree )
      val traitWritesBus   = TypeDef( Modifiers( Flags.TRAIT ), typeName( "WritesBus" ),     Nil, EmptyTree )
      val strIIdxSeq       = "IIdxSeq"
//      val identIIdxSeq     = Ident( strIIdxSeq )
      val identIIdxSeq     = ident( strIIdxSeq )
//      val identBinOpUGen   = ident( "BinaryOpUGen" )
      val strMakeUGens     = "makeUGens"
//      val identMakeUGens   = ident( strMakeUGens )
      val strMakeUGen      = "makeUGen"
      val identMakeUGen    = ident( strMakeUGen )
      val strExpand        = "expand"
//      val identExpand      = ident( strExpand )
//      val strUGenInLike    = "UGenInLike"
      val strUArgs         = "_args"
      val identUArgs       = ident( strUArgs )
      val identUnwrap      = ident( "unwrap")
      val strGE            = "GE"
//      val strAnyGE         = strGE // no distinction any more
      val strRatePlaceholder="Rate" // "R"
      val strMaybeRate     = "MaybeRate"
      val strMaybeResolve  = "?|"

      val identApply       = ident( "apply" )
      val identName        = ident( "name" )

      val strRateArg       = "rate"
      val strRateMethod    = "rate"
      val identRateArg     = ident( strRateArg )
//      val identRateType    = ident( "Rate" )
      val dateString       = new java.util.Date().toString

      (xml \ "file") foreach { node =>
         val name       = (node \ "@name").text
         val fileName   = name + ".scala"
//         val ast        = treeFrom( "package de.sciss.synth.ugen\n" )
         val fltNode    = (node \ "ugen").filter( node => filter( name, (node \ "@name").text ))
         var importFloat = false
         val ugens: List[ Tree ] = fltNode.flatMap( node => {
            val name          = (node \ "@name").text
            val readsBus      = getBoolAttr( node, "readsbus" )
            val writesBus     = getBoolAttr( node, "writesbus" )
//if( name == "Out" ) println( " OUT : " + writesBus )
            val readsBuffer   = getBoolAttr( node, "readsbuf" )   // currently unused
            val readsFFT      = getBoolAttr( node, "readsfft" )   // currently unused
            val writesBuffer  = getBoolAttr( node, "writesbuf" )
            val writesFFT     = getBoolAttr( node, "writesfft" )
            val sideEffect    = getBoolAttr( node, "sideeffect" ) // || writesBus || writesBuffer
            val indSideEffect = writesBuffer || writesFFT || writesBus
            val doneFlag      = getBoolAttr( node, "doneflag" )
            val random        = getBoolAttr( node, "random" )
            val indiv         = getBoolAttr( node, "indiv" ) // || random
            val indIndiv      = indSideEffect || random
            val rates0: List[ RateInfo ] = (node \ "rate").map( n => {
               val name       = (n \ "@name").text
               val mName0     = (n \ "@method").text
               val mName1     = (n \ "@methodalias").text
               val mName2     = if( mName0 != "" ) mName0 else name match {
                  case "audio"   => "ar"
                  case "control" => "kr"
                  case "scalar"  => "ir"
                  case "demand"  => "dr"
               }
               val methodNames   = if( mName1 != "" ) {
                  mName1 :: mName2 :: Nil
               } else {
                  mName2 :: Nil
               }
               val implied    = getBoolAttr( n, "implied" )
               RateInfo( name, methodNames, implied, n )
            })( breakOut )

//            if( name == "Out" ) println( "for Out, rates = " + rates0 )

            val impliedRate   = rates0.find( _.implied )
            val (rates, caseDefaults) = impliedRate.map( irate => {
               require( rates0.size == 1, "Can only have one implied rate (" + name + ")" )
               val rateInfo   = rates0.head
               val hasApply   = rateInfo.methodNames.contains( "apply" )
               ((if( hasApply ) {
                  rateInfo.copy( methodNames = rateInfo.methodNames.filterNot( _ == "apply" )) :: Nil
               } else rates0), hasApply)
            }).getOrElse( (rates0, false) )

            val docNode       = (node \ "doc")
            val docSees       = (docNode \ "see").map( _.text )
            val docWarnPos    = docNode.headOption.map( getBoolAttr( _, "warnpos" )).getOrElse( false )
            val docText       = (docNode \ "text").text

            val argsTup : List[ (UGenArgInfo, Int) ] = (node \ "arg").zipWithIndex.map( tup => {
               val (n, idx0)  = tup
               val idx        = getIntAttr( n, "pos", idx0 )
               val aname      = (n \ "@name").text
               val multi      = getBoolAttr( n, "multi" )
               val doneFlagArg= getBoolAttr( n, "doneflag" )
               val expandBin  = (n \ "@expandbin").text match {
                  case ""  => None
                  case "*" => Some( "Times" )
                  case x   => err( "Illegal expandbin value '" + x + "' (" + name + ")" )
               }
               if( multi ) require( !doneFlagArg, "Multi arguments cannot have done flag (" + name + ")" )

               def createInfo( rateNode: Option[ Node ], rateInfo: Option[ RateInfo ]) : ArgInfo = {
                  def getAttr( name: String ) : Option[ String ] = (n \ ("@" + name)).headOption.map( _.text )
                  def getEitherNode( name: String ) : Option[ String ] = {
                     (n \ name).headOption.orElse( rateNode.flatMap( n => (n \ name).headOption )).map( _.text )
                  }
                  def getEitherAttr( name: String ) : Option[ String ] = getEitherNode( "@" + name )
                  val (typInfo, rateCons) = getEitherAttr( "type" ).map( s => (TypeInfo( (s -> Nil) :: Nil ), None) )
                     .getOrElse({
//                        val rateAttr = getEitherAttr( "rate" ) // (n \ "@rate").text
//                        def r0 = if( doneFlagArg || expandBin.isDefined ) {
//                           val par0 = strAnyGE -> Nil // if( expandBin.isDefined ) ...
//                           val par1 = if( doneFlagArg ) par0 :: ("HasDoneFlag" -> Nil) :: Nil else par0 :: Nil
//                           TypeInfo( par1, None )
//                        } else {
//                           TypeInfo( (strAnyGE -> Nil) :: Nil )
//                        }
//                        def r1( rateName: String ) = {
//                           val t0   = Nil // TypeInfo( (rateName -> Nil) :: Nil ) :: Nil
//                           val par0 = strGE -> t0
//                           val par1 = if( doneFlagArg ) par0 :: ("HasDoneFlag" -> Nil) :: Nil else par0 :: Nil
//                           TypeInfo( par1, None )
//                        }
//                        (rateAttr, rateInfo) match {
//                           case (None, _)                            => r0
//                           case (Some( "ugen" ), None)               => r0
//                           case (_, _) if( expandBin.isDefined )     => r0
//                           case (Some( "ugen" ), Some( info ))       => r1( info.name )
//                           case (Some( rateName ), _)                => r1( rateName )
//                        }
                        val par0 = strGE -> Nil
                        val par1 = if( doneFlagArg ) par0 :: ("HasDoneFlag" -> Nil) :: Nil else par0 :: Nil
                        val typ = TypeInfo( par1, None )

                        val cons = getAttr( "rate" ).map {
                           case "ugen"   =>
//                              println( "--UGen for" + name )
                              RateCons.UGen
                           case rateName => RateCons.Name( rateName )
                        }
                        (typ, cons)
                     })
                  val default    = getEitherAttr( "default" )
                  if( default == Some( "inf" ) || default == Some( "-inf" )) {
                     importFloat = true
                  }
                  val doc        = getEitherNode( "doc" )
                  ArgInfo( typInfo, default, doc, rateCons )
               }

               val argDefault = createInfo( None, None )
               val argMap: Map[ RateInfo, ArgInfo ] = rates.map( rateInfo => {
                  val rateNode = (rateInfo.xml \ "arg").find( n => (n \ "@name").text == aname )
                  rateInfo -> createInfo( rateNode, Some( rateInfo ))
               })( breakOut )
               UGenArgInfo( aname, argDefault, argMap, multi, expandBin ) -> idx
            })( breakOut )

            val numArgsIn  = argsTup.size
            val argsOut    = argsTup.map( _._1 ).filter( a => a.isGE || a.isString )
            val argsIn     = List.tabulate( numArgsIn )(
               idx => argsTup.find( _._2 == idx ).getOrElse( err( "Wrong argument positions (" + name + ")" ))._1 )
            val argsInS = argsIn  // no exceptions at the moment

            val expandBin = argsIn.map( _.expandBin ).collect { case Some( exp ) => exp } match {
               case exp :: Nil => Some( exp )
               case Nil => None
               case _ => err( "Can only have one expandBin (" + name + ")" )
            }
            // this is to ensure for makeUGen that when expandBin is defined, we simply do not pass in
            // _args to the UGen constructor, simplifying the process of filtering out the argument
            // which corresponds to expandBin!
            require( expandBin.isEmpty || argsIn.size == 1, "Currently when using expandBin, there cannot be other input args" )

//            {
//               val test = argsIn.filter( _.argDefault.rateCons == Some( RateCons.UGen ))
//               if( test.nonEmpty ) println( "JO : for " + name + " got ugen cons : " + test )
//            }
            val maybeRate  = if( maybeRates && impliedRate.isEmpty ) argsIn.find( _.argDefault.rateCons == Some( RateCons.UGen )) else None
//if( maybeRate.isDefined ) println( "JO. Maybe rate for " + name )

            val outputs       = (node \ "outputs").headOption match {
               case Some( n ) => (n \ "@num").text match {
                  case "0" => ZeroOutputs
                  case t   => argsIn.find( _.name == t ).map( a => ArgMultiOutput( a )).getOrElse( FixedMultiOutput( t.toInt ))
               }
               case None      => SingleOutput
            }

            val objectMethodDefs = rates.flatMap( rateInfo => {
               val objectMethodArgs0 : List[ ValDef ] = argsIn.map( uArgInfo => {
                  val argInfo = uArgInfo.arg( rateInfo )
                  ValDef(
                     Modifiers( Flags.PARAM ),
                     uArgInfo.name,
                     ident( argInfo.typ.toString ),
                     uArgInfo.defaultTree( argInfo )
                  )
               })
               val objectMethodArgs : List[ List[ ValDef ]] =
//                  if( expandBin.isDefined ) {
//                  val curry : List[ List[ ValDef ]] = List( ValDef(
//// the next line would produce implicit val... for reasons beyond my imagination
////                     NoMods withPosition (Flags.PARAM, NoPosition) withPosition (Flags.IMPLICIT, NoPosition),
//                     Modifiers( Flags.PARAM ) withPosition (Flags.IMPLICIT, NoPosition),
//                     identRateOrder.toString, // XXX dirty
//// stupid bug... this would result in a closing bracket missing in the method's type parameter...
//                     TypeDef( NoMods, strRateOrder, TypeDef( NoMods, rateInfo.typ, Nil, EmptyTree ) :: typExpandBin, EmptyTree ),
////                     Ident( "RateOrder<" + rateInfo.typ + ",S,T>" ), // XXX dirty
//                     EmptyTree
//                  )) :: Nil
//                  if( objectMethodArgs0.nonEmpty ) objectMethodArgs0 :: curry else curry
//               } else {
                  if( objectMethodArgs0.nonEmpty ) objectMethodArgs0 :: Nil else Nil
//               }

               val args0 = argsInS.map( i => Ident( i.name ))
               val methodBody = {
                  val typApply0  = Nil // if( expandBin.isDefined ) typExpandBin else Nil
                  val typApply1  = typApply0 // if( impliedRate.isEmpty ) TypeDef( NoMods, rateInfo.typ, Nil, EmptyTree ) :: typApply0 else typApply0
                  val args1      = /* if( expandBin.isDefined ) {
                     Select( identRateOrder, "out" ) :: args0
                  } else */ if( impliedRate.isEmpty ) {
                     Ident( rateInfo.name ) :: args0
                  } else {
                     args0
                  }
// BUG: TypeApply trennt typen nicht durch kommas
//                  val apply0 = Apply( if( typApply1.nonEmpty ) TypeApply( identApply, typApply1 ) else identApply, args1 )
                  val apply0 = if( typApply1.nonEmpty ) {
//                     Apply( TypeDef( NoMods, "apply", typApply1, EmptyTree ), args1 )  // XXX dirty
                     Apply( TypeApply( Ident( "apply" ), typApply1 ), args1 )  // XXX dirty
                  } else Apply( identApply, args1 )
                  /* if( expandBin.isDefined ) Apply( apply0, identRateOrder :: Nil ) else */ apply0
               }
               val mdocs = collectMethodDocs( argsIn, Some( rateInfo ))
               val def0 = rateInfo.methodNames.map { mName =>
                  val df = DefDef(
                     NoMods withPosition (Flags.METHOD, NoPosition),
                     mName,
                     Nil, // if( expandBin.isDefined ) typExpandBinDf else Nil,        // tparams
                     objectMethodArgs,    // vparamss
                     if( mName != "apply" ) EmptyTree else TypeDef( NoMods, typeName( name ), Nil, EmptyTree ),
                     methodBody // rhs
                  )
                  wrapDoc( df, 1, methodDocs = mdocs )
               }
               val allDefaults = argsIn.nonEmpty && argsIn.forall( _.arg( rateInfo ).default.isDefined )
               if( allDefaults && rateInfo.methodNames.nonEmpty ) {
//                  require( rateInfo.methodNames.nonEmpty, "No method names for rate " + rateInfo.name + " (" + name + ")" )
                  val mName = rateInfo.methodNames.head
//                  val methodBody = Apply( Ident( mName ), Ident( " " ) :: Nil )  // XXX how to get ar() with the parentheses?
                  val methodBody = Apply( Ident( mName ), Nil )  // XXX how to get ar() with the parentheses?
                  DefDef(
                     NoMods withPosition (Flags.METHOD, NoPosition),
                     mName,
                     Nil,        // tparams
                     Nil,        // vparams
                     TypeDef( NoMods, typeName( name ), {
//                        val typ0 = TypeDef( NoMods, rateInfo.typ, Nil, EmptyTree )
//                        val typ1 = if( expandBin.isDefined ) {
//                           TypeDef( NoMods, "scalar", Nil, EmptyTree ) :: typ0 :: Nil
//                        } else Nil
//                        if( impliedRate.isEmpty ) (typ0 :: typ1) else typ1
                        Nil
                     }, EmptyTree ),
                     methodBody // rhs
                  ) :: def0
               } else def0
            })

            val objectDef = if( objectMethodDefs.nonEmpty ) {
               val mod = ModuleDef(
                  NoMods,
                  name,
                  Template(
                     EmptyTree :: Nil, // parents
                     emptyValDef,      // self
                     objectMethodDefs  // body
                  )
               )
               wrapDoc( mod, 0, docText, docSees, docWarnPos ) :: Nil
            } else Nil

            val caseClassConstrArgs  = {
//               val args0 = argsInS map { uArgInfo => (NoMods, uArgInfo.name + ": " + uArgInfo.argDefault.typ,
//                  if( caseDefaults ) uArgInfo.defaultTree() else EmptyTree) }
               val args0 = argsInS map { uArgInfo => (NoMods, uArgInfo.name, {
                  val typ0 = Ident( uArgInfo.argDefault.typ.toString )
                  if( caseDefaults ) uArgInfo.defaultTree() match {
                     case EmptyTree => typ0
                     case t => Assign( typ0, t )
                  } else typ0
               }) }
               if( impliedRate.isDefined ) args0 else {
                  (NoMods, strRateArg + ": " + (/*if( rateTypes ) (*/ /* if( expandBin.isDefined ) "T" else */
                     if( maybeRate.isDefined ) strMaybeRate else strRatePlaceholder /*) else "Rate" */), EmptyTree) :: args0
               }
            }

            // hmmmm... is this the cleanest way to define R <: Rate?
//            if( impliedRate.isDefined ) Nil else (TypeDef( NoMods, "R", Nil, TypeBoundsTree( EmptyTree, TypeDef( NoMods, "Rate", Nil, EmptyTree ))) :: Nil), // tparams
            val ugenCaseClassTypeParam = Nil // if( impliedRate.isEmpty ) (TypeDef( NoMods, "R <: Rate", Nil, EmptyTree ) :: Nil) else Nil
            val caseClassTypeParam = Nil // if( expandBin.isDefined ) ugenCaseClassTypeParam ::: typExpandBinDf else ugenCaseClassTypeParam

//            val expArgs    = argsOut.filter( _.isGE )
//            val hasArgsOut = argsOut.size > 0
//          val ugenName   = if( hasArgsOut ) name + "UGen" else name
            val ugenName   = name + "UGen"

            val caseCommonParents : List[ TypeDef ] = {
               val p1 = if( doneFlag )             (traitDoneFlag :: Nil)     else Nil
               val p2 = if( random )               (traitRandom :: p1)        else p1
               val p3 = if( writesBus )            (traitWritesBus :: p2)     else p2
               val p4 = if( writesBuffer )         (traitWritesBuffer :: p3)  else p3
               val p5 = if( writesFFT )            (traitWritesFFT :: p4)     else p4
               val p6 = if( indiv && !indIndiv )   (traitIndiv :: p5)         else p5
//               val p6 = if( sideEffect && !indSideEffect ) ...
               if( ratedTrait ) impliedRate.map( r => TypeDef( NoMods, typeName( r.traitTyp ), Nil, EmptyTree ) :: p6 ).getOrElse( p6 ) else p6
//               p6
            }

//            val argsExpOut          = argsOut.filter( _.isGE )
//            val moreThanZeroArgOut  = argsExpOut.size > 0
//            val moreThanOneArgOut   = argsExpOut.size > 1

            /*
             * `protected def makeUGens: UGenInLike = ...`
             */
            val makeUGensDef = {
//               val bufE    = buf.expand
//               val multiE  = multi.expand
//               val numExp  = math.max( bufE.size, multiE.size )
//               IIdxSeq.tabulate( numExp )( i => DiskOutUGen( bufE( i % numExp ), multiE( i % numExp ).expand ))
               val methodBody : Tree = {
                  val args0         = argsOut // .filterNot( _.expandBin.isDefined )
                  val argsApp       = args0.map( a => {
                     val id = Ident( a.name )
                     if( a.isGE ) {
                        val sel = Select( id, strExpand )
                        if( a.multi ) Select( sel, "outputs" ) else sel
                     } else if( a.isString ) {
                        Apply( Ident( "stringArg" ), id :: Nil )
                     } else id
                  })

                  def split(as: IIdxSeq[UGenArgInfo], ts: IIdxSeq[Tree]): IIdxSeq[Tree] = {
                     require( as.size == ts.size )
                     var from = 0
                     var keep = false
                     var res  = IIdxSeq.empty[Tree]
                     while( from < as.size ) {
                        val n = as.segmentLength( u => (u.multi || u.isString) == keep, from )
                        if( n > 0 ) {
                           val slice = ts.slice( from, from + n )
                           res = if( keep ) res ++ slice else res :+ Apply( identIIdxSeq, slice.toList )
                           from += n
                        }
                        keep = !keep
                     }
                     res
                  }

                  val argsApp2 = split( args0.toIndexedSeq, argsApp.toIndexedSeq )
                  argsApp2.lastOption match {
                     case None =>
                        Apply( identMakeUGen, Select( identIIdxSeq, "empty" ) :: Nil )  // shortcut, works because we do not use type apply
                     case Some( l ) =>
                        Apply( identUnwrap, argsApp2.init.foldRight( l )( (a, t) => Apply( Select( a, "++" ), t :: Nil )) :: Nil )
                  }
               }

               DefDef(
                  NoMods withPosition (Flags.PROTECTED, NoPosition) withPosition (Flags.METHOD, NoPosition),
                  strMakeUGens,
                  Nil, // tparams
                  Nil,        // vparamss
                  TypeDef( NoMods, typeName( outputs.resName ), Nil, EmptyTree ), // TypeTree( NoType ), // tpt -- empty for testing
                  methodBody // rhs
               )
            }

            /*
             * `protected def makeUGen(_args: IIdxSeq[UGenIn]): UGenInLike = ...`
             */
            val makeUGenDef = {
               val methodBody : Tree = {
                  val (preBody, outUGenArgs) = {
                     val strResolvedRateArg = if( maybeRate.isDefined ) "_rate" else strRateArg
                     val preArgs = outputs match {
                        case m: MultiOutputLike =>
                           val tree = if( multiOutArg ) {
                              Ident( "numOutputs" )
                           } else {
                              m match {
                                 case fm: FixedMultiOutput => fm.tree
                                 case am @ ArgMultiOutput( a ) => if( a.isGE ) {
                                    require( !argsOut.exists( _.isString ), "Currently strings not supported for arg-multioutput" )
                                    val numFixedArgs  = argsOut.size - 1
                                    val selSz = Select( identUArgs, "size" )
                                    if( numFixedArgs == 0 ) selSz else Apply( Select( selSz, "-" ), Literal( Constant( numFixedArgs )) :: Nil )
                                 } else am.tree
                              }
                           }
                           Apply( Apply( Select( identIIdxSeq, "fill" ), tree :: Nil ),
                              Ident( impliedRate.map( _.typ ).getOrElse( strResolvedRateArg )) :: Nil ) :: Nil
                        case _ => Nil
                     }
                     // might need some more intelligent filtering eventually
                     val args3 = preArgs :+ (if( expandBin.isDefined ) Select( identIIdxSeq, "empty" ) else identUArgs)
                     val args4 = identName /* Literal( Constant( name )) */ :: Ident( impliedRate.map( _.typ ).getOrElse( strResolvedRateArg )) :: args3

                     val preBody = maybeRate.map( ua => {
                        val aPos = argsOut.indexOf( ua )
                        require( argsOut.take( aPos ).forall( a => a.isGE && !a.multi ), "Cannot resolve MaybeRate ref after multi args" )
                        // val _rate = rate |? _args(<pos>).rate
                        ValDef(
                           NoMods,
                           strResolvedRateArg,
                           EmptyTree,
                           Apply( Select( identRateArg, strMaybeResolve ), Select( Apply( identUArgs, Literal( Constant( aPos )) :: Nil ), strRateMethod ) :: Nil)
                        )
                     })

                     (preBody, args4 :: Nil) // no currying
                  }
                  val app1 = New( Ident( outputs.typ ), outUGenArgs )
                  val app2 = expandBin.map( binSel => {
                     val a = argsInS.find( _.expandBin.isDefined ).get
                     require( !argsOut.exists( a => a.multi || a.isString ), "Mixing binop with multi args is not yet supported" )
                     val aPos = argsOut.indexOf( a )
                     assert( aPos >= 0 )
                     Apply( Select( Select( Ident( "BinaryOp" ), binSel ), "make1" ), app1 :: Apply( identUArgs, Literal( Constant( aPos )) :: Nil ) :: Nil )
                  }).getOrElse( app1 )
                  preBody match {
                     case Some( tree ) => Block( tree, app2 )
                     case None => app2
                  }
               }

               val methodArgs = List( List( ValDef(
                  Modifiers( Flags.PARAM ),
                  strUArgs,
                  TypeDef( NoMods, typeName( strIIdxSeq ), TypeDef( NoMods, typeName( "UGenIn" ), Nil, EmptyTree ) :: Nil, EmptyTree ),
                  EmptyTree
               )))

               DefDef(
                  NoMods withPosition (Flags.PROTECTED, NoPosition) withPosition (Flags.METHOD, NoPosition),
                  strMakeUGen,
                  Nil, // tparams
                  methodArgs,        // vparamss
                  TypeDef( NoMods, typeName( outputs.resName ), Nil, EmptyTree ), // TypeTree( NoType ), // tpt -- empty for testing
                  methodBody // rhs
               )
            }

            val caseClassMethods = {
//            if( indiv ) {
//               caseClassExpandDef :: methodOverrideEquals :: methodOverrideHashCode :: Nil
//            } else {
               val m1 = makeUGensDef :: makeUGenDef :: Nil
//               if( ratedTrait ) impliedRate.map( r => {
//                  ValDef(
//                     NoMods,
//                     strRate,
//                     identRateType,
//                     Ident( r.name )
//                  ) :: m1
//               }).getOrElse( m1 ) else
               m1
            }

            val caseClassSuperArgs = if( multiOutArg ) {
               outputs match {
                  case m: MultiOutputLike =>
                     val tree = m match {
                        case fm: FixedMultiOutput => fm.tree
                        case am @ ArgMultiOutput( a ) => if( a.isGE ) {
                           require( !argsOut.exists( _.isString ), "Currently strings not supported for arg-multioutput" )
//                           val numFixedArgs  = argsOut.size - 1
//                           val selSz = Select( identUArgs, "size" )
//                           if( numFixedArgs == 0 ) selSz else Apply( Select( selSz, "-" ), Literal( Constant( numFixedArgs )) :: Nil )
                           Select( ident( a.name ), "numOutputs" )
                        } else am.tree
                     }
                     tree :: Nil
                  case _ => Nil
               }
            } else Nil

            val caseClassDef0 = mkCaseClass(
//               NoMods,
               NoMods withPosition (Flags.FINAL, NoPosition),
               name,

               caseClassTypeParam, // tparams
               caseClassConstrArgs :: (
//                  if( expandBin.isDefined ) {
//                  List( ( // curry
//                     NoMods withPosition (Flags.PARAM, NoPosition) withPosition (Flags.IMPLICIT, NoPosition),
//                     /* "implicit " + */ identRateOrder.toString + ": " + strRateOrder + "[" + impliedRate.map( _.typ ).getOrElse( "R" ) + ", S, T]",  // XXX dirty
//                     EmptyTree
//                  ) ) :: Nil
//               } else
                     Nil),
               caseClassMethods,
               { // parents
                  val p4 = if( sideEffect && !indSideEffect ) (traitSideEffect :: caseCommonParents) else caseCommonParents
                  TypeDef( NoMods, typeName( outputs.sourceName ), {
                     /* TypeDef( NoMods, (if( expandBin.isDefined ) "T" else impliedRate.map( _.typ ).getOrElse( "R" )): String,
                        Nil, EmptyTree ) :: */ Nil
                  }, EmptyTree ) :: p4

               },
               Literal( Constant( name )) :: caseClassSuperArgs  // super args
            )
            val caseClassDef = wrapDoc( caseClassDef0, 0, docText, docSees, docWarnPos, collectMethodDocs( argsIn ))

            val classes0 = objectDef ::: (caseClassDef :: Nil)

            if( ugenClass ) {
               val ugenCaseClassConstrArgs = {
                  val args0 = argsIn.filterNot( _.expandBin.isDefined ) map { uArgInfo =>
                     (NoMods, uArgInfo.name + ": " + uArgInfo.deriveGE( true ), EmptyTree)
                  }
                  if( impliedRate.isDefined ) args0 else {
                     (NoMods, strRateArg + ": " + (/*if( rateTypes ) */ strRatePlaceholder /* else "Rate" */), EmptyTree) :: args0
                  }
               }

               val ugenCaseClassParents: List[ TypeDef ] = {
                  // note: ZeroOutUGen already extends HasSideEffect
                  val p4 = if( sideEffect && !indSideEffect && (outputs != ZeroOutputs) ) (traitSideEffect :: caseCommonParents) else caseCommonParents
                  TypeDef( NoMods, typeName( outputs.typ ), if( outputs == ZeroOutputs ) Nil else {
                     TypeDef( NoMods, typeName( impliedRate.map( _.typ ).getOrElse( "R" ): String ), Nil, EmptyTree ) :: Nil
                  }, EmptyTree ) :: p4
               }
//            val ugenCaseClassParents: List[ TypeDef ] = impliedRate.map( r =>
//               ugenCaseClassParents0 :: TypeDef( NoMods, r.traitTyp, Nil, EmptyTree ) :: Nil
//            ).getOrElse( ugenCaseClassParents0 :: Nil )

               val ugenCaseClassDef = mkCaseClass(
                  NoMods,
                  ugenName,
                  ugenCaseClassTypeParam, // tparams
                  ugenCaseClassConstrArgs :: Nil,
                  Nil,
                  ugenCaseClassParents,
                  superArgs = {
                     val geArgs = argsOut.filterNot( _.expandBin.isDefined )
                     val args0 = geArgs.lastOption match {
                        case Some( a ) if( a.multi ) =>
      //                        val rvsArgs = geArgs.dropRight( 1 ).reverse
      //                        rvsArgs.foldLeft[ Tree ]( Select( Ident( a.arg.name ), "expand" ))( (a, b) => Apply( Select( a, "+:" ), Ident( b.arg.name ) :: Nil )) :: Nil
                           val args1   = geArgs.dropRight( 1 )
      //                        val sel     = Select( Ident( a.name ), "expand" )
                           val sel     = Ident( a.name ) // already expanded to IIdxSeq!
                           (if( args1.nonEmpty ) {
                              Apply( Select( Apply( TypeApply( identIIdxSeq, Ident( /* if( rateTypes ) "AnyUGenIn" else */ "UGenIn" ) :: Nil ),
                                 geArgs.dropRight( 1 ).map( a => Ident( a.name ))),
                                 "++" ), sel :: Nil )
                           } else {
                              sel
                           }) :: Nil
                        case _ => (if( geArgs.nonEmpty ) {
                           Apply( identIIdxSeq, geArgs.map( a => Ident( a.name )))
                        } else {
                           Select( identIIdxSeq, "empty" )
                        }) :: Nil
                     }
                     val args1 = outputs match {
                        case m: MultiOutputLike =>
                           Apply( Apply( Select( identIIdxSeq, "fill" ), m.tree :: Nil ),
                              Ident( impliedRate.map( _.typ ).getOrElse( strRateArg )) :: Nil ) :: args0
                        case _ => args0
                     }
                     Literal( Constant( ugenName )) :: /* Ident( if( expandBin.isDefined ) "T" else impliedRate.map( _.typ ).getOrElse( "R" )) :: */ args1
                  }
               )

               classes0 ++ List( ugenCaseClassDef ) // how to prepend a blank line??
            } else {
               classes0
            }

//            println( outputText )
         })( breakOut )

         if( ugens.nonEmpty ) {
            val imports0 = if( importFloat ) {
               Import( ident( "Float" ), ImportSelector( "PositiveInfinity", -1, "inf", -1 ) :: Nil ) :: Nil
            } else Nil
            val imports1 = Import( Select( ident( "collection" ), "immutable" ), ImportSelector( "IndexedSeq", -1, identIIdxSeq.name, -1 ) :: Nil ) ::
                  Import( Select( ident( "aux" ), "UGenHelper" ), ImportSelector( nme.WILDCARD, -1, nme.WILDCARD, -1 ) :: Nil ) :: imports0
            val packageDef = PackageDef( Select( Select( ident( "de" ), "sciss" ), "synth" ),
               PackageDef( Ident( "ugen" ), imports1 ::: ugens ) :: Nil )
            println( "Writing " + fileName )
            val osw = new OutputStreamWriter( new FileOutputStream( new File( dir, fileName )), "UTF-8" )
            osw.write( """/*
 * """ + fileName + """
 * (ScalaCollider-UGens)
 *
 * This is a synthetically generated file.
 * Created: """ + dateString + """
 * ScalaCollider-UGens version: """ + UGens.versionString + """
 */

""" )
            osw.write( createText( packageDef ))
            osw.close()
         }
      }
      println( "Done.")
   }

   private def getBoolAttr( n: Node, name: String, default: Boolean = false ) =
      (n \ ("@" + name)).headOption.map( _.text.toBoolean ).getOrElse( default )

   private def getIntAttr( n: Node, name: String, default: Int ) =
      (n \ ("@" + name)).headOption.map( _.text.toInt ).getOrElse( default )

   private case class RateInfo( name: String, methodNames: List[ String ], implied: Boolean, xml: Node ) {
//      def typ = name + ".type"
      def typ = name // + ".type"
      def traitTyp = name.capitalize + "Rated"
//      def traitTyp = name + ".rated"
   }

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

   object RateCons {
      case object UGen extends RateCons
      final case class Name( name: String ) extends RateCons
   }
   sealed trait RateCons

   private case class ArgInfo( typ: TypeInfo, default: Option[ String ], doc: Option[ String ], rateCons: Option[ RateCons ])
   private trait UGenArgInfoLike {
      def name : String
      def argDefault : ArgInfo
      def multi : Boolean
      def expandBin: Option[ String ]

      def arg( rate: RateInfo ) : ArgInfo

      def isGE = argDefault.typ.tuples.headOption match {
         case Some( ("GE", _) )        => true
         case Some( ("AnyGE", _) )     => true
//         case Some( (`strMulti`, List( TypeInfo( List( (sub, _), _* ), _ ), _* ))) => sub match {
//            case "GE"      => true
//            case "AnyGE"   => true
//            case _         => false
//         }
//         case Some( ("MultiGE", _) )   => true
         case _                        => false
      }

      def isInt      = argDefault.typ.tuples == List( ("Int", Nil) )
      def isString   = argDefault.typ.tuples == List( ("String", Nil) )

//      def isExpands = argDefault.typ.tuples.headOption match {
//         case Some( (`strMulti`, _ ) ) => true
//         case _ => false
//      }

      def deriveGE( geToSeq: Boolean ) : String = deriveGEFrom( argDefault.typ, geToSeq )

      private def deriveGEFrom( typ: TypeInfo, geToSeq: Boolean ) : String =
         /* if( rateTypes ) */ deriveGEFromR( typ, geToSeq ) /* else deriveGEFromNR( typ, geToSeq ) */

      private def deriveGEFromR(  typ: TypeInfo, geToSeq: Boolean ) : String = typ.tuples.headOption match {
         case Some( ("GE", _) )                    => "UGenIn"
//         case Some( (`strMulti`, List( sub, _* )))  => if( geToSeq ) "IIdxSeq[" + deriveGEFrom( sub, false ) + "]" else sub.toString
         case Some( ("AnyGE", _) )                 => "UGenIn"
         case _                                    => argDefault.typ.toString
      }

      def defaultTree( arg: ArgInfo = argDefault ): Tree = arg.default.map( s => if( isGE ) {
         try {
            ident( s.toFloat.toString + "f" )  // XXX workaround for scala-refactoring bug of missing f
         } catch {
            case e: NumberFormatException => ident( s )
         }
      } else if( isString ) Literal( Constant( s )) else Ident( s )).getOrElse( EmptyTree )
   }

   private case class UGenArgInfo( name: String, argDefault: ArgInfo, argMap: Map[ RateInfo, ArgInfo ], multi: Boolean, expandBin: Option[ String ])
   extends UGenArgInfoLike {
      def arg( rate: RateInfo ) : ArgInfo = argMap.getOrElse( rate, err( "Accessing illegal rate " + rate.name + " (" + name + ")" ))
   }

   private case class SyntheticUGenArgInfo( name: String, argDefault: ArgInfo )
   extends UGenArgInfoLike {
      def multi = false
      def arg( rate: RateInfo ) : ArgInfo = argDefault
      def expandBin = None
   }

   private abstract sealed class Outputs {
      def typ: String
      def sourceName: String
      def resName: String
   }
   private case object ZeroOutputs extends Outputs {
      val typ        = "UGen.ZeroOut"
      val sourceName = "UGenSource.ZeroOut"
      val resName    = "Unit"
   }
   private case object SingleOutput extends Outputs {
      val typ        = "UGen.SingleOut"
      val sourceName = "UGenSource.SingleOut"
      val resName    = "UGenInLike"
   }
   private sealed trait MultiOutputLike extends Outputs {
      val typ        = "UGen.MultiOut"
      val sourceName = "UGenSource.MultiOut"
      val resName    = "UGenInLike"
      def tree: Tree
   }
   private case class FixedMultiOutput( num: Int ) extends MultiOutputLike {
      def tree = Literal( Constant( num ))
   }
   private case class ArgMultiOutput( arg: UGenArgInfoLike ) extends MultiOutputLike {
      def tree = if( arg.isGE ) {
         if( arg.multi /* isExpands */) {
            Select( ident( arg.name ), "size" )
         } else {
            err( "Not yet supported ("+ arg.name + ")" )
         }
      } else {
         ident( arg.name )
      }
   }
}