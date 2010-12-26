//package de.sciss.synth
//
//class ObjectElem( name: String ) {
//   def apply( code: => Unit ) {}
//}
//
//class DefElem( name: String, args: Seq[ ArgLike ]) {
//   def ===( code: => Unit ) {}
//}
//
//case class Type( name: String )
//
//case class CallElem( name: String, typ: Option[ Type ], args: Seq[ String ])
//
//trait ArgLike {
//   def name: String
//   def typ: Type
//}
//
//case class ValArg( name: String, typ: Type ) extends ArgLike
//case class Arg( name: String, typ: Type ) extends ArgLike
//
//object CodeDSL {
//   def Object( name: String ) : ObjectElem = new ObjectElem( name )
//   def Def( name: String )( args: ArgLike* ) : DefElem = new DefElem( name, args )
//   def Call( name: String, typ: Option[ Type ] = None ) : CallElem = new CallElem( name, typ, Nil )
//   def CallArgs( name: String, typ: Option[ Type ] = None )( args: String* ) : CallElem = new CallElem( name, typ, args )
//}
//
//object CodeTest {
//   import CodeDSL._
//
//   Object( "SinOsc" ) {
//      Def( "ar" )( Arg( "freq", Type( "GE" ))) === CallArgs( "apply", typ = Some( Type( "audio.type"   )))( "freq" )
//      Def( "kr" )( Arg( "freq", Type( "GE" ))) === CallArgs( "apply", typ = Some( Type( "control.type" )))( "freq" )
//   }
//
////   def ar( freq: GE[ AnyUGenIn ]) = apply[ audio.type ](   freq )
////   def kr( freq: GE[ AnyUGenIn ]) = apply[ control.type ]( freq )
//}