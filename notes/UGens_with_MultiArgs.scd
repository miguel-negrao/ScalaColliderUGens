AudioControl
BufWr
Control
Demand
DiskOut
EnvGen
IEnvGen
LagControl
LocalOut
(Mix)
Out
PackFFT
Poll
RecordBuf
XOut

DiskOut.ar( buf, SinOsc.ar( 444 :: 555 :: Nil ))
DiskOut.ar( buf1 :: buf2 :: Nil, SinOsc.ar( 444 :: 555 :: Nil )) ?
DiskOut.ar( buf1 :: buf2 :: Nil, expand SinOsc.ar( 444 :: 555 :: Nil )) ?
DiskOut.ar( buf1 :: buf2 :: Nil, Expand( SinOsc.ar( 444 :: 555 :: Nil ))) ?
DiskOut.ar( buf1 :: buf2 :: Nil, Expand( SinOsc.ar( 444 :: 555 :: Nil ), 1 )) ?
DiskOut.ar( buf1 :: buf2 :: Nil, Expand( SinOsc.ar( 444 :: 555 :: 666 :: 777 :: Nil ), 2 )) ?

Out.ar( idx0 :: idx1 :: idx2 :: Nil, Expand( In.ar( 0, 3 )))

[ 1, 2, 3, 4, 5 ].clump( 1 )

Out.ar( idx0 :: idx1 :: idx2 :: Nil, In.ar( 0, 3 ) expand )
Out.ar( idx0 :: idx1 :: idx2 :: Nil, In.ar( 0, 6 ) expand 2 )
Out.ar( idx0 :: idx1 :: idx2 :: Nil, In.ar( 0, 6 ) clump 2 )

object Expand {
   def none( g: GE ) : Expand  // default (implicit)
   def apply( g: GE, step: Int = 1 ) : Expand
   def iterate( g: GE, start: Int = 0 )( f: Int => Int )
   def tabulate( g: GE )( f: Int => Int )
   def fill( g: GE )( elem: => Int )
}

/// oder viel einfacher?

object DiskOut {
   def ar( buf: GE, multi: => GE ) : DiskOut
}

?
