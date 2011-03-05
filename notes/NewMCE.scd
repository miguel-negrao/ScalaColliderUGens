unbubble

[[[1],[2]], [3]].unbubble(1).unbubble(1)
[[[1],[2]], [[3]]].unbubble(1).unbubble(1).unbubble(1).unbubble(1)

SynthDef( \mc, { DiskOut.ar( 0, SinOsc.ar([[11,22],[33,44]]))}).writeDefFile( "/Users/hhrutz/Desktop/" )

SynthDef( \mc2, { DiskOut.ar( [5,6], SinOsc.ar([[11,22],[33,44]]))}).writeDefFile( "/Users/hhrutz/Desktop/" )

SynthDef( \mc3, { DiskOut.ar( [5,6], SinOsc.ar([11,22,33,44]))}).writeDefFile( "/Users/hhrutz/Desktop/" )

SynthDef( \mc3b, { DiskOut.ar( [5,6], SinOsc.ar([[11,22,33,44]]))}).writeDefFile( "/Users/hhrutz/Desktop/" )

SinOsc.ar([11,22,33,44])
SinOsc.ar([[11,22,33,44]])

SynthDef( \mc4, { DiskOut.ar( [5,6], SinOsc.ar([[[11,22],[33,44]]]))}).writeDefFile( "/Users/hhrutz/Desktop/" )

SynthDef( \mc4, { DiskOut.ar( [5,6], SinOsc.ar([11,22,33,44]))}).writeDefFile( "/Users/hhrutz/Desktop/" )

SinOsc.ar([[[11,22],[33,44]]])
DiskOut.ar([5, 6], [[[1,2],[3,4]]])

SynthDef( \mc4c, { Out.ar([7,8], DiskOut.ar([5,6], SinOsc.ar([[[11,22],[33,44]]])))}).writeDefFile( "/Users/hhrutz/Desktop/" )

SynthDef( \mc6, { Out.ar([7,8], DiskOut.ar([5,6], Pan2.ar( SinOsc.ar([[[11],22]]), [9,10])))}).writeDefFile( "/Users/hhrutz/Desktop/" )

Out( Seq(7, 8), DiskOut( Seq(5, 6), SinOsc(Seq(Seq(Seq(1, 2), Seq(3, 4))))))


///

sealed trait UGenInLike {
   def ungroup: IIdxSeq[ UGenInLike ]
   def numElements: Int
   def elementAt( i: Int )
}

sealed trait UGenIn extends UGenInLike ...
sealed trait UGenInGroup extends UGenInLike ...
