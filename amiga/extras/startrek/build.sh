#/bin/sh
if [ -f "startrek.cob" ] ; then
   rm "startrek.cob"
fi
if [ -f "starmain.cob" ] ; then
   rm "starmain.cob"
fi
if [ -f "startrek.coo" ] ; then
   rm "startrek.coo"
fi
if [ -f "starmain.coo" ] ; then
   rm "starmain.coo"
fi
if [ -f "startrek.s" ] ; then
   rm "startrek.s"
fi
if [ -f "startrek.o" ] ; then
   rm "startrek.o"
fi
if [ -f "startrek.68k" ] ; then
   rm "startrek.68k"
fi

cowfe-for-32bita2-with-nncgen -I$COWGOLPATH/rt/ -I$COWGOLPATH/rt/amigacpm/ startrek.cow startrek.cob
cowbe-for-68000-with-nncgen startrek.cob startrek.coo
cowfe-for-32bita2-with-nncgen -I$COWGOLPATH/rt/ -I$COWGOLPATH/rt/amigacpm/ starmain.cow starmain.cob
cowbe-for-68000-with-nncgen starmain.cob starmain.coo
cowlink-for-amigacpm-with-nncgen $COWGOLPATH/.obj/rt/amigacpm/+cowgolcoo/cowgol.coo startrek.coo starmain.coo -o startrek.s
m68k-atari-mint-as startrek.s -o startrek.o 
m68k-atari-mint-ld -T $COWGOLPATH/third_party/amigacpm/amigacpm.ld -o startrek.68k startrek.o
ls -l startrek.68k

