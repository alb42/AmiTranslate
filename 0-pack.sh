#!/bin/bash -e

rm -f package/AmiTranslate/*.uaem

#make catalog
make locale
cp locale/AmiTranslate.cd package/AmiTranslate/Catalogs/AmiTranslate.cd
cp -r Catalogs/* package/AmiTranslate/Catalogs/

# Amiga 68020
echo "compile Amiga 020"
rm -f package/AmiTranslate/AmiTranslate
rm -f package/AmiTranslate$1.lha
fpc4amiga.sh -XX -Xs -CX -O3 -B -FUlib/m68k-amiga AmiTranslate.lpr >msg.log
m68k-amigaos-strip --strip-all AmiTranslate
rm -f package/AmiTranslate/*.res
cp AmiTranslate package/AmiTranslate/
#cp AmiTranslate.guide package/AmiTranslate/
cd package
lha ao5 AmiTranslate$1.lha AmiTranslate/ AmiTranslate.info >>../msg.log
du -h AmiTranslate$1.lha
cd ..

# Amiga 68000
#echo "compile Amiga 000"
#rm -f package/AmiTranslate/AmiTranslate
#rm -f package/AmiTranslate$1_000.lha
#fpc4amiga000.sh -XX -Xs -CX -O3 -B -FUlib/m68k-amiga AmiTranslate.lpr -opackage/AmiTranslate/AmiTranslate >>msg.log
#m68k-amigaos-strip --strip-all package/AmiTranslate/AmiTranslate
#rm -f package/AmiTranslate/*.res
#cd package
#lha ao5 AmiTranslate$1_000.lha AmiTranslate/ AmiTranslate.info >>../msg.log
#du -h AmiTranslate$1_000.lha
#cd ..

# AROS i386
echo "compile AROS i386"
rm -f package/AmiTranslate/AmiTranslate
rm -f package/AmiTranslate$1_AROS.lha
fpc4aros.sh -XX -Xs -CX -O3 -B -FUlib/i386-aros AmiTranslate.lpr -opackage/AmiTranslate/AmiTranslate >>msg.log
rm -f package/AmiTranslate/*.res
cd package
lha ao5 AmiTranslate$1_AROS.lha AmiTranslate/ AmiTranslate.info >>../msg.log
du -h AmiTranslate$1_AROS.lha
cd ..

# AROS x64
echo "compile AROS x64"
rm -f package/AmiTranslate/AmiTranslate
rm -f package/AmiTranslate$1_AROS64.lha
fpc4aros64.sh -XX -Xs -CX -O3 -B -FUlib/x86_64-aros AmiTranslate.lpr -opackage/AmiTranslate/AmiTranslate >>msg.log
rm -f package/AmiTranslate/*.res
cd package
lha ao5 AmiTranslate$1_AROS64.lha AmiTranslate/ AmiTranslate.info >>../msg.log
du -h AmiTranslate$1_AROS64.lha
cd ..

# AROS ARM
echo "compile AROS ARM"
rm -f package/AmiTranslate/AmiTranslate
rm -f package/AmiTranslate$1_AROSARM.lha
fpc4arosarm.sh -XX -Xs -CX -O3 -B -FUlib/arm-aros AmiTranslate.lpr -opackage/AmiTranslate/AmiTranslate >>msg.log
rm -f package/AmiTranslate/*.res
cd package
lha ao5 AmiTranslate$1_AROSARM.lha AmiTranslate/ AmiTranslate.info >>../msg.log
du -h AmiTranslate$1_AROSARM.lha
cd ..

# MorphOS
echo "compile MorphOS"
rm -f package/AmiTranslate/AmiTranslate
rm -f package/AmiTranslate$1_MorphOS.lha
fpc4mos.sh -XX -Xs -CX -O3 -B -FUlib/powerpc-morphos -Fuopenssl3 -Flopenssl3  AmiTranslate.lpr -opackage/AmiTranslate/AmiTranslate >>msg.log
rm -f package/AmiTranslate/*.res
cd package
lha ao5 AmiTranslate$1_MorphOS.lha AmiTranslate/ AmiTranslate.info >>../msg.log
du -h AmiTranslate$1_MorphOS.lha
cd ..

# OS4
#echo "compile OS 4"
#rm -f package/AmiTranslate/AmiTranslate
#rm -f package/AmiTranslate$1_OS4.lha
#fpc4os4.sh -XX -Xs -CX -O3 -B -XV -Avasm -FUlib/powerpc-amiga AmiTranslate.lpr -opackage/AmiTranslate/AmiTranslate >>msg.log
#rm -f package/AmiTranslate/*.res
#cd package
#lha ao5 AmiTranslate$1_OS4.lha AmiTranslate/ AmiTranslate.info >>../msg.log
#du -h AmiTranslate$1_OS4.lha
#cd ..

echo "all done."