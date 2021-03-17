
CURRENT-WINDOW:HEIGHT = 20.
CURRENT-WINDOW:WIDTH = 350.

/* how much of the TYPE I Storage Area is used up by allocated blocks (HWM)? 
What is the true size of the Database? 
?How fast is my database growing? iow: What is the real occupation? */

define variable lprcnt_full as decimal format ">>9.99" label "% Full" no-undo.
define variable lempty_blocks as decimal format ">>,>>>,>>9" label "Empty" no-undo.
define variable lhiwater as decimal format ">>,>>>,>>9" label "Hiwater" no-undo.
define variable lmb_used as decimal format ">>>,>>9.99" label "MB Used" no-undo.
define variable lmb_avail as decimal format ">>>,>>9.99" label "MB Avail" no-undo.
define variable lmb_tused as decimal format ">>>,>>9.99" label "Total MB used" initial 0.0 no-undo.
define variable lmb_tavail as decimal format ">>>,>>9.99" label "Total MB avail" initial 0.0 no-undo.
DEF VAR iLedigeEkstenter AS INT FORMAT ">>9" NO-UNDO.
DEF VAR bFixed AS LOG NO-UNDO.
DEF VAR lFillimit AS DEC FORMAT ">>>,>>>,>>>,>>9" NO-UNDO.
/* output to sa.xml. */

ASSIGN 
    lFilLimit = 1600000000
    .

FOR EACH _Area NO-LOCK:
 IF _Area-name = 'control area' THEN
     NEXT.
 FIND _Areastatus WHERE 
     _Areastatus-Areanum = _Area._Area-number NO-LOCK.
 FIND _AreaThreshold WHERE 
     _AreaThreshold._AreaThresholdArea = _AreaStatus-AreaNum NO-ERROR.

 lhiwater = _AreaStatus-Hiwater.
 if lhiwater = ? then lhiwater = 0.0.

 lempty_blocks = _AreaStatus-Totblocks - lhiwater - _AreaStatus-Extents.

 lprcnt_full = (1.0 - (lempty_blocks / _AreaStatus-Totblocks)) * 100.0.
 
 lmb_avail = lempty_blocks / 1048576 * _Area-BlockSize.
 lmb_tavail = lmb_tavail + lmb_avail.
     
 lmb_used = lhiwater / 1048576 * _Area-BlockSize.
 lmb_tused = lmb_tused + lmb_used.

 iLedigeekstenter = 0.
 ASSIGN 
     iLedigeEkstenter = _AreaStatus-Extents - INT(REPLACE(REPLACE(ENTRY(2,_AreaStatus-Lastextent,'.'),'d',''),'b',''))
     NO-ERROR.
 IF ERROR-STATUS:ERROR THEN 
     iLedigeEkstenter =  ?.

 FILE-INFO:FILE-NAME = _AreaStatus-Lastextent.

 FIND LAST _FileList NO-LOCK WHERE 
     _fileList._FileList-Name = _AreaStatus-Lastextent no-error.
 
 /*
    - Er ledige ekstenter > 0 skal det ikke varsles.
    - Er ledige ekstenter = 0
        - Er %Full >= 98, skal filstørrelse på disk sjekkes.
            - Er denne større enn 1,6GB skal >80% varsel sendes.
        - Er %full >= 80% skal det varsles.
 */

 DISPLAY
   DBNAME FORMAT "x(20)"
   _Area-name LABEL 'Name' format "x(21)"
   _AreaStatus-AreaNum COLUMN-LABEL "Area#" FORMAT ">>>9"
   _Area-blocksize LABEL 'DBBlockSize'
   _AreaStatus-Extents LABEL '#Extents' format ">>9" 
   iLedigeEkstenter LABEL '#LedigeEkst'
   _AreaThreshold._AreaThreshold LABEL 'Treshold'
   _AreaStatus-Lastextent LABEL "HWM extent" 
   lprcnt_full 
   lhiwater
   lempty_blocks
   _AreaStatus-Totblocks - _AreaStatus-Extents LABEL 'T.Blocks' FORMAT ">>,>>>,>>9"
   lprcnt_full 
   /*_FileList._FileList-size WHEN AVAILABLE _FileList FORMAT ">>,>>>,>>9"*/
   FILE-INFO:FILE-SIZE  FORMAT ">>,>>>,>>>,>>9" LABEL 'Filstørrelse disk'
   lmb_used
   lmb_avail WITH WIDTH 350.

END.

display lmb_tused
    lmb_tavail.

