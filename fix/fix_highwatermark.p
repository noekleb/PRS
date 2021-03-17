
/* Progress OpenEdge ABL : DisplayAreaStatus.p
   by Bent Olsby, Bent@prodba.no, Phone +47 93282106 */

CURRENT-WINDOW:WIDTH = 200.
DEF VAR w%Full AS DECIMAL NO-UNDO.

.\ OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + LDBNAME("dictdb") + "_AreaStatus.txt").

FOR EACH _AreaStatus 
  BREAK BY _AreaStatus-TotBlocks DESC BY _AreaStatus-AreaNum 
  WITH TITLE "AreaStatus for " + LDBNAME("DictDb") WIDTH 200:
  w%Full = (_AreaStatus-hiwater / _AreaStatus-totblocks) * 100.
  
  DISPLAY 
      _AreaStatus-AreaNum COLUMN-LABEL "Area#" FORMAT ">>>9"
      _AreaStatus-AreaName COLUMN-LABEL "Name" FORMAT "x(20)"
      w%Full COLUMN-LABEL "% Full"   FORMAT ">>9%"
      _AreaStatus-TotBlocks (TOTAL) COLUMN-LABEL "Tot Blks" FORMAT ">>>>,>>>,>>9"
      _AreaStatus-Extents COLUMN-LABEL "Extents" FORMAT ">>9"
      _AreaStatus-LastExtent COLUMN-LABEL "LastExtent" FORMAT "x(50)"
      _AreaStatus-HiWater COLUMN-LABEL "HWM" FORMAT ">>>>,>>>,>>9"
      _AreaStatus-FreeNum COLUMN-LABEL "Free Chn" FORMAT ">>>,>>9"
      _AreaStatus-RmNum COLUMN-LABEL "RM Chn" FORMAT ">>>,>>9".
  IF w%Full > 80 THEN COLOR DISPLAY messages w%Full.
END.
