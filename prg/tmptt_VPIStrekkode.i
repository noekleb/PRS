/* Temp table definisjoner for eksterne programmer */

DEFINE {&New} {&Shared} TEMP-TABLE tmptt_VPIStrekkode NO-UNDO LIKE VPIStrekkode
  FIELD RecType AS INTEGER
  INDEX Strekkode RecType EkstVPILevNr VareNr.
DEFINE {&New} {&Shared} TEMP-TABLE tt_VPIStrekkode    NO-UNDO LIKE VPISTrekkode
  FIELD RecType AS INTEGER
  INDEX Strekkode RecType EkstVPILevNr VareNr.

/* Definisjon av shared stream */
&IF '{&New}' <> 'NEW' &THEN          
  DEF {&Shared} STREAM InnFil.
&ELSE

&ENDIF
