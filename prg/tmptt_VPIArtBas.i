/* Temp table definisjoner for eksterne programmer */

DEFINE {&New} {&Shared} TEMP-TABLE tmptt_VPIArtBas    NO-UNDO LIKE VPIArtBas
  FIELD RecType AS INTEGER
  INDEX RecType RecType.
DEFINE {&New} {&Shared} TEMP-TABLE tt_VPIArtBas       NO-UNDO LIKE VPIArtBas
  FIELD RecType AS INTEGER
  INDEX RecType RecType.


/* Definisjon av shared stream */
&IF '{&New}' <> 'NEW' &THEN          
  DEF {&Shared} STREAM InnFil.
&ELSE

&ENDIF



