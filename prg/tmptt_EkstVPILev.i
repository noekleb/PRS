/* Temp table definisjoner for eksterne programmer */

DEFINE {&New} {&Shared} TEMP-TABLE tmptt_EkstVPILev    NO-UNDO LIKE EkstVPILev
  FIELD RecType AS INT.
DEFINE {&New} {&Shared} TEMP-TABLE tt_EkstVPILev       NO-UNDO LIKE EkstVPILev
  FIELD RecType AS INT.


/* Definisjon av shared stream */
&IF '{&New}' <> 'NEW' &THEN          
  DEF {&Shared} STREAM InnFil.
&ELSE

&ENDIF



