/* Temp table definisjoner for eksterne programmer */
DEFINE {&New} {&Shared} TEMP-TABLE tmpTT_DefaultLevDato NO-UNDO LIKE DefaultLevDato
  FIELD RecType AS INT.
DEFINE {&New} {&Shared} TEMP-TABLE TT_DefaultLevDato         NO-UNDO LIKE DefaultLevDato
  FIELD RecType AS INT.

/* Definisjon av shared stream */
&IF '{&New}' <> 'NEW' &THEN          
  DEF {&Shared} STREAM InnFil.
&ELSE

&ENDIF
  