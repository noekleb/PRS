/* Temp table definisjoner for eksterne programmer */
DEFINE {&New} {&Shared} TEMP-TABLE TT_Anv-Kod    NO-UNDO LIKE Anv-Kod
  FIELD RecType AS INT.
DEFINE {&New} {&Shared} TEMP-TABLE tmpTT_Anv-Kod NO-UNDO LIKE Anv-Kod
  FIELD RecType AS INT.

/* Definisjon av shared stream */
&IF '{&New}' <> 'NEW' &THEN          
  DEF {&Shared} STREAM InnFil.
&ELSE

&ENDIF
  