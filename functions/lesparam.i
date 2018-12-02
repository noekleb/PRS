FUNCTION LesParam RETURNS CHARACTER
  ( INPUT wPStr AS CHARACTER, INPUT wParam AS CHARACTER, INPUT wDelimiter AS CHARACTER):
/*------------------------------------------------------------------------------
  Forfatter: SJ, Skog-Data AS
     Formål: Henter ut en parameterverdi fra en liste hvor verdiene er adskilt
             med et angitt tegn.
    Notater: Bruk: Verdi = LesParam(Parameterstreng,Pameter,Skilletegn).
             Parameterstreng forutsettes bygget opp med slik:
                 param1=verdi1;param2=verdi2 osv.
                 Både param og verdi kan være sammensatt av mer enn ett ord,
                 men det må ikke være blanke før eller etter liketstegnet.
             Default skilletegn er pipetegn.
  Endringer: 03.02.98 Lagt inn skilletegn som parameter
------------------------------------------------------------------------------*/
   DEF VAR wTestStr AS CHAR NO-UNDO.
   DEF VAR i        AS INTE NO-UNDO.

   ASSIGN 
      wParam     = wParam + "="
      wTestStr   = wPStr
      wDelimiter = IF wDelimiter = "" THEN "|" ELSE wDelimiter.
   
   DO WHILE TRUE:
      ASSIGN i = INDEX(wTestStr,wParam).
      IF i > 1 AND SUBSTR(wTestStr,i - 1,1) <> wDelimiter THEN
           ASSIGN wTestStr = SUBSTR(wTestStr,i + 1).
      ELSE LEAVE. 
   END.  

   ASSIGN
      i = IF i > 0 AND LOOKUP(wParam,SUBSTR(wTestStr,1,i + LENGTH(wParam) - 1),wDelimiter) > 0
          THEN LOOKUP(wParam,SUBSTR(wTestStr,1,i + LENGTH(wParam) - 1),wDelimiter)
          ELSE 0.

   RETURN IF i > 0 THEN ENTRY(2,ENTRY(i,wTestStr,wDelimiter),"=") ELSE "?".

END FUNCTION.
