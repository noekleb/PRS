/*------------------------------------------------------------------------
  File:               lng.i
  Description:        Felles include for språkstyring
  Author:             Sturla Johnsen
  Created:            18.12.98
------------------------------------------------------------------------
	Last change:  SJ   16 Apr 2000    2:38 pm
*/


&IF "{&SDO}" = "" &THEN
  DEF {&NEW} SHARED VAR wCurrLng   AS CHAR INITIAL "DES"  NO-UNDO.
  DEF {&NEW} SHARED VAR wLngHandle AS HANDLE NO-UNDO.
&ELSE
  DEF NEW SHARED VAR wCurrLng   AS CHAR   INITIAL "DES" NO-UNDO.
  DEF NEW SHARED VAR wLngHandle AS HANDLE NO-UNDO.
  RUN GetLng IN h_dproclib (OUTPUT wCurrLng).
  RUN GetLngHandle IN h_dproclib (OUTPUT wLngHandle).
&ENDIF

{&NewCode}

&IF "{&SilentProc}" = "" &THEN
   FUNCTION GetPrgWidget RETURNS WIDGET ():
      IF "{&PROCEDURE-TYPE}" MATCHES ("*WINDOW*") THEN 
           RETURN {&WINDOW-NAME}.
      ELSE RETURN FRAME {&FRAME-NAME}:HANDLE.
   END FUNCTION.

   FUNCTION Tx RETURNS CHARACTER (INPUT wTxt AS CHARACTER, INPUT wTxNr AS INTEGER):
      DEF VAR i AS INTE NO-UNDO.
      DO i = 1 TO 2:
         FIND Tekst WHERE Tekst.PrgNavn = (IF i = 1 THEN ENTRY(1,THIS-PROCEDURE:FILE-NAME,".") ELSE "") AND
                          Tekst.TxtNr   = wTxNr    AND
                          Tekst.Lng     = wCurrLng NO-LOCK NO-ERROR.
         IF AVAIL Tekst THEN LEAVE.
      END.
      RETURN IF AVAIL Tekst THEN Tekst.Tekst ELSE wTxt.
   END FUNCTION.
   /* om SmartDialog skall 'RUN SwitchLng' in före RUN SUPER i initializeObject*/
/*    &IF NOT "{&PROCEDURE-TYPE}" = "SmartDialog" &THEN */
   &IF NOT "{&PROCEDURE-TYPE}" = "SmartDialog" &THEN
       RUN SwitchLng.
   &ENDIF

   ON "ALT-1" of &IF "{&PROCEDURE-TYPE}" matches "*DIALOG*"
                   &THEN frame {&FRAME-NAME}
                   &ELSE current-window
                 &ENDIF ANYWHERE DO:
      {&PreALT-<}
      RUN d-blng.w(THIS-PROCEDURE:FILE-NAME,GetPrgWidget()).
      {&PostALT-<}
   END.

   /* Språkkode skal ikke kunne byttes online. Det gir for mange
      problemer.
   ON "ALT-2" of &IF "{&PROCEDURE-TYPE}" matches "*DIALOG*"
                   &THEN frame {&FRAME-NAME}
                   &ELSE current-window
                 &ENDIF ANYWHERE DO:
      IF THIS-PROCEDURE:PERSISTENT THEN DO:
         {&PreALT->}
         RUN d-clng.w.
         {&PostALT->}
      END.   
   END.
   */

   ON "ALT-2" of &IF "{&PROCEDURE-TYPE}" matches "*DIALOG*"
                   &THEN frame {&FRAME-NAME}
                   &ELSE current-window
                 &ENDIF ANYWHERE DO:
      {&PreALT-|}
      RUN d-btekst.w(ENTRY(1,THIS-PROCEDURE:FILE-NAME,".")).
      {&PostALT-|}
   END.

   PROCEDURE SwitchLng:
      IF wCurrLng <> "DES" THEN DO:

         {&PreLng}

         IF NOT VALID-HANDLE(wLngHandle) THEN 
             RUN lng.p PERSISTENT SET wLngHandle.

         RUN GetLng IN wLngHandle ("",THIS-PROCEDURE:FILE-NAME,GetPrgWidget()).
         
         /* For alt som lng.p ikke oversetter */
         RUN Lng IN THIS-PROCEDURE NO-ERROR. 

         {&PostLng}
     END.
   END PROCEDURE.
&ENDIF

&IF "{&PROCEDURE-TYPE}" matches "*DIALOG*" &THEN
   DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
   {incl/frametrigg.i}
&ELSE
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
	{incl/wintrigg.i}
&ENDIF
