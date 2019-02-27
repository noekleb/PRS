/*
  Name:        meny.i
  Purpose:     Nødvendig kode for å starte et program i menyen
               og legge til meny i et vindu eller som popup-meny.
  Parameters:  Se prosedyrer
  Author    :  Sturla Johnsen, juni 1998
  Notes:

    3/7-98      TN      Lagt inn håndtering av parameter for persistent-run.

	Last change:  TN   16 Dec 99    0:08 am
*/


PROCEDURE Avslutt:
  DEF INPUT PARAMETER parInnData as CHAR NO-UNDO.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE AddMeny:
   DEF INPUT PARAMETER wh        AS WIDGET NO-UNDO.
   DEF INPUT PARAMETER wMenyNavn AS CHAR   NO-UNDO.
   IF wMenyNavn = ? THEN DO:
      IF wh:TYPE = "WINDOW" THEN
            ASSIGN wh:MENU-BAR   = ?.
      ELSE  ASSIGN wh:POPUP-MENU = ?.
      RETURN NO-APPLY.
   END.
   FIND Meny WHERE Meny.Navn = wMenyNavn NO-LOCK NO-ERROR.
   IF NOT AVAIL Meny THEN DO:
      MESSAGE 'Finner ikke menyen "' + TRIM(wMenyNavn) + '".'
         VIEW-AS ALERT-BOX WARNING TITLE "Advarsel".
      RETURN NO-APPLY.
   END.
   IF CURRENT-WINDOW:MENU-BAR <> ? then
     RUN lagmeny.p(Meny.MData,CURRENT-WINDOW:MENU-BAR,THIS-PROCEDURE).
   else
     RUN lagmeny.p(Meny.MData,wh,THIS-PROCEDURE).
END PROCEDURE.

PROCEDURE WinExec EXTERNAL "kernel32.dll":
   DEF INPUT PARAMETER wProg AS CHAR.
   DEF INPUT PARAMETER wPres AS SHORT.
END PROCEDURE. 

PROCEDURE StartMenyProgram:
  DEF INPUT PARAMETER wProgram    AS CHAR   NO-UNDO.
  DEF INPUT PARAMETER wItemHandle AS WIDGET NO-UNDO.

  DEF VAR wPara        AS CHAR   NO-UNDO.
  DEF VAR wMsgPara     AS CHAR   NO-UNDO.
  DEF VAR wPrivData    AS CHAr   NO-UNDO.
  DEF VAR wSvar        AS LOGI   NO-UNDO.
  DEF VAR wLib         AS LOGI   NO-UNDO.
  DEF VAR wLok         AS LOGI   NO-UNDO.
  DEF VAR wPersistent  as LOGI   NO-UNDO.
  DEF VAR wSendParam   as LOGI   NO-UNDO.
  DEF VAR wWinH        AS HANDLE NO-UNDO.
  DEF VAR wMain        AS CHAR   NO-UNDO.
  DEF VAR wExt         AS CHAR   NO-UNDO.
  DEF VAR ipProgHandle as HANDLE NO-UNDO.

  /* Sjekker om det er en aktiv transaksjon i et annet vindu, eller i  */
  /* det aktive vinduet.                                               */
  if TRANSACTION then
   do:
     message "Det er en aktiv transaksjon i et av de åpne vinduene." skip
             "Nye programmer kan ikke startes så lenge det er en   " skip
             "aktiv transaksjon." view-as alert-box WARNING Title "Feil ved åpning av program".
     return no-apply.
   end.
  
  assign
    wLib = false
    wLok = FALSE.

  IF wItemHandle:TOGGLE-BOX THEN
      ASSIGN wMsgPara = CAPS(TRIM(STRING(wItemHandle:CHECKED))).
  IF wItemHandle:PRIVATE-DATA <> "" THEN DO:
     ASSIGN
       wPrivData   = ENTRY(4,wItemHandle:PRIVATE-DATA,";")
       wLib        = if ENTRY(2,wItemHandle:PRIVATE-DATA,";") = "Lib" THEN TRUE ELSE false
       wLok        = if ENTRY(3,wItemHandle:PRIVATE-DATA,";") = "Lok" THEN TRUE ELSE FALSE
       wPersistent = if ENTRY(5,wItemHandle:PRIVATE-DATA,";") = "YES" THEN TRUE ELSE FALSE.
       wSendParam  = if ENTRY(6,wItemHandle:PRIVATE-DATA,";") = "YES" THEN TRUE ELSE FALSE.

   IF wPrivData <> "" THEN
      ASSIGN wMsgPara = wMsgPara +
                        (IF wMsgPara <> "" THEN ", " ELSE "") + '"' +
                        wPrivData + '"'
             wPara    = wPrivData.

  END.
  {&PreRun}
  RunProg: DO:
     IF wProgram = "" THEN DO:
        MESSAGE "Programnavn er ikke oppgitt i menyen, kan ikke kjøre."
           VIEW-AS ALERT-BOX ERROR TITLE "Feil".
        RETURN NO-APPLY.   
     END.
     
     IF R-INDEX(wProgram,".") > 0 THEN 
          ASSIGN wExt  = SUBSTR(wProgram,R-INDEX(wProgram,".") + 1)
                 wMain = SUBSTR(wProgram,1,LENGTH(wProgram) - (LENGTH(wExt) + 1)).
     ELSE ASSIGN wMain = wProgram.       

     IF wProgram = "d-hjelp.w" THEN
     DO:
         if VALID-HANDLE(wLibHandle) then
           RUN CtrlHjelp IN wLibHandle.

         RETURN NO-APPLY.
     END
         .
     /* Starter progress rutiner. */
     IF wExt <> "EXE" THEN DO:
        /* Sjekker at programmet finnes. */
        IF ENTRY(2,wItemHandle:PRIVATE-DATA,";") = "" and
           ENTRY(3,wItemHandle:PRIVATE-DATA,";") = "" and
           SEARCH(wMain + ".r") = ? AND
           SEARCH(wMain + ".w") = ? AND
           SEARCH(wMain + ".p") = ? 
        THEN DO:
           MESSAGE 'Finner ikke programmet "' + wProgram + '".' SKIP(1)
                   'Kan ikke utføre menyvalget.'
              VIEW-AS ALERT-BOX ERROR TITLE "Feil".
           RETURN NO-APPLY.   
        END.

        /* Oppretter vindu hvis dette er satt i menyen. */
        IF ENTRY(1,wItemHandle:PRIVATE-DATA,";") BEGINS("<LagVindu>") THEN DO:
          if wPersistent then
            RUN w-nyvin.w PERSISTENT SET wWinH  (wProgram, wPara).
          ELSE
            RUN w-nyvin.w (wProgram, wPara).

           IF VALID-HANDLE(wWinH) THEN DO:
              ASSIGN CURRENT-WINDOW = wWinh:CURRENT-WINDOW.
              APPLY "ENTRY" TO CURRENT-WINDOW.
           END.

           RETURN NO-APPLY.
        END.

        /* Starter programmvalget */

        /* Menyvalget er en sjekkboks. */
        IF wItemHandle:TOGGLE-BOX THEN
             RUN VALUE(wProgram) PERSISTENT (wItemHandle:CHECKED,wPara).

        /* Menyvalget er en intern procedure som skal kjøres lokalt. */
        ELSE IF wLok then
          DO:
            if wSendParam then
              RUN VALUE(wProgram) in THIS-PROCEDURE (wPara).
            ELSE
              RUN VALUE(wProgram) in THIS-PROCEDURE.
          END.
        /* Menyvalget er en intern prosedyre som skal kjøres i prosedyrebibloteket. */
        ELSE IF wLib then
          DO:
            if wSendParam then
              RUN VALUE(wProgram) in wLibHandle (wPara).
            ELSE
              RUN VALUE(wProgram) in wLibHandle.
          END.

        /* Menyvalget er et GUI program som skal startes. */
        ELSE DO:
          if wSendParam THEN
            DO:
              if wPersistent then
                DO:
                  RUN VALUE(wProgram) PERSISTENT set ipProgHandle (wPara).
                  /* Hvis det er et adm object skal dette gj›res. */
                  if VALID-HANDLE(ipProgHandle) THEN DO:
                      RUN initializeObject in ipProgHandle NO-ERROR.
                      RUN MoveToTop in ipProgHandle NO-ERROR.
                  END.
                  IF VALID-HANDLE(ipProgHandle) THEN
                     run OpprettProgramListe in wLibHandle (ipProgHandle, ?, ?).
                END.
              else
                RUN VALUE(wProgram) (wPara).
            END.
          ELSE DO:
            if wPersistent then
              DO:
                RUN VALUE(wProgram) PERSISTENT set ipProgHandle.
                /* Hvis det er et adm object skal dette gj›res. */
                if VALID-HANDLE(ipProgHandle) THEN DO:
                    RUN initializeObject in ipProgHandle NO-ERROR.
                    RUN MoveToTop in ipProgHandle NO-ERROR.
                END.
                IF VALID-HANDLE(ipProgHandle) THEN
                   run OpprettProgramListe in wLibHandle (ipProgHandle, ?, ?).
              END.
            else
              RUN VALUE(wProgram).
          END.
        END.

        /* Lukker CHAR vindu. */
        IF VALID-HANDLE(wWinH) THEN APPLY "CLOSE" TO wWinh.
     END.

     /* Starter Windows programm. */
     ELSE RUN WinExec(wProgram,1). /* 1=Normalt vindu, 2=Minimert */
  END.
END PROCEDURE.

