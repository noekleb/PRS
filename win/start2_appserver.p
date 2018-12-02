/*
    File        : start.w
    Purpose     : Oppstart av SkoTex

    Syntax      : start.w

    Description : Rutinen utf›rer f›lgende:
                    1. Oppstart valg av database og innloggingskontroll.
                    2. Oppstart av prosedurebiblotek.
                    3. Oppstart av ModulMeny - Hovedmeny som benyttes av bruker.
                    4. Nedkj›ring av systemet. Rydder opp og sletter alle
                       aktive (Peristente programmer) f›r sesjonen avsluttes.
                       

    Author(s)   : Tom N›kleby
    Created     : 11/6-98
    Notes       :

	Last change:  TN   22 Aug 101   12:14 pm
*/

DEF INPUT PARAMETER cProgram AS CHAR NO-UNDO.
DEF VAR hProgram AS HANDLE NO-UNDO.
DEF VAR hServer AS HANDLE NO-UNDO.

/* Definerer variabler som benyttes av gamle SkoTex programmer. */
/* {syscom.i " "}. */
{dproclibstart.i} /* SDO Procedurebibliotek            */
{runlib.i}        /* WIN Oppstart av prosedurebiblotek */

&SCOP SilentProc JA
/*
{lng.i &New = "NEW GLOBAL" &NewCode = "ASSIGN wCurrLng = 'DES'."}
*/
    DEF NEW SHARED VAR wCurrLng   AS CHAR   INITIAL "DES" NO-UNDO.
    DEF NEW SHARED VAR wLngHandle AS HANDLE NO-UNDO.
    RUN GetLng IN h_dproclib (OUTPUT wCurrLng).
    RUN GetLngHandle IN h_dproclib (OUTPUT wLngHandle).
/* ------------------*/

IF VALID-HANDLE(h_dproclib) THEN
    RUN SetLng IN h_dproclib (INPUT wCurrLng).

IF NOT VALID-HANDLE(wLngHandle) THEN 
    RUN lng.p PERSISTENT SET wLngHandle.
IF VALID-HANDLE(h_dproclib) THEN
    RUN SetLngHandle IN h_dproclib (INPUT wLngHandle).

/* Initierer clipboard for å ungå feil ved paste av bilde. */
assign
  CLIPBOARD:VALUE = "SkoTex"
  cProgram        = IF cProgram = ""
                      THEN 'w-modul.w'
                      ELSE cProgram
  .

/* Initiering av systemtabeller */
RUN sysinit.p.

/* Setter RETURN til å virke som TAB */
if  VALID-HANDLE(wLibHandle) then
  RUN DataEntryReturn in wLibHandle ("True").

{swn.i}

FUNCTION SjekkForVarsel RETURNS LOGICAL ( ):    
  DEF VAR cAlarmSjekkProgListe AS CHAR NO-UNDO.
  DEF VAR ix          AS INT    NO-UNDO.
  DEF VAR bAlarm      AS LOG    NO-UNDO.

  cAlarmSjekkProgListe = DYNAMIC-FUNCTION("getFieldList","SysGruppe,SysPara;distinct Parameter1;Parameter2",
                                          "WHERE SysHId = 300 AND NOT Beskrivelse BEGINS 'x'"
                                        + ",EACH SysPara OF SysGruppe NO-LOCK"
                                          ).

  DO ix = 1 TO NUM-ENTRIES(cAlarmSjekkProgListe,"|") BY 2:
    IF NOT bAlarm AND SEARCH(ENTRY(ix,cAlarmSjekkProgListe,"|")) NE ? THEN
      bAlarm = DYNAMIC-FUNCTION("runProc",ENTRY(ix + 1,cAlarmSjekkProgListe,"|"),"",?).
    ELSE IF bAlarm THEN LEAVE.
  END.
  RETURN bAlarm.
END FUNCTION.


IF cProgram = 'w-modul.w' THEN
DO:

  DEF VAR hDynMenu    AS HANDLE NO-UNDO.
  DEF VAR iMenuId     AS INT    NO-UNDO.
  DEF VAR bEventMsg   AS LOG    NO-UNDO INIT YES.

/* Appserver connect */
  CREATE SERVER hServer.
  IF ENTRY(1,SESSION:PARAM,";") NE "" THEN
    hServer:CONNECT(ENTRY(1,SESSION:PARAM,";")) NO-ERROR.
  ELSE
    hServer:CONNECT("-H 10.125.250.28 -S 20020 -AppService PRS -DirectConnect") NO-ERROR. 
/*    hServer:CONNECT("-URL http://aia.appfarm.no/aia/Aia?AppService=Sports2000") NO-ERROR. */

  IF hServer:CLIENT-CONNECTION-ID = "" THEN DO:
    MESSAGE PROGRAM-NAME(1) SKIP 
            "Could not connect to Appserver:" SKIP    
            ERROR-STATUS:GET-MESSAGE(1)
            VIEW-AS ALERT-BOX ERROR.
    QUIT.
  END.
  DYNAMIC-FUNCTION("setAppserviceHandle",hServer).
  DYNAMIC-FUNCTION("setAppserviceId",ENTRY(3,hServer:CLIENT-CONNECTION-ID,":")).
  IF ENTRY(1,SESSION:PARAM,";") NE "" THEN
    DYNAMIC-FUNCTION("setAppserviceConnectString","-H aia.appfarm.no -S 31234 -AppService sports2000 -DirectConnect") NO-ERROR.
/* end Appserver */

  RUN JBoxJlwDynMenu.w PERSISTENT SET hDynMenu.
  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainmenuhandle",hDynMenu).

  IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
    iMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).
  ELSE DO:
/*     DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen meny er definert for bruker","","").  */
/*     QUIT. */
  END.

  /* Hvis det skal varsles om noen hendelse: " 
  IF bEventMsg THEN DO:      
    RUN setEventMsgProg  IN hDynMenu ("<program-navn>").
    RUN setViewBtnEvents IN hDynMenu (YES).
  END.
  */

  RUN InitializeObject IN hDynMenu (iMenuId).

  SUBSCRIBE TO "InvalidateHandle" IN hDynMenu.
  /* Alarm for pakkseddel */
  PUBLISH "setEventMsgProg" ("alarmMenu.w").
  SUBSCRIBE TO "getEventMsgProc" ANYWHERE.

  PUBLISH "setViewBtnEvents" (SjekkForVarsel()).
  

/*   IF bMax THEN DO:                                                                                   */
/*     PROCESS EVENTS.                                                                                  */
/*     hDynMenu:CURRENT-WINDOW:WINDOW-STATE = WINDOW-MAXIMIZED.                                         */
/*     DYNAMIC-FUNCTION("setWidgetResize",hDynMenu:CURRENT-WINDOW,hDynMenu:CURRENT-WINDOW,"Resize",""). */
/*   END.                                                                                               */
/*                                                                                                      */
/*   RUN MoveToTop IN hDynMenu.                                                                         */

  WAIT-FOR CLOSE OF THIS-PROCEDURE.
  QUIT.

/*     RUN value(cProgram).                                                                    */
/*     RUN AvsluttSystem.   /* Nedkjøring av systemet. Sletter alle persistente programmer. */ */
/*     quit.        /* Avslutter. */                                                           */
END.
ELSE  DO:
    /* For programmer som kjører AutoLogin */
    RUN value(cProgram) PERSISTENT SET hProgram.
    SUBSCRIBE "InvalidateHandle" IN hProgram.
    /* Alarm for pakkseddel */
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    QUIT.
END.

/* ----------------- Proceduredefinisjoner ----------------------- */
PROCEDURE InvalidateHandle:
    DEF INPUT PARAMETER hProgram AS HANDLE NO-UNDO.
  QUIT.
END.
PROCEDURE AvsluttSystem: /* Avsluttningsprosedure */

  if  VALID-HANDLE(wLibHandle) then
    DO:
      RUN StoppProgramListe in wLibHandle. /* Stopper aktive programmer */
      {stopplib.i &Stopp = "Stopp"}                  /* Stopp av prosedurebiblotek. */
    END.

  DYNAMIC-FUNCTION("ReleaseExcelHandle").
END PROCEDURE.

/* Styring av Alarm. */
PROCEDURE getEventMsgProc:
  DEF OUTPUT param ocEventMsgProg AS CHAR NO-UNDO.

  ocEventMsgProg = "alarmMenu.w".

  PUBLISH "setViewBtnEvents" (SjekkForVarsel()).

END PROCEDURE.

/* ------------------- Procedurer ferdig ------------------------- */

ON 'close':U OF THIS-PROCEDURE
DO:
  QUIT.
END.
