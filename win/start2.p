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
DEF VAR hWaitForProc AS HANDLE NO-UNDO.
DEF VAR hSuperBibl   AS HANDLE NO-UNDO.

DEF VAR hDynMenu       AS HANDLE NO-UNDO.
DEF VAR iMenuId        AS INT    NO-UNDO.
DEF VAR bEventMsg      AS LOG    NO-UNDO INIT YES.
DEF VAR cMainMenuType  AS CHAR   NO-UNDO.

SESSION:SUPPRESS-WARNINGS = YES.

/* Definerer variabler som benyttes av gamle SkoTex programmer. */
/* {syscom.i " "}. */
{dproclibstart.i} /* SDO Procedurebibliotek            */
{runlib.i}        /* WIN Oppstart av prosedurebiblotek */

/* Starter funksjonsbibliotek. */
RUN superBibl.p PERSIST SET hSuperBibl.
SESSION:ADD-SUPER-PROC(hSuperBibl).

&SCOP SilentProc JA
/*
{lng.i &New = "NEW GLOBAL" &NewCode = "ASSIGN wCurrLng = 'DES'."}
*/
    DEF NEW SHARED VAR wCurrLng   AS CHAR   INITIAL "DES" NO-UNDO.
    DEF NEW SHARED VAR wLngHandle AS HANDLE NO-UNDO.
    RUN GetLng IN h_dproclib (OUTPUT wCurrLng).
    RUN GetLngHandle IN h_dproclib (OUTPUT wLngHandle).
/* ------------------*/

FIND Bruker NO-LOCK WHERE
    Bruker.BrukerId = USERID("SkoTex") NO-ERROR.
IF NOT CAN-FIND(Sprak WHERE sprak.lng = Bruker.lng) THEN
    wCurrLng = "DES".
 ELSE wCurrLng = Bruker.lng.

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

IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN DO:
  iMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).
  
/*   iMenuId = 376. /* (liten butikkmeny) */  */

  cMainMenuType = DYNAMIC-FUNCTION("getFieldValues","JBoxMenu","WHERE iJBoxMenuId = " + STRING(iMenuId),"cLaunchType").
END.


IF cMainMenuType NE "ribbon" THEN DO:
/* IF cProgram = 'w-modul.w' THEN DO: */

  IF cMainMenuType = "plain" THEN
    RUN JBoxDynMenu.w PERSISTENT SET hDynMenu.
  ELSE 
    RUN JBoxJlwDynMenu.w PERSISTENT SET hDynMenu.
  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainmenuhandle",hDynMenu).

/*   IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN */
/*     iMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).           */
/*   ELSE DO:                                                              */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen meny er definert for bruker","","").  */
/*     QUIT. */
/*   END. */

  /* Hvis det skal varsles om noen hendelse: " 
  IF bEventMsg THEN DO:      
    RUN setEventMsgProg  IN hDynMenu ("<program-navn>").
    RUN setViewBtnEvents IN hDynMenu (YES).
  END.
  */

  CASE cMainMenuType:
    WHEN "buttonpanel" THEN DYNAMIC-FUNCTION("setMenuStyle" IN hDynMenu,0).
    WHEN "nav-bar"     THEN DYNAMIC-FUNCTION("setMenuStyle" IN hDynMenu,1).
    WHEN "treeview"    THEN DYNAMIC-FUNCTION("setMenuStyle" IN hDynMenu,2).
  END CASE.

  RUN InitializeObject IN hDynMenu (iMenuId).

  SUBSCRIBE TO "InvalidateHandle" IN hDynMenu.

  /* 12/10-15 TN Blokkerer ut varsel på pakkseddel for å unngå feilmelding hos Gant
  IF cMainMenuType NE "plain" THEN DO:

  /* Alarm for pakkseddel */
    PUBLISH "setEventMsgProg" ("alarmMenu.w").
    SUBSCRIBE TO "getEventMsgProc" ANYWHERE.
  /*   PUBLISH "setViewBtnEvents" (SjekkForVarsel()). */
    
    DYNAMIC-FUNCTION("setTimerProperties" IN hDynMenu,
                     30000, /* intervall (millisek) */
                     "",    /* parameter til serverprosedyre */
                     "sjekk_for_varsel.p", /* serverprosedyre */
                     "setViewBtnEvents"    /* klientprosedyre  - her internt i menyen */
                     ).
  END.
  ELSE 
  ----- TN Slutt */ 
  hDynMenu:CURRENT-WINDOW:HIDDEN = NO.                                                                         

/*   IF bMax THEN DO:                                                                                   */
/*     PROCESS EVENTS.                                                                                  */
/*     hDynMenu:CURRENT-WINDOW:WINDOW-STATE = WINDOW-MAXIMIZED.                                         */
/*     DYNAMIC-FUNCTION("setWidgetResize",hDynMenu:CURRENT-WINDOW,hDynMenu:CURRENT-WINDOW,"Resize",""). */
/*   END.                                                                                               */
/*                                                                                                      */

  IF DYNAMIC-FUNCTION("getAttribute",SESSION,"useAdvGui") = "yes" THEN
    WAIT-FOR System.Windows.Forms.Application:Run().
  ELSE 
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  QUIT.

/*     RUN value(cProgram).                                                                    */
/*     RUN AvsluttSystem.   /* Nedkjøring av systemet. Sletter alle persistente programmer. */ */
/*     quit.        /* Avslutter. */                                                           */
END.

ELSE IF cProgram = "Ribbon" OR cMainMenuType = "ribbon" THEN DO:

  IF SEARCH("controls.dll") NE ? THEN
    DYNAMIC-FUNCTION("setAttribute",SESSION,"tabfolderprog","JBoxJLWtabFolder.w").

  RUN JBoxRibbonMenu.p PERSISTENT SET hDynMenu.
  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainmenuhandle",hDynMenu).

/*   IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN */
/*     iMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).           */
/*   ELSE DO:                                                              */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen meny er definert for bruker","","").  */
/*     QUIT. */
/*   END.         */
/*                */
/*   iMenuId = 1. */

  DYNAMIC-FUNCTION("setClientSize" IN hDynMenu,1280,900).
  DYNAMIC-FUNCTION("setCreateStatusBar" IN hDynMenu,NO).

  RUN InitializeObject IN hDynMenu (iMenuId).

  /* Alarm for pakkseddel */
  PUBLISH "setEventMsgProg" ("alarmMenu.w","Ventende oppgaver").
/*   SUBSCRIBE TO "getEventMsgProc" ANYWHERE. */
/*   PUBLISH "setViewBtnEvents" (SjekkForVarsel()). */
  DYNAMIC-FUNCTION("setTimerProperties" IN hDynMenu,
                   30000, /* intervall (millisek) */
                   "",    /* parameter til serverprosedyre */
                   "sjekk_for_varsel.p", /* serverprosedyre */
                   "setViewBtnEvents"    /* klientprosedyre  - her internt i menyen */
                   ).


  SUBSCRIBE TO "InvalidateHandle" IN hDynMenu.

  DYNAMIC-FUNCTION("setWindowTitle" IN hDynMenu,"InfoPos SE [" + DYNAMIC-FUNCTION("getCompanyName") + "]").
  DYNAMIC-FUNCTION("setStatusText" IN hDynMenu,DYNAMIC-FUNCTION("getASuserName")).
  DYNAMIC-FUNCTION("setWinMenuGroupLabel" IN hDynMenu,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Behandle" ELSE "Actions").
  DYNAMIC-FUNCTION("setAppStyleSheet" IN hDynMenu,"vs2008_test.isl").
  RUN JBoxAppStart_oo.p PERSIST SET hWaitForProc. 

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

/*   PUBLISH "setViewBtnEvents" (SjekkForVarsel()).  */

END PROCEDURE.

PROCEDURE RestartMenu: 
  DEF INPUT PARAM iiRestartMenu AS INT NO-UNDO.

  DYNAMIC-FUNCTION("msetAttribute",SESSION:FIRST-PROCEDURE,"*","").

  RUN JBoxRibbonMenu.p PERSISTENT SET hDynMenu.
  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainmenuhandle",hDynMenu).

  DYNAMIC-FUNCTION("setClientSize" IN hDynMenu,1280,900).
  DYNAMIC-FUNCTION("setCreateStatusBar" IN hDynMenu,NO).

  RUN InitializeObject IN hDynMenu (iiRestartMenu).

  SUBSCRIBE TO "InvalidateHandle" IN hDynMenu.

  DYNAMIC-FUNCTION("setWindowTitle" IN hDynMenu,"InfoPos SE [" + DYNAMIC-FUNCTION("getCompanyName") + "]").
  DYNAMIC-FUNCTION("setStatusText" IN hDynMenu,DYNAMIC-FUNCTION("getASuserName")).
  DYNAMIC-FUNCTION("setWinMenuGroupLabel" IN hDynMenu,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Behandle" ELSE "Actions").
  DYNAMIC-FUNCTION("setAppStyleSheet" IN hDynMenu,"vs2008_test.isl").

END.

/* ------------------- Procedurer ferdig ------------------------- */

ON 'close':U OF THIS-PROCEDURE
DO:
  QUIT.
END.
