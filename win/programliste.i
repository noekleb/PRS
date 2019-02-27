/************************************************************
    Program:  programliste.i
    Created:  TN   12 Nov 98
Description:  Include som benyttes i p-biblo.p
              (Prosedyrebibliteket).
              Inneholder rutiner for håndtering av programlisten.

Last change:  TN   14 Jan 1999    3:04 pm
************************************************************/

DEF TEMP-TABLE ProgramListe
  FIELD ProgramHandle as handle
  FIELD WindowHandle  as handle
  field ProgramNavn   as char
  FIELD Dato          as date
  FIELD Tid           as INT.

PROCEDURE OpprettProgramListe:
  DEF INPUT PARAMETER ipProgramHandle as handle NO-UNDO.
  DEF INPUT PARAMETER ipWindowHandle  as HANDLE NO-UNDO.
  DEF INPUT PARAMETER ipInnData as CHAR NO-UNDO.

  /* Logger kun de programmer som virkelig er startet. */
  if NOT VALID-HANDLE(ipProgramHandle) then
    RETURN.

  ELSE
    DO:
      FIND FIRST ProgramListe NO-LOCK where
        ProgramListe.ProgramHandle = ipProgramHandle NO-ERROR.

      if NOT available ProgramListe then
         DO:
           CREATE ProgramListe.
           ASSIGN
             ProgramListe.ProgramHandle = ipProgramHandle
             ProgramListe.ProgramNavn   = ENTRY(1,ipInnData)
             ProgramListe.WindowHandle  = ipWindowHandle
             ProgramListe.Dato          = today
             ProgramListe.Tid           = TIME.
         END.
    END.
END.

PROCEDURE SlettProgramListe:
  DEF INPUT PARAMETER ipProgramHandle as handle NO-UNDO.
  DEF INPUT PARAMETER ipWindowHandle  as HANDLE NO-UNDO.
  DEF INPUT PARAMETER ipInnData as CHAR NO-UNDO.

  /* Logger kun de programmer som virkelig er startet. */
  if NOT VALID-HANDLE(ipProgramHandle) then
    RETURN "AVBRUDD".

  ELSE
    DO:
      FIND FIRST ProgramListe NO-LOCK where
        ProgramListe.ProgramHandle = ipProgramHandle NO-ERROR.

      if AVAILABLE ProgramListe then
        DELETE ProgramListe.
      RETURN "OK".
    END.
END.

PROCEDURE VisPrgLst:
  DEF VAR wLogFil     as CHAR INITIAL ".\kom\log\prglst.log".
  DEF VAR wLogKatalog as CHAR NO-UNDO.
  DEF VAR wDatoTid    as CHAR NO-UNDO.
  DEFINE VARIABLE cTmpnamn AS CHARACTER   NO-UNDO.

  /* Filnavn */
  assign
    wLogFil  = "prglstDDMMAAHHMM.log"
    wDatoTid = string(day(today),"99") +
               string(month(today),"99") +
               substring(string(year(today),"9999"),3,2) +
               substring(string(time,"HH:MM"),1,2) +
               substring(string(time,"HH:MM"),4,2).

  /* Setter inn dato og klokkeslett i filnavnet. */
  OVERLAY(wLogFil, index(wLogFil,"DDMMAAHHMM"), 10, "CHARACTER") = wDatoTid.

  /* Katalognavn */
  if available SysPara then release SysPara.
  if opsys = "unix"
    then {syspar2.i 50 1 100 wLogKatalog}
  else {syspara.i 50 1 100 wLogKatalog}

  if wLogKatalog = "" then
    wLogKatalog = if opsys = "unix" then "." else ".".
  if substring(wLogKatalog,length(wLogKatalog),1) = "/" or
     substring(wLogKatalog,length(wLogKatalog),1) = "\" then
   wLogKatalog = substring(wLogKatalog,1,length(wLogKatalog) - 1).
    
  /* Bygger full path til fil */
  assign
    wLogFil = wLogKatalog +
              (if opsys = "unix"
                 then "/"
                else "\") +
              wLogFil.

  DO WITH FRAME ProgramListe
    DOWN OVERLAY CENTERED ROW 10 WIDTH 200 /* view-as DIALOG-BOX */:

    OUTPUT TO VALUE(wLogFil).
    PUT UNFORMATTED "  * Aktive programmer " + STRING(TODAY) +
                    " " + STRING(TIME,"HH:MM:SS") + " (w-modul.w) *" SKIP(1)
                    "Programnavn     " +
                    "Valid " +
                    "Dato       " +
                    "Tid      " +
                    "Handle     " +
                    "WinHandle  " SKIP.

    FOR EACH ProgramListe NO-LOCK:
        cTmpnamn = SEARCH(ProgramListe.ProgramNavn).
        cTmpnamn = IF cTmpnamn <> ? THEN cTmpnamn ELSE ProgramListe.ProgramNavn.
      put "  "
/*           cTmpnamn FORMAT "x(25)" */

          ProgramListe.ProgramNavn FORMAT "x(15)"
          STRING(VALID-HANDLE(ProgramListe.ProgramHandle)) format "x(5)"
          STRING(ProgramListe.Dato) FORMAT "x(10)"
          STRING(ProgramListe.Tid,"HH:MM:SS") FORMAT "x(10)"
          STRING(INT(ProgramListe.ProgramHandle)) FORMAT "x(10)"
          STRING(INT(ProgramListe.WindowHandle)) FORMAT "x(10)" SKIP.
    END.
    HIDE FRAME ProgramListe.
    OUTPUT CLOSE.
  END.

  RUN d-visfil.w (0,"Liste over persistente programmer",wLogFil,"F",0,"").
  /* RUN StartNotePad (wLogFil). */

END PROCEDURE.

PROCEDURE StoppProgramListe:
  FOR EACH ProgramListe:
    IF VALID-HANDLE(ProgramListe.ProgramHandle) then
      DELETE PROCEDURE ProgramListe.ProgramHandle.
  END.
END PROCEDURE.

