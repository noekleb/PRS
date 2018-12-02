/************************************************************
    Program:  divfunk.i
    Created:  TN   12 Nov 98
Description:  Diverse funksjoner i prosedyrebibloteket som
              ikke passer inn i andre grupperinger.

Last change:  TN   17 Mar 100    2:17 pm
************************************************************/

DEF VAR wLayout as CHAR initial "?" NO-UNDO.

PROCEDURE GetMedlemsNr:
  DEF OUTPUT PARAMETER ipMedlemsNr AS DEC NO-UNDO.

  DEF VAR ipLoop    AS   dec               NO-UNDO.
  DEF VAR ipStart   AS   DEC               NO-UNDO.
  DEF VAR wDbId     AS   CHAR              NO-UNDO.
  DEF VAR wLedige   AS   INT               NO-UNDO.

  DEF BUFFER ledMedlem FOR Medlem.

  {syspara.i 1 1 17 wDbId}          /* DatabaseID */
  {syspara.i 14 1 4 wLedige INT}    /* Føste eller neste ledige medlemsnummer. */

                       
  CASE wLedige:
      WHEN 1 THEN /* Første ledige */
      DO:
          ASSIGN
              ipStart = IF ipMedlemsNr < dec(wDbId)
                         THEN dec(wDbId)
                         ELSE ipMedlemsNr.
          LOOPEN:
          DO ipLoop = ipStart TO DEC(wDbId) + 9999999.0:
              IF NOT CAN-FIND(Medlem WHERE
                              Medlem.MedlemsNr = ipLoop) THEN
              DO:
                ASSIGN
                  ipMedlemsNr = ipLoop.
                LEAVE LOOPEN.
              END.
              ELSE
                  ASSIGN
                      ipMedlemsNr = ?.
          END. /* LOOPEN */
      END.
      WHEN 2 THEN /* Neste ledige */
      DO:
          FIND LAST ledMedlem NO-LOCK NO-ERROR.
          IF AVAILABLE ledMedlem THEN
              ipMedlemsNr = ledMedlem.MedlemsNr + 1.
          IF ipMedlemsNr < dec(wDbId) THEN
          LOOPEN:
          DO ipLoop = DEC(wDbId) + 1 TO DEC(wDbId) + 9999999.0:
              IF NOT CAN-FIND(Medlem WHERE
                              Medlem.MedlemsNr = ipLoop) THEN
              DO:
                ASSIGN
                  ipMedlemsNr = ipLoop.
                LEAVE LOOPEN.
              END.
              ELSE
                  ASSIGN
                      ipMedlemsNr = ?.
          END. /* LOOPEN */
      END.
      OTHERWISE 
          DO:
              MESSAGE "Tildeling av Medlemnummer - Parameter (Syspara 14 2 4) er ikke satt."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Initieringsfeil".
          END.
  END CASE.
  
  /* Sjekker om nummerserien er full */
  IF ipMedlemsNr = ? THEN
  DO:
      MESSAGE "Nummerserien er full. Kontakt systemansvarlig eller leverandør."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Initieringsfeil".
      ASSIGN
          ipMedlemsNr = 0.
  END.

  RETURN string(ipMedlemsNr).
END PROCEDURE.

PROCEDURE TestAvLib:
  def input parameter ipInnData as char no-undo.

  Message "** Prosedyrebiblotek startet med parameter ="
           ipInnData view-as alert-box.
  return no-apply.

END PROCEDURE.

PROCEDURE SetEtikettLayout:
  DEF INPUT PARAMETER iLayout as CHAR NO-UNDO.
  assign
    wLayout = iLayout.

END procedure.

PROCEDURE GetEtikettLayout:
  DEF output PARAMETER iLayout as CHAR NO-UNDO.
  assign
    iLayout = wLayout.

END procedure.

PROCEDURE DataEntryReturn:
  DEF INPUT parameter ipModus as CHAR NO-UNDO.

  if ipModus = "true" then
    assign
      session:data-entry-return = true.
  else
    assign
      session:data-entry-return = false.

END PROCEDURE.

PROCEDURE DataEntryToggle:
  DEF INPUT PARAMETER ipData as CHAR NO-UNDO.
  DEF VAR wStatus as CHAR NO-UNDO.

  if session:data-entry-return = false then
    assign
      session:data-entry-return = true
      wStatus = "På".
  else
    assign
      session:data-entry-return = false
      wStatus = "Av".

  MESSAGE
    "Data Entry RETURN er satt til :" wStatus ipData VIEW-AS ALERT-BOX.

END PROCEDURE.

/* †pner en fil i NotePad.Exe */
PROCEDURE StartNotePad:
  DEF INPUT PARAMETER wFilNavn as CHAR NO-UNDO.

  if SEARCH(wFilNavn) = ? then
    DO:
      MESSAGE "Filen som skal åpnes finnes ikke" wFilNavn VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
    END.
  ELSE
    OS-COMMAND silent VALUE("notepad.exe") VALUE(wFilNAvn).
END PROCEDURE.

/* Husker siste fil som ble kompilert. */
PROCEDURE SistKompilert:
  DEF INPUT-OUTPUT PARAMETER ipSistKompilert as CHAR NO-UNDO.

  if ipSistKompilert = ""
    THEN ipSistKompilert = wSistKompilert.
  else
    wSistKompilert = ipSistKompilert.
END.

/* Fikser størrelsen */
PROCEDURE FiksStorl:
  def INPUT-output parameter wStorl as char NO-UNDO.

  DEF VAR wDecimaler as CHAR NO-UNDO.

  {syspara.i 1 1 16 wDecimaler}

  assign
     wStorl = trim(wStorl)
     wStorl = caps(wStorl)
     wStorl = if (length(wStorl) = 1 or
                  length(wStorl) = 3
                  )
                 then " " + wStorl
                 else wStorl.

  /* Bytter ut eventuelle comma med punkt. */
  if index(wStorl,",") <> 0 then
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  /* Sjekker om det er benyttet gyldige tegn i halvnummer. */
  /* Er det ikke det, tas halvnummeret bort.               */
  if NUM-ENTRIES(wStorl,".") = 2 then
    DO:
      if NOT CAN-DO(wDecimaler,ENTRY(2,wStorl,".")) then
        wStorl = ENTRY(1,wStorl,".").
    END.

  RETURN wStorl.   /* Function return value. */

END PROCEDURE.

/* Sjekker om st›rrelsen finnes i st›rrelsestypen. */
procedure StrTypeSjekk:
  DEF INPUT PARAMETER wStorl     as CHAR NO-UNDO.
  DEF INPUT PARAMETER wStrTypeId as INT NO-UNDO.

  DEF VAR wStrListe as CHAR NO-UNDO.
  DEF VAR wStatus   as CHAR INITIAL "AVBRYT" NO-UNDO.

  /* Kontrollen skal ikke utf›res p† st›rrelsestype 1. */
  if wStrTypeId > 1 then
    DO:
      FOR EACH StrTStr NO-LOCK WHERE
        StrTStr.StrTypeId = wStrTypeId:

        wStrListe = wStrListe +
                    (if wStrListe = ""
                       THEN ""
                       ELSE ",") +
                    StrTStr.SoStorl.
      END.

      if CAN-DO(wStrListe,wStorl) then
        wStatus = "OK".
    END.
  else
    wStatus = "OK".
  RETURN wStatus.

END PROCEDURE.

PROCEDURE GetWeekNum:
DEFINE INPUT  PARAMETER indate   AS DATE.  /* Input date , eg 10/17/90 */
DEFINE OUTPUT PARAMETER wWeek    AS INT.   /* Output week              */

DEFINE VARIABLE yr   AS INT.  /* Year of indate, eg 1990      */
DEFINE VARIABLE d1   AS INT.  /* Weekday of 1/1 current year, eg 2  */
			      /* (01/01/90 is a Monday)      */
DEFINE VARIABLE dat1 AS DATE. /* Starting date of week 1     */
DEFINE VARIABLE wn   AS INT.  /* Week number , eg 45         */

ASSIGN
  yr   = YEAR(indate)
  d1   = WEEKDAY(DATE( 1 , 1 , yr))
  dat1 = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
			  DATE(1, 10, yr) - d1 )
  wn    = TRUNCATE((indate - dat1 + 7) / 7 , 0)
  wWeek = wn.

IF wn < 1 THEN       /* Week 52 or 53 previous year ? */
ASSIGN
  yr     = yr - 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
 			    DATE(1, 10, yr) - d1 )
  wn     = TRUNCATE((indate - dat1 + 7) / 7 , 0)
  wWeek  = wn.

ELSE IF wn > 52 THEN  /* Week 53 this year or week 1 next year ? */
ASSIGN
  yr     = yr + 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  wWeek  = IF d1 EQ 6 OR d1 EQ 7 OR d1 EQ 1
	      THEN 53 ELSE  1.
END PROCEDURE.
