&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xexcelvpiinnles.p
    Purpose     :

    Syntax      :

    Description : Leser inn excelvpi filen og omformer den til en pricat fil.

    Author(s)   : Tom Nøkleby
    Created     : 26/10-10
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cExcelFilNavn AS CHAR NO-UNDO.
DEF VAR cCsvFilNavn   AS CHAR NO-UNDO.
DEF VAR ctmpFilNavn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUtFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcBkuFil   AS CHARACTER NO-UNDO.
DEF VAR hPgmHandle    AS HANDLE NO-UNDO.
DEF VAR h_dvpifilhode AS HANDLE NO-UNDO.
DEF VAR h_dvpiartbas  AS HANDLE NO-UNDO.
DEF VAR h_PrisKo      AS HANDLE NO-UNDO.
DEFINE VARIABLE ctmpKatalog    AS CHARACTER NO-UNDO.
DEFINE VARIABLE bAvbrytVedFeil AS LOG NO-UNDO.
DEFINE VARIABLE cOKVersion AS CHARACTER INIT "Rigal98,RIGAL02" NO-UNDO.
DEF VAR cRigalversion AS CHAR NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.

DEFINE TEMP-TABLE tt_ErrorLinje
  FIELD LinjeNr   AS INT
  FIELD Tekst     AS CHAR
  FIELD Gradering AS INT 
  .

{windows.i}
{lesexcelvpifil.i &NEW = "NEW" &SHARED = "SHARED"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 19.05
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cExcelFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

ASSIGN
  ctmpKatalog = SESSION:TEMP-DIRECTORY
  cCsvFilNavn = SESSION:TEMP-DIR + "tmp.csv".
  ctmpFilNavn = SESSION:TEMP-DIR + "tmp.txt".
  .

/* Henter ID for konvertering */
FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
IF NOT AVAILABLE EkstVPILev THEN
DO:
    MESSAGE "Ingen ekstern VPI leverandør tilgjengelig." SKIP
            "Id: " + STRING(VPIFilHode.EkstVPILevNr) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.
IF EkstVPILev.KortNavn = '' THEN
DO:
    MESSAGE "Det er ikke satt opp KortNavn på ekstern VPI leverandør." SKIP
            "Dette må satt for at konvertering av varegrupper skal kunne skje." SKIP
            "Id: " + STRING(VPIFilHode.EkstVPILevNr) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Leser Excel filen og lagrer inholdet til en csv fil. */
RUN lesexcelvpifil.p (cExcelFilNavn, cCsvFilNavn).
/*
MESSAGE 'lesexcelvpifil.p'  SKIP
        'cExcelFilNavn' cExcelFilNavn SKIP
        'cCsvFilNavn' cCsvFilNavn
VIEW-AS ALERT-BOX.
*/
/* Leser inn fra CSV filen og bygger temp tabell. */
IF SEARCH(cCsvFilNavn) <> ? THEN
  DO:
    RUN lesexcelvpifilimportera.p (cCsvFilNavn,EkstVPILev.EkstVPILevNr).
    /*
    MESSAGE 'lesexcelvpifilimportera.p'  SKIP
        'cCsvFilNavn' cCsvFilNavn SKIP
        'EkstVPILev.EkstVPILevNr' EkstVPILev.EkstVPILevNr
        VIEW-AS ALERT-BOX.
    */
  END.

/* Skriver til RIGAL fil - ALLE (*) varer. */
RUN lesexcelExportRigal.p ('*', OUTPUT cUtFilNavn).
    /*
    MESSAGE 'lesexcelExportRigal.p'  SKIP
        'cUtFilNavn' cUtFilNavn SKIP
        VIEW-AS ALERT-BOX.
    */
/* Stempler posten som innlest. */
DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering < 50) THEN
        ASSIGN
            VPIFilHode.VPIFilStatus = 9.
    ELSE
        ASSIGN
            VPIFilHode.VPIFilStatus = 5.
    FIND CURRENT VPIFilHode NO-LOCK.    
END.
IF AVAILABLE VPIFilHode THEN
    FIND CURRENT VPIFilHode NO-LOCK.

/* Ved vellykket import/eksport, flyttes importfil tilbackup katalog. */
IF bAvbrytVedFeil = FALSE THEN
  RUN flyttFil. 
ELSE IF bAvbrytVedFeil AND NOT CAN-FIND(FIRST tt_ErrorLinje) THEN
  RUN flyttFil. 
IF CAN-FIND(FIRST tt_ErrorLinje) THEN
  RUN ErrorLogg.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cErrorFil AS CHAR NO-UNDO.

  ASSIGN
      cErrorFil = OS-GETENV('TMP') + '\' 
                 + "Error_" 
                 + VPIFilHode.FilNavn
                 + ".Txt".

  OUTPUT TO VALUE(cErrorFil).
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
      .

    /* Feil som avbryter innlesning av fil. */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 1) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UGYLDIGE VERDIER (ErrorLogg) ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 1:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.
    /* Linjer med ugyldige koder */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 2) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UKJENTE TRANSAKSJONSkoder ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   Det ligger ukjente transaksjonstyper på disse linjene." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 2:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.
    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 3) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** BLANKE EAN KODER (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" + chr(10) +
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 3:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.
    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 4) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UGYLDIGE VERDIER i EAN koder (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 4:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.
    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 5) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** EAN MED FEIL SJEKKSIFFER (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 5:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 6) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** LEVERANDØRNR SOM MANGLER MAPPING (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 6:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 7) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** PRODUSENTNR SOM MANGLER MAPPING (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 7:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 8) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** VAREGRUPPER SOM MANGLER MAPPING (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 8:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 9) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** FEIL VED PANTVARER/LINK (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 9:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 10) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** FEIL SALGSENHET (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 10:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 11) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** FEIL FUNKSJONSKODE (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 11:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_ErrorLinje WHERE
                tt_ErrorLinje.Gradering = 12) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UKJENT BUTIKKNUMMER - FILIALNUMMER (Linjevalidering) - FEIL FUNNET VED KONTROLL AV LINJER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje WHERE
            tt_ErrorLinje.Gradering = 12:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.

    /* Fra linjevalidering */
    IF CAN-FIND(FIRST tt_ErrorLinje) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** ØVRIGE FEILMELDINGER ***" SKIP
            "   Linjer som inneholder ugyldige verdier er ikke importert." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_ErrorLinje:
          PUT UNFORMATTED tt_ErrorLinje.Tekst SKIP.
          DELETE tt_ErrorLinje.
        END.
    END.
  
  OUTPUT CLOSE.
  
  /* TN 5/11-10 Legg inn feilhåndtering her. Sending av feillogg på eMail */
  /*
  IF SEARCH(cErrorFil) <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cErrorFil),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-flyttFil) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flyttFil Procedure
PROCEDURE flyttFil:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    ASSIGN
      pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
                 VPIFilHode.FilNavn.

    /* Sikrer at backup katalog finnes. */
    OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
    /* Flytter filen til backup katalog. */
    OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) 
            value(pcBkuFil).
    /* ------------------
    /* Renser bort fil */
    IF SEARCH(pcBkuFil) <> ? THEN
    DO:
        /* Filen tas bort fra katalogen. */
        IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
            OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
    END.
    /* Renser bort temp fil */
    IF SEARCH(VPIFilHode.Katalog + "~\" + cCsvFilNavn) <> ? THEN
      OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + cCsvFilNavn).
    ------------- */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
      iTotAntLinjer = -1 /* Första linjen är en header */
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  IMPORT STREAM InnFil UNFORMATTED cLinje.
  cLinje = TRIM(cLinje,'"').
  IF NOT CAN-DO(cOKVersion,ENTRY(1,cLinje)) THEN DO:
      INPUT STREAM InnFil CLOSE.
      RETURN "FEIL".
  END.
  ASSIGN cRigalversion = ENTRY(1,cLinje).
  REPEAT:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */
