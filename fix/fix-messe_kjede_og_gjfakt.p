&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

current-window:width = 200.

DEF VAR cFilNavn AS CHAR FORMAT "x(30)"  NO-UNDO.
DEF VAR cLoggFil AS CHAR FORMAT "x(30)"  NO-UNDO.
DEF VAR cLinje   AS CHAR FORMAT "x(100)" NO-UNDO.   
DEF VAR pcSkjerm      AS CHAR NO-UNDO.
DEF VAR h_PrisKo      AS HANDLE NO-UNDO.

DEF VAR dcValPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcInnPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcUtpris      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcRabatt      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcFrakt       AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR cEndelse      AS CHAR   NO-UNDO.
DEF VAR cTekst        AS CHAR   NO-UNDO.
DEF VAR rArtBasRecid  AS RECID  NO-UNDO.
DEF VAR iSasong       AS INT    NO-UNDO.

DEF VAR iProfilNr     AS INT NO-UNDO.
DEF VAR iCl AS INT NO-UNDO.

DEF VAR lKorriger AS LOG INITIAL FALSE NO-UNDO.

DEF VAR lArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.

DEF STREAM InnFil.
DEF STREAM LoggFil.

DEF VAR wMesseNr LIKE Messe.MesseNr NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 5 1 1 iCl INT}
    
/* Profilnr for sentrallager. */
FIND Butiker NO-LOCK WHERE
    Butiker.butik = iCl NO-ERROR.
ASSIGN
    iProfilNr = Butiker.ProfilNr
    wMesseNr = 90000012
    .
    
FIND messe NO-LOCK WHERE
    Messe.MesseNr = wMesseNr NO-ERROR.
IF NOT AVAILABLE Messe THEN
DO :
    MESSAGE "Ukjent messe " wMesseNr
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
end.    
assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962754.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.

assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962755.csv"
    cLoggFil = cFilNavn + ".log"
    .
    pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.

assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962756.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.

assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962757.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.

assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962758.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.

assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962759.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.

assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962760.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.

assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962761.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.

assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962762.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.

assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962763.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.

assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962764.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.


assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962765.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.

assign    
    cFilNavn = "\\192.168.200.8\HK-server\Appdir\SE\Portland-overført\ORD02962766.csv"
    cLoggFil = cFilNavn + ".log"
    .
pause 0. display cFilNavn format "x(180)" skip with width 200.
RUN LesInnFil.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iStrKode AS INT  FORMAT "999"   NO-UNDO.
DEF VAR iStorl   AS CHAR FORMAT "x(4)"  NO-UNDO.
DEF VAR cBilde   AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR cKode    AS CHAR FORMAT "x(14)" NO-UNDO.
DEF VAR cBeskr   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR lAnonse  AS LOG                 NO-UNDO.
DEF VAR iAntIPakn AS INT NO-UNDO.
DEF VAR lKatalogpris AS DEC NO-UNDO.
DEF VAR lForHRab%    AS DEC NO-UNDO.
DEF VAR lSuppRab%    AS DEC NO-UNDO.
DEF VAR cLevDato1 AS CHAR NO-UNDO.
DEF VAR cLevDato2 AS CHAR NO-UNDO.
DEF VAR cLevDato3 AS CHAR NO-UNDO.
DEF VAR cLevDato4 AS CHAR NO-UNDO.
DEF VAR iLevNr    AS INT  NO-UNDO.
DEF VAR cLevFarg  AS CHAR NO-UNDO.
DEF VAR cModellNr AS CHAR NO-UNDO.
DEF VAR cBestillingsnr AS CHAR NO-UNDO.
DEF VAR iVareGruppe    AS INT  NO-UNDO.
DEF VAR iStrType       AS INT  NO-UNDO.
DEF VAR iDec AS dec NO-UNDO.
DEF VAR iLopNr AS INT NO-UNDO.
DEF VAR iLevKod AS CHAR NO-UNDO.

DEF VAR piTilbud AS INT NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.

DEF VAR lKjedeLevert       AS LOG NO-UNDO.
DEF VAR lGjennomfaktureres AS LOG NO-UNDO.

DEF BUFFER bArtBas FOR ArtBas.

CURRENT-WINDOW:WIDTH = 200.

INPUT  STREAM InnFil  FROM VALUE(cFilNavn).
OUTPUT STREAM LoggFil TO VALUE(cLoggFil).

/* Blank lije mellom artikklene */
PUT STREAM LoggFil SKIP(1).
PUT STREAM LoggFil 
    "Innlesning av fil " cFilNavn " startet. " TODAY " " STRING(TIME,"HH:MM:SS")
    SKIP.

ASSIGN
    /*
    lKorriger = FALSE /* Kun logging */
    */
    lKorriger = TRUE  /* Utfører korreksjonene */
    piTilbud  = 1
    .

MAINLOOP:
REPEAT:
    IMPORT STREAM InnFil  UNFORMATTED cLinje.

    /* Skipper heading. */
    IF ENTRY(1,cLinje,";") = '"iRectype"' THEN
        NEXT MAINLOOP.

/*
    MESSAGE ENTRY(22,cLinje,";") SKIP
            ENTRY(107,cLinje,";") SKIP
            ENTRY(77,cLinje,";") SKIP
        substring(
                                    trim(ENTRY(22,cLinje,";"),'"'),1,LENGTH(trim(ENTRY(22,cLinje,";"),'"')) - 3
                                    ) SKIP
        substring(
                                       trim(ENTRY(22,cLinje,";"),'"'),LENGTH(trim(ENTRY(22,cLinje,";"),'"')) - 2,LENGTH(trim(ENTRY(22,cLinje,";"),'"'))
                                       )
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        */

    ASSIGN
        lArtikkelNr = DEC(
                          substring(
                                    trim(ENTRY(22,cLinje,";"),'"'),1,LENGTH(trim(ENTRY(22,cLinje,";"),'"')) - 3
                                    )
                             )
        iStrKode    = int(
                             substring(
                                       trim(ENTRY(22,cLinje,";"),'"'),LENGTH(trim(ENTRY(22,cLinje,";"),'"')) - 2,LENGTH(trim(ENTRY(22,cLinje,";"),'"'))
                                       )
                             )
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE "Feil i artikkelnummer:" ENTRY(22,cLinje,";")
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN
        cTekst = trim(ENTRY(107,cLinje,";"))
        .
    IF cTekst = '*' OR cTekst = '1' OR cTekst = 'yes' OR cTekst = 'Ja' THEN
        lKjedeLevert.
    ASSIGN
        cTekst = TRIM(ENTRY(77,cLinje,";"))
        .
    IF cTekst = '*' OR cTekst = '1' OR cTekst = 'yes' OR cTekst = 'Ja' THEN
        lGjennomfaktureres. 

    IF NOT CAN-FIND(ArtBas WHERE 
                    ArtBAs.ArtikkelNr = lArtikkelNr) THEN
    DO:
        MESSAGE "Ukjent artikkel:" lArtikkelNr
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        NEXT MAINLOOP.
    END.

    /* ------------ VAREBOKHODE ------------------ */
    FOR EACH VareBokHode OF Messe NO-LOCK:
      FOR EACH VareBokLinje OF VareBokHode EXCLUSIVE-LOCK WHERE
          VareBokLinje.ArtikkelNr = lArtikkelNr:
          ASSIGN
              VareBokLinje.KjedeVare         = lKjedeLevert
              VareBokLinje.Gjennomfaktureres = lGjennomfaktureres
              .
      END.
    END.

    /* --------------- VAREBEHHODE ----------------- */
    FOR EACH VareBehHode OF Messe NO-LOCK:
        FOR EACH VareBehLinje OF VareBehHode EXCLUSIVE-LOCK:
            ASSIGN
                VareBehLinje.KjedeVare         = lKjedeLevert
                VareBehLinje.Gjennomfaktureres = lGjennomfaktureres
                .
        END.
    END.



END. /* MAINLOOP */

OUTPUT STREAM LoggFil CLOSE.
INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

