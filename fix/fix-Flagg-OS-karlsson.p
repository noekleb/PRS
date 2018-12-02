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

DEF BUFFER buf1Strekkode FOR Strekkode.
DEF BUFFER buf2Strekkode FOR Strekkode.

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
    .


ASSIGN
    cFilNavn = "C:\Appdir\Art nr med OS 220307.csv"
    cLoggFil = "C:\Appdir\Art nr med OS 220307.log"
    .

RUN LesInnFil.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     Endret EDato, tid og bruker på alle artikler som er listet i fil.
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
DEF VAR lSlettes AS LOG NO-UNDO.

DEF VAR piTilbud AS INT NO-UNDO.

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
    lKorriger = false  /* Utfører korreksjonene */
    .

MAINLOOP:
REPEAT:
    IF AVAILABLE buf1Strekkode THEN RELEASE buf1Strekkode.
    IF AVAILABLE buf2Strekkode THEN RELEASE buf2Strekkode.

    IMPORT STREAM InnFil  UNFORMATTED cLinje.

    ASSIGN
        lArtikkelNr = DEC(
                          substring(
                                    ENTRY(2,cLinje,";"),1,LENGTH(ENTRY(2,cLinje,";")) - 3
                                    )
                             )
        iStrKode    = int(
                             substring(
                                       ENTRY(2,cLinje,";"),LENGTH(ENTRY(2,cLinje,";")) - 2,LENGTH(ENTRY(2,cLinje,";"))
                                       )
                             )
        .
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas THEN
    KORR:
    DO TRANSACTION:
        FIND CURRENT ArtBas EXCLUSIVE-LOCK.

        /* Flagger endring på artikkelen. */
        IF lKorriger THEN
        ASSIGN
            ArtBas.EDato    = TODAY
            ArtBAs.ETid     = TIME
            ArtBas.BrukerId = "Karlsson " + string(TIME)
            .

        PUT STREAM LoggFil " " SKIP.

        /* Det er kun "OS" som skal justeres. */
        IF iStrKode <> 264 THEN
        DO:
            PUT STREAM LoggFil UNFORMATTED
                "Artikkel " lArtikkelNr " " iStrKode " " ArtBas.Beskr " LevKod: " ArtBas.LevKod " LevFarg: " ArtBas.LevFarg " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP
                "     Strekkode ' 1'(" STRING(iStrKode) + "): Denne er ok. IKKE behandlet." SKIP
                .
            LEAVE KORR.
        END.

        /* Henter strekkoden som ligger på OS størrelsen. */
        FIND FIRST buf1Strekkode OF ArtBas WHERE
            buf1Strekkode.StrKode = iStrKode NO-ERROR. /* "OS" */
        /* Henter strekkoden som ligger på " 1" størrelsen. */
        FIND FIRST buf2Strekkode OF ArtBas WHERE
            buf2Strekkode.StrKode = 001 NO-ERROR. /* " 1" */
        /* Det ligger ingen OS strekkode på artikkelen. Og det ligger ingen " 1" heller. */
        IF NOT AVAILABLE buf1Strekkode AND NOT AVAILABLE buf2Strekkode THEN
        DO:
            PUT STREAM LoggFil UNFORMATTED
                "Artikkel " lArtikkelNr " " iStrKode " " ArtBas.Beskr " LevKod: " ArtBas.LevKod " LevFarg: " ArtBAs.LevFarg " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP
                "     Strekkode 'OS' og ' 1' mangler (" + STRING(iStrKode) + ")" SKIP.
            LEAVE KORR.
        END.
        /* Det ligger ingen OS strekkode på artikkelen. Og det finnes en " 1" fra før. */
        ELSE IF NOT AVAILABLE buf1Strekkode AND AVAILABLE buf2Strekkode THEN
        DO:
            PUT STREAM LoggFil UNFORMATTED
                "Artikkel " lArtikkelNr " " iStrKode " " ArtBas.Beskr " LevKod: " ArtBas.LevKod " LevFarg: " ArtBAs.LevFarg " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP
                "     Strekkode 'OS' mangler, men ' 1' ligger der fra før." SKIP.
            LEAVE KORR.
        END.
        /* Her bytter vi bare strkode på strekkoden. */
        ELSE IF AVAILABLE buf1Strekkode AND NOT AVAILABLE buf2Strekkode THEN
        DO:
            /* Ny StrKode på strekkoden. */
            IF lKorriger THEN
            ASSIGN
                buf1Strekkode.StrKode = 001
                .
            PUT STREAM LoggFil UNFORMATTED
                "Artikkel " lArtikkelNr " " iStrKode " " ArtBas.Beskr " LevKod: " ArtBas.LevKod " LevFarg: " ArtBAs.LevFarg " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP
                "     Strekkode 'OS' byttet til ' 1' (" STRING(iStrKode) + "): " + " Funnet " + buf1Strekkode.Kode + " " + STRING(buf1Strekkode.StrKode) + " " + Buf1Strekkode.Bestillingsnummer SKIP.
            LEAVE KORR.
        END.
        /* Her må vi ta bort 02 koden */
        ELSE IF AVAILABLE buf1Strekkode AND AVAILABLE buf2Strekkode THEN
        DO:
            /* Tar bort 02 kode. */
            IF buf2Strekkode.Kode BEGINS "02" THEN
            DO:
                PUT STREAM LoggFil UNFORMATTED
                    "Artikkel " lArtikkelNr " " iStrKode " " ArtBas.Beskr " LevKod: " ArtBas.LevKod " LevFarg: " ArtBAs.LevFarg " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP
                    "     Strekkode 'OS' byttet til ' 1' (" STRING(iStrKode) + "): " + " Funnet " + buf1Strekkode.Kode + " " + STRING(buf1Strekkode.StrKode) + " " + Buf1Strekkode.Bestillingsnummer SKIP
                    "     Slettet 02 kode: " buf2Strekkode.Kode "  " buf2Strekkode.StrKode SKIP.
                FOR EACH VareBehLinjeTrans EXCLUSIVE-LOCK WHERE
                    VareBehLinjeTrans.Kode = buf2Strekkode.Kode:
                    PUT STREAM LoggFil UNFORMATTED
                        "     Strekkode: " buf2Strekkode.Kode " finnes på messeordre:" skip
                              VareBehLinjeTrans.VareBEhNr " " 
                              VareBehLinjeTrans.ButikkNr " "
                              VareBehLinjeTrans.Kode " bytter til strekkode "
                              buf2Strekkode.Kode
                        SKIP.
                    /* Endrer strekkoden til den som ligger på OS posten. */
                    IF lKorriger THEN
                        VareBehLinjeTrans.Kode = buf1Strekkode.Kode.
                END.
                /* Dødens tar denne 02 strekkoden. */
                IF lKorriger THEN 
                    DELETE buf2Strekkode.
                /* Flytter OS StrKode på strekkoden. */
                IF lKorriger THEN
                    ASSIGN
                    buf1Strekkode.StrKode = 001
                    .
            END.
            /* To gyldige Strekkoder */
            ELSE DO:
                PUT STREAM LoggFil UNFORMATTED
                    "Artikkel " lArtikkelNr " " iStrKode " " ArtBas.Beskr " LevKod: " ArtBas.LevKod " LevFarg: " ArtBAs.LevFarg " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP
                    "     To gyldige strekkoder 'OS' og ' 1' " buf1Strekkode.Kode + " " + buf2Strekkode.Kode SKIP.
                LEAVE KORR.
            END.
            LEAVE KORR.
        END.
        FIND CURRENT ArtBas NO-LOCK.
    END. /* KORR */
    ELSE DO:
        PUT STREAM LoggFil 
            "** Ukjent artikkel " lArtikkelNr " " iStrKode TODAY " " STRING(TIME,"HH:MM:SS")
            SKIP.
    END.
END. /* MAINLOOP */

OUTPUT STREAM LoggFil CLOSE.
INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

