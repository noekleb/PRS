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
    cFilNavn = "C:\Appdir\Strekkode_02_dublett.csv"
    cLoggFil = "C:\Appdir\Strekkode_02_dublett.log" + STRING(TIME)
    .

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
DEF VAR lSlettes AS LOG NO-UNDO.
DEF VAR iAntall AS INT NO-UNDO.
DEF VAR iTot    AS INT NO-UNDO.
DEF VAR iLest   AS INT NO-UNDO.
DEF VAR iSlettet AS INT NO-UNDO.

DEF VAR piTilbud AS INT NO-UNDO.

DEF BUFFER bArtBas FOR ArtBas.

CURRENT-WINDOW:WIDTH = 200.

OUTPUT STREAM LoggFil TO VALUE(cLoggFil).

/* Blank lije mellom artikklene */
PUT STREAM LoggFil SKIP(1).
PUT STREAM LoggFil 
    "Innlesning av fil " cFilNavn " startet. " TODAY " " STRING(TIME,"HH:MM:SS")
    SKIP.

display "Teller opp" with frame B.
/* Totalt antall */
/*
FOR EACH Strekkode NO-LOCK:
    iTot = iTot + 1.
END.
*/

ASSIGN
    /*
    lKorriger = FALSE /* Kun logging */
    */
    lKorriger = false /* Utfører korreksjonene */
    piTilbud  = 1
    .

pause 0.
display "Leser strekkoder......" with frame b.
pause 0.
MAINLOOP:
FOR EACH Strekkode EXCLUSIVE-LOCK 
    WHERE Strekkode.ArtikkelNr = 9001015:
    iLest = iLest + 1.
     

    IF NOT Strekkode.Kode BEGINS "02" THEN
        NEXT MAINLOOP.

    IF LENGTH(Strekkode.Kode) <> 13 THEN
        NEXT MAINLOOP.

    IF NOT Strekkode.Kode MATCHES "*" + STRING(Strekkode.ArtikkelNr) + "*" THEN
        NEXT MainLoop.

    FIND ArtBas OF Strekkode NO-LOCK.

    BUFFER_KODE:
    FOR EACH buf1Strekkode NO-LOCK WHERE
        buf1Strekkode.ArtikkelNr = Strekkode.ArtikkelNr AND
        buf1Strekkode.StrKode    = Strekkode.StrKode AND
        RECID(buf1Strekkode) <> RECID(Strekkode) AND
        LENGTH(buf1Strekkode.Kode) = 13:

        PUT STREAM LoggFil UNFORMATTED
            "Strekkodedublett slettet: " Strekkode.Kode 
            " Org.kode: " buf1Strekkode.Kode 
            " StrKode/Artikkel/Varetekst:  "
            Strekkode.StrKode " "
            ArtBas.ArtikkelNr " "
            ArtBas.Beskr " "             
            SKIP.

        /* Lister opp strekkoder i vareh.bøkene */
        FOR EACH VareBehLinjeTrans EXCLUSIVE-LOCK WHERE
            VareBehLinjeTrans.Kode = Strekkode.Kode:

            PUT STREAM LoggFil UNFORMATTED
                "              Vareboklinje: " 
                VareBehLinjeTrans.VareBehNr " "
                VareBehLinjeTrans.ArtikkelNr "  "
                VareBehLinjeTrans.StrKode "  "
                VareBehLinjeTrans.Kode
                SKIP.
            /* Endrer strekkoden. */
            IF lKorriger THEN
                ASSIGN
                VareBehLinjeTrans.Kode = buf1Strekkode.Kode
                .
        END.

        /* Fjerner 02 dubletter. */
        IF lKorriger THEN
            DELETE Strekkode.
        iSlettet = iSlettet + 1.
        LEAVE BUFFER_KODE.
    END. /* BUFFER_KODE */



    iAntall = iAntall + 1.
    PAUSE 0 BEFORE-HIDE.
    IF iAntall MODULO 100 = 0 THEN
      DISPLAY
        "Totalt   :" iTot  skip
        "Lest     :" iLest SKIP
        "Behandlet:" iAntall SKIP
        "Slettet  :" iSlettet
        WITH FRAME b NO-LABELS.

    IF iAntall > 10000 THEN
        LEAVE MAINLOOP.

END. /* MAINLOOP */

        PUT STREAM LoggFil UNFORMATTED
            "RESULTAT Totalt antall strekkoder: " iTot
            " Lest: " iLest 
            " Behandlet: " iAntall 
            "Slettet :" iSlettet
            SKIP.

OUTPUT STREAM LoggFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

