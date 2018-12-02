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
    cFilNavn = "C:\Home\Lindbak\ANKOMMET\Pricat-retting.csv"
    cLoggFil = "C:\Home\Lindbak\ANKOMMET\Pricat-retting.log"
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
    lKorriger = TRUE  /* Utfører korreksjonene */
    piTilbud  = 1
    .

MAINLOOP:
REPEAT:
    IMPORT STREAM InnFil  UNFORMATTED cLinje.

    ASSIGN
        iLevNr         = int(trim(ENTRY(2,cLinje,";")))
        cKode          = trim(ENTRY(4,cLinje,";"))
        cBeskr         = trim(ENTRY(5,cLinje,";"))
        iAntIPakn      = int(ENTRY(12,cLinje,";"))
        iSasong        = int(ENTRY(34,cLinje,";"))
        cModellNr      = trim(ENTRY(3,cLinje,";"))
        cBestillingsNr = trim(ENTRY(25,cLinje,";"))
        cLevFarg       = trim(ENTRY(7,cLinje,";"))
        iVareGruppe    = int(ENTRY(23,cLinje,";"))
        lAnonse        = IF trim(ENTRY(33,cLinje,";")) = "1" THEN TRUE ELSE FALSE
        lKatalogpris   = dec(replace(trim(trim(ENTRY(13,cLinje,";"),'"'),"%"),' ',''))
        lForhRab%      = DEC(trim(trim(trim(ENTRY(15,cLinje,";"),'"'),"%"),''))
        lSuppRab%      = DEC(trim(trim(trim(ENTRY(16,cLinje,";"),'"'),"%"),''))
        cLevDato1      = trim(ENTRY(19,cLinje,";"),'"')
        cLevDato2      = trim(ENTRY(20,cLinje,";"),'"')
        cLevDato3      = trim(ENTRY(21,cLinje,";"),'"')
        cLevDato4      = trim(ENTRY(22,cLinje,";"),'"')
        /*
            ttPriKat.VeilPris       = trim(ENTRY(17,VPIFilLinje.StorTekst,";"),'"')
            ttPriKat.VeilPris       = trim(REPLACE(ttPriKat.VeilPris,' ',''),"%")
            ttPriKat.PAKstru        = trim(ENTRY(18,VPIFilLinje.StorTekst,";"),'"')
            ttPriKat.Enh            = trim(ENTRY(11,VPIFilLinje.StorTekst,";"),'"')
            ttPriKat.Enh            = (IF ttPriKat.Enh = "" THEN "Stk" ELSE ttPriKat.Enh)
        */
        cBilde      = trim(ENTRY(35,cLinje,";"))
        lArtikkelNr = DEC(
                          substring(
                                    ENTRY(37,cLinje,";"),1,LENGTH(ENTRY(37,cLinje,";")) - 3
                                    )
                             )
        iStrKode    = int(
                             substring(
                                       ENTRY(37,cLinje,";"),LENGTH(ENTRY(37,cLinje,";")) - 2,LENGTH(ENTRY(37,cLinje,";"))
                                       )
                             )
        .
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas THEN
    KORR:
    DO TRANSACTION:
        FIND CURRENT ArtBas EXCLUSIVE-LOCK.

        PAUSE 0 BEFORE-HIDE.
        DISPLAY
            ArtBAs.ArtikkelNr
            ArtBAs.Beskr
            ArtBAs.LevFargKod
            cLevFarg
            WITH WIDTH 200.

        /* Oppdaterer VPI info */
/*         ASSIGN                               */
/*             ArtBas.LevFargKod     = cLevFarg */
/*             .                                */

    END. /* KORR */
    FIND CURRENT ArtBas NO-LOCK.
END. /* MAINLOOP */

OUTPUT STREAM LoggFil CLOSE.
INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

