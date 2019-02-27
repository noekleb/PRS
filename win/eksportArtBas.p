&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg.



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
DEFINE INPUT  PARAMETER cLanButiker     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cFtpButiker     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cPRSFtpButiker     AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cVareFiler      AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cMixFiler       AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntVarer       AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER iAntPakkeLinjer AS INTEGER    NO-UNDO.

DEFINE VARIABLE cTekst            AS CHAR        NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
DEFINE VARIABLE cEksportKatalog   AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cExportVareFil    AS CHARACTER INIT "vare." NO-UNDO.
DEFINE VARIABLE cExportMixFil     AS CHARACTER INIT "mix." NO-UNDO.
DEFINE VARIABLE dMaxPris          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cBonus            AS CHARACTER  NO-UNDO. /* Hämtas för tillfället från syspara */
DEFINE VARIABLE lBonus            AS LOGICAL    NO-UNDO. 
DEFINE VARIABLE cFeilFil AS CHARACTER  INIT "FeilVareEksport.txt" NO-UNDO.
DEFINE VARIABLE bHoyLavMva        AS LOG        NO-UNDO.
DEFINE VARIABLE iCl               AS INT        NO-UNDO.
DEFINE VARIABLE cButikkLst        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iKasseEksportFormat AS INTEGER NO-UNDO.
DEFINE VARIABLE iKasseTabellformat  AS INTEGER NO-UNDO.
DEFINE VARIABLE cReturPant AS CHARACTER INITIAL '99999' NO-UNDO. /* Varenr 99999 er i InfoPOS 8.0 kassen satt av til returpant. */
DEFINE BUFFER   bTT_Elogg FOR TT_Elogg.
DEFINE BUFFER   clButiker FOR Butiker.

DEFINE TEMP-TABLE TT_FeilVareKost NO-UNDO
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Beskr      LIKE ArtBas.Beskr
    FIELD VareKost   AS DECIMAL DECIMALS 2 FORMAT "->>,>>9.99".

DEFINE TEMP-TABLE TT_FeilPris NO-UNDO
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Beskr      LIKE ArtBas.Beskr
    FIELD Pris   AS DECIMAL DECIMALS 2 FORMAT "->>,>>9.99".

DEFINE TEMP-TABLE TT_FeilStrekKode NO-UNDO
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Beskr      LIKE ArtBas.Beskr
    FIELD Kode       LIKE StrekKode.Kode.

/* Lagt inn profilnr i tabellen for å kunne håndtere utlegg til de profiler det gjelder. */ 
DEFINE TEMP-TABLE TT_ArtTilEksport NO-UNDO
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD ProfilNr   LIKE ArtPris.ProfilNr
    FIELD Slette     AS LOGICAL
    FIELD Strekkoder AS CHARACTER
    FIELD ButikkLst  AS CHARACTER
    INDEX Artnr IS PRIMARY Artikkelnr ProfilNr Slette.

DEFINE TEMP-TABLE TT_Vare NO-UNDO
/* 1  */    FIELD butnr       AS INTEGER FORMAT ">>9"                      /* I (3)      Butikknr                                                                                 */
/* 2  */    FIELD ean         AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" /* DE (13,0)  EAN/PLU-nr                                                                               */
/* 3  */    FIELD hgr         AS INTEGER FORMAT ">>>9"                     /* I (4)      Varegruppenr                                                                             */
/* 4  */    FIELD bong        AS CHARACTER FORMAT "x(20)"                  /* C (20)     Bongtekst                                                                                */
/* 5  */    FIELD opris       AS LOGICAL                                   /* L (yes/no) Flagg for åpen pris                                                                      */
/* 6  */    FIELD link        AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" /* De (13,0)  EAN/PLU-nr for lenkevare (for eksempel pant)                                             */
/* 7  */    FIELD kotype      AS INTEGER FORMAT "9"                        /* I (1)      Køtype (endringstype) - 1=ny/endring, 9=sletting                                         */
/* 8  */    FIELD vekt        AS INTEGER FORMAT "9"                        /* I (1)      Vekt/salgsenhet (0=stk, 1=kg, 3=meter, 4=m², 5=liter)                                    */
/* 9  */    FIELD utprisn     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Ordinær utsalgspris                                                                      */
/* 10 */    FIELD bonus       AS LOGICAL                                   /* L (yes/no) Flagg som angir om varen gir kjøpeutbytte                                                */
/* 11 */    FIELD mva         AS DECIMAL DECIMALS 2 FORMAT ">9.99"         /* De (2,2)   Momssats                                                                                 */
/* 12 */    FIELD krabatt     AS LOGICAL                                   /* L (yes/no) Flagg for om varen skal gi automatisk kunderabatt (via varegr)                           */
/* 13 */    FIELD varetekst   AS CHARACTER FORMAT "x(20)"                  /* C (30)     Lang varetekst - i InfoPOS etikettekst 1, kundevennlig                                   */
/* 14 */    FIELD nettokr     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* DE (5,2)   Ordinær nettopris                                                                        */
/* 15 */    FIELD bestnr      AS INTEGER FORMAT ">>>>>>>9"                 /* I (8)      Bestillingsnr                                                                            */
/* 16 */    FIELD mixnr       AS INTEGER FORMAT ">>>9"                     /* I (4)      Kobling til mixmatch fil på kassene                                                      */
/* 17 */    FIELD pakkenr     AS INTEGER FORMAT ">>>9"                     /* I (4)      For pakkevarer angir dette kobling til innholdet (i mixmatch)                            */
/* 18 */    FIELD bestvare    AS LOGICAL                                   /* L (yes/no) Benyttes til pantevarer (yes=pantevare/no=vanlig)                                                                              */
/* 19 */    FIELD utprist     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Kampanjepris                                                                             */
/* 20 */    FIELD tidsstyrtt  AS LOGICAL                                   /* L (yes/no) Flagg for om kampanjepris skal være aktiv når tidsstyrt pris ellers er den aktive prisen */
/* 21 */    FIELD fradatot    AS DATE                                      /* Da         Fradato for kampanjepris                                                                 */
/* 22 */    FIELD fratidt     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Fratidspunkt for kampanjepris                                                            */
/* 23 */    FIELD tildatot    AS DATE                                      /* Da         Tildato for kampanjepris                                                                 */
/* 24 */    FIELD tiltidt     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Tiltidspunkt for kampanjepris                                                            */
/* 25 */    FIELD utprism     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Medlemspris                                                                              */
/* 26 */    FIELD tidsstyrtm  AS LOGICAL                                   /* L (yes/no) Flagg for om medlemspris skal være aktiv når tidsstyrt pris ellers er den aktive prisen  */
/* 27 */    FIELD fradatom    AS DATE                                      /* Da         Fradato for medlemspris                                                                  */
/* 28 */    FIELD fratidm     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Fratidspunkt for medlemspris                                                             */
/* 29 */    FIELD tildatom    AS DATE                                      /* Da         Tildato for medlemspris                                                                  */
/* 30 */    FIELD tiltidm     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Tiltidspunkt for medlemspris                                                             */
/* 31 */    FIELD utprisa     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Tidsstyrt pris                                                                           */
/* 32 */    FIELD fra         AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Fratidspunkt for tidsstyrt pris                                                          */
/* 33 */    FIELD til         AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Tiltidspunkt for tidsstyrt pris                                                          */
/* 34 */    FIELD idkrav      AS INTEGER                                   /* I Id krav alder - Flagg for om salg av vare er aldersbegrenset (legitimasjonsplikt)                        */
/* 35 */    FIELD lager       AS LOGICAL                                   /* L (yes/no) Flagg for om vare er on-line lagerstyrt (kun mot InfoPOS bakromsløsning)                 */
/* 36 */    FIELD individ     AS LOGICAL                                   /* L (yes/no) Flagg for om varen er en individvare                                                     */
/* 37 */    FIELD garantikl   AS INTEGER FORMAT ">>>9"                     /* I (2)      Garantiklasse                                                                            */
/* 38 */    FIELD bilde       AS CHARACTER FORMAT "x(20)"                  /* C (20)     Navn på evt. bildefil                                                                    */
/* 39 */    FIELD timestat    AS LOGICAL                                   /* L (yes/no) Flagg for om statistikk for varen skal lagres på timenivå                                */
/* 40 */    FIELD nettokrt    AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)  Nettopris for kampanje                                                                    */
/* 41 */    FIELD Stoppkode      AS INTEGER FORMAT "9"                     /* integer 1 siffer 0=ikke stoppet, 2=stoppet, 3=opphev stopp (InfoPOS bakrom legger alltid ut 0)                       */
/* 42 */    FIELD Storrelsesnr   AS INTEGER FORMAT ">>>>9"                 /* integer 5 siffer SE/Gresvig har krav til inntil 5 siffer (0 i Coop)                                                  */
/* 43 */    FIELD Storrelsestype AS INTEGER FORMAT ">>>>>9"                   /* integer 3 siffer Foreløpig kun i SE (0 i Coop). Hentes fra modell i InfoPOS  */
/* 44 */    FIELD Lopenr         AS INTEGER FORMAT ">>>>>9"                /* integer 6 siffer Internt i SE (0 i Coop) */
/* 45 */    FIELD Handlingskode  AS INTEGER FORMAT ">>>>>9"                /* DECIMAL 6 siffer   Internt i SE (0 i Coop) */
/* 46 */    FIELD Modell         AS DECIMAL DECIMALS 0 FORMAT ">>>>>>9"                /* decimal 7 siffer Gresvig/SE (0 i Coop) */
/* 47 */    FIELD Bestnr2        AS CHARACTER FORMAT "x(20)"               /* character 20 tegn Alfanumerisk bestnr. Foreløpig kun SE. Hentes fra pris.bestnr2. ("" i Coop) */
/* 48 */    FIELD Vekttilbud     AS INTEGER FORMAT "9"                     /* INTEGER 1 siffer Likt med felt 8 (vektkode ordinær pris), men dedikert til kampanje. */
/* 49 */    FIELD Varenr         AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" /* DECIMAL 13 siffer       12 siffer i Gresvig/SE. For søk/salg av varer som er merket på gammelmåten */
/* 50 */    FIELD Fargenr        AS INTEGER FORMAT ">>>>9"                   /* INTEGER 5 siffer SE/Gresvig har krav til inntil 5 siffer (0 i Coop) */
/* 51 */    FIELD Rabikas        AS LOGICAL 
/* 52 */    FIELD Hovedean       AS DECIMAL FORMAT ">>>>>>>>>>>>9" 
/* 53 */    FIELD Nonsale        AS LOGICAL FORMAT "yes/no"
/* 54 */    FIELD Mengde         AS DECIMAL FORMAT ">>>>9.999" 
/* 55 */    FIELD Enhetstekst    AS CHARACTER FORMAT "x(30)" 
/* 56 */    FIELD Miljo          AS LOGICAL FORMAT "yes/no"
/* 57 */    FIELD Okologisk      AS LOGICAL FORMAT "yes/no"
/* 58 */    FIELD Gjennomfaktureres AS LOGICAL
/* 59 */    FIELD KjedeVare      AS LOGICAL
/* 10 */    FIELD ArtikkelNr     AS DEC
            INDEX ButStrkKotype butnr ean kotype DESCENDING
            INDEX ButikkNr ButNr.

DEF BUFFER bufTT_Vare FOR TT_Vare.
DEF TEMP-TABLE tmpbufTT_vare LIKE TT_Vare.

DEFINE TEMP-TABLE TT_PakkeTilEksport NO-UNDO
    FIELD Pakkenr LIKE Pakkelinje.PakkeNr
    FIELD Slette     AS LOGICAL
    FIELD Strekkoder AS CHAR
    FIELD Antall     AS CHAR
    INDEX PakkeNr IS PRIMARY PakkeNr Slette.

DEFINE TEMP-TABLE TT_MixMatchTilEksport NO-UNDO
    FIELD MixNr LIKE MixMatchHode.MixNr
    FIELD Slette     AS LOGICAL
    FIELD Strekkoder AS CHAR
    FIELD Antall     AS INTE
    FIELD radant     AS DECIMAL
    FIELD MixMatchType LIKE MixMatchHode.MixMatchType
    FIELD Utpris       LIKE MixMatchHode.Utpris
    INDEX MixNr IS PRIMARY MixNr Slette.

DEFINE TEMP-TABLE TT_Mix NO-UNDO
    FIELD butnr  AS INTEGER FORMAT ">>9"                      /* I (3)      Butikknr                                                                                 */
    FIELD aksjon AS INTEGER FORMAT "9"                        /* I (1)          Posttype, 1=ny/endring mixmatch m/totalt antall,2= ny/endring mixmatch m/antall pr vare,3= ny/endring pakkevareinnhold, 9=sletting */
    FIELD mixnr  AS INTEGER FORMAT ">>>9"                     /* I (4)          Mixmatchnr eller pakkevarenr */
    FIELD antall AS DECIMAL DECIMALS 3 FORMAT ">>>9.999"      /* De (4,3)       Totalt antall for alle varer som inngår i mixmatch */
    FIELD utpris AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)       Salgspris for varene som inngår i mixmatch ved oppnåelse av denne */
    FIELD ean    AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" /* De (13,0)      EAN/PLU-nr for varer i mixmatch eller pakkevareinnhold */
    FIELD radant AS DECIMAL DECIMALS 3 FORMAT ">>>9.99"       /* De (4,3)       Antall pr vare i mixmatch eller pakkevare */
    .

DEFINE TEMP-TABLE TT_Hgr NO-UNDO
    FIELD butnr    AS INTEGER FORMAT ">>9"              /* I (3)      Butikknr                                                                                 */
    FIELD hgr      AS INTEGER FORMAT ">>>9"             /* I (4)      Varegruppenr */
    FIELD hgrtekst AS CHARACTER FORMAT "x(30)"          /* C (30)     Varegruppetekst */
    FIELD bonus    AS LOGICAL                           /* L (yes/no) Ikke i bruk (ikke salg direkte på varegr.tast) */
    FIELD mva_like AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Ikke i bruk (ikke salg direkte på varegr.tast) */
    FIELD kunrab1  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 1 */
    FIELD kunrab2  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 2 */
    FIELD kunrab3  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 3 */
    FIELD kunrab4  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 4 */
    FIELD kunrab5  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 5 */
    FIELD kunrab6  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 6 */
    FIELD kunrab7  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 7 */
    FIELD kunrab8  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for andre butikker (intern salg) */
    FIELD kunrab9  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for ansatte (personalrabatt) */
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getLinkNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLinkNr Procedure 
FUNCTION getLinkNr RETURNS DECIMAL
  ( INPUT dLinkNr AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentbutikkListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hentbutikkListe Procedure 
FUNCTION hentbutikkListe RETURNS CHARACTER
        (INPUT iProfilNr AS INTEGER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_ELogg T "?" NO-UNDO data ELogg
   END-TABLES.
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
{syspara.i 2 1 11 dMaxPris DECI}
IF dMaxPris = 0 OR dMaxPris = ? THEN
    ASSIGN dMaxPris = 99999.
{syspara.i 2 4 15 cBonus}
ASSIGN lBonus = CAN-DO("1,ja,yes,true",TRIM(cBonus)).
{syspara.i 2 4 19 cTekst}
ASSIGN bHoyLavMva = CAN-DO("1,ja,yes,true",TRIM(cTekst)).

/* Setter datoformat for eksport til kassen. */
{syspara.i 1 1 55 iKasseEksportFormat INT}
/* Setter tabellformatet på varefil til kassen. */
{syspara.i 1 1 57 iKasseTabellformat INT}

/* eksport katalog til kasse. */
{syspara.i 1 1 56 cTekst}
IF cTekst <> '' THEN 
  cEksportKatalog = RIGHT-TRIM(cTekst,'\') + '\'.
ASSIGN
  cExportVareFil = cEksportKatalog + cExportVareFil
  cExportMixFil  = cEksportKatalog + cExportMixFil
  . 

/* Henter sentrallageret. */
{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN
DO:
    MESSAGE "*** Sentrallager butikk er ikke satt opp." SKIP
            "Kontakt systemansvarlig."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Kanske vi skall hämta iformation om kassor och filer först. */
/* Kopierer alle ELogg poster til TT_ELogg */
RUN KopierElogg.

/* Er det flagget at ALLE varer skal legges ut, så klargjøres alle varer her. */
/* Det skapes ELogg poster for alle varer og ale profiler.                    */                               
IF CAN-FIND(TT_ELogg WHERE TT_ELogg.TabellNavn = "ArtPris" AND
     TT_ELogg.EksterntSystem = "POS"    AND TT_ELogg.Verdier = "ALLE") THEN
    RUN SkapaEloggAlle.
IF CAN-FIND(TT_ELogg WHERE TT_ELogg.TabellNavn = "MixMatch" AND
     TT_ELogg.EksterntSystem = "POS" AND TT_ELogg.Verdier = "ALLE") THEN
    RUN SkapaEloggAlleMix.

/* Renser bort ELogg psoter som flagger at alle skal legges ut. */    
RUN SlettTTmedALLE.

/* TEST */
FOR EACH tt_Elogg:
    iCount = iCount + 1.
END.

/* Her bygges tabellen TT_VareTilEksport. Den danner underlag for opprettelse av varefil        */
/* Det opprettes en TT_VareTilEksport pr. berørt profil. Videre legges det en kommaseparert     */
/* butikkliste inn på hver post. Denne skal senere benyttes til å ekspandere posten pr. butikk. */
RUN FixArtBasEndringer. /*  */
RUN FixPakkeLinjeEndringer. /* Vi skall bara ta hand om slettede, övriga är hanterade tidigare */
RUN FixMixMatchEndringer.
/* RUN !!ALLEANDRE!! */

RUN SkapaVareFil.
RUN SkapaMixFilPakke.
RUN SkapaMixFilMixMatch.

FOR EACH TT_Vare:
    iCount = iCount + 1.
END.

IF cLanButiker <> "" THEN DO:
/* Vi tömmer inte filen. Ligger filen kvar sedan tidigare så har det gått fel */
/* vi appendar ny info till de gamla filerna */
/*   OUTPUT TO VALUE(cExportVareFil + "txt"). /* töm filen */ */
/*   OUTPUT CLOSE.                                            */

  /* Det er alltid bare en LAN butikk :) */
  DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
      RUN ExportVare IN THIS-PROCEDURE (INT(ENTRY(iCount,cLanButiker)),"txt"). /* parameter = den loopade butiken */
  END.
  DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
      RUN ExportPakkeMix IN THIS-PROCEDURE (INT(ENTRY(iCount,cLanButiker)),"txt"). /* parameter = den loopade butiken */
  END.
END.
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportVare IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportPakkeMix IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
/* RUN SlettTT_ELoggVare. */
IF CAN-FIND(FIRST TT_FeilVareKost) OR CAN-FIND(FIRST TT_FeilStrekKode) OR 
                                      CAN-FIND(FIRST TT_FeilPris) THEN DO:

    OUTPUT TO VALUE(SESSION:TEMP-DIR + cFeilFil).
    IF CAN-FIND(FIRST TT_FeilPris) THEN DO:
        PUT UNFORMATTED "Varer med feil pris" SKIP(2)
        "   Artikkelnr Beskrivelse                      Pris" SKIP.
        FOR EACH TT_FeilPris BY TT_FeilPris.ArtikkelNr:
            PUT TT_FeilPris.Artikkelnr " " TT_FeilPris.Beskr FORMAT "x(30)" " " TT_FeilPris.Pris SKIP.
        END.
    END.
    IF CAN-FIND(FIRST TT_FeilVareKost) THEN DO:
        PUT UNFORMATTED SKIP(1) "Varer med feil varekost" SKIP(2)
        "   Artikkelnr Beskrivelse                      Varekost" SKIP.
        FOR EACH TT_FeilVareKost BY TT_FeilVareKost.ArtikkelNr:
            PUT TT_FeilVareKost.Artikkelnr " " TT_FeilVareKost.Beskr FORMAT "x(30)" " " TT_FeilVareKost.Varekost SKIP.
        END.
    END.
    IF CAN-FIND(FIRST TT_FeilStrekKode) THEN DO:
        PUT UNFORMATTED SKIP(1) "Feilaktige strekkoder" SKIP(2)
        "   Artikkelnr Beskrivelse                      StrekKode" SKIP.
        FOR EACH TT_FeilStrekKode BY TT_FeilStrekKode.ArtikkelNr:
            PUT TT_FeilStrekKode.Artikkelnr " " TT_FeilStrekKode.Beskr FORMAT "x(30)" " " TT_FeilStrekKode.Kode SKIP.
        END.
    END.
    OUTPUT CLOSE.
    /*OS-COMMAND NO-WAIT VALUE("notepad "  + SESSION:TEMP-DIR + cFeilFil).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportPakkeMix) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportPakkeMix Procedure 
PROCEDURE ExportPakkeMix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cNumericFormat    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat       AS CHARACTER  NO-UNDO.
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "American"
           SESSION:DATE-FORMAT    = "dmy".
    OUTPUT TO VALUE(cExportMixFil + cFilsuffix) APPEND CONVERT TARGET "IBM850".
    FOR EACH TT_Mix:
        EXPORT iButik 
               TT_Mix.aksjon
               TT_Mix.mixnr 
               TT_Mix.antall
               TT_Mix.utpris
               TT_Mix.ean   
               TT_Mix.radant.
    END.
    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = cExportMixFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportMixFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cMixFiler,cExportMixFil + cFilsuffix) THEN
        ASSIGN cMixFiler = cMixFiler + (IF cMixFiler = "" THEN "" ELSE ",") + cExportMixFil + cFilsuffix.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExportVare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportVare Procedure 
PROCEDURE ExportVare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cNumericFormat    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat       AS CHARACTER  NO-UNDO.

    DEFINE BUFFER bufButiker   FOR Butiker.
    DEFINE BUFFER plukkButiker FOR Butiker.

    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "American".
    IF iKasseEksportFormat = 1 
      THEN SESSION:DATE-FORMAT    = "ymd".
    ELSE SESSION:DATE-FORMAT    = "dmy".
      
    OUTPUT TO VALUE(cExportVareFil + cFilsuffix) APPEND CONVERT TARGET "IBM850".

    FIND bufButiker NO-LOCK WHERE
         bufButiker.Butik = iButik NO-ERROR.
    IF bufButiker.PlukkButikk > 0 AND CAN-FIND(plukkButiker WHERE plukkButiker.Butik = bufButiker.PlukkButikk)
     THEN FIND plukkButiker NO-LOCK WHERE plukkButiker.Butik = bufButiker.PlukkButikk NO-ERROR.

    TT_LOOP:
    FOR EACH TT_Vare WHERE TT_Vare.ButNr = iButik:
        /* Skaper en temp-table post å jobbe med. */
        CREATE tmpbufTT_Vare.

        /* Klargjør buffer. */
        BUFFER-COPY TT_Vare TO tmpbufTT_vare.
        /* Her preppes buffer med de data som er unike for prisprofilen */
        IF tmpbufTT_Vare.kotype <> 9 THEN
        IKKE_SLETTEPOSTER:
        DO: 
        
        KLARGJOR_PROFIL:
        DO:
            FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = TT_Vare.ArtikkelNr AND
                ArtPris.ProfilNr   = bufButiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN 
              FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = TT_Vare.ArtikkelNr AND
                ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
            
            IF NOT AVAILABLE ArtPris THEN
            DO: 
              IF AVAILABLE tmpbufTT_Vare 
                THEN DELETE tmpbufTT_Vare.
              NEXT TT_LOOP.
            END.
            ELSE 
              ASSIGN
                tmpbufTT_Vare.utprisn     = ArtPris.Pris[1]
                tmpbufTT_Vare.nettokr     = ArtPris.VareKost[1] /* ArtPris.VareKost[iPrisType] */
                tmpbufTT_Vare.utprist     = IF ArtPris.Tilbud THEN ArtPris.Pris[2] ELSE 0
                tmpbufTT_Vare.fradatot    = IF ArtPris.Tilbud THEN ArtPris.TilbudFraDato ELSE ?
                tmpbufTT_Vare.fratidt     = IF ArtPris.Tilbud THEN STRING(ArtPris.TilbudFraTid,"HH:MM") ELSE ""
                tmpbufTT_Vare.tildatot    = IF ArtPris.Tilbud THEN ArtPris.TilbudTilDato ELSE ?
                tmpbufTT_Vare.tiltidt     = IF ArtPris.Tilbud THEN STRING(ArtPris.TilbudTilTid,"HH:MM") ELSE ""
                tmpbufTT_Vare.nettokrt    = IF ArtPris.Tilbud = TRUE THEN ArtPris.VareKost[2] ELSE 0
                .
        END. /* KLARGJOR_PROFIL */

        /* Legger ut vektet varekost istedenfor kalkulert varekost hvis den er <> 0 for lagerstyrte varer */
        IF tmpbufTT_Vare.kotype <> 9 AND ArtBas.Lager THEN 
        VEKTET_VAREKOST:
        DO:
            IF AVAILABLE plukkButiker THEN
            PLUKKBUTIKK: 
            DO:
              FIND Lager NO-LOCK WHERE
                Lager.ArtikkelNr = TT_Vare.ArtikkelNr AND
                Lager.Butik      = plukkButiker.Butik NO-ERROR.
              IF AVAILABLE Lager THEN
              DO:
                IF (Lager.VVareKost > 0 AND Lager.VVareKost <> ?) THEN
                    ASSIGN
                    tmpbufTT_Vare.nettokr  = Lager.VVareKost
                    tmpbufTT_Vare.nettokrt = IF ArtPris.Tilbud = TRUE THEN Lager.VVareKost ELSE 0
                    .
              END.
            END. /* PLUKKBUTIKK */
            ELSE DO:
              FIND Lager NO-LOCK WHERE
                Lager.ArtikkelNr = TT_Vare.ArtikkelNr AND
                Lager.Butik      = iButik NO-ERROR.
              IF AVAILABLE Lager THEN
              DO:
                IF (Lager.VVareKost > 0 AND Lager.VVareKost <> ?) THEN
                    ASSIGN
                    tmpbufTT_Vare.nettokr  = Lager.VVareKost
                    tmpbufTT_Vare.nettokrt = IF ArtPris.Tilbud = TRUE THEN Lager.VVareKost ELSE 0
                    .
              END.
            END.
        END. /* VEKTET_VAREKOST */
        ELSE /* Ikke lagerstyrte varer. */
          ASSIGN
            tmpbufTT_Vare.nettokr  = ArtPris.VareKost[1]
            tmpbufTT_Vare.nettokrt = IF ArtPris.Tilbud = TRUE 
                                       THEN ArtPris.VareKost[2]
                                       ELSE 0
            .
        /* Er kalkylen lagt opp med 0 varekost, skal varekost beregnes basert på kost% på varegruppen. */
        IF tmpbufTT_Vare.NettoKr <= 0 THEN
        DO:
          FIND VarGr NO-LOCK WHERE
            VarGr.Vg = TT_Vare.hgr NO-ERROR.
          IF AVAILABLE VarGr AND VarGr.Kost_Proc > 0 THEN 
          DO:
            ASSIGN
              tmpbufTT_Vare.nettokr  = (ArtPris.Pris[1] - ArtPris.MvaKr[1]) * (VarGr.Kost_Proc / 100)
              tmpbufTT_Vare.nettokrt = IF ArtPris.Tilbud = TRUE 
                                         THEN (ArtPris.Pris[2] - ArtPris.MvaKr[2]) * (VarGr.Kost_Proc / 100)
                                         ELSE 0
              tmpbufTT_Vare.nettokr  = IF tmpbufTT_Vare.nettokr = ? THEN 0 ELSE tmpbufTT_Vare.nettokr
              tmpbufTT_Vare.nettokrt = IF tmpbufTT_Vare.nettokrt = ? THEN 0 ELSE tmpbufTT_Vare.nettokrt 
              .
          END.
        END. 
        END. /* IKKE_SLETTEPOSTER */
        /* Kasser som skal ha 51 entries. */
        IF iKasseTabellformat = 0 THEN 
        EXPORT tmpbufTT_Vare.ButNr /* iButik */
               tmpbufTT_Vare.ean          
               tmpbufTT_Vare.hgr          
               tmpbufTT_Vare.bong
               tmpbufTT_Vare.opris        
               tmpbufTT_Vare.link         
               tmpbufTT_Vare.kotype       
               tmpbufTT_Vare.vekt         
               tmpbufTT_Vare.utprisn      
               tmpbufTT_Vare.bonus        
               
               tmpbufTT_Vare.mva          
               tmpbufTT_Vare.krabatt      
               tmpbufTT_Vare.varetekst
               tmpbufTT_Vare.nettokr      
               tmpbufTT_Vare.bestnr       
               tmpbufTT_Vare.mixnr        
               tmpbufTT_Vare.pakkenr      
               tmpbufTT_Vare.bestvare     
               tmpbufTT_Vare.utprist      
               tmpbufTT_Vare.tidsstyrtt   
               
               tmpbufTT_Vare.fradatot     
               tmpbufTT_Vare.fratidt      
               tmpbufTT_Vare.tildatot     
               tmpbufTT_Vare.tiltidt      
               tmpbufTT_Vare.utprism      
               tmpbufTT_Vare.tidsstyrtm   
               tmpbufTT_Vare.fradatom     
               tmpbufTT_Vare.fratidm      
               tmpbufTT_Vare.tildatom     
               tmpbufTT_Vare.tiltidm      
               
               tmpbufTT_Vare.utprisa      
               tmpbufTT_Vare.fra          
               tmpbufTT_Vare.til          
               tmpbufTT_Vare.idkrav       
               tmpbufTT_Vare.lager        
               tmpbufTT_Vare.individ      
               tmpbufTT_Vare.garantikl    
               tmpbufTT_Vare.bilde        
               tmpbufTT_Vare.timestat     
               tmpbufTT_Vare.nettokrt     
               
               tmpbufTT_Vare.Stoppkode    
               tmpbufTT_Vare.Storrelsesnr  
               tmpbufTT_Vare.Storrelsestype
               tmpbufTT_Vare.Lopenr        
               tmpbufTT_Vare.Handlingskode 
               tmpbufTT_Vare.Modell        
               tmpbufTT_Vare.Bestnr2       
               tmpbufTT_Vare.Vekttilbud    
               tmpbufTT_Vare.VareNr
               tmpbufTT_Vare.Fargenr 
               
               tmpbufTT_Vare.Rabikas
               .           
        ELSE /* Kasser med fler enn 51 entries */ 
        EXPORT tmpbufTT_Vare.ButNr /* iButik */
               tmpbufTT_Vare.ean          
               tmpbufTT_Vare.hgr          
               tmpbufTT_Vare.bong
               tmpbufTT_Vare.opris        
               tmpbufTT_Vare.link         
               tmpbufTT_Vare.kotype       
               tmpbufTT_Vare.vekt         
               tmpbufTT_Vare.utprisn      
               tmpbufTT_Vare.bonus        
               
               tmpbufTT_Vare.mva          
               tmpbufTT_Vare.krabatt      
               tmpbufTT_Vare.varetekst
               tmpbufTT_Vare.nettokr      
               tmpbufTT_Vare.bestnr       
               tmpbufTT_Vare.mixnr        
               tmpbufTT_Vare.pakkenr      
               tmpbufTT_Vare.bestvare     
               tmpbufTT_Vare.utprist      
               tmpbufTT_Vare.tidsstyrtt   
               
               tmpbufTT_Vare.fradatot     
               tmpbufTT_Vare.fratidt      
               tmpbufTT_Vare.tildatot     
               tmpbufTT_Vare.tiltidt      
               tmpbufTT_Vare.utprism      
               tmpbufTT_Vare.tidsstyrtm   
               tmpbufTT_Vare.fradatom     
               tmpbufTT_Vare.fratidm      
               tmpbufTT_Vare.tildatom     
               tmpbufTT_Vare.tiltidm      
               
               tmpbufTT_Vare.utprisa      
               tmpbufTT_Vare.fra          
               tmpbufTT_Vare.til          
               tmpbufTT_Vare.idkrav       
               tmpbufTT_Vare.lager        
               tmpbufTT_Vare.individ      
               tmpbufTT_Vare.garantikl    
               tmpbufTT_Vare.bilde        
               tmpbufTT_Vare.timestat     
               tmpbufTT_Vare.nettokrt     
               
               tmpbufTT_Vare.Stoppkode    
               tmpbufTT_Vare.Storrelsesnr  
               tmpbufTT_Vare.Storrelsestype
               tmpbufTT_Vare.Lopenr        
               tmpbufTT_Vare.Handlingskode 
               tmpbufTT_Vare.Modell        
               tmpbufTT_Vare.Bestnr2       
               tmpbufTT_Vare.Vekttilbud    
               tmpbufTT_Vare.VareNr
               tmpbufTT_Vare.Fargenr 
               
               tmpbufTT_Vare.Rabikas
               tmpbufTT_Vare.Hovedean        
               tmpbufTT_Vare.Nonsale        
               tmpbufTT_Vare.Mengde          
               tmpbufTT_Vare.Enhetstekst     
               tmpbufTT_Vare.Miljo           
               tmpbufTT_Vare.Okologisk
               .           
        /* Renser buffer */
        DELETE tmpbufTT_Vare.
    END. /* TT_LOOP */
    OUTPUT CLOSE.
    
    FILE-INFO:FILE-NAME = cExportVareFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportVareFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cVareFiler,cExportVareFil + cFilsuffix) THEN
        ASSIGN cVareFiler = cVareFiler + (IF cVareFiler = "" THEN "" ELSE ",") + cExportVareFil + cFilsuffix.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixArtBasEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixArtBasEndringer Procedure 
PROCEDURE FixArtBasEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lPakkeBort AS LOGICAL    NO-UNDO.
DEFINE VARIABLE dTest      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cTekst     AS CHAR       NO-UNDO.
DEFINE VARIABLE cFixStrekkode AS CHARACTER   NO-UNDO.
DEF VAR piLoop AS INT NO-UNDO.

DEFINE BUFFER bTT_ELogg FOR TT_ELogg.

/* Behandler sletting av strekkoder. Elogg poster fra Strekkode, har 3 entries i verdi feltet.    */
/* NB: Sletting av strekkoder skal gå til ALLE profiler.                                          */
/* Först samlar vi samman alla Streckkoder som tillhör en record till en behandlingspost          */
/* Därefter ser vi om vi har en ändringspost på själva ArtBas. Om den inte finns ser vi på ArtBas */
/* om flaggan för 'iKassa' är satt. Om den inte är det kan vi ignorera sletting av strekkoderna   */
FOR EACH TT_ELogg WHERE 
            TT_ELogg.TabellNavn     = "ArtPris" AND
            TT_ELogg.EksterntSystem = "POS"    AND
            TT_ELogg.EndringsType   = 3 AND 
            NUM-ENTRIES(TT_ELogg.Verdier,CHR(1)) = 3 BY TT_ELogg.Verdier:
    /* skall vi se på om artikeln inte har ändrats och iKasse inte har satts?? */
    FIND TT_ArtTilEksport WHERE TT_ArtTilEksport.ArtikkelNr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))) NO-ERROR.
    IF NOT AVAIL TT_ArtTilEksport THEN DO:
        CREATE TT_ArtTilEksport.
        ASSIGN TT_ArtTilEksport.ArtikkelNr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1)))
               TT_ArtTilEksport.Slette     = TRUE.
    END.
    ASSIGN cFixStrekkode = ENTRY(3,TT_ELogg.Verdier,CHR(1)).
/*     IF LENGTH(cFixStrekkode) = 13 AND cFixStrekkode BEGINS "2" THEN */
/*         ASSIGN cFixStrekkode = SUBSTR(cFixStrekkode,1,12) + "0".    */
    ASSIGN TT_ArtTilEksport.Strekkoder = TT_ArtTilEksport.Strekkoder + (IF TT_ArtTilEksport.Strekkoder = "" THEN "" ELSE ",")
                                         + cFixStrekkode.
END.

/* Behandler artikkel og prisendringer. Disse har 2 entries i verdi feltet. */
/* Här ser vi om det finns en ändring på själva artikeln.                   */
/* Om vi "iKasse" är satt skall vi sända ut ändring på alla streckkoder     */
/* Om den inte är satt skall vi sända sletteposter för alla strekkoder      */
TT_ELOGG:
FOR EACH TT_ELogg WHERE 
            TT_ELogg.TabellNavn     = "ArtPris" AND
            TT_ELogg.EksterntSystem = "POS"    AND
            NUM-ENTRIES(TT_ELogg.Verdier,CHR(1)) = 2 BY TT_ELogg.Verdier:

    /* Lager en liste over de butikker som er i profilen. */  
    ASSIGN
      cButikkLst = hentButikkListe(int(ENTRY(2,TT_ELogg.Verdier,CHR(1)))) NO-ERROR.        

    FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
    FIND ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                       ArtPris.ProfilNr   = INT(ENTRY(2,TT_ELogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
    /* Hvis det ikke finnes en lokal ArtPris, skal HK's ArtPris legges ut istedenfor. */
    /* Når det finnes en Elogg post, SKAL det sendes ut til kassen.                   */                   
    IF NOT AVAILABLE ArtPris THEN 
      FIND ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                         ArtPris.ProfilNr   = clButiker.ProfilNr NO-LOCK NO-ERROR.
    IF AVAIL ArtBas AND AVAILABLE ArtPris THEN DO:
        FOR EACH StrekKode OF ArtBas NO-LOCK WHERE StrekKode.IKasse = TRUE:
            ASSIGN dTest = ABS(DECI(StrekKode.Kode)) NO-ERROR.
            /* det har förekommit koder med - på slutet, därför denna test på slutet */
            IF ERROR-STATUS:ERROR OR LENGTH(StrekKode.Kode) > 13 THEN DO:
                CREATE TT_FeilStrekKode.
                ASSIGN TT_FeilStrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                       TT_FeilStrekKode.Beskr      = ArtBas.Beskr
                       TT_FeilStrekKode.Kode       = StrekKode.Kode.
                NEXT.
            END.
            /* Kun numeriske verdier tillatt i strekkoden i kassen. */
/*             DO piLoop = 1 TO LENGTH(TRIM(Strekkode.Kode)):                 */
/*                 ASSIGN cTekst = SUBSTRING(TRIM(StrekKode.Kode),piLoop,1).  */
/*                 IF NOT CAN-DO("0,1,2,3,4,5,6,7,8,9",cTekst) THEN DO:       */
/*                     CREATE TT_FeilStrekKode.                               */
/*                     ASSIGN TT_FeilStrekKode.ArtikkelNr = ArtBas.ArtikkelNr */
/*                            TT_FeilStrekKode.Beskr      = ArtBas.Beskr      */
/*                            TT_FeilStrekKode.Kode       = StrekKode.Kode.   */
/*                     NEXT TT_ELOGG.                                         */
/*                 END.                                                       */
/*             END.                                                           */

            FIND TT_ArtTilEksport WHERE TT_ArtTilEksport.ArtikkelNr = ArtBas.ArtikkelNr AND
                                        TT_ArtTilEksport.ProfilNr   = ArtPris.ProfilNr AND 
                                        TT_ArtTilEksport.Slette = (NOT ArtBas.iKasse) NO-ERROR.
            ASSIGN cFixStrekkode = StrekKode.Kode.
/*             IF LENGTH(cFixStrekkode) = 13 AND cFixStrekkode BEGINS "2" THEN */
/*                 ASSIGN cFixStrekkode = SUBSTR(cFixStrekkode,1,12) + "0".    */

            IF AVAIL TT_ArtTilEksport AND NOT CAN-DO(TT_ArtTilEksport.Strekkoder,cFixStrekkode) THEN
                ASSIGN TT_ArtTilEksport.Strekkoder = TT_ArtTilEksport.Strekkoder + "," + cFixStrekkode.
            ELSE DO:
                CREATE TT_ArtTilEksport.
                ASSIGN TT_ArtTilEksport.ArtikkelNr = ArtBas.ArtikkelNr
                       TT_ArtTilEksport.ProfilNr   = ArtPris.ProfilNr
                       TT_ArtTilEksport.Slette     = NOT ArtBas.iKasse
                       TT_ArtTilEksport.Strekkoder = cFixStrekkode
                       TT_ArtTilEksport.ButikkLst  = cButikkLst.
            END.
            /* Tom pakke */
            ASSIGN TT_ArtTilEksport.Slette = /* IF lPakkeBort THEN TRUE ELSE */ TT_ArtTilEksport.Slette.
        END.
        IF ArtBas.Pakke = TRUE /* AND ArtBas.iKasse = TRUE */ AND CAN-FIND(FIRST PakkeLinje OF Artbas) THEN DO:
            CREATE TT_PakkeTilEksport.
            ASSIGN TT_PakkeTilEksport.Pakkenr    = ArtBas.PakkeNr
                   TT_PakkeTilEksport.Slette     = ArtBas.iKasse = FALSE.
            FOR EACH PakkeLinje WHERE Pakkelinje.PakkeNr = ArtBas.PakkeNr NO-LOCK:
                /* se efter om det finns en slette i ELogg */
                FIND bTT_ELogg WHERE
                     bTT_ELogg.TabellNavn     = "Pakkelinje" AND
                     bTT_ELogg.EksterntSystem = "POS"    AND
                     bTT_ELogg.Verdier        = STRING(Pakkelinje.PakkeNr) + CHR(1) + string(Pakkelinje.StrKode) NO-ERROR.
                IF AVAIL bTT_ELogg THEN
                    DELETE bTT_ELogg.
                ASSIGN TT_PakkeTilEksport.Strekkoder = TT_PakkeTilEksport.Strekkoder +
                                      (IF TT_PakkeTilEksport.Strekkoder = "" THEN "" ELSE ",") +
                                       string(Pakkelinje.StrKode)
                       TT_PakkeTilEksport.Antall = TT_PakkeTilEksport.Antall +
                                       (IF TT_PakkeTilEksport.Antall = "" THEN "" ELSE ",") +
                                           STRING(Pakkelinje.Antall).
                       
            END.
        END.
    END.
END.
/* Alle strekkoder skal generere en ny/endrepost */
FOR EACH TT_ELogg WHERE 
            TT_ELogg.TabellNavn     = "ArtPris" AND
            TT_ELogg.EksterntSystem = "POS"    AND
            TT_ELogg.EndringsType   = 1 AND 
            NUM-ENTRIES(TT_ELogg.Verdier,CHR(1)) = 3 BY TT_ELogg.Verdier:
            
    FIND TT_ArtTilEksport WHERE TT_ArtTilEksport.ArtikkelNr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))) AND
                                TT_ArtTilEksport.ProfilNr   = INT(ENTRY(2,TT_ELogg.Verdier,CHR(1))) AND 
                                TT_ArtTilEksport.Slette     = FALSE NO-ERROR.
    IF NOT AVAIL TT_ArtTilEksport THEN DO:
        /* Lager en liste over de butikker som er i profilen. */  
        cButikkLst = hentButikkListe(int(ENTRY(2,TT_ELogg.Verdier,CHR(1)))).        
    
        CREATE TT_ArtTilEksport.
        ASSIGN TT_ArtTilEksport.ArtikkelNr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1)))
               TT_ArtTilEksport.ProfilNr   = DECI(ENTRY(2,TT_ELogg.Verdier,CHR(1))) 
               TT_ArtTilEksport.Slette     = FALSE
               TT_ArtTilEksport.ButikkLst  = cButikkLst.
    END.
    ASSIGN cFixStrekkode = ENTRY(3,TT_ELogg.Verdier,CHR(1)).
/*     IF LENGTH(cFixStrekkode) = 13 AND cFixStrekkode BEGINS "2" THEN */
/*         ASSIGN cFixStrekkode = SUBSTR(cFixStrekkode,1,12) + "0".    */
    IF NOT CAN-DO(TT_ArtTilEksport.Strekkoder,cFixStrekkode) THEN
        ASSIGN TT_ArtTilEksport.Strekkoder = TT_ArtTilEksport.Strekkoder + (IF TT_ArtTilEksport.Strekkoder = "" THEN "" ELSE ",")
                                         + cFixStrekkode.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixMixMatchEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixMixMatchEndringer Procedure 
PROCEDURE FixMixMatchEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTest AS DECIMAL    NO-UNDO.
    FOR EACH TT_ELogg WHERE 
             TT_ELogg.TabellNavn     = "MixMatch" AND
             TT_ELogg.EksterntSystem = "POS"    AND
             TT_ELogg.EndringsType   = 3 AND NUM-ENTRIES(TT_ELogg.Verdier,CHR(1)) = 2
             BY TT_ELogg.Verdier:
        FOR EACH StrekKode WHERE StrekKode.ArtikkelNr = DECI(ENTRY(2,TT_ELogg.Verdier,CHR(1))) NO-LOCK.
            ASSIGN dTest = DECI(StrekKode.Kode) NO-ERROR.
            IF ERROR-STATUS:ERROR OR LENGTH(StrekKode.Kode) > 13 THEN DO:
/*                 ASSIGN TT_FeilStrekKode.ArtikkelNr = StrekKode.ArtikkelNr */
/*                        TT_FeilStrekKode.Beskr      = ""                   */
/*                        TT_FeilStrekKode.Kode       = StrekKode.Kode.      */
                NEXT.
            END.
            CREATE TT_MixMatchTilEksport.
            ASSIGN TT_MixMatchTilEksport.MixNr      = INT(ENTRY(1,TT_ELogg.Verdier,CHR(1))) /* ArtBas.PakkeNr */
                   TT_MixMatchTilEksport.Slette     = TRUE.
                   TT_MixMatchTilEksport.Strekkoder = StrekKode.Kode.
        END.
    END.
    FOR EACH TT_ELogg WHERE 
             TT_ELogg.TabellNavn     = "MixMatch" AND
             TT_ELogg.EksterntSystem = "POS"    AND
             TT_ELogg.EndringsType   = 1 AND NUM-ENTRIES(TT_ELogg.Verdier,CHR(1)) = 1
             BY TT_ELogg.Verdier:
        FIND MixMatchHode WHERE MixMatchHode.MixNr = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
        IF NOT AVAIL MixMatchHode THEN
            NEXT.
        FOR EACH MixMatchRad OF MixMatchHode NO-LOCK:
            FOR EACH StrekKode WHERE StrekKode.ArtikkelNr = DECI(MixMatchRad.Kode) AND StrekKode.KodeType < 2 NO-LOCK.
                ASSIGN dTest = DECI(StrekKode.Kode) NO-ERROR.
                IF ERROR-STATUS:ERROR OR LENGTH(StrekKode.Kode) > 13 THEN DO:
/*                     ASSIGN TT_FeilStrekKode.ArtikkelNr = StrekKode.ArtikkelNr */
/*                            TT_FeilStrekKode.Beskr      = ""                   */
/*                            TT_FeilStrekKode.Kode       = StrekKode.Kode.      */
                    NEXT.
                END.
                FIND TT_MixMatchTilEksport WHERE TT_MixMatchTilEksport.MixNr   = INT(TT_ELogg.Verdier) AND
                                                 TT_MixMatchTilEksport.Slette  = TRUE AND
                                                 TT_MixMatchTilEksport.Strekkoder = StrekKode.Kode NO-ERROR.
                IF AVAIL TT_MixMatchTilEksport THEN
                    ASSIGN TT_MixMatchTilEksport.MixMatchType = MixMatchHode.MixMatchType
                           TT_MixMatchTilEksport.Antall       = MixMatchHode.Antall
                           TT_MixMatchTilEksport.Utpris       = MixMatchHode.Utpris
                           TT_MixMatchTilEksport.radant       = MixMatchrad.Antall
                           TT_MixMatchTilEksport.Strekkoder   = StrekKode.Kode
                           TT_MixMatchTilEksport.Slette       = FALSE.
                ELSE DO:
                    CREATE TT_MixMatchTilEksport.
                    ASSIGN TT_MixMatchTilEksport.MixNr        = MixMatchHode.MixNr
                           TT_MixMatchTilEksport.MixMatchType = MixMatchHode.MixMatchType
                           TT_MixMatchTilEksport.Antall       = MixMatchHode.Antall
                           TT_MixMatchTilEksport.Utpris       = MixMatchHode.Utpris   
                           TT_MixMatchTilEksport.radant       = MixMatchrad.Antall
                           TT_MixMatchTilEksport.Strekkoder   = StrekKode.Kode
                           TT_MixMatchTilEksport.Slette       = FALSE.
                END.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixPakkeLinjeEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixPakkeLinjeEndringer Procedure 
PROCEDURE FixPakkeLinjeEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTest AS DECIMAL    NO-UNDO.
    FOR EACH TT_ELogg WHERE 
             TT_ELogg.TabellNavn     = "PakkeLinje" AND
             TT_ELogg.EksterntSystem = "POS"    AND
             TT_ELogg.EndringsType   = 3
             BY TT_ELogg.Verdier:
        ASSIGN dTest = DECI(ENTRY(2,TT_ELogg.Verdier,CHR(1))) NO-ERROR.
        IF ERROR-STATUS:ERROR OR LENGTH(ENTRY(2,TT_ELogg.Verdier,CHR(1))) > 13 THEN DO:
/*             ASSIGN TT_FeilStrekKode.ArtikkelNr = ?                                           */
/*                    TT_FeilStrekKode.Beskr      = "Pakke " + ENTRY(1,TT_ELogg.Verdier,CHR(1)) */
/*                    TT_FeilStrekKode.Kode       = ENTRY(2,TT_ELogg.Verdier,CHR(1)).           */
            NEXT.
        END.
        FIND TT_PakkeTilEksport WHERE TT_PakkeTilEksport.Pakkenr = INT(ENTRY(1,TT_ELogg.Verdier,CHR(1))) AND
                                      TT_PakkeTilEksport.Slette  = TRUE NO-ERROR.
        IF NOT AVAIL TT_PakkeTilEksport THEN DO:
             CREATE TT_PakkeTilEksport.
             ASSIGN TT_PakkeTilEksport.Pakkenr    = INT(ENTRY(1,TT_ELogg.Verdier,CHR(1))) /* ArtBas.PakkeNr */
                    TT_PakkeTilEksport.Slette     = TRUE.
        END.
        ASSIGN TT_PakkeTilEksport.Strekkoder = TT_PakkeTilEksport.Strekkoder +
                                  (IF TT_PakkeTilEksport.Strekkoder = "" THEN "" ELSE ",") +
                                   ENTRY(2,TT_ELogg.Verdier,CHR(1)).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dDatoTid AS DECIMAL     NO-UNDO.
    DEFINE BUFFER bElogg FOR Elogg.

    dDatoTid =  dec(STRING(YEAR(TODAY),"9999") +
                    string(MONTH(TODAY),"99") + 
                    string(DAY(TODAY),"99") +
                    string(TIME)).


    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "ArtPris" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") OR 
        CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "Pakkelinje" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") OR 
        CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "MixMatch" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN 
    DO TRANSACTION: 
        FIND ELogg WHERE ELogg.TabellNavn     = "ArtPris" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        
        FIND ELogg WHERE ELogg.TabellNavn     = "Pakkelinje" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        
        FIND ELogg WHERE ELogg.TabellNavn     = "MixMatch" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END. /* TRANSACTION */
    ELSE DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "ArtPris" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK TRANSACTION:
            IF dDatoTid - ELogg.Opprettet < 60 THEN
                NEXT.
            /* Retur pant artikkel skal ikke legges ut. */ 
            IF NOT ENTRY(1,ELogg.Verdier,CHR(1)) = cReturPant THEN
                /* Allt annet skal ut */                  
                BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg AND NOT LOCKED bELogg  AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        
        FOR EACH ELogg WHERE ELogg.TabellNavn = "Pakkelinje" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK TRANSACTION:
            IF dDatoTid - ELogg.Opprettet < 60 THEN
                NEXT.
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg  AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        
        FOR EACH ELogg WHERE ELogg.TabellNavn = "MixMatch" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK TRANSACTION:
            IF dDatoTid - ELogg.Opprettet < 60 THEN
                NEXT.
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg  AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        /* Ny artikkel som skal sendes til alle profiler. */
        NYARTIKKEL:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "ArtBas" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK TRANSACTION:
            IF dDatoTid - ELogg.Opprettet < 60 THEN
                NEXT.
            /* Det skal være lagt på strekkode før artikkelen kan sendes */
            IF NOT CAN-FIND(FIRST Strekkode WHERE
                            Strekkode.ArtikkelNr = DECI(ENTRY(1,ELogg.Verdier,CHR(1))))
              THEN NEXT NYARTIKKEL.
            ELSE DO:            
                /* Retur pant skal ikke legges ut. */
                IF ENTRY(1,ELogg.Verdier,CHR(1)) <> cReturPant THEN 
                BUTIKKLOOP:
                FOR EACH Butiker NO-LOCK WHERE
                  Butiker.harButikksystem = TRUE AND
                  Butiker.ApningsDato <= TODAY AND
                  Butiker.NedlagtDato  = ?
                  BREAK BY Butiker.ProfilNr:
                  
                  IF FIRST-OF(Butiker.ProfilNr) THEN 
                  DO:
                    CREATE TT_Elogg.
                    ASSIGN TT_ELogg.TabellNavn     = "ArtPris"
                           TT_ELogg.EksterntSystem = "POS"   
                           TT_ELogg.Verdier        = ENTRY(1,ELogg.Verdier,CHR(1)) + CHR(1) + string(Butiker.ProfilNr)
                           NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN DO:
                      DELETE TT_ELogg.
                      NEXT BUTIKKLOOP. 
                    END.
                  END.                
                END. /* BUTIKKLOOP */
                
                FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
                IF AVAIL bElogg AND NOT LOCKED bELogg  AND cPRSFtpButiker = '' THEN
                    DELETE bELogg.
                IF AVAILABLE TT_Elogg THEN
                    RELEASE TT_ELogg.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaEloggAlle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaEloggAlle Procedure 
PROCEDURE SkapaEloggAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

DEFINE BUFFER klarButiker FOR Butiker.
DEFINE BUFFER klarArtPris FOR ArtPris.

IF TRIM(cLanButiker) <> '' THEN 
LANBUTIKKER:
DO piLoop = 1 TO NUM-ENTRIES(cLanbutiker):
  FIND klarButiker NO-LOCK WHERE
    klarButiker.Butik = int(ENTRY(piLoop,cLanbutiker)) NO-ERROR.
  IF NOT AVAILABLE klarButiker THEN 
    LEAVE LANBUTIKKER.
      
  FOR EACH ArtBas WHERE ArtBas.IKasse = TRUE NO-LOCK:
    /* Leser alle priser på kjeden og logger for den aktuelle profilen. */
    FOR EACH ArtPris OF ArtBas NO-LOCK WHERE 
      ArtPris.ProfilNr = clButiker.ProfilNr:
      FIND tt_ELogg WHERE 
           tt_ELogg.TabellNavn     = "ArtPris" AND
           tt_ELogg.EksterntSystem = "POS"    AND
           tt_ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(klarButiker.ProfilNr) NO-ERROR.
      IF NOT AVAIL tt_Elogg THEN DO:
        CREATE tt_Elogg.
        ASSIGN tt_ELogg.TabellNavn     = "ArtPris"
               tt_ELogg.EksterntSystem = "POS"   
               tt_ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(klarButiker.ProfilNr).
      END.
      ASSIGN tt_ELogg.EndringsType = 1
             tt_ELogg.Behandlet    = FALSE.
    END.
  END.
END. /* LANBUTIKKER */

IF TRIM(cFtpButiker) <> '' THEN 
FTPBUTIKKER:
DO piLoop = 1 TO NUM-ENTRIES(cFtpButiker):
  FIND klarButiker NO-LOCK WHERE
    klarButiker.Butik = int(ENTRY(piLoop,cFtpButiker)) NO-ERROR.
  IF NOT AVAILABLE klarButiker THEN 
    LEAVE FTPBUTIKKER.

  FOR EACH ArtBas WHERE ArtBas.IKasse = TRUE NO-LOCK:
    /* Leser alle priser på kjeden og logger for den aktuelle profilen. */
    FOR EACH ArtPris OF ArtBas NO-LOCK WHERE 
      ArtPris.ProfilNr = clButiker.ProfilNr:
      FIND tt_ELogg WHERE 
           tt_ELogg.TabellNavn     = "ArtPris" AND
           tt_ELogg.EksterntSystem = "POS"    AND
           tt_ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(klarButiker.ProfilNr) NO-ERROR.
      IF NOT AVAIL tt_Elogg THEN DO:
        CREATE tt_Elogg.
        ASSIGN tt_ELogg.TabellNavn     = "ArtPris"
               tt_ELogg.EksterntSystem = "POS"   
               tt_ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(klarButiker.ProfilNr).
      END.
      ASSIGN tt_ELogg.EndringsType = 1
             tt_ELogg.Behandlet    = FALSE.
    END.
  END.
END. /* FTPBUTIKKER */

/* TN 31/5-10 Originalkode i denne proceduren
  FOR EACH ArtBas WHERE ArtBas.IKasse = TRUE NO-LOCK:
    FOR EACH ArtPris OF ArtBas NO-LOCK:     
      FIND ELogg WHERE 
           ELogg.TabellNavn     = "ArtPris" AND
           ELogg.EksterntSystem = "POS"    AND
           ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr) NO-ERROR.
      IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "ArtPris"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr).
      END.
      ASSIGN ELogg.EndringsType = 1
             ELogg.Behandlet    = FALSE.
    END.
  END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaEloggAlleMix) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaEloggAlleMix Procedure 
PROCEDURE SkapaEloggAlleMix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH MixMatchHode WHERE MixMatchHode.MixAktiver = TRUE:
      FIND TT_ELogg WHERE 
           TT_ELogg.TabellNavn     = "MixMatch" AND
           TT_ELogg.EksterntSystem = "POS"    AND
           TT_ELogg.Verdier        = STRING(MixMatchHode.MixNr) NO-ERROR.
      IF NOT AVAIL TT_ELogg THEN DO:
          CREATE TT_ELogg.
          ASSIGN TT_ELogg.TabellNavn     = "MixMatch"
                 TT_ELogg.EksterntSystem = "POS"   
                 TT_ELogg.Verdier        = STRING(MixMatchHode.MixNr).
      END.
      ASSIGN TT_ELogg.EndringsType = 1 
             TT_ELogg.Behandlet    = FALSE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaMixFilMixMatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaMixFilMixMatch Procedure 
PROCEDURE SkapaMixFilMixMatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    FOR EACH TT_MixMatchTilEksport WHERE Slette = TRUE:
        CREATE TT_Mix.
        ASSIGN TT_Mix.butnr   = 0
               TT_Mix.aksjon  = 9
               TT_Mix.MixNr   = TT_MixMatchTilEksport.MixNr
               TT_Mix.ean     = DECI(TT_MixMatchTilEksport.Strekkoder)
               iAntPakkelinjer = iAntPakkelinjer + 1.
        DELETE TT_MixMatchTilEksport.
    END.
    FOR EACH TT_MixMatchTilEksport: /* vi har bara ändringsposter kvar */
/*         FIND TT_Mix WHERE TT_Mix.butnr  = 0 AND                                                           */
/*                           TT_Mix.aksjon = 9 AND                                                           */
/*                           TT_Mix.MixNr  = TT_MixMatchTilEksport.MixNr AND                                 */
/*                           TT_Mix.ean     = DECI(ENTRY(iCount,TT_MixMatchTilEksport.Strekkoder)) NO-ERROR. */
/*         IF AVAIL TT_Mix THEN                                                                              */
/*             ASSIGN TT_Mix.aksjon = 1                                                                      */
/*                    TT_Mix.radant  = DECI(ENTRY(iCount,TT_MixMatchTilEksport.Antall)).                     */
/*         ELSE DO:                                                                                          */
            CREATE TT_Mix.
            ASSIGN TT_Mix.butnr   = 0
                   TT_Mix.aksjon  = MixMatchType
                   TT_Mix.mixnr   = TT_MixMatchTilEksport.MixNr
                   TT_Mix.antall  = TT_MixMatchTilEksport.Antall
                   TT_Mix.ean     = DECI(TT_MixMatchTilEksport.Strekkoder)
                   TT_Mix.radant  = TT_MixMatchTilEksport.radant
                   TT_Mix.Utpris  = TT_MixMatchTilEksport.Utpris
                   iAntPakkelinjer = iAntPakkelinjer + 1.
/*         END. */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaMixFilPakke) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaMixFilPakke Procedure 
PROCEDURE SkapaMixFilPakke :
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    FOR EACH TT_PakkeTilEksport WHERE Slette = TRUE:
        IF TT_PakkeTilEksport.Strekkode <> "" THEN
        DO iCount = 1 TO NUM-ENTRIES(TT_PakkeTilEksport.Strekkoder):
            CREATE TT_Mix.
            ASSIGN TT_Mix.butnr   = 0
                   TT_Mix.aksjon  = 9
                   TT_Mix.MixNr   = TT_PakkeTilEksport.Pakkenr
                   TT_Mix.ean     = DECI(ENTRY(iCount,TT_PakkeTilEksport.Strekkoder))
                   iAntPakkelinjer = iAntPakkelinjer + 1.
        END.
        DELETE TT_PakkeTilEksport.
    END.
    FOR EACH TT_PakkeTilEksport WHERE TT_PakkeTilEksport.Strekkode <> "": /* vi har bara ändringsposter kvar */
        DO iCount = 1 TO NUM-ENTRIES(TT_PakkeTilEksport.Strekkode):
            FIND TT_Mix WHERE TT_Mix.butnr  = 0 AND
                              TT_Mix.aksjon = 9 AND
                              TT_Mix.MixNr  = TT_PakkeTilEksport.Pakkenr AND
                              TT_Mix.ean     = DECI(ENTRY(iCount,TT_PakkeTilEksport.Strekkoder)) NO-ERROR.
            IF AVAIL TT_Mix THEN
                ASSIGN TT_Mix.aksjon = 3
                       TT_Mix.radant  = DECI(ENTRY(iCount,TT_PakkeTilEksport.Antall)).
            ELSE DO:
                CREATE TT_Mix.
                ASSIGN TT_Mix.butnr   = 0
                       TT_Mix.aksjon  = 3
                       TT_Mix.mixnr   = TT_PakkeTilEksport.Pakkenr
                       TT_Mix.ean     = DECI(ENTRY(iCount,TT_PakkeTilEksport.Strekkoder))
                       TT_Mix.radant  = DECI(ENTRY(iCount,TT_PakkeTilEksport.Antall))
                       iAntPakkelinjer = iAntPakkelinjer + 1.
            END.
        END.           
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaVareFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaVareFil Procedure 
PROCEDURE SkapaVareFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLoop        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dInterLeaf   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cStrekKode   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStrl        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cKode        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iPrisType    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dLinkNr      AS DECIMAL DECIMALS 0   NO-UNDO.
    DEFINE VARIABLE dTest      AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cExpButiker AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dHovedEan    AS DEC NO-UNDO.
    
    FOR EACH TT_ArtTilEksport WHERE Slette = TRUE:
    
        IF TT_ArtTilEksport.Strekkoder <> "" THEN
        DO iCount = 1 TO NUM-ENTRIES(TT_ArtTilEksport.Strekkoder):
            ASSIGN dTest = ABS(DECI(ENTRY(iCount,TT_ArtTilEksport.Strekkoder))) NO-ERROR.
            /* det har förekommit koder med - på slutet, därför denna test på slutet */
            IF dTest = 0 THEN NEXT. /* Blanke strekkoder skal ikke med. */
            IF ERROR-STATUS:ERROR OR (ENTRY(iCount,TT_ArtTilEksport.Strekkoder) <> STRING(dTest,FILL("9",LENGTH(ENTRY(iCount,TT_ArtTilEksport.Strekkoder))))) THEN
                NEXT.
            /* Oppretter en TT_Vare pr. butikk i profilen */
           ASSIGN cExpButiker = TRIM(cLanButiker)
                  cExpButiker = cExpButiker + (IF cExpButiker <> "" THEN "," ELSE "") + cFtpButiker.
            IF TRIM(cExpButiker) <> '' THEN 
            PROFIL:
            DO iLoop = 1 TO NUM-ENTRIES(cExpButiker):
              CREATE TT_Vare.
              ASSIGN TT_Vare.butnr  = INTEGER(ENTRY(iLoop,cExpButiker))
                     TT_Vare.ean    = DECI(ENTRY(iCount,TT_ArtTilEksport.Strekkoder))
                     TT_Vare.kotype = 9
                     iAntVarer      = iAntVarer + 1 NO-ERROR.

              IF ERROR-STATUS:ERROR THEN 
                DO:
                  IF AVAILABLE tt_Vare THEN DELETE tt_Vare.
                END.
            END. /* PROFIL */
              
        END.
        DELETE TT_ArtTilEksport.
    END.
    
    FOR EACH TT_ArtTilEksport: /* vi har bara ändringsposter kvar */
        FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_ArtTilEksport.ArtikkelNr NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN 
          NEXT.
        
        FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE  
            ArtPris.ProfilNr = TT_ArtTilEksport.ProfilNr NO-ERROR.
        IF NOT AVAIL ArtPris THEN
          FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtPris THEN
            NEXT.                  
        FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
        IF NOT AVAIL VarGr THEN
            NEXT.
        FIND StrType OF ArtBas NO-LOCK NO-ERROR.
        IF NOT AVAIL StrType THEN
            NEXT.
        FIND Moms OF VarGr NO-LOCK NO-ERROR.
        FIND FIRST Strekkode OF ArtBas NO-LOCK WHERE Strekkode.HovedNr = TRUE NO-ERROR.
        IF NOT AVAILABLE Strekkode THEN 
            FIND FIRST Strekkode OF ArtBas NO-ERROR.
        IF AVAILABLE Strekkode 
            THEN ASSIGN dHovedEan = DEC(Strekkode.Kode) NO-ERROR.
        ELSE dHovedEan = ?.

        /* Setter pristype. */
        ASSIGN iPrisType = IF ArtPris.Tilbud THEN 2 ELSE 1.
                
        IF ArtPris.Pris[iPrisType] > dMaxPris THEN DO:
            CREATE TT_FeilPris.
            ASSIGN TT_FeilPris.ArtikkelNr = ArtBas.ArtikkelNr
                   TT_FeilPris.Beskr      = ArtBas.Beskr
                   TT_FeilPris.Pris   = ArtPris.Pris[iPrisType].
                RELEASE TT_FeilPris.
            NEXT.
        END.
        ELSE IF ArtPris.Pris[iPrisType] <= 0 THEN DO:
            CREATE TT_FeilPris.
            ASSIGN TT_FeilPris.ArtikkelNr = ArtBas.ArtikkelNr
                   TT_FeilPris.Beskr      = ArtBas.Beskr
                   TT_FeilPris.Pris       = ArtPris.Pris[iPrisType].
                RELEASE TT_FeilPris.
            NEXT.
        END.
        ELSE IF ArtPris.Pris[iPrisType] = ? THEN DO:
            CREATE TT_FeilPris.
            ASSIGN TT_FeilPris.ArtikkelNr = ArtBas.ArtikkelNr
                   TT_FeilPris.Beskr      = ArtBas.Beskr
                   TT_FeilPris.Pris       = ArtPris.Pris[iPrisType].
                RELEASE TT_FeilPris.
            NEXT.
        END.
        DO iCount = 1 TO NUM-ENTRIES(TT_ArtTilEksport.Strekkode):
            FIND StrekKode WHERE StrekKode.Kode = ENTRY(iCount,TT_ArtTilEksport.Strekkoder) NO-LOCK NO-ERROR.
            IF NOT AVAIL StrekKode THEN
                NEXT.
            ASSIGN dTest = ABS(DECI(StrekKode.Kode)) NO-ERROR.
            IF dTest = 0 THEN NEXT. /* Blanke strekkoder skal ikke med. */
            /* det har förekommit koder med - på slutet, därför denna test på slutet */
            IF ERROR-STATUS:ERROR OR LENGTH(StrekKode.Kode) > 13 OR (StrekKode.Kode <> STRING(dTest,FILL("9",LENGTH(StrekKode.Kode)))) THEN DO:
                CREATE TT_FeilStrekKode.
                ASSIGN TT_FeilStrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                       TT_FeilStrekKode.Beskr      = ArtBas.Beskr 
                       TT_FeilStrekKode.Kode       = StrekKode.Kode.
                NEXT.
            END.
            ASSIGN dInterLeaf = 0
                   cStrekKode = ENTRY(iCount,TT_ArtTilEksport.Strekkoder).
            IF ArtBas.Vg > 0 AND ArtBas.Vg <= 999 AND ArtBas.LopNr <> ? AND ArtBas.LopNr > 0 AND ArtBas.LopNr <= 9999 THEN DO:
                IF LENGTH(cStrekKode) = 12 THEN /* Interleaf */
                    ASSIGN dInterLeaf = DECI(cStrekKode) NO-ERROR.
                ELSE IF LENGTH(cStrekKode) = 13 THEN DO:
                    IF StrekKode.StrKode > 0 THEN DO:
                        FIND StrKonv WHERE StrKonv.StrKode = StrekKode.StrKode NO-LOCK NO-ERROR.
                        IF AVAIL StrKonv THEN DO:
                            ASSIGN cStrl = IF NUM-ENTRIES(StrKonv.Storl,".") = 2 THEN
                               TRIM(REPLACE(StrKonv.Storl,".","")) ELSE TRIM(StrKonv.Storl) + "0"
                                cStrl = FILL("0",4 - LENGTH(cStrl)) + cStrl
                                cKode = STRING(ArtBas.Vg,"999")     +
                                     STRING(ArtBas.LopNr,"9999") +
                                   "0" +
                                   cStrl NO-ERROR.
                           ASSIGN dInterLeaf = DECI(cKode) NO-ERROR.
                           IF ERROR-STATUS:ERROR THEN
                               ASSIGN dInterLeaf = 0.
                        END.
                    END.
                END.
            END.
            FIND BildeRegister NO-LOCK WHERE
                BildeRegister.BildNr = ArtBas.BildNr NO-ERROR.
            IF ArtBas.LinkVareNr > 0 THEN
                ASSIGN dLinkNr = getLinkNr(ArtBas.LinkVareNr) NO-ERROR.
            ELSE ASSIGN dLinkNr = 0.

            /* Oppretter en TT_Vare pr. butikk i profilen */
            IF TRIM(TT_ArtTilEksport.ButikkLst) <> '' THEN 
            PROFIL:
            DO iLoop = 1 TO NUM-ENTRIES(TT_ArtTilEksport.ButikkLst):
              CREATE TT_Vare.
              ASSIGN 
                   TT_Vare.butnr       = INTEGER(ENTRY(iLoop,TT_ArtTilEksport.ButikkLst))
                   TT_Vare.ean         = DECI(cStrekKode)
                   TT_Vare.hgr         = ArtBas.Vg
                   TT_Vare.bong        = IF ArtBas.Bongtekst <> "" THEN SUBSTRING(ArtBas.Bongtekst,1,20)
                                           ELSE SUBSTRING(ArtBas.Beskr,1,20)
                   TT_Vare.bong        = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(TT_Vare.bong,";"," "),'"',' '),"'"," "),CHR(13),""),CHR(10),""),',','.')
                   TT_Vare.opris       = ArtBas.OPris
                   TT_Vare.link        = dLinkNr
                   TT_Vare.kotype      = (IF ArtBas.IKasse = TRUE
                                            THEN 1
                                            ELSE 9)
/* skall öppnas */ TT_Vare.vekt        = ArtBas.ArtSlag /* 0-Stk, 1-Kg, 2-Hg, 3-m, 4-m2 ,5-l */ 
/* !! */           TT_Vare.utprisn     = ArtPris.Pris[1]
/* ?? */           TT_Vare.bonus       = IF lBonus = TRUE THEN TRUE ELSE ArtBas.Medlemsutbytte /* FALSE ?? */
                   TT_Vare.mva         = IF AVAIL Moms THEN Moms.MomsProc ELSE ?
                   TT_Vare.krabatt     = ArtBas.KundeRabatt
                   TT_Vare.varetekst   = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(CAPS(SUBSTRING(ArtBas.Beskr,1,30)),";"," "),'"',' '),"'"," "),CHR(13),""),CHR(10),""),',','.')
                   TT_Vare.nettokr     = ArtPris.VareKost[1] /* ArtPris.VareKost[iPrisType] */
                   TT_Vare.bestnr      = ? /* Ikke i bruk av SE */
                   TT_Vare.mixnr       = 0
                   TT_Vare.pakkenr     = ArtBas.Pakkenr
                   TT_Vare.bestvare    = ArtBas.Pant
/* tas i bruk */   TT_Vare.utprist     = IF ArtPris.Tilbud THEN ArtPris.Pris[2] ELSE 0
/* tas i bruk */   TT_Vare.tidsstyrtt  = FALSE
/* tas i bruk */   TT_Vare.fradatot    = IF ArtPris.Tilbud THEN ArtPris.TilbudFraDato ELSE ?
/* tas i bruk */   TT_Vare.fratidt     = IF ArtPris.Tilbud THEN STRING(ArtPris.TilbudFraTid,"HH:MM") ELSE ""
/* tas i bruk */   TT_Vare.tildatot    = IF ArtPris.Tilbud THEN ArtPris.TilbudTilDato ELSE ?
/* tas i bruk */   TT_Vare.tiltidt     = IF ArtPris.Tilbud THEN STRING(ArtPris.TilbudTilTid,"HH:MM") ELSE ""
/* tas i bruk */   TT_Vare.utprism     = 0 /* ? */
/* tas i bruk */   TT_Vare.tidsstyrtm  = FALSE
/* tas i bruk */   TT_Vare.fradatom    = ?
/* tas i bruk */   TT_Vare.fratidm     = ""
/* tas i bruk */   TT_Vare.tildatom    = ?
/* tas i bruk */   TT_Vare.tiltidm     = ""
/* tas i bruk */   TT_Vare.utprisa     = 0 /* tidsstyrt Tilbudspris */
/* tas i bruk */   TT_Vare.fra         = ""
/* tas i bruk */   TT_Vare.til         = ""
                   TT_Vare.idkrav      = ArtBas.Alder
                   TT_Vare.lager       = FALSE /* online lageruppdatering InfoPos PRO */
                   TT_Vare.individ     = FALSE /* ArtBas.IndividType > 0 */ /* Individ */
                   TT_Vare.garantikl   = ArtBas.GarantiKl /* ? */
                   TT_Vare.bilde       = IF AVAIL Bilderegister THEN Bilderegister.FilNavn ELSE ""
                   TT_Vare.timestat    = FALSE
                   TT_Vare.nettokrt    = IF ArtPris.Tilbud = TRUE THEN ArtPris.VareKost[2] ELSE 0
                   TT_Vare.Stoppkode      = 0
                   TT_Vare.Storrelsesnr   = StrekKode.StrKode
                   TT_Vare.Storrelsestype = IF Artbas.Storrelse = FALSE OR (StrType.Fordeling <> "" AND NUM-ENTRIES(StrType.Fordeling) = 1) THEN 0 ELSE ArtBas.StrTypeID
/*                    TT_Vare.Lopenr         = ArtBas.LopNr */
                   TT_Vare.Handlingskode  = IF bHoyLavMva THEN (IF ArtBas.HoyLavMva THEN 1 ELSE 0) ELSE ArtBas.BehKode
                   TT_Vare.Modell         = (IF Artbas.ArtikkelNr <= 99999999.0 THEN ArtBas.ArtikkelNr ELSE 0)
                   TT_Vare.Bestnr2        = IF StrekKode.Bestillingsnummer <> "" THEN TRIM(StrekKode.Bestillingsnummer) ELSE TRIM(ArtBas.LevKod)
                   TT_Vare.Bestnr2        = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(TT_Vare.Bestnr2,";"," "),'"',' '),"'"," "),CHR(13),""),CHR(10),""),',','.')
                   TT_Vare.BestNr2        = IF TT_Vare.BestNr2 = ? THEN '' ELSE TT_Vare.BestNr2 
/*                    TT_Vare.BestNr2        = IF LENGTH(TT_Vare.BestNr2) > 12 THEN '' ELSE TT_Vare.BestNr2 */
                   TT_Vare.Vekttilbud     = ArtBas.ArtSlag /* ?? */
                   TT_Vare.Varenr         = IF dInterLeaf = 0 THEN ? ELSE dInterLeaf /* Interleafkode */ 
                   TT_Vare.Fargenr        = IF ArtBas.Farg = ? THEN 0 ELSE ArtBas.Farg
                   TT_Vare.Rabikas        = NOT ArtBas.ManRabIKas
                   TT_Vare.Hovedean       = (IF DECI(cStrekKode) = dHovedEAN THEN ? ELSE dHovedEAN) 
                   TT_Vare.Nonsale        = ArtBas.Non_Sale
                   TT_Vare.Mengde         = ArtBas.Mengde 
                   TT_Vare.Enhetstekst    = ArtBas.JamforEnhet 
                   TT_Vare.Miljo          = FALSE 
                   TT_Vare.Okologisk      = FALSE     
                   TT_Vare.Gjennomfaktureres = ArtBas.Gjennomfaktureres
                   TT_Vare.ArtikkelNr     = ArtBas.ArtikkelNr
                   iAntVarer              = iAntVarer + 1
                   NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                 IF AVAILABLE tt_Vare THEN DELETE tt_vare.
               END.
               ELSE RELEASE TT_Vare.
            END. /* PROFIL */
        END.
    END.
    /* IBM850 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettTTmedALLE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTTmedALLE Procedure 
PROCEDURE SlettTTmedALLE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE TT_ELogg.Verdier = "ALLE":
      DELETE TT_ELogg.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettTT_ELoggVare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggVare Procedure 
PROCEDURE SlettTT_ELoggVare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "ArtPris" AND
              TT_ELogg.EksterntSystem = "POS":
      DELETE TT_Elogg.
  END.
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "Pakkelinje" AND
              TT_ELogg.EksterntSystem = "POS":
      DELETE TT_Elogg.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getLinkNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLinkNr Procedure 
FUNCTION getLinkNr RETURNS DECIMAL
  ( INPUT dLinkNr AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dTest AS DECIMAL NO-UNDO.
  
  DEFINE BUFFER bStrekKode FOR StrekKode.
  
  FIND FIRST bStrekkode WHERE bStrekKode.ArtikkelNr = dLinkNr AND
                              bStrekKode.HovedNr    = TRUE AND 
                              bStrekkode.Kode > '' NO-LOCK NO-ERROR.
  IF NOT AVAIL bStrekKode THEN
      FIND FIRST bStrekkode WHERE bStrekKode.ArtikkelNr = dLinkNr AND
                                  bStrekKode.KodeType = 0 AND 
                                  bStrekkode.Kode > '' NO-LOCK NO-ERROR.
  IF NOT AVAIL bStrekKode THEN
      FIND FIRST bStrekkode WHERE bStrekKode.ArtikkelNr = dLinkNr AND 
        bStrekkode.Kode > '' NO-LOCK NO-ERROR.
        
  IF AVAILABLE bStrekkode THEN 
    ASSIGN dTest = DECIMAL(bStrekkode.Kode) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
    RETURN 0.0.
  ELSE
    RETURN dTest.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentbutikkListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hentbutikkListe Procedure 
FUNCTION hentbutikkListe RETURNS CHARACTER
        (INPUT iProfilNr AS INTEGER ):

        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cResult    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE chkButiker AS CHARACTER NO-UNDO.
    
    ASSIGN 
      chkButiker = TRIM(cLanButiker + 
                   (IF cLanbutiker <> '' THEN ',' ELSE '') +
                   cFtpButiker).
        
    BUTIKKLOOP:
    FOR EACH Butiker NO-LOCK WHERE
      Butiker.ProfilNr = iProfilNr AND
      Butiker.harButikksystem = TRUE AND
      Butiker.ApningsDato <= TODAY AND
      Butiker.NedlagtDato  = ?:
    
      IF chkButiker <> '' AND NOT CAN-DO(chkButiker,STRING(Butiker.butik)) THEN 
        NEXT BUTIKKLOOP.
             
      cResult = cResult + (IF cResult = '' THEN '' ELSE ',') + string(Butiker.Butik).        
    END. /* BUTIKKLOOP */

    RETURN cResult.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

