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
DEFINE INPUT  PARAMETER cFtpButiker     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cExportVareFil  AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cVareFiler      AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cMixFiler       AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntVarer       AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER iAntPakkeLinjer AS INTEGER    NO-UNDO.

DEFINE VARIABLE cTekst              AS CHAR        NO-UNDO.
DEFINE VARIABLE iCount              AS INTEGER NO-UNDO.
DEFINE VARIABLE cEksportKatalog     AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE dMaxPris            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cBonus              AS CHARACTER NO-UNDO. /* Hämtas för tillfället från syspara */
DEFINE VARIABLE lBonus              AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE cFeilFil            AS CHARACTER INIT "FeilVareEksport.txt" NO-UNDO.
DEFINE VARIABLE bHoyLavMva          AS LOG       NO-UNDO.
DEFINE VARIABLE iCl                 AS INT       NO-UNDO.
DEFINE VARIABLE cButikkLst          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iKasseEksportFormat AS INTEGER   NO-UNDO.
DEFINE VARIABLE iKasseTabellformat  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cBestillingsnr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStrekKode          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStrl               AS CHARACTER NO-UNDO.
DEFINE VARIABLE dInterLeaf          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lLagerUtlegg        AS LOG NO-UNDO.
DEFINE VARIABLE cOptProfilbutik     AS CHARACTER   NO-UNDO.
DEFINE BUFFER   bTT_Elogg FOR TT_Elogg.
DEFINE BUFFER   clButiker FOR Butiker.
DEFINE BUFFER   clOptButiker FOR Butiker.

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

DEFINE TEMP-TABLE TT_Vare NO-UNDO LIKE ArtBas
            FIELD Aksjon        AS INTEGER   FORMAT ">9"         
            FIELD ButikkNr      AS INTEGER   FORMAT ">>>>>9"   
            FIELD KoType        AS INTEGER   FORMAT "9"   
            INDEX ButStrkKotype ButikkNr ArtikkelNr KoType DESCENDING
            INDEX ButikkNr ButikkNr ArtikkelNr.

DEFINE TEMP-TABLE TT_TouchaArtBas NO-UNDO
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    INDEX artikkelnr IS PRIMARY UNIQUE artikkelnr.


DEF BUFFER bufTT_Vare FOR TT_Vare.
DEF TEMP-TABLE tmpbufTT_vare LIKE TT_Vare.

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

{syspara.i 2 4 64 cTekst}
ASSIGN 
    lLagerUtlegg = CAN-DO("1,ja,yes,true",TRIM(cTekst)).

/* Setter datoformat for eksport til kassen. */
{syspara.i 1 1 55 iKasseEksportFormat INT}
/* Setter tabellformatet på varefil til kassen. */
{syspara.i 1 1 57 iKasseTabellformat INT}

/* eksport katalog til kasse. */
{syspara.i 1 1 56 cTekst}
IF cTekst <> '' THEN 
  cEksportKatalog = RIGHT-TRIM(cTekst,'\') + '\'.
ASSIGN
  cExportVareFil = cEksportKatalog + cExportVareFil. 

/* Henter sentrallageret. */
{syspara.i 5 1 1 iCL INT}
{syspar2.i 5 1 1 cOptProfilbutik}
cOptProfilbutik = TRIM(cOptProfilbutik).        
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

RUN SkapaVareFil.

FOR EACH TT_Vare:
    iCount = iCount + 1.
END.

IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportVare IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
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
/* TT_TouchaArtBas skall generera ny elogg */
RUN TouchaArtBas.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
    
    DEFINE BUFFER bufButiker   FOR Butiker.
    DEFINE BUFFER plukkButiker FOR Butiker.
    
    DEFINE VARIABLE cString        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVVAreKost     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTilbudFraDato AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTilbudTilDato AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTilbudFraTid  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTilbudTilTid  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cKode          AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dTest          AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE  cNumericFormat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat    AS CHARACTER  NO-UNDO.
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".

    OUTPUT TO VALUE(cExportVareFil + cFilsuffix) APPEND.

    FIND bufButiker NO-LOCK WHERE
         bufButiker.Butik = iButik NO-ERROR.
    IF bufButiker.PlukkButikk > 0 AND CAN-FIND(plukkButiker WHERE plukkButiker.Butik = bufButiker.PlukkButikk)
     THEN FIND plukkButiker NO-LOCK WHERE plukkButiker.Butik = bufButiker.PlukkButikk NO-ERROR.

    TT_LOOP_SLETT:
    FOR EACH TT_Vare WHERE TT_Vare.ButikkNr = iButik AND 
             TT_Vare.KoType = 9:
/* 01 */  cString = "ARTBAS;" 
        /* 02 */             + "3;" 
        /* 03 */             + STRING(TT_Vare.ArtikkelNr) + ";"  
        /* 04 */             + ";"  
        /* 05 */             + ";".
          PUT UNFORMATTED cString SKIP.
          DELETE TT_Vare.
    END. /* TT_LOOP_SLETT */
    
    /* Utlegg av Moms - Gjøres bare hvis det finnes varer å legge ut. */
    IF CAN-FIND(FIRST TT_Vare WHERE TT_Vare.ButikkNr = iButik) THEN 
    MOMS: 
    DO:
      FOR EACH Moms NO-LOCK:
        cString = "MOMS;1" + ";" + 
                  STRING(Moms.MomsKod)     + ";" + 
                  STRING(Moms.MomsProc)    + ";" + 
                  STRING(Moms.Beskrivelse)
                  .
        PUT UNFORMATTED cString SKIP.            
      END.
    END. /* MOMS */
    
    TT_LOOP:
    FOR EACH TT_Vare WHERE TT_Vare.ButikkNr = iButik:
        IF CAN-FIND(FIRST butsortiment WHERE butsortiment.ArtikkelNr = TT_Vare.ArtikkelNr) THEN DO:
            IF NOT CAN-FIND(butsortiment WHERE butsortiment.butik = iButik AND
                                               butsortiment.artikkelnr = TT_Vare.ArtikkelNr) THEN
                NEXT TT_LOOP.
        END.
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = TT_Vare.ArtikkelNr NO-ERROR. 
        IF NOT AVAILABLE ArtBas OR ArtBas.LopNr = ? THEN
          NEXT TT_LOOP.
        
        /* Skaper en temp-table post å jobbe med. */
        CREATE tmpbufTT_Vare.

        /* Klargjør buffer. */
        BUFFER-COPY TT_Vare TO tmpbufTT_vare.
        /* Her preppes buffer med de data som er unike for prisprofilen */
        IF tmpbufTT_Vare.kotype <> 9 THEN
        IKKE_SLETTEPOSTER:
        DO: 
        
        /* Henter ArtPris for profilen. */
        KLARGJOR_PROFIL:
        DO:
            FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = TT_Vare.ArtikkelNr AND
                ArtPris.ProfilNr   = bufButiker.ProfilNr NO-ERROR.
            IF NOT AVAIL artpris AND cOptProfilbutik <> "" THEN DO:
                RELEASE clOptButiker.
                IF CAN-FIND(clOptButiker WHERE clOptButiker.butik = bufButiker.clButikkNr) THEN
                    FIND clOptButiker WHERE clOptButiker.butik = bufButiker.clButikkNr NO-LOCK NO-ERROR.
                IF AVAIL clOptButiker THEN
                    FIND ArtPris NO-LOCK WHERE
                      ArtPris.ArtikkelNr = TT_Vare.ArtikkelNr AND
                      ArtPris.ProfilNr   = clOptButiker.ProfilNr NO-ERROR.
            END.
            IF NOT AVAILABLE ArtPris THEN 
              FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = TT_Vare.ArtikkelNr AND
                ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN 
              FIND FIRST Artpris NO-LOCK WHERE
                ArtPris.ArtikkelNr = TT_Vare.ArtikkelNr NO-ERROR.
                
            /* Finner vi ikke kalkylen, skal ikke artikkelen legges ut. */            
            IF NOT AVAILABLE ArtPris THEN
            DO: 
              IF AVAILABLE tmpbufTT_Vare 
                THEN DELETE tmpbufTT_Vare.
              NEXT TT_LOOP.
            END.
        END. /* KLARGJOR_PROFIL */

        /* Klargjør vektet varekost */
        ASSIGN cVVAreKost = ''.
        /* Legger ut vektet varekost istedenfor kalkulert varekost hvis den er <> 0 for lagerstyrte varer */
        IF TT_Vare.kotype <> 9 AND ArtBas.Lager THEN 
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
                    cVVAreKost = STRING(Lager.VVareKost).
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
                    cVVAreKost = STRING(Lager.VVareKost).
              END.
            END.
        END. /* VEKTET_VAREKOST */
        ELSE /* Ikke lagerstyrte varer. */
          ASSIGN
            cVVAreKost = STRING(ArtPris.VareKost[1]).
        /* Er kalkylen lagt opp med 0 varekost, skal varekost beregnes basert på kost% på varegruppen. */
        IF DEC(cVVAreKost) <= 0 THEN
        DO:
          FIND VarGr NO-LOCK WHERE
            VarGr.Vg = ArtBas.Vg NO-ERROR.
          IF AVAILABLE VarGr AND VarGr.Kost_Proc > 0 THEN 
          DO:
            ASSIGN
              cVVAreKost = STRING((ArtPris.Pris[1] - ArtPris.MvaKr[1]) * (VarGr.Kost_Proc / 100))
              cVVAreKost = IF cVVAreKost = ? THEN '0' ELSE cVVAreKost.
          END.
          ELSE cVVarekost = '0'.
        END. 
        END. /* IKKE_SLETTEPOSTER */
        /* Sjekk av VVarekost. */
        cVVAreKost = IF cVVAreKost = ? THEN '' ELSE cVVAreKost.
        
        /* Utlegg av ArtBas */
        FIND Bilderegister OF ArtBas NO-LOCK NO-ERROR.
/* 01 */  cString = "ARTBAS" + ";" +
        /* 02 */             "1" + ";" +
        /* 03 */             STRING(ArtBas.Artikkelnr) + ";" + 
        /* 04 */             STRING(ArtBas.Vg)         + ";" + 
        /* 05 */             STRING(ArtBas.Lopnr)      + ";" + 
        /* 06 */    REPLACE(REPLACE(ArtBas.Bongtekst,";",""),'"'," ") + ";" + 
        /* 07 */             STRING(ArtBas.Opris,"J/N") + ";" +
        /* 08 */    (IF AVAIL Bilderegister AND Bilderegister.bildnr > 0 THEN "mini" + bilderegister.filnavn ELSE "") + ";" +
        /* 09 */             STRING(Artbas.vekt,"J/N") + ";" +
        /* 10 */             STRING(ArtBas.Non_Sale,"J/N") + ";" +
        /* 11 */             STRING(ArtBas.Negvare,"J/N")  + ";" +
        /* 12 */             STRING(ArtBas.ManRabIKas,"J/N") + ";" +
        /* 13 */             STRING(ArtBas.ArtSlag) + ";" +
        /* 14 */        STRING(ArtBas.Hg) + ";" +
        /* 15 */        STRING(ArtBas.Farg) + ";" +
        /* 16 */        REPLACE(REPLACE(ArtBas.Beskr,";",""),'"'," ") + ";" +
        /* 17 */        STRING(ArtBas.LevNr) + ";" +
        /* 18 */        REPLACE(REPLACE(ArtBas.LevKod,";",""),'"'," ") + ";" +
        /* 19 */        STRING(ArtBas.RabKod) + ";" +
        /* 20 */        REPLACE(REPLACE(ArtBas.LevFargKod,";",""),'"'," ") + ";" +
        /* 21 */        STRING(ArtBas.BildeIKasse,"J/N") + ";" +
        /* 22 */        STRING(ArtBas.Pakke,"J/N") + ";" +
        /* 23 */        STRING(ArtBas.Alder) + ";" +
        /* 24 */        STRING(ArtBas.KundeRabatt,"J/N") + ";" +
        /* 25 */        REPLACE(REPLACE(ArtBas.SalgsEnhet,";",""),'"'," ") + ";" +
        /* 26 */        STRING(ArtBas.LinkVareNr) + ";" +
        /* 27 */        STRING(ArtBas.Mengde) + ";" +
        /* 28 */        STRING(ArtBas.IndividType) + ";" +
        /* 29 */        STRING(ArtBas.Pant,"J/N") + ";" +
        /* 30 */        STRING(ArtBas.GarantiKl) + ";" +
        /* 31 */        STRING(ArtBas.AntIPakn) + ";" +
        /* 32 */        "" + ";" + /* REPLACE(REPLACE(REPLACE(ArtBas.VareFakta,";",""),'"'," "),CHR(13),CHR(1)) + ";" + */
        /* 33 */        STRING(ArtBas.Depositum) + ";" +
        /* 34 */        STRING(ArtBas.HoyLavMva,"J/N") + ";" +
        /* 35 */        STRING(ArtBas.VareType) + ";" +
        /* 36 */        STRING(ArtBas.Telefonkort,"J/N") + ";"  +
        /* 37 */        STRING(ArtBas.MengdeRabatt,"J/N") + ";"  +
        /* 38 */        STRING(ArtBas.Kjokkenskriver) + ";"  +
        /* 39 */        STRING(ArtBas.OnLineLevNr)
          NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT TT_LOOP.
        PUT UNFORMATTED cString SKIP.

        /* Utlegg av ArtPris */
        IF AVAILABLE ArtPris AND ArtPris.TilBud THEN DO:
            IF ArtPris.TilbudFraDato = ? THEN
                ASSIGN cTilbudFraDato = ""
                       iTilbudFraTid  = 0.
            ELSE
                ASSIGN cTilbudFraDato = STRING(YEAR(ArtPris.TilbudFraDato),"9999") +
                                        STRING(MONTH(ArtPris.TilbudFraDato),"99")  +
                                        STRING(DAY(ArtPris.TilbudFraDato),"99")
                       iTilbudFraTid  = ArtPris.TilbudFraTid.
            IF ArtPris.TilbudTilDato = ? THEN
                ASSIGN cTilbudTilDato = ""
                       iTilbudTilTid  = 0.
            ELSE
                ASSIGN cTilbudTilDato = STRING(YEAR(ArtPris.TilbudTilDato),"9999") +
                                        STRING(MONTH(ArtPris.TilbudTilDato),"99")  +
                                        STRING(DAY(ArtPris.TilbudTilDato),"99")
                       iTilbudTilTid  = ArtPris.TilbudTilTid.
        END.
        ELSE
            ASSIGN cTilbudFraDato = ""
                   cTilbudTilDato = ""
                   iTilbudFraTid  = 0
                   iTilbudTilTid  = 0.
        
        IF AVAILABLE ArtPris THEN 
        DO:
            cString = "ARTPRIS;1" + ";" + STRING(ArtPris.Artikkelnr)  + ";" + 
                                        STRING(Artpris.profilnr)      + ";" + 
                                        STRING(Artpris.tilbud,"J/N")  + ";" + 
                                               cTilbudFraDato         + ";" + 
                                        STRING(iTilbudFraTid)         + ";" + 
                                               cTilbudTilDato         + ";" + 
                                        STRING(iTilbudTilTid)         + ";" + 
                                        STRING(Artpris.pris[1])       + ";" + 
                                        STRING(Artpris.pris[2])       + ";" + 
                                        STRING(Artpris.VareKost[1])   + ";" + 
                                        STRING(Artpris.VareKost[2])   + ";" +
                                        cVVAreKost                    + ";" +
                                        STRING(Artpris.MengdeRabAnt)  + ";" +
                                        STRING(Artpris.MengdeRabPris) + ";" +
                                        STRING(Artpris.Mva%[1])       + ";" +
                                        STRING(Artpris.MvaKr[1])      + ";" +
                                        STRING(Artpris.MomsKod[1])    + ";" +
                                        STRING(Artpris.DbKr[1])       + ";" +
                                        STRING(Artpris.Db%[1])        + ";" +
                                        STRING(Artpris.Mva%[2])       + ";" +
                                        STRING(Artpris.MvaKr[2])      + ";" +
                                        STRING(Artpris.MomsKod[2])    + ";" +
                                        STRING(Artpris.DbKr[2])       + ";" +
                                        STRING(Artpris.Db%[2])        
                                        .
            PUT UNFORMATTED cString SKIP.
        END.
        /* Utlegg av strekkoder */
        FOR EACH StrekKode OF ArtBas NO-LOCK:        
            /* Interleavenummer */
            ASSIGN dTest = ABS(DECI(StrekKode.Kode)) NO-ERROR.
            /* det har förekommit koder med - på slutet, därför denna test på slutet */
            IF ERROR-STATUS:ERROR OR LENGTH(StrekKode.Kode) > 13 THEN
                NEXT.
            INTERLEAF:
            DO:
                ASSIGN dInterLeaf = 0
                       cStrekKode = Strekkode.Kode.
                IF ArtBas.Vg > 0 AND ArtBas.Vg <= 999 AND ArtBas.LopNr <> ? AND ArtBas.LopNr > 0 AND ArtBas.LopNr <= 9999 THEN 
                DO:
                    IF LENGTH(cStrekKode) = 12 THEN /* Interleaf */
                        ASSIGN dInterLeaf = DECI(cStrekKode) NO-ERROR.
                    ELSE IF LENGTH(cStrekKode) = 13 THEN DO:
                        IF StrekKode.StrKode > 0 THEN DO:
                            FIND StrKonv WHERE StrKonv.StrKode = StrekKode.StrKode NO-LOCK NO-ERROR.
                            IF AVAIL StrKonv THEN DO:
                                ASSIGN cStrl = IF NUM-ENTRIES(StrKonv.Storl,".") = 2 THEN
                                   TRIM(REPLACE(StrKonv.Storl,".","")) ELSE
                                   IF LENGTH(storl) < 4 THEN TRIM(StrKonv.Storl) + "0" ELSE ""
                                    cStrl = FILL("0",4 - LENGTH(cStrl)) + cStrl
                                    cKode = STRING(ArtBas.Vg,"999")     +
                                         STRING(ArtBas.LopNr,"9999") +
                                       "0" +
                                       cStrl NO-ERROR.
                               IF LENGTH(cKode) > 12
                                   THEN cKode = "".
                               ASSIGN dInterLeaf = DECI(cKode) NO-ERROR.
                               IF ERROR-STATUS:ERROR THEN
                                   ASSIGN dInterLeaf = 0.
                            END.
                        END.
                    END.
                END.
                cBestillingsnr  = (IF dInterLeaf > 0 THEN STRING(dInterLeaf,"999999999999") ELSE STRING(StrekKode.Bestillingsnummer)).                        
            END. /* INTERLEAF */
            
            FIND strkonv OF strekkode NO-LOCK NO-ERROR.
            cString = "STREKKODE;1" + ";" + STRING(StrekKode.Artikkelnr) + ";" + 
                                          cBestillingsnr                 + ";" + 
                                          STRING(StrekKode.IKasse,"J/N") + ";" + 
                                          StrekKode.Kode                 + ";" + 
                                          STRING(StrekKode.KodeType)     + ";" + 
                                          STRING(StrekKode.StrKode)      + ";" + 
                                          (IF AVAIL strkonv THEN TRIM(strkonv.storl) ELSE "") + ";" +
                                          STRING(StrekKode.VareId)       + ";" +
                                          Strekkode.ERPNr.
            PUT UNFORMATTED cString SKIP.
        END.
        
        /* Utlegg av lager */
        IF lLagerUtlegg THEN 
        LAGER: 
        DO:
          FIND Lager NO-LOCK WHERE
            Lager.ArtikkelNr = TT_Vare.ArtikkelNr AND
            Lager.Butik      = iButik NO-ERROR.
          IF AVAILABLE Lager THEN 
          DO:
            cString = "LAGER;1" + ";" + STRING(Lager.Artikkelnr)  + ";" + 
                                    STRING(Lager.Butik)      + ";" + 
                                    (IF Lager.Lagant >= 0 THEN STRING(Lager.LagAnt) ELSE '0') + ";" +
                                    (IF (Lager.VVareKost <> ? AND Lager.VVareKost > 0) THEN STRING(Lager.VVareKost) ELSE '0')         
                                    .
            PUT UNFORMATTED cString SKIP.            
          END.  
        END.
        
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
DEFINE VARIABLE lSlette    AS LOGICAL     NO-UNDO.

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

    /* TT_TouchaArtBas innehåller poster som felaktigt tas bort när man tagit bort 1 streckkod */
    /* vi tar bort dessa elogg och touchar artbas för att nytt utlägg skall ske */
    lSlette = TRUE.
    IF NOT CAN-FIND(TT_TouchaArtBas WHERE TT_TouchaArtBas.artikkelnr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1)))) THEN DO:
        FIND artbas WHERE artbas.artikkelnr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))) NO-ERROR.
        IF AVAIL artbas AND artbas.iKasse = TRUE THEN DO:
            IF CAN-FIND(FIRST strekkode OF artbas WHERE strekkode.ikasse = TRUE) THEN DO:
                lSlette = FALSE.
                CREATE TT_TouchaArtBas.
                ASSIGN TT_TouchaArtBas.Artikkelnr = artbas.artikkelnr NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE TT_TouchaArtBas.
            END.
        END.
        IF lSlette = TRUE THEN DO:
            FIND TT_ArtTilEksport WHERE TT_ArtTilEksport.ArtikkelNr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))) NO-ERROR.
            IF NOT AVAIL TT_ArtTilEksport THEN DO:
                CREATE TT_ArtTilEksport.
                ASSIGN TT_ArtTilEksport.ArtikkelNr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1)))
                       TT_ArtTilEksport.Slette     = lSlette.
            END.
        END.
    END.
    DELETE TT_ELogg.
END.

/* GJør om ARTBAS til ARTPRIS */
TT_ELOGG_ARTBAS:
FOR EACH TT_ELogg WHERE 
            TT_ELogg.TabellNavn     = "ArtBas" AND
            TT_ELogg.EksterntSystem = "POS"    BY TT_ELogg.Verdier:
    IF TT_ELogg.EndringsType <> 5 THEN /* sätts vid redelete. om artikeln finns så kommer den annars inte att sända slette till kassan */
        FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
    IF AVAILABLE ArtBas AND Artbas.iKasse = TRUE THEN 
    DO:
      FOR EACH ArtPris OF ArtBas NO-LOCK:
        FIND FIRST bTT_ELogg EXCLUSIVE-LOCK WHERE 
           bTT_ELogg.TabellNavn     = "ArtPris" AND
           bTT_ELogg.EksterntSystem = "POS"    AND
           bTT_ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr) NO-ERROR NO-WAIT.
    
        IF LOCKED bTT_ELogg THEN 
          NEXT.
        
        IF NOT AVAIL bTT_Elogg THEN 
        DO:
          CREATE bTT_Elogg.
          ASSIGN bTT_ELogg.TabellNavn     = "ArtPris"
                 bTT_ELogg.EksterntSystem = "POS"   
                 bTT_ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr) 
                 bTT_ELogg.EndringsType   = 1
                 bTT_ELogg.Behandlet      = FALSE           
                 NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
          DO:
            DELETE bTT_ELogg.
            NEXT.
          END.
        END. /* Not avail */
        ELSE bTT_ELogg.EndringsType   = 1.
      END. /* ARTPRIS */
    END. /* Available ArtBas */
    ELSE DO:
/*         tabortpost. */
        FIND TT_ArtTilEksport WHERE TT_ArtTilEksport.ArtikkelNr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1))) NO-ERROR.
        IF NOT AVAIL TT_ArtTilEksport THEN DO:
            CREATE TT_ArtTilEksport.
            ASSIGN TT_ArtTilEksport.ArtikkelNr = DECI(ENTRY(1,TT_ELogg.Verdier,CHR(1)))
                   TT_ArtTilEksport.Slette     = TRUE.
        END.
    END.
END. /* TT_ELOGG_ARTBAS */

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
    /* Vis kall väl ändå inte skapa elogg för butiker som inte finns!? */
    IF cButikkLst = "" THEN
        NEXT.
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
            FIND TT_ArtTilEksport WHERE TT_ArtTilEksport.ArtikkelNr = ArtBas.ArtikkelNr AND
                                        TT_ArtTilEksport.ProfilNr   = ArtPris.ProfilNr AND 
                                        TT_ArtTilEksport.Slette     = (NOT ArtBas.iKasse) NO-ERROR.
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
    DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.

    dDatoTid =  dec(STRING(YEAR(TODAY),"9999") +
                    string(MONTH(TODAY),"99") + 
                    string(DAY(TODAY),"99") +
                    string(TIME)).


    IF CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn     = "RedeleteArtBas" AND
                            ELogg.EksterntSystem = "POS") THEN     
    DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "RedeleteArtBas" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            DO TRANSACTION:
                CREATE TT_Elogg.
                BUFFER-COPY Elogg TO TT_Elogg NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE TT_Elogg.
                ELSE DO:
                    ASSIGN TT_Elogg.TabellNavn = "ArtBas"
                           TT_Elogg.Endringstype = 5. /* för att tala om att det är en redelete i Fixart... */
                    FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
                    IF AVAILABLE bElogg THEN DELETE bELogg.
                    IF CAN-FIND(artbas WHERE artbas.artikkelnr = DECI(TT_Elogg.verdier)) AND NOT 
                       CAN-FIND(bELogg WHERE bELogg.TabellNavn = "ArtBas"  AND
                                             bELogg.EksterntSystem = "POS" AND
                                             bELogg.Verdier         = TT_Elogg.Verdier) THEN DO:
                        CREATE bELogg.
                        ASSIGN bELogg.TabellNavn     = "ArtBas"
                               bELogg.EksterntSystem = "POS"   
                               bELogg.Verdier        = STRING(TT_Elogg.Verdier)
                               bELogg.EndringsType = 1
                               bELogg.Behandlet    = FALSE NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN
                            DELETE bELogg.
                    END.
                END.
            END. /* 160112 */
        END.
    END. /* TRANSACTION */
    ELSE IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "ArtPris" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "ALLE") THEN     
    DO TRANSACTION:
        FIND ELogg WHERE ELogg.TabellNavn     = "ArtPris" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "ALLE" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END. /* TRANSACTION */
    ELSE IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "ArtPris" AND
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
            IF AVAIL bElogg THEN
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
            ASSIGN lDec = DECI(ENTRY(1,ELogg.Verdier,CHR(1))) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
                RUN bibl_loggDbFri.p ('eksportPRSArtBas_Error', 'eksportPRSArtBas.p - Kopier Elogg: Feil i verdifelt.' 
                               + ' Tabell: ' + ELogg.TabellNavn 
                               + ' System: ' + ELogg.EksterntSystem 
                               + ' Verdi: '  + ELogg.Verdier
                               + ' EDato: '  + STRING(ELogg.EDato)
                               + ' ETid: '   + STRING(ELogg.ETid,"HH:MM:SS")
                               ).         
                NEXT.
            END.
             
            /* Artikkel 0 skal ikke ut */
            IF DECI(ENTRY(1,ELogg.Verdier,CHR(1))) = 0 THEN 
            DO:
              FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
              IF AVAILABLE bElogg THEN DELETE bELogg.
              NEXT.
            END.
            FIND ArtBas NO-LOCK WHERE
              ArtBas.ArtikkelNr = DECI(ENTRY(1,ELogg.Verdier,CHR(1))) NO-ERROR.
            /* PakkeArtikkel skal ikke ut */
            IF AVAILABLE ArtBas AND ArtBas.Pakke = TRUE THEN 
            DO:
              FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
              IF AVAILABLE bElogg THEN DELETE bELogg.
              NEXT.
            END.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg AND NOT LOCKED bELogg THEN
              DO:
                BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
                DELETE bELogg.
              END.  
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END. /* TRANSACTION*/
        
        /* Ny artikkel som skal sendes til alle profiler. */
        NYARTIKKEL:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "ArtBas" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK TRANSACTION:
            IF dDatoTid - ELogg.Opprettet < 60 THEN
                NEXT.
            /* 160112 */
            IF Elogg.Endringstype = 3 THEN DO:
                CREATE TT_Elogg.
                BUFFER-COPY Elogg TO TT_Elogg NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE TT_Elogg.
                ELSE DO:
                    FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
                    IF AVAILABLE bElogg THEN DELETE bELogg.
                END.
                NEXT NYARTIKKEL.
            END. /* 160112 */
            /* Artikkel 0 skal ikke ut */
            IF DECI(ENTRY(1,ELogg.Verdier,CHR(1))) = 0 THEN 
            DO:
              FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
              IF AVAILABLE bElogg THEN DELETE bELogg.
              NEXT.
            END.
            FIND ArtBas NO-LOCK WHERE
              ArtBas.ArtikkelNr = DECI(ENTRY(1,ELogg.Verdier,CHR(1))) NO-ERROR.
            /* PakkeArtikkel skal ikke ut och likaså om artikel inte finns */
/*             IF AVAILABLE ArtBas AND ArtBas.Pakke = TRUE THEN */
            IF NOT AVAILABLE ArtBas OR ArtBas.Pakke = TRUE THEN 
            DO:
              FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
              IF AVAILABLE bElogg THEN DELETE bELogg.
              NEXT.
            END.
            /* Det skal være lagt på strekkode før artikkelen kan sendes */
            IF NOT CAN-FIND(FIRST Strekkode WHERE
                            Strekkode.ArtikkelNr = DECI(ENTRY(1,ELogg.Verdier,CHR(1)))) THEN DO:
                IF ELogg.EDato < TODAY THEN DO:
                    FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
                    IF AVAILABLE bElogg THEN DELETE bELogg.
                END.
                NEXT NYARTIKKEL.
            END.
            ELSE DO:            
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
                IF AVAIL bElogg AND NOT LOCKED bELogg THEN
                    DELETE bELogg.
                IF AVAILABLE TT_Elogg THEN
                    RELEASE TT_ELogg.
            END.
        END. /* TRANSACTION */
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
      ArtPris.ProfilNr = clButiker.ProfilNr TRANSACTION:
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
    END. /* TRANSACTION */
  END.
END. /* FTPBUTIKKER */

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
    DEFINE VARIABLE iLoop        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iPrisType    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cExpButiker AS CHARACTER   NO-UNDO.
    

    FOR EACH TT_ArtTilEksport WHERE Slette = TRUE:
        /* Oppretter en TT_Vare pr. butikk i profilen */
        ASSIGN cExpButiker = TRIM(cFtpButiker)
              cExpButiker = cExpButiker + (IF cExpButiker <> "" THEN "," ELSE "") + cFtpButiker.
        IF TRIM(cExpButiker) <> '' THEN 
        PROFIL:
        DO iLoop = 1 TO NUM-ENTRIES(cExpButiker):
            IF NOT CAN-FIND(FIRST TT_Vare WHERE 
                            TT_Vare.ButikkNr   = INTEGER(ENTRY(iLoop,cExpButiker)) AND 
                            TT_Vare.ArtikkelNr = TT_ArtTilEksport.ArtikkelNr) THEN 
            DO:
              CREATE TT_Vare.
              ASSIGN TT_Vare.ButikkNr   = INTEGER(ENTRY(iLoop,cExpButiker))
                     TT_Vare.ArtikkelNr = TT_ArtTilEksport.ArtikkelNr
                     TT_Vare.kotype     = 9
                     iAntVarer          = iAntVarer + 1 NO-ERROR.
              IF ERROR-STATUS:ERROR THEN 
                DO:
                  IF AVAILABLE tt_Vare THEN DELETE tt_Vare.
                END.
            END.
        END. /* PROFIL */
        DELETE TT_ArtTilEksport.
    END.
    
    FOR EACH TT_ArtTilEksport: /* vi har bara ändringsposter kvar */
        FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_ArtTilEksport.ArtikkelNr NO-LOCK NO-ERROR.
        IF AVAIL ArtBas THEN 
        FUNNET_ARTBAS:
        DO:
            FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE  
                ArtPris.ProfilNr = TT_ArtTilEksport.ProfilNr NO-ERROR.
            IF NOT AVAIL ArtPris THEN
              FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
            IF NOT AVAIL ArtPris THEN
                NEXT.                  
            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            FIND StrType OF ArtBas NO-LOCK NO-ERROR.
            FIND Moms OF VarGr NO-LOCK NO-ERROR.
            /* Setter pristype. */
            ASSIGN iPrisType = IF ArtPris.Tilbud THEN 2 ELSE 1.
                    
            IF ArtPris.Pris[iPrisType] > dMaxPris THEN DO:
                CREATE TT_FeilPris.
                ASSIGN TT_FeilPris.ArtikkelNr = ArtBas.ArtikkelNr
                       TT_FeilPris.Beskr      = ArtBas.Beskr
                       TT_FeilPris.Pris       = ArtPris.Pris[iPrisType].
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
        END. /* FUNNET_ARTBAS */
        
        /* Oppretter en TT_Vare pr. butikk i profilen */
        IF TRIM(TT_ArtTilEksport.ButikkLst) <> '' AND AVAILABLE ArtBas THEN 
        PROFIL:
        DO iLoop = 1 TO NUM-ENTRIES(TT_ArtTilEksport.ButikkLst):
          IF NOT CAN-FIND(FIRST TT_Vare WHERE 
                          TT_Vare.ButikkNr   = INTEGER(ENTRY(iLoop,TT_ArtTilEksport.ButikkLst)) AND 
                          TT_Vare.ArtikkelNr = TT_ArtTilEksport.ArtikkelNr) THEN 
          DO:
              CREATE TT_Vare.
              BUFFER-COPY ArtBas TO TT_Vare.
              
              ASSIGN 
                   TT_Vare.ButikkNr   = INTEGER(ENTRY(iLoop,TT_ArtTilEksport.ButikkLst))
                   TT_Vare.ArtikkelNr = TT_ArtTilEksport.ArtikkelNr
                   iAntVarer          = iAntVarer + 1
                   NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                 IF AVAILABLE tt_Vare THEN DELETE tt_vare.
               END.
               ELSE RELEASE TT_Vare.
           END.
        END. /* PROFIL */
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

&IF DEFINED(EXCLUDE-TouchaArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TouchaArtBas Procedure 
PROCEDURE TouchaArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_TouchaArtBas:
       FIND ELogg WHERE 
            ELogg.TabellNavn     = "ArtBas" AND
            ELogg.EksterntSystem = "POS"    AND
            ELogg.Verdier        = STRING(TT_TouchaArtBas.artikkelnr) NO-ERROR NO-WAIT.
       IF LOCKED ELogg THEN NEXT.
       IF NOT AVAIL Elogg THEN DO:
           CREATE Elogg.
           ASSIGN ELogg.TabellNavn     = "ArtBas"
                  ELogg.EksterntSystem = "POS"   
                  ELogg.Verdier        = STRING(TT_TouchaArtBas.artikkelnr) NO-ERROR.
           END.
           IF ERROR-STATUS:ERROR THEN
               DELETE ELogg.
       ASSIGN ELogg.EndringsType = 1
              ELogg.Behandlet    = FALSE NO-ERROR.
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
      chkButiker = cFtpButiker.
        
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

