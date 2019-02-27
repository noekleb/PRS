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
DEFINE INPUT  PARAMETER cLanButiker AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cFtpButiker AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cPRSFtpButiker AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cKundeFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntKunder  AS INTEGER    NO-UNDO.

DEFINE VARIABLE  iCount         AS INTEGER                                NO-UNDO.
DEFINE VARIABLE cExportFil      AS CHARACTER INIT "kunde."                NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.
DEFINE VARIABLE bEkspPrButikk   AS LOG                                    NO-UNDO. 
DEFINE VARIABLE iKasseEksportFormat AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE TT_Kunde
/* 1  */ FIELD butnr    AS INTEGER FORMAT ">>9"                   /* I (3)           Butikknr                                    */ 
/* 2  */ FIELD kundenr  AS INTEGER FORMAT ">>>>>9"                /* I (6)           Kundenr                                     */ 
/* 3  */ FIELD slagnr   AS INTEGER FORMAT ">>>9"                  /* I (4)           S-lagsnr                                    */
/* 4  */ FIELD mednr    AS INTEGER FORMAT ">>>>>9"                /* I (6)           Medlemsmr                                   */ 
/* 5  */ FIELD kundegr  AS INTEGER FORMAT "9"                     /* I (1)           Kundegr                                     */ 
/* 6  */ FIELD navn     AS CHARACTER FORMAT "x(50)"               /* C (50)          Kundenavn                                   */ 
/* 7  */ FIELD sperret  AS LOGICAL                                /* L (yes/no)      Flagg for om en kunde er sperret for salg   */ 
/* 8  */ FIELD limit    AS DECIMAL DECIMALS 2 FORMAT ">>>>>9.99"  /* De (6,2)        Kredittgrense                               */ 
/* 9  */ FIELD saldo    AS DECIMAL DECIMALS 2 FORMAT ">>>>>>9.99" /* De (7,2)        Kundens saldo pr siste EOD med endring      */ 
/* 10 */ FIELD aksjon   AS INTE FORMAT "9"                        /* I (1)           Posttype (1=ny/endring, 2=sletting) */
/* 11 */ FIELD adresse  AS CHARACTER FORMAT "x(30)"               /* C (30)          Adresse                                     */ 
/* 12 */ FIELD postnr   AS INTEGER FORMAT ">>>>9"                 /* I (5)           Postnr                                      */ 
/* 13 */ FIELD poststed AS CHARACTER FORMAT "x(30)"               /* C (30)          Poststed                                    */ 
/* 14 */ FIELD dato     AS DATE                                   /* Da                  Dato for siste endring på saldo         */ 
/* 15 */ FIELD kreditt  AS LOGICAL                                /* yes = kreditkunde no = kontantkunde                         */
/* 16 */ FIELD avgiftdato AS DATE FORMAT "99/99/99"               /* Dato da kunden sist betalte medlemsavgift.                  */
/* 17 */ FIELD tvangrefnr AS LOG FORMAT "yes/no"                  /* Er det tvang på regsitrering av ref. på kreditkunder.       */
         .

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
         WIDTH              = 60.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* eksport katalog til kasse. */
{syspara.i 1 1 56 cTekst}
IF cTekst <> '' THEN 
  cEksportKatalog = RIGHT-TRIM(cTekst,'\') + '\'.
ASSIGN
  cExportFil = cEksportKatalog + cExportFil
  . 

/* Setter datoformat for eksport til kassen. */
{syspara.i 1 1 55 iKasseEksportFormat INT}

/* eksport av kunder pr. butikk (Kunde.Butik). */
{syspara.i 14 2 6 cTekst}
IF CAN-DO('1,J,Ja,True,Yes',cTekst) 
  THEN bEkspPrButikk = TRUE.
  ELSE bEkspPrButikk = FALSE.

/* Vi exporterar direkt från ELogg till fil */
/* Ingen extra behandling behöver göras */
RUN KopierElogg.
/* Kanske vi skall hämta iformation om kassor och filer först. */
RUN FixKundeEndringer.
/* Här skall vi loopa runt alla kassor mm */
IF cLanButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
      RUN ExportKunde IN THIS-PROCEDURE (INT(ENTRY(iCount,cLanButiker)),"txt"). /* parameter = den loopade butiken */
  END.
END.
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportKunde IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggKunde. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportKunde) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportKunde Procedure 
PROCEDURE ExportKunde PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  TN   20/08-02 Kun kunder med betalingstype 2 legges ut. Andre kunder sendes
                det sletteposter (action 2) på.
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE         iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE         iNumeric   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE         iPostNr    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  cNumericFormat    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  deSaldo           LIKE KundeSaldo.Saldo NO-UNDO.
    DEFINE VARIABLE  dtDatoSiste       AS DATE       NO-UNDO.
    DEFINE VARIABLE  iSlagsNr          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  iMedlemsNr        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  dDeciTst          AS DECIMAL    NO-UNDO.

    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "American".
    IF iKasseEksportFormat = 1 
      THEN SESSION:DATE-FORMAT    = "ymd".
    ELSE SESSION:DATE-FORMAT    = "dmy".

    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND CONVERT TARGET "IBM850".
    CREATE TT_Kunde.
    ASSIGN TT_Kunde.butnr = iButik
           iAntKunder     = 0. /* tyvärr, vi loopar runt butiker */

    /* Legger ut sletteposter for kunde.          */
    /* Dette lar vi gå til alle butikker uansett. */
    FOR EACH TT_Elogg WHERE
             TT_ELogg.TabellNavn     = "Kunde" AND
             TT_ELogg.EksterntSystem = "POS"   AND
             TT_ELogg.EndringsType = 3:

           ASSIGN TT_Kunde.kundenr = INT(ENTRY(2,TT_ELogg.Verdier,";")) /* kortnumret */
                  TT_Kunde.aksjon = 2
                  TT_Kunde.dato   = TODAY
                  iAntKunder      = iAntKunder + 1.
           EXPORT TT_Kunde.
    END.
    
/*     /* Legger ut sletteposter for kundekort */                                           */
/*     FOR EACH TT_Elogg WHERE                                                              */
/*              TT_ELogg.TabellNavn     = "KundeKort" AND                                   */
/*              TT_ELogg.EksterntSystem = "POS"   AND                                       */
/*              TT_ELogg.EndringsType = 3:                                                  */
/*                                                                                          */
/*            ASSIGN TT_Kunde.kundenr = INT(ENTRY(2,TT_ELogg.Verdier,";")) /* kortnumret */ */
/*                   TT_Kunde.aksjon = 2                                                    */
/*                   TT_Kunde.dato   = TODAY                                                */
/*                   iAntKunder      = iAntKunder + 1.                                      */
/*            EXPORT TT_Kunde.                                                              */
/*     END.                                                                                 */

    /* Legger ut nye og endringer */
    FOR EACH TT_Elogg WHERE
             TT_Elogg.TabellNavn     = "Kunde" AND
             TT_ELogg.EksterntSystem = "POS"   AND
             TT_ELogg.EndringsType = 1:

        FIND Kunde WHERE Kunde.KundeNr = INT(ENTRY(1,TT_ELogg.Verdier,";")) NO-LOCK NO-ERROR.
        IF NOT AVAIL Kunde OR LENGTH(ENTRY(2,TT_ELogg.Verdier,";")) > 6 THEN 
            NEXT.
        /* Er parameter satt, skal butikken bare ha de kunder som tilhører den. */
        IF bEkspPrButikk AND Kunde.Butik <> iButik THEN 
           NEXT.
           
        ASSIGN deSaldo     = Kunde.KundeSaldo
               dtDatoSiste = Kunde.SisteKjop.
/*         FOR EACH KundeSaldo WHERE KundeSaldo.KundeNr = Kunde.KundeNr NO-LOCK:                                       */
/*             ASSIGN deSaldo     = deSaldo + KundeSaldo.Saldo                                                         */
/*                    dtDatoSiste = IF dtDatoSiste = ? OR KundeSaldo.DatoSiste > dtDatoSiste THEN KundeSaldo.DatoSiste */
/*                                         ELSE dtDatoSiste.                                                           */
/*         END.                                                                                                        */
        FIND KundeKort OF Kunde WHERE KundeKort.KortNr = ENTRY(2,TT_ELogg.Verdier,";") NO-LOCK NO-ERROR.
        IF NOT AVAIL KundeKort THEN
            NEXT.
        ASSIGN iNumeric = INT(KundeKort.KortNr) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT.
        FIND Post OF Kunde NO-LOCK NO-ERROR.
        IF AVAIL Post AND LENGTH(Post.PostNr) < 6 THEN DO:
            ASSIGN iPostNr = INT(Post.PostNr) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN iPostnr = 0.
        END.
        ELSE 
            ASSIGN iPostnr = 0.

        /* Standard metode for utlegg av medlemsnummer */
        ASSIGN iSlagsNr   = 0
               iMedlemsNr = 0.

        /* Direkte koblet medlemskort */
        IF KundeKort.InterntKKortId > 0 THEN
        KOBLET_MEDLEMSKORT:
        DO:
            FIND FIRST MedlemsKort NO-LOCK WHERE
                MedlemsKort.InterntKKortId = KundeKort.InterntKKortId NO-ERROR.
            IF AVAILABLE MedlemsKort THEN
            DO:
                ASSIGN dDeciTst = DECI(MedlemsKort.KortNr) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR THEN 
                DO:
                    IF LENGTH(MedlemsKort.KortNr) > 6 THEN
                        ASSIGN iSlagsNr   = INT(SUBSTR(MedlemsKort.KortNr,1,LENGTH(MedlemsKort.KortNr) - 6))
                               iMedlemsNr = INT(SUBSTR(MedlemsKort.KortNr,LENGTH(MedlemsKort.KortNr) - 5)).
                    ELSE
                        ASSIGN iSlagsNr   = 0
                               iMedlemsNr = INT(MedlemsKort.KortNr).

                END.
            END.
        END. /* KOBLET_MEDLEMSKORT */
        ELSE /* Håndtering av gamle kundekort */
        MEDLEMSKORT:
        FOR EACH Medlem NO-LOCK WHERE Medlem.KundeNr = Kunde.KundeNr AND
                           CAN-FIND(FIRST MedlemsKort OF Medlem WHERE 
                                    MedlemsKort.InterntKKortId = 0 AND
                                    LENGTH(MedlemsKort.KortNr) > 6 AND
                                    LENGTH(MedlemsKort.KortNr) < 11 AND
                                    MedlemsKort.Sperret = FALSE)
                           BY Medlem.HovedMedlemFlagg DESCENDING.
            FIND FIRST MedlemsKort OF Medlem WHERE 
                MedlemsKort.InterntKKortId = 0 AND
                LENGTH(MedlemsKort.KortNr) > 6 AND
                LENGTH(MedlemsKort.KortNr) < 11 AND
                MedlemsKort.Sperret = FALSE.
            ASSIGN dDeciTst = DECI(MedlemsKort.KortNr) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN DO:
                IF LENGTH(MedlemsKort.KortNr) > 6 THEN
                    ASSIGN iSlagsNr   = INT(SUBSTR(MedlemsKort.KortNr,1,LENGTH(MedlemsKort.KortNr) - 6))
                           iMedlemsNr = INT(SUBSTR(MedlemsKort.KortNr,LENGTH(MedlemsKort.KortNr) - 5)).
                ELSE
                    ASSIGN iSlagsNr   = 0
                           iMedlemsNr = INT(MedlemsKort.KortNr).

                LEAVE MEDLEMSKORT.
            END.
        END.
        
        ASSIGN TT_Kunde.kundenr  = INT(KundeKort.KortNr)
               TT_Kunde.slagnr   = iSlagsNr
               TT_Kunde.mednr    = iMedlemsNr
               TT_Kunde.kundegr  = (IF Kunde.GruppeId <= 9
                                      THEN Kunde.GruppeId
                                      ELSE 1)
               TT_Kunde.navn     = KundeKort.Innehaver /* Kunde.Navn */
               TT_Kunde.sperret  = IF (Kunde.Aktiv = FALSE OR 
                                       Kunde.KreditSperret OR 
                                       KundeKort.Sperret) 
                                        THEN TRUE 
                                        ELSE FALSE
               TT_Kunde.limit    = (IF Kunde.MaksKredit > 999999
                                      THEN 999999
                                      ELSE Kunde.MaksKredit)
               TT_Kunde.saldo    = (IF deSaldo > 999999
                                      THEN 999999
                                      ELSE deSaldo)
/* ändras */     TT_Kunde.aksjon   = 1 /* (IF Kunde.BetType = 2
/* + TRIG */                         THEN 1
                                     ELSE 2) */
               TT_Kunde.adresse  = Kunde.Adresse1
               TT_Kunde.postnr   = iPostNr
               TT_Kunde.poststed = IF AVAIL Post THEN Post.Beskrivelse ELSE ""
               TT_Kunde.dato     = IF dtDatoSiste = ? THEN TODAY ELSE dtDatoSiste
/* nytt */         TT_Kunde.Kreditt = Kunde.BetType = 2
               iAntKunder      = iAntKunder + 1.
        EXPORT TT_Kunde.
    END.

    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cKundeFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cKundeFiler = cKundeFiler + (IF cKundeFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.

    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixKundeEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixKundeEndringer Procedure 
PROCEDURE FixKundeEndringer PRIVATE :
/* /*------------------------------------------------------------------------------   */
/*   Purpose:                                                                         */
/*   Parameters:  <none>                                                              */
/*   Notes:                                                                           */
/* ------------------------------------------------------------------------------*/   */
     DEFINE VARIABLE iNr AS INTEGER    NO-UNDO.
     DEFINE BUFFER bTT_Elogg FOR TT_Elogg.

     /* Tar hånd om sletteposter på kunde. Kunde har bare ett entry. */
     FOR EACH TT_Elogg WHERE TT_ELogg.TabellNavn     = "Kunde" AND
                             TT_ELogg.EksterntSystem = "POS"   AND
                             TT_ELogg.EndringsType = 3         AND
                             NUM-ENTRIES(TT_ELogg.Verdier,";") = 1:  
     
         FOR EACH KundeKort OF Kunde NO-LOCK WHERE LENGTH(KundeKort.Kortnr) < 7:
             ASSIGN iNr = INT(KundeKort.KortNr) NO-ERROR.
             IF ERROR-STATUS:ERROR THEN
                 NEXT.
             FIND bTT_ELogg WHERE bTT_ELogg.TabellNavn = "Kunde" AND
                  bTT_ELogg.EksterntSystem = "POS"               AND
                  bTT_ELogg.Verdier = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.Kortnr) NO-ERROR.
             IF AVAIL bTT_ELogg THEN
                 ASSIGN bTT_ELogg.EndringsType = 3.
             ELSE DO:
                 CREATE bTT_ELogg.
                 ASSIGN bTT_ELogg.TabellNavn     = "Kunde"
                        bTT_ELogg.EksterntSystem = "POS"  
                        bTT_ELogg.EndringsType   = 3
                        bTT_ELogg.Verdier        = STRING(Kunde.KundeNr) + ";" + STRING(KundeKort.Kortnr).
                 RELEASE bTT_ELogg.
             END.
         END.
         DELETE TT_Elogg.
     END.

     /* Hvis ALLE kunder skal legges ut. */
     IF CAN-FIND(TT_ELogg WHERE TT_ELogg.TabellNavn     = "Kunde" AND
                                TT_ELogg.EksterntSystem = "POS"   AND
                                TT_ELogg.EndringsType = 1         AND
                                TT_ELogg.Verdier = "ALLE") THEN DO:
         /* Tar bort de andre loggpostene da disse ikke behøves. */
         FOR EACH bTT_ELogg WHERE bTT_ELogg.TabellNavn     = "Kunde" AND
                                  bTT_ELogg.EksterntSystem = "POS"   AND
                                  bTT_ELogg.EndringsType = 1:
             DELETE bTT_ELogg.
         END.
         FOR EACH Kunde /* WHERE Kunde.BetType = 2 */ NO-LOCK:
             FOR EACH KundeKort OF Kunde NO-LOCK WHERE LENGTH(KundeKort.Kortnr) < 7:
                 ASSIGN iNr = INT(KundeKort.KortNr) NO-ERROR.
                 IF ERROR-STATUS:ERROR THEN
                     NEXT.
                 IF CAN-FIND(bTT_ELogg WHERE bTT_ELogg.TabellNavn     = "Kunde" AND
                       bTT_ELogg.EksterntSystem = "POS"  AND
                       bTT_ELogg.Verdier = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.Kortnr)) THEN
                     NEXT.
                 CREATE bTT_ELogg.
                 ASSIGN bTT_ELogg.TabellNavn     = "Kunde"
                        bTT_ELogg.EksterntSystem = "POS"  
                        bTT_ELogg.EndringsType   = 1
                        bTT_ELogg.Verdier        = STRING(Kunde.KundeNr) + ";" + STRING(KundeKort.Kortnr).
                 RELEASE bTT_ELogg.
             END.
         END.
     END.
     /* Tar hånd om enkeltkunder */
     ELSE DO:
         FOR EACH TT_Elogg WHERE TT_ELogg.TabellNavn     = "Kunde" AND
                                 TT_ELogg.EksterntSystem = "POS"   AND
                                 TT_ELogg.EndringsType = 1         AND
                                 NUM-ENTRIES(TT_ELogg.Verdier,";") = 1:  
             FIND Kunde WHERE Kunde.KundeNr = INT(ENTRY(1,TT_ELogg.Verdier,";")) NO-LOCK NO-ERROR.
             IF NOT AVAIL Kunde THEN DO:
                 DELETE TT_Elogg.
                 NEXT.
             END.
             FOR EACH KundeKort OF Kunde NO-LOCK WHERE LENGTH(KundeKort.Kortnr) < 7:
                 ASSIGN iNr = INT(KundeKort.KortNr) NO-ERROR.
                 IF ERROR-STATUS:ERROR THEN
                     NEXT.
                 IF CAN-FIND(bTT_ELogg WHERE bTT_ELogg.TabellNavn     = "Kunde" AND
                       bTT_ELogg.EksterntSystem = "POS"  AND
                       bTT_ELogg.Verdier = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.Kortnr)) THEN
                     NEXT.
                 CREATE bTT_ELogg.
                 ASSIGN bTT_ELogg.TabellNavn     = "Kunde"
                        bTT_ELogg.EksterntSystem = "POS"  
                        bTT_ELogg.EndringsType   = 1
                        bTT_ELogg.Verdier        = STRING(Kunde.KundeNr) + ";" + STRING(KundeKort.Kortnr).
                 RELEASE bTT_ELogg.
             END.
             DELETE TT_Elogg.
         END.
     END.
/* /* InfoPos kasse tar bara emot nya */                                              */
/*     DEFINE VARIABLE cButiker AS CHARACTER  NO-UNDO.                                */
/*     DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.                                */
/*     ASSIGN cButiker = IF cLanButiker <> "" AND cFtpButiker <> "" THEN              */
/*                    cLanButiker + "," + cFtpButiker ELSE IF cLanButiker <> "" THEN  */
/*                        cLanButiker ELSE cFtpButiker.                               */
/*     FOR EACH TT_Elogg WHERE                                                        */
/*         TT_ELogg.TabellNavn     = "Kunde" AND                                      */
/*         TT_ELogg.EksterntSystem = "POS"           AND                              */
/*         TT_ELogg.EndringsType = 3:                                                 */
/*                                                                                    */
/*         CREATE TT_KundeTilEksport.                                                 */
/*         ASSIGN TT_KundeTilEksport.KundeNr  = INT(ENTRY(1,TT_ELogg.Verdier,CHR(1))) */
/*                TT_KundeTilEksport.Slettes = TRUE.                                  */
/*         RELEASE TT_KundeTilEksport.                                                */
/*     END.                                                                           */
/*     IF CAN-FIND(FIRST TT_Elogg WHERE TT_ELogg.TabellNavn = "Kunde" AND             */
/*                                      TT_ELogg.EksterntSystem = "POS" AND           */
/*                                      TT_ELogg.Verdier = "ALLE") THEN DO:           */
/*         DO iCount = 1 TO NUM-ENTRIES(cButiker):                                    */
/*             FOR EACH Kunde WHERE Kunde.Butik = INT(ENTRY(iCount,cButiker)):        */
/*                 CREATE TT_KundeTilEksport.                                         */
/*                 ASSIGN TT_KundeTilEksport.Butik   = Kunde.Butik                    */
/*                        TT_KundeTilEksport.KundeNr  = Kunde.KundeNr.                */
/*                 RELEASE TT_KundeTilEksport.                                        */
/*             END.                                                                   */
/*         END.                                                                       */
/*     END.                                                                           */
/*     ELSE FOR EACH TT_Elogg WHERE                                                   */
/*         TT_ELogg.TabellNavn     = "Kunde" AND                                      */
/*         TT_ELogg.EksterntSystem = "POS"           AND                              */
/*         TT_ELogg.EndringsType = 1:                                                 */
/*         CREATE TT_KundeTilEksport.                                                 */
/*         ASSIGN TT_KundeTilEksport.Butik   = INT(ENTRY(1,TT_ELogg.Verdier,CHR(1)))  */
/*                TT_KundeTilEksport.KundeNr  = INT(ENTRY(2,TT_ELogg.Verdier,CHR(1))) */
/*                TT_KundeTilEksport.Slettes = FALSE.                                 */
/*         RELEASE TT_KundeTilEksport.                                                */
/*     END.                                                                           */
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
    DEFINE BUFFER bElogg FOR Elogg.
    
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "Kunde" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "Kunde" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO TRANSACTION:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END. /* TRANSACTION */
    END.
    ELSE DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "Kunde" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            DO TRANSACTION:
                BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
                FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
                IF AVAIL bElogg AND cPRSFtpButiker = '' THEN
                    DELETE bELogg.
                IF AVAILABLE TT_Elogg THEN
                    RELEASE TT_ELogg.
            END. /* TRANSACTION */
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettTT_ELoggKunde) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggKunde Procedure 
PROCEDURE SlettTT_ELoggKunde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "Kunde" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

