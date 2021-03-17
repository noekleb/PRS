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
DEFINE INPUT  PARAMETER cFtpButiker AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cExportFil  AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cKundeFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntKunder  AS INTEGER    NO-UNDO.

DEFINE VARIABLE  iCount         AS INTEGER                                NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.
DEFINE VARIABLE bEkspPrButikk   AS LOG                                    NO-UNDO. 
DEFINE VARIABLE iKasseEksportFormat AS INTEGER NO-UNDO.
DEFINE VARIABLE cDatoTekst      AS CHARACTER EXTENT 10 NO-UNDO.


DEFINE TEMP-TABLE TT_Kunde
/* 1  */ FIELD butnr    AS INTEGER FORMAT ">>>>>9"                   /* I (3)           Butikknr                                    */ 
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
/* 18 */ FIELD TotalRabatt% AS DEC FORMAT "->9.99"                /* */
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

&IF DEFINED(EXCLUDE-DatoChar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DatoChar Procedure 
FUNCTION DatoChar RETURNS CHARACTER
  ( INPUT dDato AS DATE )  FORWARD.

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
    DEFINE VARIABLE  deSaldo           LIKE KundeSaldo.Saldo NO-UNDO.
    DEFINE VARIABLE  dtDatoSiste       AS DATE       NO-UNDO.
    DEFINE VARIABLE  iSlagsNr          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  iMedlemsNr        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  dDeciTst          AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cString            AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDato              AS CHARACTER  NO-UNDO.

    DEF BUFFER bPost FOR Post. 

    DEFINE VARIABLE  cNumericFormat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat    AS CHARACTER  NO-UNDO.
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".

    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.
    
    CREATE TT_Kunde.
    ASSIGN TT_Kunde.butnr = iButik
           iAntKunder     = 0. /* tyvärr, vi loopar runt butiker */

    /* Legger ut sletteposter for kunde.          */
    /* Dette lar vi gå til alle butikker uansett. */
    FOR EACH TT_Elogg WHERE
             TT_ELogg.TabellNavn     = "Kunde" AND
             TT_ELogg.EksterntSystem = "POS"   AND
             TT_ELogg.EndringsType = 3:

        ASSIGN TT_Kunde.kundenr = INT(ENTRY(1,TT_ELogg.Verdier,";")) /* kortnumret */
               TT_Kunde.aksjon = 3
               TT_Kunde.dato   = TODAY
               iAntKunder      = iAntKunder + 1.

        ASSIGN cString = 'KUNDE;3'  + ";" +
                         ENTRY(1,TT_ELogg.Verdier,";")  + FILL(";",70)
               iAntKunder      = iAntKunder + 1.
        if cString <> ? THEN
            PUT UNFORMATTED CString SKIP.
    END.
    
    /* Legger ut nye og endringer */
    FOR EACH TT_Elogg WHERE
             TT_Elogg.TabellNavn     = "Kunde" AND
             TT_ELogg.EksterntSystem = "POS"   AND
             TT_ELogg.EndringsType = 1:

        FIND Kunde WHERE Kunde.KundeNr = INT(ENTRY(1,TT_ELogg.Verdier,";")) NO-LOCK NO-ERROR.
        IF NOT AVAIL Kunde THEN 
            NEXT.
        /* Er parameter satt, skal butikken bare ha de kunder som tilhører den. */
        IF bEkspPrButikk AND Kunde.Butik <> iButik THEN 
           NEXT.
           
        ASSIGN deSaldo     = Kunde.KundeSaldo
               dtDatoSiste = Kunde.SisteKjop.

        FIND Post  NO-LOCK WHERE Post.PostNr = Kunde.PostNr  NO-ERROR.
        FIND bPost NO-LOCK WHERE Post.PostNr = Kunde.PostNr  NO-ERROR.

        cDatoTekst[1] = DatoChar(Kunde.Opphort). 
        cDatoTekst[2] = DatoChar(Kunde.SisteKjop). 

        ASSIGN
/*  1 */     cString =  'KUNDE' + ';' +
/*  2 */                (IF Kunde.Opphort <> ? OR Kunde.Aktiv = FALSE THEN '3' ELSE '1') + ';' +  /* lägg in  sletteflagga om kunde.utgått */
/*  3 */                STRING(Kunde.KundeNr) + ';' +             
/*  4 */                REPLACE(REPLACE(Kunde.Navn,";",""),'"'," ") + ";" +
/*  5 */                STRING(Kunde.TypeId) + ';' +                       
/*  6 */                STRING(Kunde.GruppeId) + ';' +            
/*  7 */                REPLACE(REPLACE(Kunde.Adresse1,";",""),'"'," ") + ';' +            
/*  8 */                (IF Kunde.KreditSperret = TRUE THEN '0' ELSE IF Kunde.BetType = 1 THEN '-1' ELSE STRING(Kunde.MaksKredit)) + ';' +          
/*  9 */                STRING(Kunde.KreditSperret) + ';' +       
/* 10 */                (IF bEkspPrButikk THEN STRING(Kunde.ButikkNr) ELSE STRING(iButik)) + ';' +            
/* 11 */                STRING(Kunde.TotalRabatt%) + ';' +        
/* 12 */                (IF Kunde.Alder <> ? THEN STRING(Kunde.Alder) ELSE '') + ';' +               
/* 13 */                STRING(Kunde.KundeSaldo) + ';' +          
/* 14 */                STRING(Kunde.Aktiv) + ';' +
/* 15 */            STRING(Kunde.PostNr) + ';' +
/* 16 */            (IF AVAILABLE Post THEN REPLACE(REPLACE(Post.Beskrivelse,";",""),'"'," ") ELSE '')
                    /* STRING(Kunde.Adresse2) + ';' + */
                    /* STRING(Kunde.Telefon) + ';' +                              */
                    /* STRING(Kunde.Telefaks) + ';' +                             */
                    /* STRING(Kunde.MobilTlf) + ';' +                             */
                    /* REPLACE(REPLACE(Kunde.KontNavn,";",""),'"'," ") + ";" +    */
                    /* STRING(Kunde.KontTelefon) + ';' +                          */
                    /* STRING(Kunde.Stilling) + ';' +                             */
                    /* STRING(Kunde.KontTelefaks) + ';' +                         */
                    /* STRING(Kunde.KontMobilTlf) + ';' +                         */
                    /* STRING(Kunde.LevAdresse1) + ';' +                          */
                    /* STRING(Kunde.LevAdresse2) + ';' +                          */
                    /* STRING(Kunde.LevPostNr) + ';' +                            */
                    /* IF AVAILABLE bPost THEN bPost.Beskrivelse ELSE '') + ';' + */
                    /* STRING(Kunde.LevLand) + ';' +                              */
                    /* STRING(Kunde.Land) + ';' +                                 */
                    /* STRING(Kunde.BydelsNr) + ';' +     */
                    /* STRING(Kunde.ePostAdresse) + ';' + */
                    /* STRING(Kunde.KontE-Post) + ';' +   */
                    /* STRING(Kunde.OrgNr) + ';' +        */
                    /* STRING(Kunde.BankKonto) + ';' +                                       */
                    /* STRING(Kunde.Postgiro) + ';' +                                        */
                    /* STRING(Kunde.BetBet) + ';' +                                          */
                    /* STRING(Kunde.Etablert) + ';' +                                        */
                    /* STRING(Kunde.SamleFaktura) + ';' +                                    */
                    /* STRING(Kunde.PrivatTlf) + ';' +                                       */
                    /* STRING(Kunde.Kjon) + ';' +                                            */
                    /* (IF Kunde.SisteKjop = ? THEN '' ELSE STRING(Kunde.SisteKjop)) + ';' + */
                    /* STRING(Kunde.FaktAdresse1) + ';' + */
                    /* STRING(Kunde.FaktAdresse2) + ';' + */
                    /* STRING(Kunde.FaktPostNr) + ';' +   */
                    /* STRING(Kunde.FaktTekstNr) + ';' +  */
                    /* STRING(Kunde.DeresRef) + ';' +     */
                    /* STRING(Kunde.Privat) + ';' +       */
                    /* STRING(Kunde.FaktLand) + ';' +     */
                    /* STRING(Kunde.ValKod) + ';' +       */
                    /* STRING(Kunde.BetType) + ';' +      */
                    /* STRING(Kunde.Purregebyr) + ';' +   */
                    /* STRING(Kunde.Fakturagebyr) + ';' + */
                    /* STRING(Kunde.WebKunde) + ';' +     */
                    /* STRING(Kunde.Hovedkunde) + ';' +          */
                    /* STRING(Kunde.KobletTilKunde) + ';' +      */
                    /* STRING(Kunde.Faktureringsperiode) + ';' + */
                    /* STRING(Kunde.Kilde) + ';' +               */
                    /* STRING(Kunde.TilgKilde) + ';' +           */
                    /* STRING(Kunde.EksterntKundeNr) + ';' +     */
                    /* STRING(Kunde.Momskod) + ';' +             */
                    /* STRING(Kunde.ByNavn) + ';' +              */
                    /* STRING(Kunde.Avdeling) + ';' +            */
                    /* STRING(Kunde.Tittel) + ';' +              */
                    /* STRING(Kunde.Hilsen) + ';' +              */
                    /* STRING(Kunde.eMailFirma) + ';' +          */
                    /* STRING(Kunde.TelefonFirma) + ';' +        */
                    /* STRING(Kunde.BankNavn) + ';' +            */
                    /* STRING(Kunde.BankKode) + ';' +            */
                    /* STRING(Kunde.WebKanSetteOrdre) + ';' +    */
                    /* STRING(Kunde.WebKanSendeEMail) + ';' +    */
                    /* STRING(Kunde.UrlFirma) + ';' +            */
                    /* STRING(Kunde.Region)                      */
                       NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT.
        if cString <> ? THEN
            PUT UNFORMATTED cString SKIP.

        FOR EACH KundeKort OF Kunde WHERE 
            KundeKort.sperret = FALSE AND 
            (KundeKort.Utgardato = ? OR KundeKort.UtgarDato > TODAY) 
            NO-LOCK:
            cDatoTekst[1] = DatoChar(KundeKort.AktivertDato). 
            cDatoTekst[2] = DatoChar(KundeKort.UtgarDato). 
            
            cDato = IF KundeKort.Utgardato = ? THEN "" ELSE STRING(YEAR(KundeKort.Utgardato),"9999") + 
                                                            STRING(MONTH(KundeKort.Utgardato),"99") +
                                                            STRING(DAY(KundeKort.Utgardato),"99").
            ASSIGN
/*  1 */      cString = 'KUNDEKORT' + ';' +
/*  2 */                '1' + ';' + 
/*  3 */                STRING(KundeKort.KundeNr) + ';' +        
/*  4 */                STRING(KundeKort.KortNr) + ';' +         
/*  5 */                STRING(KundeKort.Merknad) + ';' +        
/*  6 */                cDatoTekst[1] + ';' +   
/*  7 */                cDatoTekst[2] + ';' +         
/*  8 */                STRING(KundeKort.Sperret) + ';' +        
/*  9 */                STRING(KundeKort.Innehaver) + ';' + 
/* 10 */                STRING(KundeKort.InterntKKortId) /* Kobling til medlemsnr. */
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            if cString <> ? THEN
                PUT UNFORMATTED cString SKIP.
        END.
        
        FOR EACH Kundesaldo WHERE KundeSaldo.KundeNr = Kunde.KundeNr NO-LOCK:
            cDatoTekst[1] = DatoChar(KundeSaldo.ForsteDato). 
            cDatoTekst[2] = DatoChar(KundeSaldo.DatoSiste). 

            ASSIGN
                cString = 'KUNDESALDO;1' + ';' +
                          STRING(KundeSaldo.KundeNr) + ';' +      
                          STRING(KundeSaldo.ButikkNr) + ';' +   
                          cDatoTekst[1] + ';' +   
                          cDatoTekst[2] + ';' +   
                          STRING(KundeSaldo.ForsteTid) + ';' +  
                          STRING(KundeSaldo.SisteTid) + ';' +   
                          STRING(KundeSaldo.Saldo) + ';' +      
                          STRING(KundeSaldo.TotaltKjop) 
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            if cString <> ? THEN
                PUT UNFORMATTED cString SKIP.
        END.
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
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.
    ELSE DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "Kunde" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
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

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-DatoChar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DatoChar Procedure 
FUNCTION DatoChar RETURNS CHARACTER
  ( INPUT dDato AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

        DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
        
        cTekst = IF dDato = ? 
                   THEN '00000000' 
                   ELSE (
                         STRING(YEAR(dDato),'9999') + 
                         STRING(MONTH(dDato),'99') + 
                         STRING(DAY(dDato),'99')
                        ).
        RETURN cTekst.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

