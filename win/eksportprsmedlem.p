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
DEFINE INPUT  PARAMETER cFtpButiker   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cExportFil    AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cMedlemFiler  AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntMedlemmer AS INTEGER    NO-UNDO.

DEFINE VARIABLE  iCount         AS INTEGER                                NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.
DEFINE VARIABLE bEkspPrButikk   AS LOG                                    NO-UNDO. 
DEFINE VARIABLE iKasseEksportFormat AS INTEGER NO-UNDO.
DEFINE VARIABLE cDatoTekst        AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE cTidTekst         AS CHARACTER EXTENT 10 NO-UNDO.

DEFINE TEMP-TABLE TT_Medlem NO-UNDO LIKE Medlem
         FIELD Aksjon   AS INTEGER FORMAT ">9"
         FIELD butnr    AS INTEGER FORMAT ">>>>>9"   
         FIELD KortNr   LIKE Medlemskort.KortNr
         INDEX KortNr ButikkNr KortNr
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
    ( INPUT dDato AS DATE ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TidChar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TidChar Procedure 
FUNCTION TidChar RETURNS CHARACTER
    (INPUT iTid AS INTEGER  ) FORWARD.

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

/* eksport av medlemmer pr. butikk (Medlem.Butik). */
/* Bruker samme parameter som for kunde.           */
{syspara.i 14 2 6 cTekst}
IF CAN-DO('1,J,Ja,True,Yes',cTekst) 
  THEN bEkspPrButikk = TRUE.
  ELSE bEkspPrButikk = FALSE.

/* Vi exporterar direkt från ELogg till fil */
/* Ingen extra behandling behöver göras */
RUN KopierElogg.
/* Kanske vi skall hämta iformation om kassor och filer först. */
RUN FixMedlemEndringer.
/* Här skall vi loopa runt alla kassor mm */
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportMedlem IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggMedlem. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportMedlem Procedure 
PROCEDURE ExportMedlem PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE         iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE         iNumeric   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE         iPostNr    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  iSlagsNr          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  iMedlemsNr        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  dDeciTst          AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cString            AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDato              AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE  cNumericFormat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat    AS CHARACTER  NO-UNDO.
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".

    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.
    
    CREATE TT_Medlem.
    ASSIGN TT_Medlem.ButikkNr = iButik
           iAntMedlemmer     = 0. /* tyvärr, vi loopar runt butiker */

    /* Legger ut sletteposter for medlem.          */
    /* Dette lar vi gå til alle butikker uansett. */
    FOR EACH TT_Elogg WHERE
             TT_ELogg.TabellNavn     = "Medlem" AND
             TT_ELogg.EksterntSystem = "POS"   AND
             TT_ELogg.EndringsType = 3:

        ASSIGN cString = 'MEDLEM;3'  + ";" +
                         ENTRY(1,TT_ELogg.Verdier,";") + FILL(";",30)
                         iAntMedlemmer      = iAntMedlemmer + 1.
        PUT UNFORMATTED CString SKIP.
    END.
    
    /* Legger ut nye og endringer */
    MEDLEMSLOGG:
    FOR EACH TT_Elogg WHERE
             TT_Elogg.TabellNavn     = "Medlem" AND
             TT_ELogg.EksterntSystem = "POS"   AND
             TT_ELogg.EndringsType = 1:

        FIND Medlem WHERE Medlem.MedlemsNr = DEC(ENTRY(1,TT_ELogg.Verdier,";")) NO-LOCK NO-ERROR.
        IF NOT AVAIL Medlem THEN 
            NEXT.
        /* Er parameter satt, skal butikken bare ha de medlemmer som tilhører den. */
        IF bEkspPrButikk AND Medlem.ButikkNr <> iButik THEN 
           NEXT.

        cDatoTekst[1] = DatoChar(Medlem.Opphort). 
        cDatoTekst[2] = DatoChar(Medlem.FodselsDato). 

        ASSIGN cString = 'MEDLEM;1'  + ";" +
               STRING(Medlem.MedlemsNr) + ';' +        
               REPLACE(REPLACE(Medlem.ForNavn,";",""),'"'," ") + ";" +
               REPLACE(REPLACE(Medlem.Etternavn,";",""),'"'," ") + ";" +
               STRING(Medlem.MedType) + ';' +          
               STRING(Medlem.MedGruppe) + ';' +        
               STRING(Medlem.Adresse1) + ';' +         
               STRING(Medlem.Adresse2) + ';' +         
               STRING(Medlem.PostNr) + ';' +           
               (IF AVAIL Post THEN Post.Beskrivelse ELSE "") + ';' +
               STRING(Medlem.Telefon) + ';' +          
               STRING(Medlem.Telefaks) + ';' +         
               STRING(Medlem.MobilTlf) + ';' +         
               STRING(Medlem.Land) + ';' +             
               cDatoTekst[1] + ';' +          
               STRING(Medlem.ButikkNr) + ';' +         
               STRING(Medlem.BydelsNr) + ';' +         
               STRING(Medlem.ePostAdresse) + ';' +     
               STRING(Medlem.HovedMedlemFlagg) + ';' + 
               STRING(Medlem.HovedMedlemsNr) + ';' +   
               cDatoTekst[2] + ';' +      
               STRING(Medlem.FodtAr) + ';' +           
               STRING(Medlem.Kjonn) + ';' +            
               STRING(Medlem.RegKode) + ';' +          
               STRING(Medlem.KundeNr) + ';' +          
               STRING(Medlem.Aktiv) + ';' +            
               STRING(Medlem.AktivertFraWeb) + ';' +   
               STRING(Medlem.WebBrukerId) + ';' +      
               STRING(Medlem.WebPassord) + ';' +       
               STRING(Medlem.Kilde) + ';' +            
               STRING(Medlem.TilgKilde) + ';' +        
               STRING(Medlem.Rabatt) + ';' +           
               STRING(Medlem.EksterntMedlemsNr) + ';' +
               STRING(Medlem.Bonus_Berettiget) + ';' + 
               STRING(Medlem.Bonus_Forsendelse) + ';' +
               STRING(Medlem.Bonus_varsel) + ';' +     
               STRING(Medlem.MedlemNotat) + ';' +      
               STRING(Medlem.MedlemInfo)       
               iAntMedlemmer  = iAntMedlemmer + 1 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT MEDLEMSLOGG.
        ELSE
            PUT UNFORMATTED CString SKIP.

        MEDLEMSKORT:
        FOR EACH MedlemsKort NO-LOCK WHERE
            MedlemsKort.MedlemsNr = Medlem.MedlemsNr:

            cDatoTekst[1] = DatoChar(MedlemsKort.AktivertDato). 
            cDatoTekst[2] = DatoChar(MedlemsKort.UtgarDato). 

            ASSIGN 
                cString = 'MEDLEMSKORT;1;' + 
                STRING(MedlemsKort.MedlemsNr) + ';' +      
                STRING(MedlemsKort.KortNr) + ';' +         
                STRING(MedlemsKort.Merknad) + ';' +        
                cDatoTekst[1] + ';' +   
                cDatoTekst[2] + ';' +      
                STRING(MedlemsKort.Sperret) + ';' +        
                STRING(MedlemsKort.Innehaver) + ';' +      
                STRING(MedlemsKort.KundeRabattKort) + ';' +
                STRING(MedlemsKort.KortType) + ';' +       
                STRING(MedlemsKort.InterntKKortId) 
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT MEDLEMSKORT.
            ELSE
                PUT UNFORMATTED CString SKIP.

        END. /* MEDLEMSKORT */

        MEDLEMSALDO:
        FOR EACH MedlemSaldo WHERE 
            MedlemSaldo.MedlemsNr = Medlem.MedlemsNr NO-LOCK:

            cDatoTekst[1] = DatoChar(MedlemSaldo.ForsteDato). 
            cDatoTekst[2] = DatoChar(MedlemSaldo.DatoSiste). 
            cTidTekst[1]   = TidChar(MedlemSaldo.ForsteTid).
            cTidTekst[2]   = TidChar(MedlemSaldo.SisteTid).
            ASSIGN
                cString = 'MEDLEMSALDO;1;' +
                STRING(MedlemSaldo.MedlemsNr) + ';' +  
                STRING(MedlemSaldo.ButikkNr) + ';' +  
                cDatoTekst[1] + ';' +
                cDatoTekst[2] + ';' + 
                STRING(MedlemSaldo.ForsteTid) + ';' + 
                STRING(MedlemSaldo.SisteTid) + ';' +  
                STRING(MedlemSaldo.Saldo) + ';' +     
                STRING(MedlemSaldo.TotaltKjop)
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT MEDLEMSALDO.
            ELSE
                PUT UNFORMATTED CString SKIP.
        END. /* MEDLEMSALDO */
    END. /* MEDLEMSLOGG*/

    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cMedlemFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cMedlemFiler = cMedlemFiler + (IF cMedlemFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.

    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixMedlemEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixMedlemEndringer Procedure 
PROCEDURE FixMedlemEndringer PRIVATE :
/* /*------------------------------------------------------------------------------   */
/*   Purpose:                                                                         */
/*   Parameters:  <none>                                                              */
/*   Notes:                                                                           */
/* ------------------------------------------------------------------------------*/   */
     DEFINE VARIABLE iNr AS INTEGER    NO-UNDO.
     DEFINE BUFFER bTT_Elogg FOR TT_Elogg.

     /* Tar hånd om sletteposter på medlem. Medlem har bare ett entry. */
     FOR EACH TT_Elogg WHERE TT_ELogg.TabellNavn     = "Medlem" AND
                             TT_ELogg.EksterntSystem = "POS"   AND
                             TT_ELogg.EndringsType = 3         AND
                             NUM-ENTRIES(TT_ELogg.Verdier,";") = 1:  
     
         FOR EACH MedlemsKort OF Medlem NO-LOCK WHERE:
             ASSIGN iNr = INT(MedlemsKort.KortNr) NO-ERROR.
             IF ERROR-STATUS:ERROR THEN
                 NEXT.
             FIND bTT_ELogg WHERE bTT_ELogg.TabellNavn = "Medlem" AND
                  bTT_ELogg.EksterntSystem = "POS"               AND
                  bTT_ELogg.Verdier = STRING(MedlemsKort.MedlemsNr) + ";" + STRING(MedlemsKort.Kortnr) NO-ERROR.
             IF AVAIL bTT_ELogg THEN
                 ASSIGN bTT_ELogg.EndringsType = 3.
             ELSE DO:
                 CREATE bTT_ELogg.
                 ASSIGN bTT_ELogg.TabellNavn     = "Medlem"
                        bTT_ELogg.EksterntSystem = "POS"  
                        bTT_ELogg.EndringsType   = 3
                        bTT_ELogg.Verdier        = STRING(MedlemsKort.MedlemsNr) + ";" + STRING(MedlemsKort.Kortnr).
                 RELEASE bTT_ELogg.
             END.
         END.
         DELETE TT_Elogg.
     END.

     /* Hvis ALLE medlemmerr skal legges ut. */
     IF CAN-FIND(TT_ELogg WHERE TT_ELogg.TabellNavn     = "Medlem" AND
                                TT_ELogg.EksterntSystem = "POS"   AND
                                TT_ELogg.EndringsType = 1         AND
                                TT_ELogg.Verdier = "ALLE") THEN DO:
         /* Tar bort de andre loggpostene da disse ikke behøves. */
         FOR EACH bTT_ELogg WHERE bTT_ELogg.TabellNavn     = "Medlem" AND
                                  bTT_ELogg.EksterntSystem = "POS"   AND
                                  bTT_ELogg.EndringsType = 1:
             DELETE bTT_ELogg.
         END.
         FOR EACH Medlem NO-LOCK:
             FOR EACH MedlemsKort OF Medlem NO-LOCK:
                 ASSIGN iNr = INT(MedlemsKort.KortNr) NO-ERROR.
                 IF ERROR-STATUS:ERROR THEN
                     NEXT.
                 IF CAN-FIND(bTT_ELogg WHERE bTT_ELogg.TabellNavn     = "Medlem" AND
                       bTT_ELogg.EksterntSystem = "POS"  AND
                       bTT_ELogg.Verdier = STRING(MedlemsKort.MedlemsNr) + ";" + STRING(MedlemsKort.Kortnr)) THEN
                     NEXT.
                 CREATE bTT_ELogg.
                 ASSIGN bTT_ELogg.TabellNavn     = "Medlem"
                        bTT_ELogg.EksterntSystem = "POS"  
                        bTT_ELogg.EndringsType   = 1
                        bTT_ELogg.Verdier        = STRING(MedlemsKort.MedlemsNr) + ";" + STRING(MedlemsKort.Kortnr).
                 RELEASE bTT_ELogg.
             END.
         END.
     END.
     /* Tar hånd om enkeltmedlemmer */
     ELSE DO:
         FOR EACH TT_Elogg WHERE TT_ELogg.TabellNavn     = "Medlem" AND
                                 TT_ELogg.EksterntSystem = "POS"   AND
                                 TT_ELogg.EndringsType = 1         AND
                                 NUM-ENTRIES(TT_ELogg.Verdier,";") = 1:  
             FIND Medlem WHERE Medlem.MedlemsNr = INT(ENTRY(1,TT_ELogg.Verdier,";")) NO-LOCK NO-ERROR.
             IF NOT AVAIL Medlem THEN DO:
                 DELETE TT_Elogg.
                 NEXT.
             END.
             FOR EACH MedlemsKort OF Medlem NO-LOCK WHERE:
                 ASSIGN iNr = INT(MedlemsKort.KortNr) NO-ERROR.
                 IF ERROR-STATUS:ERROR THEN
                     NEXT.
                 IF CAN-FIND(bTT_ELogg WHERE bTT_ELogg.TabellNavn     = "Medlem" AND
                       bTT_ELogg.EksterntSystem = "POS"  AND
                       bTT_ELogg.Verdier = STRING(MedlemsKort.MedlemsNr) + ";" + STRING(MedlemsKort.Kortnr)) THEN
                     NEXT.
                 CREATE bTT_ELogg.
                 ASSIGN bTT_ELogg.TabellNavn     = "Medlem"
                        bTT_ELogg.EksterntSystem = "POS"  
                        bTT_ELogg.EndringsType   = 1
                        bTT_ELogg.Verdier        = STRING(MedlemsKort.MedlemsNr) + ";" + STRING(MedlemsKort.Kortnr).
                 RELEASE bTT_ELogg.
             END.
             DELETE TT_Elogg.
         END.
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
    DEFINE BUFFER bElogg FOR Elogg.
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "Medlem" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "Medlem" AND
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
        FOR EACH ELogg WHERE ELogg.TabellNavn = "Medlem" AND
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

&IF DEFINED(EXCLUDE-SlettTT_ELoggMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggMedlem Procedure 
PROCEDURE SlettTT_ELoggMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = "Medlem" AND
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
    ( INPUT dDato AS DATE ):
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

&IF DEFINED(EXCLUDE-TidChar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TidChar Procedure 
FUNCTION TidChar RETURNS CHARACTER
    (INPUT iTid AS INTEGER  ):
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/

        DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

        cTekst = REPLACE(STRING(iTid,'HH:MM:SS'),':','').

        RETURN cTekst.



END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

