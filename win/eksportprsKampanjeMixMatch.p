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
DEFINE OUTPUT PARAMETER cKKampFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntVarGr   AS INTEGER    NO-UNDO.

DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.
DEFINE VARIABLE iCl             AS INTEGER NO-UNDO.
DEFINE VARIABLE iKasseEksportFormat AS INTEGER NO-UNDO.
DEFINE VARIABLE cDatoTekst        AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE cTidTekst         AS CHARACTER EXTENT 10 NO-UNDO.

DEFINE TEMP-TABLE TT_KKamp
    FIELD KampId   AS DEC       FORMAT ">>>>>>>>>>>>9"
    FIELD ButNr    AS INTEGER   FORMAT ">>>>>9"           
    FIELD MixNr    AS INTEGER   FORMAT ">>>>>>>>9"        
    FIELD MixType  AS INTEGER   FORMAT ">9"               
    FIELD Medlem   AS LOGICAL                             
    FIELD FraDato  AS DATE      FORMAT "99/99-99"         
    FIELD FraTid   AS CHARACTER FORMAT "MM:SS" 
    FIELD TilDato  AS DATE      FORMAT "99/99-99"         
    FIELD TilTid   AS CHARACTER FORMAT "MM:SS"             
    FIELD Aksjon   AS INTEGER   FORMAT "9"
    FIELD Antall   AS DECIMAL DECIMALS 3 FORMAT ">>>9.999" 
    FIELD Utpris   AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99" 
    FIELD Subtotal AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99" 
    FIELD EAN      AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" 
    FIELD RadAnt   AS DECIMAL DECIMALS 3 FORMAT ">>>9.99" 
    FIELD RadPris  AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99" 
    FIELD GrpNr    AS INTEGER FORMAT ">9"
    FIELD ModellNr AS INTEGER FORMAT ">>>>>>>9" 
    INDEX MixKampanje KampId MixType MixNr.

DEF BUFFER clButiker FOR Butiker.

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
         WIDTH              = 60.
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

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCl NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
  DO:
    MESSAGE "Kan ikke finne sentrallager " + string(icl) + '. Kombinasjonskampanje er ikke eksportert.'
    VIEW-AS ALERT-BOX.
    RETURN.
  END.

/* Vi exporterar direkt från ELogg till fil */
/* Ingen extra behandling behöver göras */
RUN KopierElogg.
RUN FixAlleKampanjer.

MIXMATCH:
FOR EACH TT_ELogg WHERE 
  TT_ELogg.TabellNavn     = "KampanjeMixMatch" AND
  TT_ELogg.EksterntSystem = "POS"  AND
  TT_ELogg.EndringsType   = 1 BY TT_ELogg.Verdier :

  FIND KampanjeMixMatch NO-LOCK WHERE
    KampanjeMixMatch.KampId = DECIMAL(TT_ELogg.Verdier) NO-ERROR.
  IF NOT AVAILABLE KampanjeMixMatch THEN 
    NEXT MIXMATCH.

  /* Här skall vi loopa runt alla kassor mm */
  IF cFtpButiker <> "" THEN DO:
    DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
        /* Sjekker om den aktuelle butikken skal ha kampanjen. */
        IF NOT CAN-FIND(FIRST Kampanjebutikker WHERE
                        KampanjeButikker.KampId = KampanjeMixMatch.KampId AND
                        KampanjeButikker.Butik  = INT(ENTRY(iCount,cFtpButiker))) THEN
            NEXT.
        /* Eksport */
        RUN ExportKampanjeMixMatch IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker),KampanjeMixMatch.KampId). /* parameter = den loopade butiken */
    END.
  END.
END. /* MIXMATCH */

RUN SlettTT_ELoggKKamp. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportKampanjeMixMatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportKampanjeMixMatch Procedure 
PROCEDURE ExportKampanjeMixMatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER plKampId   LIKE KampanjeMixMatch.KampId NO-UNDO.

    DEFINE VARIABLE iCount   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFraDato AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTilDato AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cString  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE  cNumericFormat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat    AS CHARACTER  NO-UNDO.
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".

    /* Setter riktig butikknummer i filen */
    IF cFilSuffix <> "txt" THEN 
      iButik = int(cFilSuffix).
    ELSE 
      iButik = clButiker.Butik.
    
    ASSIGN iAntVarGr = 0. /* tyvärr eftersom jag gör detta flera gånger */

    IF CAN-FIND(FIRST Kampanjetilbud WHERE
                KampanjeTilbud.KampId = plKampId) THEN
    SKRIV_TIL_FIL:
    DO:
        OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.
        /* Legger ut kampanjen . */
        KAMPANJETILBUD:
        FOR EACH KampanjeTilbud NO-LOCK WHERE
            KampanjeTilbud.KampId = plKampId:
            
            cDatoTekst[1] = DatoChar(KampanjeMixMatch.KampStartDato). 
            cDatoTekst[2] = DatoChar(KampanjeMixMatch.KampSluttDato). 
            cTidTekst[1]  = TidChar(KampanjeMixMatch.KampStartTid).
            cTidTekst[2]  = TidChar(KampanjeMixMatch.KampSluttTid).

            ASSIGN
          /* 01 */ cString = "KAMPANJETILBUD;" + 
          /* 02 */           STRING(KampanjeMixMatch.KampId) + ";" +                                             
          /* 03 */           cDatoTekst[1] + ";" +                                    
          /* 04 */           STRING(KampanjeMixMatch.KampStartTid) + ";" +                                     
          /* 05 */           cDatoTekst[2] + ";" +                                    
          /* 06 */           STRING(KampanjeMixMatch.KampSluttTid) + ";" +                                     
          /* 07 */           STRING(KampanjeTilbud.KampTilbId) + ";" +                                       
          /* 08 */           STRING(KampanjeTilbud.KampTilbTypeId) + ";" +                                   
          /* 09 */           STRING(KampanjeTilbud.HapHourId) + ";" +                                        
          /* 10 */           STRING(KampanjeTilbud.KampTilbBelop) + ";" +                                    
          /* 11 */           REPLACE(REPLACE(KampanjeTilbud.KampTilbKvitteringstekst,";",""),'"'," ") + ";" +
          /* 12 */           STRING(KampanjeTilbud.KamptilbPopUpTekstBruk) + ";" +                           
          /* 13 */           STRING(KampanjeTilbud.KamptilbPopUpTekst) + ";" +                               
          /* 14 */           STRING(KampanjeTilbud.KampTilbPropBetalFor) + ";" +                             
          /* 15 */           STRING(KampanjeTilbud.KampTilbOkning) + ";" +                                   
          /* 16 */           STRING(KampanjeTilbud.KamptilbGrenseAntallBruk) + ";" +                         
          /* 17 */           STRING(KampanjeTilbud.KamptilbGrenseAntall) + ";" +                             
          /* 18 */           REPLACE(REPLACE(KampanjeTilbud.KampTilbNavn,";",""),'"'," ")
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            ELSE
                PUT UNFORMATTED cString SKIP.

            IF KampanjeTilbud.HapHourId > 0 THEN
            HAPPYHOUR:
            FOR EACH HappyHourHode NO-LOCK WHERE
                HappyHourHode.HapHourId = KampanjeTilbud.HapHourId:

                ASSIGN
                    cString = "HAPPYHOURHODE;" +
                              STRING(HappyHourHode.HapHourId) + ";" +    
                              REPLACE(REPLACE(HappyHourHode.HapHourNavn,";",""),'"'," ")
                    NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    NEXT.
                ELSE
                    PUT UNFORMATTED cString SKIP.
               
                FOR EACH HappyHourPeriode NO-LOCK WHERE
                    HappyHourPeriode.HapHourId = KampanjeTilbud.HapHourId:

                    cTidTekst[1]  = TidChar(HappyHourPeriode.HapHourPerStartTid).
                    cTidTekst[2]  = TidChar(HappyHourPeriode.HapHourPerSluttTid).

                    ASSIGN
                        cString = "HAPPYHOURPERIODE;" +
                                  STRING(HappyHourPeriode.HapHourId) + ";" +              
                                  STRING(HappyHourPeriode.HapHourPerId) + ";" +          
                                  cTidTekst[1] + ";" +    
                                  cTidTekst[2] + ";" +    
                                  STRING(HappyHourPeriode.HapHourPerUkedagListe) 
                        NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        NEXT.
                    ELSE
                        PUT UNFORMATTED cString SKIP.

                END. 
            END. /* HAPPYHOUR */

            /* Linjer i kampanjetilbud */
            LINJER:
            FOR EACH KampanjeTilbartikkel NO-LOCK WHERE
                KampanjeTilbartikkel.KampId     = KampanjeMixMatch.KampId AND
                KampanjeTilbartikkel.KamptilbId = KampanjeTilbud.KampTilbId:

                ASSIGN
             /* 01 */ cString = "KAMPANJETILBARTIKKEL;" + 
             /* 02 */           STRING(KampanjeTilbartikkel.KampId) + ";" +
             /* 03 */           STRING(KampanjeTilbartikkel.KampTilbId) + ";" +            
             /* 04 */           STRING(KampanjeTilbartikkel.KampTilbArtSeq) + ";" +      
             /* 05 */           STRING(KampanjeTilbartikkel.KampTilbArtId) + ";" +       
             /* 06 */           STRING(KampanjeTilbartikkel.KampTilbArtBelop) + ";" +    
             /* 07 */           STRING(KampanjeTilbartikkel.KampTilbArtMinAntall) + ";" +
             /* 08 */           STRING(KampanjeTilbartikkel.KampRabattTypeId) + ";" +    
             /* 09 */           STRING(KampanjeTilbartikkel.ProdFamId) + ";" +           
             /* 10 */           STRING(KampanjeTilbartikkel.KupongId)            
                    NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    NEXT.
                ELSE
                    PUT UNFORMATTED cString SKIP.

                /* Produktfamilie */
                IF KampanjeTilbartikkel.ProdFamId > 0 THEN 
                PRODFAM:
                FOR EACH ProduktFamilie NO-LOCK WHERE
                    ProduktFamilie.ProdFamId = KampanjeTilbartikkel.ProdFamId:

                    ASSIGN
                        cString = "PRODUKTFAMILIE;" +
                                  STRING(ProduktFamilie.ProdFamId) + ";" +        
                                  REPLACE(REPLACE(ProduktFamilie.ProdFamNavn,";",""),'"'," ")  + ";" + 
                                  STRING(ProduktFamilie.ProdFamPrisLinje) + ";" +
                                  STRING(ProduktFamilie.ProdFamAutoReg) + ";" +  
                                  STRING(ProduktFamilie.ProdFamAktiv)    
                        NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        NEXT.
                    ELSE
                        PUT UNFORMATTED cString SKIP.
                   
                    MEDLEM:
                    FOR EACH ProduktFamMedlem OF ProduktFamilie NO-LOCK:
                        ASSIGN
                            cString = "PRODUKTFAMMEDLEM;" +
                                      STRING(ProduktFamMedlem.ProdFamId) + ";" +         
                                      STRING(ProduktFamMedlem.ProdFamArtikkelNr) + ";" +
                                      STRING(ProduktFamMedlem.ProdFamStrKode)   
                            NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN
                            NEXT.
                        ELSE
                            PUT UNFORMATTED cString SKIP.

                    END. /* MEDLEM */

                END. /* PRODFAM */

                IF KampanjeTilbArtikkel.KupongId > 0 THEN
                KUPONG:
                FOR EACH Kupong NO-LOCK WHERE
                    Kupong.KupongId = KampanjeTilbartikkel.KupongId:

                    cDatoTekst[1] = DatoChar(Kupong.GyldigFra). 
                    cDatoTekst[2] = DatoChar(Kupong.GyldigTil). 
                    cDatoTekst[3] = DatoChar(Kupong.SisteinnlDato). 

                    ASSIGN
                        cString = "KUPONG;" +
                                  STRING(Kupong.KupongId) + ";" +        
                                  REPLACE(REPLACE(Kupong.KupBeskrivelse,";",""),'"'," ")  + ";" + 
                                  STRING(Kupong.Belop) + ";" +          
                                  STRING(Kupong.MaksBelop) + ";" +      
                                  STRING(Kupong.MinBelop) + ";" +       
                                  STRING(Kupong.EANKode) + ";" +        
                                  cDatoTekst[1] + ";" +      
                                  cDatoTekst[2] + ";" +      
                                  STRING(Kupong.Aktiv) + ";" +          
                                  STRING(Kupong.IdKrav) + ";" +         
                                  STRING(Kupong.InterleaveKode) + ";" + 
                                  STRING(Kupong.KTypeNr) + ";" +        
                                  STRING(Kupong.RabattVerdi) + ";" +    
                                  cDatoTekst[3] + ";" +  
                                  STRING(Kupong.TaVarePaKupong) + ";" + 
                                  STRING(Kupong.KEierNr)        
                        NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        NEXT.
                    ELSE
                        PUT UNFORMATTED cString SKIP.
                   
                END. /* KUPONG */

            END. /* LINJER */
        END. /* KAMPANJETILBUD */
        OUTPUT CLOSE.
        
        FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
        IF FILE-INFO:FILE-SIZE = 0 THEN
            OS-DELETE VALUE(cExportFil + cFilsuffix).
        ELSE IF NOT CAN-DO(cKKampFiler,cExportFil + cFilsuffix) THEN
            ASSIGN cKKampFiler = cKKampFiler + (IF cKKampFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.
                        
    END. /* SKRIV_TIL_FIL */

    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.

END PROCEDURE. /* eksportKKamp */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-FixAlleKampanjer) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixAlleKampanjer Procedure
PROCEDURE FixAlleKampanjer:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "KampanjeMixMatch" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN 
  BLOKKEN:
  DO:
      FIND FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "KampanjeMixMatch" AND
                 TT_ELogg.EksterntSystem = "POS"    AND
                 TT_ELogg.EndringsType   = 1  AND
                 TT_Elogg.Verdier       = "ALLE" NO-ERROR.
      IF AVAILABLE TT_ELogg THEN DELETE TT_ELogg. 
              
      FOR EACH KampanjeMixMatch NO-LOCK WHERE 
               KampanjeMixMatch.KampKlar = TRUE:
          
          IF KampanjeMixMatch.KampStartDato = ? THEN LEAVE BLOKKEN.
          IF KampanjeMixMatch.KampSluttDato = ? THEN LEAVE BLOKKEN.

          IF KampanjeMixMatch.KampStartDato > TODAY THEN LEAVE BLOKKEN.
          IF KampanjeMixMatch.KampSluttDato < TODAY THEN LEAVE BLOKKEN.          

          IF NOT CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "KampanjeMixMatch" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier        = STRING(KampanjeMixMatch.KampId)) THEN 
          DO:
              CREATE TT_Elogg.
              ASSIGN                   
                  TT_ELogg.TabellNavn     = "KampanjeMixMatch" 
                  TT_ELogg.EksterntSystem = "POS" 
                  TT_ELogg.EndringsType   = 1
                  TT_ELogg.Verdier        = STRING(KampanjeMixMatch.KampId)
                  NO-ERROR.
              IF ERROR-STATUS:ERROR AND AVAILABLE TT_ELogg THEN 
                DELETE TT_Elogg.
          END.          
      END.
  END. /* BLOKKEN */

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
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "KampanjeMixMatch" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "KampanjeMixMatch" AND
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
        FOR EACH ELogg WHERE ELogg.TabellNavn = "KampanjeMixMatch" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:                                         
            CREATE TT_Elogg.                 
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

&IF DEFINED(EXCLUDE-SlettTT_ELoggKKamp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggKKamp Procedure 
PROCEDURE SlettTT_ELoggKKamp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = "KampanjeMixMatch" AND
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

