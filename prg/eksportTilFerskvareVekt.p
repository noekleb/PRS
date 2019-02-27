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

DEFINE VARIABLE cHKinst AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE TT_FerskvareVekt NO-UNDO 
    FIELD RecType AS CHARACTER FORMAT "x(5)"
    FIELD ArtikkelNr AS CHARACTER FORMAT "x(20)"
    FIELD ArtStatus AS CHARACTER FORMAT "x(5)"
    FIELD PrisKr AS CHARACTER FORMAT "x(15)"
    FIELD EkstraPris AS CHARACTER FORMAT "x(15)"
    FIELD Varetekst AS CHARACTER FORMAT "x(40)"
    INDEX Artikkel ArtikkelNr
    INDEX RecordType RecType ArtikkelNr.

DEF VAR cTekst          AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-CanFindTTElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CanFindTTElogg Procedure 
FUNCTION CanFindTTElogg RETURNS LOGICAL
  ( INPUT cTabell AS CHARACTER,INPUT cVerdi AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FinnsElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FinnsElogg Procedure 
FUNCTION FinnsElogg RETURNS LOGICAL
  ( INPUT cTabellNavn AS CHARACTER )  FORWARD.

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
         HEIGHT             = 27.76
         WIDTH              = 63.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
  
  RUN StartEksport.

  RUN SlettBehandlet.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */
&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iMaks       AS INTEGER NO-UNDO.  
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntSlett   AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.
  
  DEFINE VARIABLE iMaksAntPrGang AS INTEGER NO-UNDO.

  DEFINE BUFFER bufELogg FOR ELogg.

  /* For å unngå å skape for store pos filer, tar vi dem i små porsjoner.         */
  /* Ref. eksport av alle Antons statistikker som selvfølgelig feilet ved import på hk. */
  ELOGGLOOP:
  FOR EACH bufELogg NO-LOCK WHERE 
      bufELogg.TabellNavn     = cTabellNavn AND 
      bufELogg.EksterntSystem = "FVEKT" AND 
      bufELogg.Behandlet      = FALSE:

      IF iMaks > 0 THEN DO:
          iMaksAntPrGang = iMaksAntPrGang + 1.
          IF iMaksAntPrGang > 1000 THEN 
              LEAVE ELOGGLOOP.
      END.

      FIND ELogg EXCLUSIVE-LOCK WHERE
        RECID(ELogg) = RECId(bufELogg) NO-ERROR NO-WAIT.
      IF NOT AVAILABLE ELogg OR LOCKED ELogg THEN 
        NEXT ELOGGLOOP.

      CREATE TT_Elogg.
      BUFFER-COPY Elogg TO TT_Elogg.
      RELEASE TT_Elogg.
      ASSIGN Elogg.Behandlet = TRUE
             iAntSlett   = iAntSlett   + IF Elogg.EndringsType = 3 THEN 1 ELSE 0
             iAntNyEndre = iAntNyEndre + IF Elogg.EndringsType = 1 AND ELogg.Verdier <> "ALLE" THEN 1 ELSE 0.
  END. /* ELOGGLOOP */
  
  FIND FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND 
                              TT_ELogg.EksterntSystem = "FVEKT" AND
                              TT_ELogg.Verdier = "ALLE" NO-ERROR.
  IF AVAIL TT_ELogg THEN DO:
      DELETE TT_ELogg.
      RUN SkapaTTELoggAlle(INPUT-OUTPUT iAntNyEndre,cTabellNavn).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-NyTTElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyTTElogg Procedure 
PROCEDURE NyTTElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabell AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cVerdi  AS CHARACTER  NO-UNDO.
    CREATE TT_Elogg.
    ASSIGN TT_ELogg.TabellNavn     = cTabell
           TT_ELogg.EksterntSystem = "FVEKT"   
           TT_ELogg.Verdier        = cVerdi
           TT_ELogg.EndringsType   = 1
           TT_ELogg.Behandlet      = FALSE.
    RELEASE TT_ELogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-OpprettFerskvareVekt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettFerskvareVekt Procedure 
PROCEDURE OpprettFerskvareVekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    
    
  DEFINE TEMP-TABLE TT_FerskvareVekt NO-UNDO 
    FIELD RecType AS CHARACTER FORMAT "x(5)"
    FIELD ArtikkelNr AS CHARACTER FORMAT "x(20)"
    FIELD ArtStatus AS CHARACTER FORMAT "x(5)"
    FIELD PrisKr AS CHARACTER FORMAT "x(15)"
    FIELD EkstraPris AS CHARACTER FORMAT "x(15)"
    FIELD Varetekst AS CHARACTER FORMAT "x(40)"
    INDEX Artikkel ArtikkelNr
    INDEX RecordType RecType ArtikkelNr.
       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "ArtBas" NO-UNDO.
    
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
 
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE 
      TT_ELogg.TabellNavn     = cTabellNavn AND 
      TT_ELogg.EksterntSystem = "FVEKT" AND 
      TT_Elogg.EndringsType   = 3:
      
      ASSIGN iAntSlett = iAntSlett + 1.
    END.
    
    /* Legger ut sletteposter */    
    IF iAntSlett > 0 THEN 
    DO:
        FOR EACH TT_ELogg WHERE 
          TT_ELogg.TabellNavn     = cTabellNavn AND 
          TT_ELogg.EksterntSystem = "FVEKT" AND 
          TT_Elogg.EndringsType   = 3:
            
          IF LENGTH(ENTRY(2,TT_Elogg.Verdier,CHR(1))) = 13 THEN 
          DO:
              CREATE TT_FerskvareVekt.
              ASSIGN 
                TT_FerskvareVekt.RecType    = '0' /* Sletting */
                TT_FerskvareVekt.ArtikkelNr = SUBSTRING(ENTRY(2,TT_Elogg.Verdier,CHR(1)),3,6)
                TT_FerskvareVekt.ArtStatus  = '0'
                TT_FerskvareVekt.PrisKr     = '0'
                TT_FerskvareVekt.EkstraPris = '0'
                TT_FerskvareVekt.Varetekst  = 'Skal slettes'
                .
              /*
              PUT UNFORMATTED  
                TT_FerskvareVekt.RecType    CHR(9)
                TT_FerskvareVekt.ArtikkelNr CHR(9)
                TT_FerskvareVekt.ArtStatus  CHR(9)
                TT_FerskvareVekt.PrisKr     CHR(9)
                TT_FerskvareVekt.EkstraPris CHR(9)
                TT_FerskvareVekt.Varetekst  
              SKIP.
              */
          END.
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE 
        TT_ELogg.TabellNavn = cTabellNavn AND 
        TT_ELogg.EksterntSystem = "FVEKT" AND 
        TT_Elogg.EndringsType = 1 
        BY verdier:
        
        ASSIGN iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN 
    DO:
        FOR EACH TT_ELogg WHERE 
          TT_ELogg.TabellNavn     = cTabellNavn AND 
          TT_ELogg.EksterntSystem = "FVEKT" AND 
          TT_Elogg.EndringsType   = 1 
          BY verdier:
          
          FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = DECIMAL(ENTRY(1,TT_Elogg.Verdier,CHR(1))) NO-ERROR.
          IF AVAILABLE ArtBas THEN 
            FIND FIRST ArtPris OF ArtBas NO-ERROR.
          IF AVAILABLE ArtBas AND 
             AVAILABLE ArtPris AND 
             LENGTH(ENTRY(2,TT_Elogg.Verdier,CHR(1))) = 13 THEN 
          DO:
              CREATE TT_FerskvareVekt.
              ASSIGN 
                TT_FerskvareVekt.RecType    = '1' /* Sletting */
                TT_FerskvareVekt.ArtikkelNr = SUBSTRING(ENTRY(2,TT_Elogg.Verdier,CHR(1)),3,6)
                TT_FerskvareVekt.ArtStatus  = '0'
                TT_FerskvareVekt.PrisKr     = REPLACE(REPLACE(STRING(ArtPris.Pris[1]),',',''),'.','')
                TT_FerskvareVekt.EkstraPris = IF ArtPris.Tilbud = FALSE THEN '' ELSE REPLACE(REPLACE(STRING(ArtPris.Pris[2]),',',''),'.','')
                TT_FerskvareVekt.Varetekst  = REPLACE(ArtBas.Beskr,CHR(9),'')
                .
              /*
              PUT UNFORMATTED  
                TT_FerskvareVekt.RecType    CHR(9)
                TT_FerskvareVekt.ArtikkelNr CHR(9)
                TT_FerskvareVekt.ArtStatus  CHR(9)
                TT_FerskvareVekt.PrisKr     CHR(9)
                TT_FerskvareVekt.EkstraPris CHR(9)
                TT_FerskvareVekt.Varetekst  
              SKIP.
              */
          END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-SkapaTTELoggAlle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTTELoggAlle Procedure 
PROCEDURE SkapaTTELoggAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:      iAntNyEndre har ett värde med sig 
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER cTabell AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVerdi AS CHARACTER  NO-UNDO.
  CASE cTabell:
      WHEN "FerskVareVekt" THEN DO:
          FOR EACH ArtBas NO-LOCK WHERE 
            ArtBas.OPris   = TRUE AND 
            ArtBas.ArtSLag = 1:
              ASSIGN cVerdi = STRING(ArtBas.ArtikkelNr).
              IF NOT CanFindTTElogg('FVEKT',cVerdi) THEN DO:
                  RUN NyTTElogg('FVEKT',cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettBehandlet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBehandlet Procedure 
PROCEDURE SlettBehandlet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Elogg WHERE ELogg.EksterntSystem = "FVEKT" AND
                         Elogg.Behandlet = TRUE:
        DELETE Elogg.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartEksport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksport Procedure 
PROCEDURE StartEksport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFilnavn           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNumericFormat     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDateFormat        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTmpFil            AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCL                AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSendesFil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKatalog           AS CHAR       NO-UNDO.
  DEFINE VARIABLE cKat1              AS CHARACTER INITIAL 'FVEKT' NO-UNDO.
  DEFINE VARIABLE iSeqNr             AS INTEGER    NO-UNDO.
  
  DEFINE BUFFER bufSysPara FOR SysPara.

  ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
         cDateFormat            = SESSION:DATE-FORMAT
         SESSION:NUMERIC-FORMAT = "EUROPEAN"
         SESSION:DATE-FORMAT    = "dmy".
         
  {syspara.i 1 1 51 cKatalog}
  IF cKatalog = '' THEN
      cKatalog = "C:\home\lindbak\sendes\".
  cKatalog = RIGHT-TRIM(cKatalog,'\').
  cKatalog = cKatalog + '\' + cKat1.
  
  EMPTY TEMP-TABLE TT_Elogg.
  EMPTY TEMP-TABLE TT_FerskvareVekt.

  /* Oppretter kataloger. */
  OS-COMMAND SILENT VALUE("md " + cKatalog).

  IF FinnsElogg("ArtBas") THEN                                                           
      RUN OpprettFerskvareVekt.                                                                    

  MD_KATALOG:
  DO FOR bufSysPara:
  FOR EACH SysPara NO-LOCK WHERE 
    SysPara.SysHId = 23 AND 
    SysPara.SysGr = 1 AND 
    SysPara.Parameter1 > '0':
      
    iSeqNr = INT(SysPara.Parameter2).
    iSeqNr = iSeqNr + 1.
    /* Oppretter kataloger. */
    OS-COMMAND SILENT VALUE("md " + cKatalog + '\' + STRING(SysPara.ParaNr)).

    ASSIGN cFilnavn   = "DELI" + STRING(iSeqNr) + ".dat"
           cTmpFil    = SESSION:TEMP-DIRECTORY + cFilNavn
           cSendesFil = cKatalog + '\' + STRING(SysPara.ParaNr) + '\' + cFilNavn.
           
    OUTPUT TO VALUE(cTmpFil).
    DO:
      FOR EACH TT_FerskVareVekt:
        PUT UNFORMATTED  
                TT_FerskvareVekt.RecType    CHR(9)
                TT_FerskvareVekt.ArtikkelNr CHR(9)
                TT_FerskvareVekt.ArtStatus  CHR(9)
                TT_FerskvareVekt.PrisKr     CHR(9)
                TT_FerskvareVekt.EkstraPris CHR(9)
                TT_FerskvareVekt.Varetekst  
        SKIP.
      END.
    END.
    OUTPUT CLOSE.
  
    DO TRANSACTION:
      FIND bufSysPara EXCLUSIVE-LOCK WHERE 
        bufSysPara.SysHId = 23 AND 
        bufSysPara.SysGr = 1 AND 
        bufSysPara.ParaNr = SysPara.ParaNr NO-ERROR.
      IF AVAILABLE bufSysPara THEN
        DO:
          ASSIGN bufSysPara.Parameter2 = STRING(iSeqNr).
          RELEASE bufSysPara.
        END. 
    END.
    
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
    FILE-INFO:FILE-NAME = cTmpFil.
    IF FILE-INFO:FILE-SIZE > 0 THEN DO:
        OS-COPY VALUE(cTmpFil) VALUE(cSendesFil).
    END.
    OS-DELETE VALUE(cTmpFil).
  END. /* MD_KATALOG */
  END. /* bufSysPara */  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-CanFindTTElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CanFindTTElogg Procedure 
FUNCTION CanFindTTElogg RETURNS LOGICAL
  ( INPUT cTabell AS CHARACTER,INPUT cVerdi AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabell AND
                                    TT_ELogg.EksterntSystem = "FVEKT" AND
                                    TT_ELogg.Verdier = cVerdi).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FinnsElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FinnsElogg Procedure 
FUNCTION FinnsElogg RETURNS LOGICAL
  ( INPUT cTabellNavn AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = cTabellNavn AND
                                    ELogg.EksterntSystem = "FVEKT").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

