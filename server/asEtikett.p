&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

  DEFINE INPUT  PARAMETER iButikknr AS INTEGER     NO-UNDO.
  DEFINE INPUT  PARAMETER iType     AS INTEGER     NO-UNDO. /* 1=enkeltetiketter */
  DEFINE INPUT  PARAMETER TABLE-HANDLE hTable.
  DEFINE OUTPUT PARAMETER lOK       AS LOGICAL     NO-UNDO.

  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.
  DEFINE VARIABLE iX AS INTEGER NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    
  DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
    
  rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().
    
  {etikettlogg.i &NEW=NEW}

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

ASSIGN 
    bTest = TRUE
    cLogg = 'asEtikett' + REPLACE(STRING(TODAY),'/','') 
    .
    
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iButikknr NO-ERROR.
IF NOT AVAIL Butiker THEN
    RETURN.

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Start asEtikett.'
). 

rStandardFunksjoner:SkrivTilLogg(cLogg, 
       '   Kall på type: ' 
     + STRING(iType) + ' ' 
     + 'ButikkNr: ' + 
     STRING(iButikkNr)  
). 

CASE iType:
    WHEN 1 THEN DO:
        RUN EnkeltEtikett.
    END.
    WHEN 2 THEN DO:
        RUN PakkseddelEtikett.
    END.
END CASE.

lOK = TRUE.

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Slutt asEtikett.'
). 

CATCH zeroError AS Progress.Lang.AppError:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '** RFIDEtikettTilFil zeroError: ' + zeroError:GetMessage(1) 
        ).
END CATCH.
CATCH oneError AS Progress.Lang.SysError:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '** RFIDEtikettTilFil oneError: ' + oneError:GetMessage(1) 
        ).
END CATCH.                
CATCH twoError AS Progress.Lang.ProError:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '** RFIDEtikettTilFil twoError: ' + twoError:GetMessage(1) 
        ).
END CATCH.    


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EnkeltEtikett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnkeltEtikett Procedure 
PROCEDURE EnkeltEtikett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE hQuery     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBuffer    AS HANDLE      NO-UNDO.
DEFINE VARIABLE cStrekkode AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAntall    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSeq       AS INTEGER  INIT 1   NO-UNDO.
DEFINE        VARIABLE  cInfoRad1 AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cInfoRad2 AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cInfoRad3 AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cInfoRad4 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cNotatKodeTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSkriverType AS INTEGER NO-UNDO.

/* skapa query mot temp-tablen */
EMPTY TEMP-TABLE Etikettlogg.
hBuffer = hTable:DEFAULT-BUFFER-HANDLE.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    DO:
        cStrekkode = hBuffer:BUFFER-FIELD("strekkode"):BUFFER-VALUE.
        iAntall    = INT(hBuffer:BUFFER-FIELD("antall"):BUFFER-VALUE).
        cNotatKodeTekst = hBuffer:BUFFER-FIELD("NotatKodeTekst"):BUFFER-VALUE.
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '   cNotatKodeTekst: ' + cNotatKodeTekst +
            ' cStrekkode: ' + cStrekkode +
            ' iAntall: ' + STRING(iAntall)
        ). 

        /* Betyding av Notatkodetekst
        11 = Vanlig,Antall fra rad 
        12 = Vanlig,Antall på lager på EAN
        13 = Vanlig,Antall på lager Alle Ean
        21 = RFID,Antall fra rad 
        22 = RFID,Antall på lager på EAN
        23 = RFID,Antall på lager Alle Ean
        */
        ASSIGN iSkriverType = INT(cNotatKodeTekst) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            cNotatKodeTekst = ''.
        
        CASE cNotatKodeTekst:
            WHEN '11' THEN RUN etikett11 (cStrekkode, iAntall, FALSE, INPUT-OUTPUT iSeq).
            WHEN '12' THEN RUN etikett12 (cStrekkode, FALSE, INPUT-OUTPUT iSeq).
            WHEN '13' THEN RUN etikett13 (cStrekkode, FALSE, INPUT-OUTPUT iSeq).
            WHEN '21' THEN RUN etikett11 (cStrekkode, iAntall, TRUE, INPUT-OUTPUT iSeq).
            WHEN '22' THEN RUN etikett12 (cStrekkode, TRUE, INPUT-OUTPUT iSeq).
            WHEN '23' THEN RUN etikett13 (cStrekkode, TRUE, INPUT-OUTPUT iSeq).
            OTHERWISE RUN etikett11 (cStrekkode, iAntall, FALSE, INPUT-OUTPUT iSeq). 
        END CASE.            
    END.
    hQuery:GET-NEXT().
END.

IF CAN-FIND(FIRST EtikettLogg) THEN
DO: /* Oppretter START etikett. Den skrives ut sist. */
    ASSIGN cInfoRad1 = "Etiketter fra kasse"
           cInfoRad3 = IF AVAIL Butiker THEN Butiker.butnamn ELSE ""
           iSeq      = iSeq + 1
           cInfoRad4 = "START"
           .
    CREATE EtikettLogg.
    ASSIGN
      EtikettLogg.Butik     = iButikknr /* Det skal skrives ut i seqnr ordning. */
      EtikettLogg.Vg        = 0   
      EtikettLogg.LopNr     = 0
      EtikettLogg.Ant       = 0
      EtikettLogg.Storl     = "INFO"
      EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
      EtikettLogg.Pris      = 0
      EtikettLogg.Pris2     = 0
      EtikettLogg.SeqNr     = iSeq.
    /*
    ASSIGN iSeq      = 0
           cInfoRad4 = "SLUTT" + STRING(iSeq).
    CREATE EtikettLogg.
    ASSIGN
    EtikettLogg.Butik     = iButikknr /* Det skal skrives ut i seqnr ordning. */
    EtikettLogg.Vg        = 0   
    EtikettLogg.LopNr     = 0
    EtikettLogg.Ant       = 0
    EtikettLogg.Storl     = "INFO"
    EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
    EtikettLogg.Pris      = 0
    EtikettLogg.Pris2     = 0
    EtikettLogg.SeqNr     = iSeq.
    */

    /* Starter etikettutskrift. */
    hQuery:QUERY-CLOSE().
    DELETE OBJECT hQuery.

    IF Butiker.BELayout < 90 THEN
        RUN x-etikettstd.w (Butiker.BELayout,Butiker.BEPrinter,Butiker.BETerminalklient).
    ELSE DO:
        IF iSkriverType > 20 THEN
        DO:  
            rStandardFunksjoner:SkrivTilLogg(cLogg, 
                '   Kaller x-etikettstd.w. ' + STRING(ERROR-STATUS:ERROR) 
            ). 
            RUN x-etikettstd.w (Butiker.BELayout,Butiker.BEPrinter,Butiker.BETerminalklient) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
                    cTekst = STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' '+ 
                             ERROR-STATUS:GET-MESSAGE(ix). 
                    rStandardFunksjoner:SkrivTilLogg(cLogg, 
                        '   ** Feil: ' + cTekst 
                    ). 
                END.
                IF ctekst <> '' THEN 
                    MESSAGE 'Feil:' cTekst.    
                
            END.
            rStandardFunksjoner:SkrivTilLogg(cLogg, 
                '   Etter x-etikettstd.w. ' + STRING(ERROR-STATUS:ERROR) 
            ). 
        END.
        ELSE DO:
            FIND SysPara NO-LOCK WHERE
                SysPara.SysHId = 5 AND
                SysPara.SysGr  = 20 AND
                SysPara.ParaNr = Butiker.BELayout NO-ERROR.
            IF AVAILABLE SysPara THEN 
                RUN x-etikettstd.w (INT(SysPara.Parameter1),Butiker.BEPrinter,Butiker.BETerminalklient) NO-ERROR.
        END.
    END.  
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '   ENKEL Antall etiketter skrevet: ' 
            + STRING(iAntall) + '.'
    ). 
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etikett11) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etikett11 Procedure
PROCEDURE etikett11:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: 11 = Vanlig,Antall fra rad 
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcStrekkode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER piAntall AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pbRFID AS LOG NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER piSeq AS INTEGER NO-UNDO.

    FIND strekkode WHERE strekkode.kode = pcStrekkode NO-LOCK NO-ERROR.
    IF AVAIL strekkode AND piantall > 0 THEN DO:
        FIND artbas WHERE artbas.artikkelnr = strekkode.artikkelnr NO-LOCK NO-ERROR.
        IF AVAIL artbas THEN DO:
            FIND FIRST ArtPris OF ArtBas WHERE 
                ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN
                FIND FIRST ArtPris NO-LOCK NO-ERROR.
            IF AVAIL ArtPris THEN DO:
                ASSIGN piSeq = piSeq + 1.
                CREATE EtikettLogg.
                ASSIGN EtikettLogg.Butik     = iButikknr
                       EtikettLogg.Vg        = ArtBas.Vg
                       EtikettLogg.LopNr     = ArtBas.LopNr
                       EtikettLogg.Ant       = piAntall
                       EtikettLogg.Storl     = pcStrekKode
                       EtikettLogg.Bongtekst = ArtBas.BongTekst
                       EtikettLogg.Pris      = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                       EtikettLogg.Pris2     = ArtBas.AnbefaltPris
                       EtikettLogg.SeqNr     = piSeq
                       EtikettLogg.individ   = (IF pbRFID THEN 1 ELSE 0)
                       .
            END.
        END.
    END.

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-etikett12) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etikett12 Procedure
PROCEDURE etikett12:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: 12 = Vanlig,Antall på lager på EAN
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcStrekkode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pbRFID AS LOG NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER piSeq AS INTEGER NO-UNDO.

    FIND strekkode WHERE strekkode.kode = pcStrekkode NO-LOCK NO-ERROR.
    
    IF AVAIL strekkode THEN 
    DO:
        FIND artbas WHERE artbas.artikkelnr = strekkode.artikkelnr NO-LOCK NO-ERROR.        
        IF AVAIL artbas THEN DO:
            FIND FIRST ArtLag NO-LOCK WHERE 
                ArtLag.ArtikkelNr = StrekKode.ArtikkelNr AND
                ArtLag.butik      = iButikkNr AND 
                ArtLag.StrKode    = StrekKode.StrKode NO-ERROR.
            IF AVAILABLE ArtLag AND ArtLag.lagant > 0 THEN 
            DO:
                FIND FIRST ArtPris OF ArtBas WHERE 
                    ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ArtPris THEN
                    FIND FIRST ArtPris NO-LOCK NO-ERROR.
                IF AVAIL ArtPris THEN DO:
                    ASSIGN piSeq = piSeq + 1.
                    CREATE EtikettLogg.
                    ASSIGN EtikettLogg.Butik     = iButikknr
                           EtikettLogg.Vg        = ArtBas.Vg
                           EtikettLogg.LopNr     = ArtBas.LopNr
                           EtikettLogg.Ant       = ArtLag.Lagant
                           EtikettLogg.Storl     = pcStrekKode
                           EtikettLogg.Bongtekst = ArtBas.BongTekst
                           EtikettLogg.Pris      = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                           EtikettLogg.Pris2     = ArtBas.AnbefaltPris
                           EtikettLogg.SeqNr     = piSeq
                           EtikettLogg.individ   = (IF pbRFID THEN 1 ELSE 0)
                           .
                END.
            END.
        END.
    END.

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-etikett13) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etikett13 Procedure
PROCEDURE etikett13:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: 13 = Vanlig,Antall på lager Alle Ean
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcStrekkode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pbRFID AS LOG NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER piSeq AS INTEGER NO-UNDO.

    DEFINE VARIABLE plArtikkelNr AS DECIMAL NO-UNDO.
    
    FIND strekkode WHERE strekkode.kode = pcStrekkode NO-LOCK NO-ERROR.
    
    IF AVAIL strekkode THEN 
    DO:
        plArtikkelNr = StrekKode.ArtikkelNr.
        FIND artbas WHERE artbas.artikkelnr = strekkode.artikkelnr NO-LOCK NO-ERROR.        
        IF AVAIL artbas THEN DO:
            FIND FIRST ArtPris OF ArtBas WHERE 
                ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN
                FIND FIRST ArtPris NO-LOCK NO-ERROR.
            IF AVAILABLE ArtPris THEN 
            FOR EACH ArtLag NO-LOCK WHERE 
                ArtLag.ArtikkelNr = plArtikkelNr AND
                ArtLag.butik      = iButikkNr AND 
                ArtLag.lagant > 0:
                FIND LAST StrekKode NO-LOCK WHERE 
                    StrekKode.ArtikkelNr = ArtLag.ArtikkelNr AND 
                    StrekKode.StrKode = ArtLag.StrKode NO-ERROR.
                IF AVAILABLE StrekKode THEN 
                DO:
                    IF AVAIL ArtPris THEN DO:
                        ASSIGN piSeq = piSeq + 1.
                        CREATE EtikettLogg.
                        ASSIGN EtikettLogg.Butik     = iButikknr
                               EtikettLogg.Vg        = ArtBas.Vg
                               EtikettLogg.LopNr     = ArtBas.LopNr
                               EtikettLogg.Ant       = ArtLag.Lagant
                               EtikettLogg.Storl     = StrekKode.Kode
                               EtikettLogg.Bongtekst = ArtBas.BongTekst
                               EtikettLogg.Pris      = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                               EtikettLogg.Pris2     = ArtBas.AnbefaltPris
                               EtikettLogg.SeqNr     = piSeq
                               EtikettLogg.individ   = (IF pbRFID THEN 1 ELSE 0)
                               .
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-PakkseddelEtikett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PakkseddelEtikett Procedure 
PROCEDURE PakkseddelEtikett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hQuery     AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hBuffer    AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hBuffer2 AS HANDLE      NO-UNDO.
  DEFINE VARIABLE cStrekkode AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAntall    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE hTable2 AS HANDLE      NO-UNDO.
  DEFINE VARIABLE iSeq       AS INTEGER  INIT 1   NO-UNDO.
  DEFINE        VARIABLE  cInfoRad1 AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cInfoRad2 AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cInfoRad3 AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cInfoRad4 AS CHARACTER  NO-UNDO.
  /* skapa query mot temp-tablen */
  hBuffer = hTable:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      CREATE EtikettLogg.
      ASSIGN Etikettlogg.Vg        = hBuffer:BUFFER-FIELD("vg"):BUFFER-VALUE
             Etikettlogg.LopNr     = hBuffer:BUFFER-FIELD("LopNr"):BUFFER-VALUE
             Etikettlogg.Ant       = hBuffer:BUFFER-FIELD("Ant"):BUFFER-VALUE
             Etikettlogg.Storl     = hBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE
             Etikettlogg.bongtekst = hBuffer:BUFFER-FIELD("bongtekst"):BUFFER-VALUE
             Etikettlogg.pris      = hBuffer:BUFFER-FIELD("pris"):BUFFER-VALUE
             Etikettlogg.pris2     = hBuffer:BUFFER-FIELD("pris2"):BUFFER-VALUE
             Etikettlogg.individ   = hBuffer:BUFFER-FIELD("individ"):BUFFER-VALUE
             Etikettlogg.butik     = hBuffer:BUFFER-FIELD("butik"):BUFFER-VALUE
             Etikettlogg.SeqNr     = hBuffer:BUFFER-FIELD("SeqNr"):BUFFER-VALUE
             iAntall               = iAntall + 1
             .
      RELEASE EtikettLogg.
      hQuery:GET-NEXT().
  END.
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery.
  
  RUN x-etikettstd.w (Butiker.BELayout,Butiker.BEPrinter,Butiker.BETerminalklient).
  
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '   PAKKSEDDEL Antall etiketter skrevet: ' 
            + STRING(iAntall) + '.'
    ). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

