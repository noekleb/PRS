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

DEFINE INPUT  PARAMETER ipiNr      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiLayoutNr     AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrintername  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplTermKlient   AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER cType           AS CHARACTER  NO-UNDO.

DEFINE BUFFER bufButiker FOR Butiker.

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

CASE cType:
    WHEN "TELLING" THEN DO:
        FIND TelleHode WHERE TelleHode.TelleNr = ipiNr NO-LOCK NO-ERROR.
        IF NOT AVAIL TelleHode THEN
            RETURN "AVBRYT".
        FIND FIRST TelleLinje OF TelleHode NO-LOCK NO-ERROR.
        IF NOT AVAIL TelleLinje THEN
            RETURN "AVBRYT".
        FIND butiker WHERE butiker.butik = INT(TelleHode.ButikkListe) NO-LOCK NO-ERROR.
        RUN SkapaEtikettLoggTelling.
    END.
    WHEN "VAREMOTTAK" THEN DO:
        FIND BatchLogg WHERE BatchLogg.BatchNr = ipiNr NO-LOCK NO-ERROR.
        IF NOT AVAIL BatchLogg THEN
            RETURN "AVBRYT".
        FIND FIRST TransLogg OF BatchLogg NO-LOCK NO-ERROR.
        IF NOT AVAIL TransLogg THEN
            RETURN "AVBRYT".
        IF Translogg.TTid <> 5 THEN
            RETURN "AVBRYT".
        RUN SkapaEtikettLoggBatch.
    END.
END CASE.


IF CAN-FIND(FIRST EtikettLogg) THEN DO:
    IF ipcPrintername = "" THEN DO:
        RUN VelgPrinter (OUTPUT ipcPrintername).
        IF ipcPrintername = "" THEN
            RETURN.
/*         IF NUM-ENTRIES(ipcPrintername,"\") > 1 THEN */
/*             iplTermKlient = TRUE.                   */
    END.
    RUN x-etikettstd.w (ipiLayoutNr,ipcPrintername,iplTermKlient).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-SkapaEtikettLoggBatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaEtikettLoggBatch Procedure 
PROCEDURE SkapaEtikettLoggBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iSeq AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  cInfoRad1 AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cInfoRad2 AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cInfoRad3 AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cInfoRad4 AS CHARACTER  NO-UNDO.
    ASSIGN cInfoRad1 = BatchLogg.Beskrivelse
           cInfoRad3 = IF AVAIL Butiker THEN Butiker.butnamn ELSE ""
           cInfoRad4 = "SLUTT"
           iSeq      = iSeq + 1.
    CREATE EtikettLogg.
    ASSIGN
    EtikettLogg.Butik     = iSeq /* Det skal skrives ut i seqnr ordning. */
    EtikettLogg.Vg        = 0   
    EtikettLogg.LopNr     = 0
    EtikettLogg.Ant       = 0
    EtikettLogg.Storl     = "INFO"
    EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
    EtikettLogg.Pris      = 0
    EtikettLogg.Pris2     = 0
    EtikettLogg.SeqNr     = iSeq.
    
    FOR EACH Translogg OF BatchLogg NO-LOCK:
        IF TransLogg.ArtikkelNr = 0 THEN
            NEXT.
        IF NOT CAN-FIND(Strekkode WHERE StrekKode.Kode = TransLogg.Kode) THEN
            NEXT.
        FIND ArtBas WHERE Artbas.ArtikkelNr = Translogg.ArtikkelNr NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtBas THEN
            NEXT.
        IF ArtBas.Etikett = 0 THEN
            NEXT.
        FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtPris THEN
            NEXT.
/*         IF Translogg.Kode = "" THEN DO:                                                                        */
/*             FIND StrKonv WHERE StrKonv.Storl = Translogg.Storl NO-LOCK NO-ERROR.                               */
/*             IF AVAIL StrKonv THEN DO:                                                                          */
/*                 FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND                                */
/*                                                      StrekKode.StrKode = StrKonv.StrKode AND                   */
/*                                                      NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.          */
/*                 IF NOT AVAIL StrekKode THEN                                                                    */
/*                     FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND                            */
/*                                                          StrekKode.StrKode = StrKonv.StrKode NO-LOCK NO-ERROR. */
/*             END.                                                                                               */
/*             IF NOT AVAIL Strekkode THEN                                                                        */
/*                 NEXT.                                                                                          */
/*         END.                                                                                                   */
        ASSIGN iSeq = iSeq + 1.
        CREATE EtikettLogg.
        ASSIGN EtikettLogg.Butik     = iSeq
               EtikettLogg.Vg        = ArtBas.Vg
               EtikettLogg.LopNr     = ArtBas.LopNr
               EtikettLogg.Ant       = Translogg.Antall
               EtikettLogg.Storl     = Translogg.Kode
               EtikettLogg.Bongtekst = ArtBas.BongTekst
               EtikettLogg.Pris      = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
               EtikettLogg.SeqNr     = iSeq.
        RELEASE StrekKode.
    END.
    ASSIGN cInfoRad4 = "START"
           iSeq      = iSeq + 1.
    CREATE EtikettLogg.
    ASSIGN
      EtikettLogg.Butik     = iSeq /* Det skal skrives ut i seqnr ordning. */
      EtikettLogg.Vg        = 0   
      EtikettLogg.LopNr     = 0
      EtikettLogg.Ant       = 0
      EtikettLogg.Storl     = "INFO"
      EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
      EtikettLogg.Pris      = 0
      EtikettLogg.Pris2     = 0
      EtikettLogg.SeqNr     = iSeq.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaEtikettLoggTelling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaEtikettLoggTelling Procedure 
PROCEDURE SkapaEtikettLoggTelling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iSeq AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  cInfoRad1 AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cInfoRad2 AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cInfoRad3 AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cInfoRad4 AS CHARACTER  NO-UNDO.
    ASSIGN cInfoRad1 = TelleHode.Beskrivelse
           cInfoRad3 = IF AVAIL Butiker THEN Butiker.butnamn ELSE ""
           cInfoRad4 = "SLUTT"
           iSeq      = iSeq + 1.
    CREATE EtikettLogg.
    ASSIGN
    EtikettLogg.Butik     = INT(TelleHode.ButikkListe)
    EtikettLogg.Vg        = 0   
    EtikettLogg.LopNr     = 0
    EtikettLogg.Ant       = 0
    EtikettLogg.Storl     = "INFO"
    EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
    EtikettLogg.Pris      = 0
    EtikettLogg.Pris2     = 0
    EtikettLogg.SeqNr     = iSeq.
    
    FIND bufButiker NO-LOCK WHERE 
      bufButiker.Butik = int(TelleHode.ButikkListe) NO-ERROR.
    
    FOR EACH TelleLinje OF TelleHode NO-LOCK
        BY TelleLinje.Artikkelnr BY TelleLinje.SeqNr:
        IF TelleLinje.AntallTalt = 0 THEN
            NEXT.
        FIND ArtBas WHERE Artbas.ArtikkelNr = TelleLinje.ArtikkelNr NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtBas THEN
            NEXT.
        IF ArtBas.Etikett = 0 THEN
            NEXT.
        IF AVAILABLE ArtPris THEN RELEASE ArtPris.
        IF AVAILABLE bufButiker THEN 
          FIND ArtPris OF ArtBas NO-LOCK WHERE
            ArtPris.ProfilNr = bufButiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtPris THEN
            NEXT.
        IF TelleLinje.Kode = "" THEN DO:
            FIND StrKonv WHERE StrKonv.Storl = TelleLinje.Storl NO-LOCK NO-ERROR.
            IF AVAIL StrKonv THEN DO:
                FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                                     StrekKode.StrKode = StrKonv.StrKode AND
                                                     NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
                IF NOT AVAIL StrekKode THEN
                    FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                                         StrekKode.StrKode = StrKonv.StrKode NO-LOCK NO-ERROR.
            END.
            IF NOT AVAIL Strekkode THEN
                NEXT.
        END.
        ASSIGN iSeq = iSeq + 1.
        CREATE EtikettLogg.
        ASSIGN EtikettLogg.Butik     = TelleLinje.Butik
               EtikettLogg.Vg        = TelleLinje.Vg
               EtikettLogg.LopNr     = TelleLinje.LopNr
               EtikettLogg.Ant       = TelleLinje.AntallTalt
               EtikettLogg.Storl     = IF AVAIL StrekKode THEN StrekKode.Kode ELSE TelleLinje.Kode
               EtikettLogg.Bongtekst = ArtBas.BongTekst
               EtikettLogg.Pris      = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
               EtikettLogg.Pris2     = IF ArtPris.Tilbud THEN ArtPris.Pris[1] ELSE 0
               EtikettLogg.SeqNr     = iSeq.
        RELEASE StrekKode.
    END.
    ASSIGN cInfoRad4 = "START"
           iSeq      = iSeq + 1.
    CREATE EtikettLogg.
    ASSIGN
      EtikettLogg.Butik     = INT(TelleHode.Butik)
      EtikettLogg.Vg        = 0   
      EtikettLogg.LopNr     = 0
      EtikettLogg.Ant       = 0
      EtikettLogg.Storl     = "INFO"
      EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
      EtikettLogg.Pris      = 0
      EtikettLogg.Pris2     = 0
      EtikettLogg.SeqNr     = iSeq.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VelgPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VelgPrinter Procedure 
PROCEDURE VelgPrinter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER cValgtPrinter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValgtVerdi AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrinter      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount           AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrinterNames AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-1:
      DO iCount = 1 TO NUM-ENTRIES(SESSION:GET-PRINTERS()):
          ASSIGN cPrinter = ENTRY(iCount,SESSION:GET-PRINTERS())
                 cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                    cPrinter + "," + STRING(iCount).
                 IF AVAIL butiker AND cPrinter = Butiker.BEPrinter THEN
                     cValgtVerdi = STRING(iCount).
      END.
      RUN d-VelgGenerellCombo.w ("Velg printer",cListItemPairs,INPUT-OUTPUT cValgtVerdi).
      IF INT(cValgtVerdi) = 0 THEN
          RETURN.
      cValgtPrinter = ENTRY(INT(cValgtVerdi),SESSION:GET-PRINTERS()).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

