&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSTransInnles.p
    Purpose     : Innlesning av data til translogg

    Syntax      : xPRSTransloggInnles.p (lFilId, h_Parent, output iantLinjer).

    Description : Leser inn data i translogg. 

    Author(s)   : Tom Nøkleby
    Created     : 13/08/2013
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.

DEF VAR piLinjeNr AS INT  NO-UNDO.
DEF VAR pcLinje   AS CHAR NO-UNDO.
DEF VAR piAntFeil AS INT  NO-UNDO. 

DEFINE VARIABLE lDec  AS DECIMAL NO-UNDO.
DEFINE VARIABLE bOk   AS LOG NO-UNDO.
DEFINE VARIABLE iBatchNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iSeqNr   AS INTEGER NO-UNDO.
DEFINE VARIABLE iTransNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.

DEFINE BUFFER bufTransLogg FOR TransLogg.

DEF STREAM InnFil.

DEFINE BUFFER bTranslogg FOR Translogg.
DEFINE TEMP-TABLE ttTranslogg LIKE Translogg
  INDEX LevnrDef LevNr.

DEFINE TEMP-TABLE ttError
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
{windows.i}
{incl/devmode.i}
{incl/custdevmode.i}
{AssignRutiner.i}

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

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

  /* Oppretter batch som skal balangsere PLU'er og ikke lagerstyrte varer */
IF iBatchNr = 0 THEN 
   RUN batchlogg.p (PROGRAM-NAME(1),
              "PRSTransInnles " +
              string(TODAY) +
              " " +
              string(TIME,"HH:MM") +
              " " +
              USERID("dictdb"),
              OUTPUT iBatchNr).


RUN LesInnFil.
RUN PosterData.

RUN BatchStatus.p (iBatchNr, 2).

/* Stempler posten som innlest. */
DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    ASSIGN
        VPIFilHode.VPIFilStatus = 5
        .
END.
IF AVAILABLE VPIFilHode THEN
    FIND CURRENT VPIFilHode    NO-LOCK.

IF CAN-FIND(FIRST ttError) THEN
  RUN ErrorLogg.

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
DEFINE VARIABLE b2Ok AS LOG NO-UNDO.

DEFINE BUFFER bSelger FOR Selger.

  /* Tømmer feillogg. */
  FOR EACH ttError:
    DELETE ttError.
  END.

  RUN TellOppLinjer.
  RUN bibl_logg.p ('PRSTransImport', 'xPRSTransInnles.p: Leser inn fil: ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

  ASSIGN
      piLinjeNr  = 0
      pcLinje    = ''
      piAntFeil  = 0
      iAntLinjer = 0
      b2Ok       = TRUE 
      .
      
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .
    /* Skipper tomme linjer */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.
        
    /* Skipper overskriftslinje fra mal. */
    IF pcLinje BEGINS "1;2;3;4;5" THEN
        NEXT LESERLINJER.
    
    /* Skipper overskrift og linjer med feil. */
    ASSIGN lDec = DECIMAL(ENTRY(1,pcLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.

    /* Skipper linjer med for få entries. */
    IF NUM-ENTRIES(pcLinje,";") < 35 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Skal være minst 35 entries) " + STRING(iAntLinjer) + ": " + pcLinje
        .
        NEXT LESERLINJER.
    END.
    
    CREATE ttTranslogg.

    RUN AssignInt(1,OUTPUT ttTransLogg.Butik, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(2,OUTPUT ttTransLogg.ForsNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(3,OUTPUT ttTransLogg.TTId, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(4,OUTPUT ttTransLogg.TBId, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(5,OUTPUT ttTransLogg.Vg, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(6,OUTPUT ttTransLogg.LopNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(9,OUTPUT ttTransLogg.LevNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(10,OUTPUT ttTransLogg.BongId, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(11,OUTPUT ttTransLogg.BongLinjeNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(12,OUTPUT ttTransLogg.KassaNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(25,OUTPUT ttTransLogg.Tid, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(26,OUTPUT ttTransLogg.BestNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(27,OUTPUT ttTransLogg.OvButik, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(28,OUTPUT ttTransLogg.OvTransNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(32,OUTPUT ttTransLogg.KortType, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(34,OUTPUT ttTransLogg.ProfilNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(37,OUTPUT ttTransLogg.RefNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.

    RUN AssignDec(7,OUTPUT ttTransLogg.ArtikkelNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(14,OUTPUT ttTransLogg.Antall, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(15,OUTPUT ttTransLogg.Pris, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(16,OUTPUT ttTransLogg.Mva, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(17,OUTPUT ttTransLogg.RabKr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(18,OUTPUT ttTransLogg.Mva%, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(19,OUTPUT ttTransLogg.Varekost, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(20,OUTPUT ttTransLogg.VVarekost, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(22,OUTPUT ttTransLogg.KalkylePris, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(30,OUTPUT ttTransLogg.MedlemsNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(33,OUTPUT ttTransLogg.KundNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(35,OUTPUT ttTransLogg.SelgerNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
                        
    RUN AssignDate(24,OUTPUT ttTransLogg.Dato, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    
    ASSIGN 
      ttTransLogg.Plukket       = CAN-DO('Ja,Yes,J,1,True',ENTRY(23,pcLinje,';'))          
      ttTransLogg.SattVVareKost = CAN-DO('Ja,Yes,J,1,True',ENTRY(21,pcLinje,';'))      
      ttTransLogg.RefTekst      = ENTRY(38,pcLinje,';')              
      ttTransLogg.BongTekst     = ENTRY(8,pcLinje,';')              
      ttTransLogg.Storl         = ENTRY(13,pcLinje,';')
      ttTransLogg.KortNr        = ENTRY(31,pcLinje,';')
      ttTransLogg.Kode          = ENTRY(36,pcLinje,';')
      ttTransLogg.TilStorl      = ENTRY(29,pcLinje,';')           
    NO-ERROR.                
    
    /* Det har vært feil i en eller flere kolonner */
    IF b2Ok = FALSE THEN 
      NEXT LESERLINJER.

    run bibl_fixstorl.p (ttTransLogg.Storl,?,'',OUTPUT cTekst,OUTPUT bOk).
    run bibl_fixstorl.p (ttTransLogg.TilStorl,?,'',OUTPUT cTekst,OUTPUT bOk).
    IF ttTransLogg.Kode <> '' THEN
    DO: 
        ASSIGN 
            ttTransLogg.Kode = IF LENGTH(ttTransLogg.Kode) > 6 
                                 THEN FILL("0",13 - LENGTH(ttTransLogg.Kode)) + ttTransLogg.Kode
                                 ELSE ttTransLogg.Kode.
        RUN bibl_chkean.p (INPUT-OUTPUT ttTransLogg.Kode).
    END.
      
    IF ttTransLogg.SelgerNr > 0 AND NOT CAN-FIND(Selger WHERE 
                                                 Selger.SelgerNr = ttTransLogg.SelgerNr) THEN 
    DO FOR bSelger:  
      CREATE bSelger.
      bSelger.SelgerNr = ttTransLogg.SelgerNr.
      RELEASE bSelger.
    END.
    
    /* Setter artikkelNr. */
    IF ttTransLogg.ArtikkelNr = 0 THEN 
    DO:
      IF ttTranslogg.ArtikkelNr = 0 THEN 
      DO:
        FIND Strekkode NO-LOCK WHERE
          Strekkode.Kode = ttTransLogg.Kode NO-ERROR.
        IF AVAILABLE Strekkode THEN 
          ttTransLogg.ArtikkelNr = Strekkode.ArtikkelNr.
      END.
      IF ttTransLogg.LopNr > 0 THEN 
      DO:
        FIND ArtBas NO-LOCK WHERE
          ArtBas.Vg = ttTransLogg.Vg AND
          ArtBas.LopNr = ttTransLogg.LopNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
          ttTransLogg.ArtikkelNr = ArtBas.ArtikkelNr.
      END.
    END.

    STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-PosterData) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterData Procedure
PROCEDURE PosterData:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  
  iTransNr = 0.
  
  FOR EACH ttTranslogg TRANSACTION:
    
    /* Setter transaksjonsnummer  */
    IF iTransNr = 0 OR 
    CAN-FIND(FIRST TransLogg WHERE 
             TransLogg.Butik   = ttTranslogg.Butik AND 
             TransLogg.TransNr = iTransNr AND 
             TransLogg.SeqNr   = 1) THEN 
      DO:
        FIND LAST bufTransLogg where
          bufTransLogg.Butik = ttTransLogg.butik
          USE-INDEX TransLogg no-error.
        IF AVAILABLE bufTransLogg then
          iTransNr = bufTransLogg.TransNr + 1.
        ELSE
          iTransNr = 1.
      END.
    ELSE DO: 
      iTransNr = iTransNr + 1.
    END.
    
    CREATE Translogg.
    ASSIGN
      TransLogg.BatchNr     = iBatchNr
      TransLogg.TransNr     = iTransNr
      TransLogg.Butik       = ttTranslogg.Butik
      TransLogg.SeqNr       = 1
      TransLogg.Postert     = FALSE
      TransLogg.PostertDato = ?
      TransLogg.PostertTid  = 0
      .

    IF TRIM(ENTRY( 2,pcLinje,';')) <> '' THEN TransLogg.ForsNr        =  ttTranslogg.ForsNr.                 
    IF TRIM(ENTRY( 3,pcLinje,';')) <> '' THEN TransLogg.TTId          =  ttTranslogg.TTId.                   
    IF TRIM(ENTRY( 4,pcLinje,';')) <> '' THEN TransLogg.TBId          =  ttTranslogg.TBId.                   
    IF TRIM(ENTRY( 5,pcLinje,';')) <> '' THEN TransLogg.Vg            =  ttTranslogg.Vg.                     
    IF TRIM(ENTRY( 6,pcLinje,';')) <> '' THEN TransLogg.LopNr         =  ttTranslogg.LopNr.                  
    IF TRIM(ENTRY( 7,pcLinje,';')) <> '' THEN TransLogg.ArtikkelNr    =  ttTranslogg.ArtikkelNr.             
    IF TRIM(ENTRY( 8,pcLinje,';')) <> '' THEN TransLogg.BongTekst     =  ttTranslogg.BongTekst.              
    IF TRIM(ENTRY( 9,pcLinje,';')) <> '' THEN TransLogg.LevNr         =  ttTranslogg.LevNr.                  
    IF TRIM(ENTRY(10,pcLinje,';')) <> '' THEN TransLogg.BongId        =  ttTranslogg.BongId.                 
    IF TRIM(ENTRY(11,pcLinje,';')) <> '' THEN TransLogg.BongLinjeNr   =  ttTranslogg.BongLinjeNr.            
    IF TRIM(ENTRY(12,pcLinje,';')) <> '' THEN TransLogg.KassaNr       =  ttTranslogg.KassaNr.                
    IF TRIM(ENTRY(13,pcLinje,';')) <> '' THEN TransLogg.Storl         =  ttTranslogg.Storl.                  
    IF TRIM(ENTRY(14,pcLinje,';')) <> '' THEN TransLogg.Antall        =  ttTranslogg.Antall.                 
    IF TRIM(ENTRY(15,pcLinje,';')) <> '' THEN TransLogg.Pris          =  ttTranslogg.Pris.                   
    IF TRIM(ENTRY(16,pcLinje,';')) <> '' THEN TransLogg.Mva           =  ttTranslogg.Mva.                    
    IF TRIM(ENTRY(17,pcLinje,';')) <> '' THEN TransLogg.RabKr         =  ttTranslogg.RabKr.                  
    IF TRIM(ENTRY(18,pcLinje,';')) <> '' THEN TransLogg.Mva%          =  ttTranslogg.Mva%.                   
    IF TRIM(ENTRY(19,pcLinje,';')) <> '' THEN TransLogg.Varekost      =  ttTranslogg.VareKost.               
    IF TRIM(ENTRY(20,pcLinje,';')) <> '' THEN TransLogg.VVarekost     =  ttTranslogg.VVareKost.              
    IF TRIM(ENTRY(21,pcLinje,';')) <> '' THEN TransLogg.SattVVareKost =  ttTranslogg.SattVVareKost.          
    IF TRIM(ENTRY(22,pcLinje,';')) <> '' THEN TransLogg.KalkylePris   =  ttTranslogg.KalkylePris.            
    IF TRIM(ENTRY(23,pcLinje,';')) <> '' THEN TransLogg.Plukket       =  ttTranslogg.Plukket.                
    IF TRIM(ENTRY(24,pcLinje,';')) <> '' THEN TransLogg.Dato          =  ttTranslogg.Dato.                   
    IF TRIM(ENTRY(25,pcLinje,';')) <> '' THEN TransLogg.Tid           =  ttTranslogg.Tid.                    
    IF TRIM(ENTRY(26,pcLinje,';')) <> '' THEN TransLogg.BestNr        =  ttTranslogg.BestNr.                 
    IF TRIM(ENTRY(27,pcLinje,';')) <> '' THEN TransLogg.OvButik       =  ttTranslogg.OvButik.                
    IF TRIM(ENTRY(28,pcLinje,';')) <> '' THEN TransLogg.OvTransNr     =  ttTranslogg.OvTransNr.              
    IF TRIM(ENTRY(29,pcLinje,';')) <> '' THEN TransLogg.TilStorl      =  ttTranslogg.TilStorl.               
    IF TRIM(ENTRY(30,pcLinje,';')) <> '' THEN TransLogg.MedlemsNr     =  ttTranslogg.MedlemsNr.              
    IF TRIM(ENTRY(31,pcLinje,';')) <> '' THEN TransLogg.KortNr        =  ttTranslogg.KortNr.                 
    IF TRIM(ENTRY(32,pcLinje,';')) <> '' THEN TransLogg.KortType      =  ttTranslogg.KortType.               
    IF TRIM(ENTRY(33,pcLinje,';')) <> '' THEN TransLogg.KundNr        =  ttTranslogg.KundNr.                 
    IF TRIM(ENTRY(34,pcLinje,';')) <> '' THEN TransLogg.ProfilNr      =  ttTranslogg.ProfilNr.               
    IF TRIM(ENTRY(35,pcLinje,';')) <> '' THEN TransLogg.SelgerNr      =  ttTranslogg.SelgerNr.               
    IF TRIM(ENTRY(36,pcLinje,';')) <> '' THEN TransLogg.Kode          =  ttTranslogg.Kode.                   
    IF TRIM(ENTRY(37,pcLinje,';')) <> '' THEN TransLogg.RefNr         =  ttTranslogg.RefNr.                  
    IF TRIM(ENTRY(38,pcLinje,';')) <> '' THEN TransLogg.RefTekst      =  ttTranslogg.Reftekst.              
    
    RELEASE Translogg.
    
  END. /* TRANSACTION */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

