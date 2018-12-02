&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSGavTilInnles.p
    Purpose     : Innlesning av data til tilgodelapper og gavekort.

    Syntax      : xPRSGavTilInnles.p (lFilId, h_Parent, output iantLinjer).

    Description : Leser inn data i gavekort og tilgode. Setter opp standardverdier. 

    Author(s)   : Tom Nøkleby
    Created     : 14/08/2013
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
DEFINE VARIABLE cStr  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE bOk   AS LOG NO-UNDO.

DEF STREAM InnFil.

DEFINE TEMP-TABLE ttGT
   /*  1 */ FIELD ID AS CHARACTER 
   /*  2 */ FIELD MedlemsNr AS CHARACTER 
   /*  3 */ FIELD KundeNr AS CHARACTER 
   /*  4 */ FIELD Navn AS CHARACTER 
   /*  5 */ FIELD Telefon AS CHARACTER 
   /*  6 */ FIELD GyldigFraDato AS CHARACTER 
   /*  7 */ FIELD GyldigTilDato  AS CHARACTER
   /*  8 */ FIELD Dato  AS CHARACTER
   /*  9 */ FIELD Tid  AS CHARACTER
   /* 10 */ FIELD TYPE  AS CHARACTER
   /* 11 */ FIELD Belop  AS CHARACTER
   /* 12 */ FIELD ButikkNr  AS CHARACTER
   /* 13 */ FIELD KasseNr AS CHARACTER
   /* 14 */ FIELD BongNr AS CHARACTER
   /* 15 */ FIELD Selger AS CHARACTER
   /* 16 */ FIELD DatoInnlost AS CHARACTER
   /* 17 */ FIELD TidInnlost AS CHARACTER
   /* 18 */ FIELD ButikkInnlost AS CHARACTER
   /* 19 */ FIELD KasseNrInnlost AS CHARACTER
   /* 20 */ FIELD BongNrInnlost AS CHARACTER
   /* 21 */ FIELD SelgerInnlost AS CHARACTER
    INDEX Id TYPE Id. 

DEF BUFFER bttGT FOR ttGT.

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

RUN LesInnFil.
RUN PosterData.

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

  /* Tømmer feillogg. */
  FOR EACH ttError:
    DELETE ttError.
  END.
  FOR EACH ttGT:
    DELETE ttGT.
  END.

  RUN TellOppLinjer.
  RUN bibl_logg.p ('PRSGavekortImport', 'xPRSGavekortInnles.p: Leser inn fil: ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

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
    IF NUM-ENTRIES(pcLinje,";") < 2 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Skal være minst 4 entries) " + STRING(iAntLinjer) + ": " + pcLinje
        .
        NEXT LESERLINJER.
    END.
    
    CREATE ttGT.    

    /*
    RUN AssignInt(1,OUTPUT ttGavekort.GavekortId, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(17,OUTPUT ttGavekort.Rab1%, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDate(24,OUTPUT ttButiker.ApningsDato, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    */
    
    ASSIGN 
      ttGT.ID             = ENTRY(1,pcLinje,';') 
      ttGT.MedlemsNr      = ENTRY(2,pcLinje,';') 
      ttGT.KundeNr        = ENTRY(3,pcLinje,';')
      ttGT.Navn           = ENTRY(4,pcLinje,';')
      ttGT.Telefon        = ENTRY(5,pcLinje,';')
      ttGT.GyldigFraDato  = ENTRY(6,pcLinje,';')
      ttGT.GyldigTilDato  = ENTRY(7,pcLinje,';')
      ttGT.Dato           = ENTRY(8,pcLinje,';')
      ttGT.Tid            = ENTRY(9,pcLinje,';')
      ttGT.TYPE           = ENTRY(10,pcLinje,';')
      ttGT.Belop          = ENTRY(11,pcLinje,';')
      ttGT.ButikkNr       = ENTRY(12,pcLinje,';')
      ttGT.KasseNr        = ENTRY(13,pcLinje,';')
      ttGT.BongNr         = ENTRY(14,pcLinje,';')
      ttGT.Selger         = ENTRY(15,pcLinje,';')
      ttGT.DatoInnlost    = ENTRY(16,pcLinje,';')
      ttGT.TidInnlost     = ENTRY(17,pcLinje,';')
      ttGT.ButikkInnlost  = ENTRY(18,pcLinje,';')
      ttGT.KasseNrInnlost = ENTRY(19,pcLinje,';')
      ttGT.BongNrInnlost  = ENTRY(20,pcLinje,';')
      ttGT.SelgerInnlost  = ENTRY(21,pcLinje,';')
    NO-ERROR.    

    /* Det har vært feil i en eller flere kolonner */
    IF b2Ok = FALSE THEN 
      NEXT LESERLINJER.
      
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
  GAVEKORT:
  FOR EACH ttGT WHERE 
    ttGT.Type = '1' TRANSACTION : /* Gavekort */
    FIND FIRST Gavekort EXCLUSIVE-LOCK WHERE 
      Gavekort.IdentNr = ttGT.ID NO-ERROR.
    IF NOT AVAILABLE Gavekort THEN 
    DO:
      CREATE Gavekort.
      ASSIGN
          Gavekort.ButNr      = INT(ttGT.ButikkNr)
          Gavekort.IdentNr    = ttGT.ID
          Gavekort.IdentType  = 1 /* Alltid 1 når det opprettes fra kassen */
      NO-ERROR.

      /* Opprettet */
      ASSIGN         
          Gavekort.Dato       = DATE(ttGT.Dato)
          Gavekort.Tid        = 0
          Gavekort.KasseNr    = 1
          Gavekort.SelgerNr   = DEC(ttGT.Selger)
          GaveKort.KassNr     = 1
          Gavekort.BongNr     = INT(ttGT.BongNr)
          Gavekort.GyldigDato = DATE(ttGT.GyldigFraDato)
          Gavekort.Belop      = DEC(ttGT.Belop)
          GaveKort.RabKr      = 0            
          GaveKort.FraB_Id    = 0
          Gavekort.Eget       = TRUE
          Gavekort.KNavn      = ttGT.Navn
          .
          
      /* Brukt */
      ASSIGN 
          GaveKort.BruktB_Id       = 0
          GaveKort.BruktButNr      = INT(ttGT.ButikkInnlost)
          Gavekort.BruktDato       = DATE(ttGT.DatoInnlost)
          Gavekort.BruktTid        = 0
          Gavekort.BruktKasseNr    = 1
          Gavekort.BruktSelgerNr   = DEC(ttGT.SelgerInnlost)
          GaveKort.BruktKassNr     = 1
          Gavekort.BruktBongNr     = DEC(ttGT.BongNrInnlost)
          Gavekort.Utgatt          = TRUE
          Gavekort.UtgattDato      = Gavekort.BruktDato
          Gavekort.UtgattTid       = 0
          .       
    END.

    IF AVAILABLE Gavekort THEN 
        RELEASE Gavekort.
    
  END. /* GAVEKORT TRANSACTION */
  
  TILGODE:
  FOR EACH ttGT WHERE 
    ttGT.Type = '2' TRANSACTION : /* Tilgode */
    FIND FIRST Tilgode EXCLUSIVE-LOCK WHERE 
      Tilgode.IdentNr = ttGT.ID NO-ERROR.
    IF NOT AVAILABLE Tilgode THEN 
    DO:
      CREATE Tilgode.
      ASSIGN
          Tilgode.ButNr      = INT(ttGT.ButikkNr)
          Tilgode.IdentNr    = ttGT.ID
          Tilgode.IdentType  = 1 /* Alltid 1 når det opprettes fra kassen */
      NO-ERROR.

      /* Opprettet */
      ASSIGN         
          Tilgode.Dato       = DATE(ttGT.Dato)
          Tilgode.Tid        = 0
          Tilgode.KasseNr    = 1
          Tilgode.SelgerNr   = DEC(ttGT.Selger)
          Tilgode.KassNr     = 1
          Tilgode.BongNr     = INT(ttGT.BongNr)
          Tilgode.GyldigDato = DATE(ttGT.GyldigFraDato)
          Tilgode.Belop      = DEC(ttGT.Belop)
          Tilgode.RabKr      = 0            
          Tilgode.FraB_Id    = 0
          Tilgode.Eget       = TRUE
          Tilgode.KNavn      = ttGT.Navn
          .
          
      /* Brukt */
      ASSIGN 
          Tilgode.BruktB_Id       = 0
          Tilgode.BruktButNr      = INT(ttGT.ButikkInnlost)
          Tilgode.BruktDato       = DATE(ttGT.DatoInnlost)
          Tilgode.BruktTid        = 0
          Tilgode.BruktKasseNr    = 1
          Tilgode.BruktSelgerNr   = DEC(ttGT.SelgerInnlost)
          Tilgode.BruktKassNr     = 1
          Tilgode.BruktBongNr     = DEC(ttGT.BongNrInnlost)
          Tilgode.Utgatt          = TRUE
          Tilgode.UtgattDato      = Tilgode.BruktDato
          Tilgode.UtgattTid       = 0
          .       
    END.

    IF AVAILABLE Tilgode THEN 
        RELEASE Tilgode.
    
  END. /* TILGODE TRANSACTION */
  
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

