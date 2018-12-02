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
DEF OUTPUT PARAMETER iBatchNr AS INT NO-UNDO.
DEF OUTPUT PARAMETER cStatus  AS CHAR NO-UNDO.
  


DEF VAR cEDB-System      AS CHAR NO-UNDO.
DEF VAR cTabell          AS CHAR NO-UNDO.
DEF VAR iCl              AS INT  NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

{overforing.i &SHARED = "Shared"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-FiksStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FiksStorl Procedure 
FUNCTION FiksStorl RETURNS CHARACTER
  ( INPUT wStorl AS CHAR )  FORWARD.

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

/* Kode for låsing av artikkelnummer ved overføring. */
{syspara.i 1 2 3 cEDB-System}
IF cEDB-System = "" THEN
  cEDB-System = "OVERFOR-LOCK".
{syspar2.i 1 2 3 cTabell}
IF cTabell = "" THEN
  cTabell = "ArtBas".
{syspara.i 5 1 1 iCl INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.

RUN PosterOverforinger.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PosterOverforinger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterOverforinger Procedure 
PROCEDURE PosterOverforinger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iTransNr LIKE TransLogg.TransNr NO-UNDO.
  DEF VAR lKundeNr LIKE Kunde.KundeNr     NO-UNDO.

  ASSIGN
      iBatchNr = ?
      cStatus = ""
      .

  FIND FIRST tmpOverfor NO-ERROR.
  IF NOT AVAILABLE tmpOverfor THEN
  DO:
      MESSAGE "Ingen poster å overføre"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

      cStatus = "Ingen poster å overføre.".
      RETURN.
  END.
  
  FIND OvBunt NO-LOCK WHERE
      OvBunt.BuntNr = tmpOverfor.BuntNr NO-ERROR.
  IF NOT AVAILABLE OvBunt THEN
  DO:
      cStatus = "Ukjent buntnummer.".
      RETURN.
  END.

  RUN batchlogg.w (PROGRAM-NAME(1), 
                   "Overføringer bunt: " + string(tmpOverfor.BuntNr) +
                   " " + string(OvBunt.DatoOppdatert) + " " + 
                   " " + string(OvBunt.TidOppdatert,"HH:MM:SS"),
                   OUTPUT iBatchNr).
  IF NOT iBatchNr > 0 THEN
  DO:
      cStatus = "Feil oppsto ved tildeling av batchnummer.".
      RETURN.
  END.

  DO TRANSACTION:
    /* Posterer overføringstransaksjonene */
    TMPOVERFOR:
    FOR EACH tmpOverfor
      BREAK
      BY tmpOverfor.TilBut
      BY tmpOverfor.FraStorl:

      IF tmpOverfor.ArtikkelNr = 0 THEN
          NEXT TMPOVERFOR.

      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = tmpOverfor.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          NEXT TMPOVERFOR.
      
      IF FIRST-OF (tmpOverfor.TilBut) THEN
        DO:
          FIND LAST TransLogg NO-LOCK WHERE
            TransLogg.Butik = int(tmpOverfor.FraBut) USE-INDEX TransLogg NO-ERROR.
          IF AVAILABLE TransLogg THEN
            iTransNr = TransLogg.TransNr + 1.
          ELSE 
            iTransNr = 1.
        END.
              
      /* Sjekker at transnr er ledig */
      IF CAN-FIND(TransLogg WHERE
                  TransLogg.Butik = int(tmpOverfor.FraBut) AND
                  TransLogg.TransNr = iTransNr) THEN
        NESTE_NR:
        DO WHILE TRUE:
          iTransNr = iTransNr + 1.
          IF CAN-FIND(TransLogg WHERE
                      TransLogg.Butik = int(tmpOverfor.FraBut) AND
                      TransLogg.TransNr = iTransNr) THEN
            NEXT NESTE_NR.
          ELSE
            LEAVE NESTE_NR.
        END. /* NESTE_NR */
        
      /* Benytter varekost i fra butikken. */  
      FIND Lager NO-LOCK WHERE
        Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
        Lager.Butik      = int(tmpOverfor.FraBut) NO-ERROR.
      IF NOT AVAILABLE Lager THEN
      DO:
          CREATE Lager.
          ASSIGN
              Lager.ArtikkelNr = ArtBas.ArtikkelNr
              Lager.Butik      = int(tmpOverfor.FraBut).
      END.
        
      IF AVAILABLE ArtBas AND AVAILABLE Lager THEN
      DO:
          CREATE TransLogg.
          ASSIGN 
               TransLogg.Butik        = int(tmpOverfor.FraBut)
               TransLogg.TransNr      = iTransNr
               TransLogg.SeqNr        = 1.

          ASSIGN
               TransLogg.BatchNr      = iBatchNr
               TransLogg.TTId         = 6 /* Overføring */
               TransLogg.TBId         = 1
               TransLogg.ArtikkelNr   = IF AVAILABLE ArtBas
                                          THEN ArtBas.ArtikkelNr
                                          ELSE 0
               TransLogg.Vg           = ArtBas.vg
               TransLogg.LopNr        = ArtBas.LopNr
               TransLogg.Antall       = tmpOverfor.Antal
               TransLogg.Pris         = Lager.VVareKost
               TransLogg.RabKr        = 0
               TransLogg.KundNr       = 0
               TransLogg.Bongtekst    = ArtBas.Beskr

               TransLogg.LevNr        = IF AVAILABLE ArtBas
                                          THEN ArtBas.LevNr
                                          ELSE 0
               TransLogg.OvButik      = int(tmpOverfor.TilBut)
               TransLogg.OvTransNr    = TransLogg.TransNr
               TransLogg.BongId       = 0
               TransLogg.BongLinjeNr  = 0
               TransLogg.KassaNr      = 0
               TransLogg.Plukket      = TRUE /* Skal ikke plukkes */
               TransLogg.Dato         = TODAY
               TransLogg.Tid          = TIME
               TransLogg.BestNr       = 0
               TransLogg.Postert      = FALSE
               TransLogg.Mva          = 0
               TransLogg.RefNr        = (IF TransLogg.RefNr = 0 THEN OvBunt.Opphav ELSE TransLogg.RefNr)
               TransLogg.RefTekst     = (IF TransLogg.RefTekst = '' THEN OvBunt.Merknad ELSE TransLogg.RefTekst)
               .
          ASSIGN  
               TransLogg.Storl        = FiksStorl(tmpOverfor.FraStorl).
          ASSIGN  
               TransLogg.TilStorl     = FiksStorl(tmpOverfor.TilStorl).

          /* Setter overføringslås på artikkelen. */
          IF NOT CAN-FIND(FIRST KonvReg NO-LOCK WHERE
                KonvReg.EDB-System = cEDB-System AND
                KonvReg.Tabell     = cTabell   AND
                KonvReg.EkstId     = string(ArtBas.ArtikkelNr)) THEN
            DO:
              FIND FIRST KonvReg NO-LOCK WHERE
                KonvReg.EDB-System = cEDB-System AND
                KonvReg.Tabell     = cTabell   AND
                KonvReg.EkstId     = string(ArtBas.ArtikkelNr) NO-ERROR.
              IF NOT AVAILABLE KonvReg THEN
                DO:
                  CREATE KonvReg.
                  ASSIGN
                    KonvReg.EDB-System = cEDB-System 
                    KonvReg.Tabell     = cTabell   
                    KonvReg.EkstId     = STRING(ArtBas.ArtikkelNr)
                    KonvReg.InterntID  = STRING(ArtBas.ArtikkelNr).
                END.
            END.
      END.

    END. /* tmpOverfor */

  END. /* TRANSACTION */
  IF AVAILABLE KonvReg THEN
    RELEASE KonvReg.

  /* Flagger batchen klar for oppdatering. */
  IF iBatchNr <> ? THEN
    RUN batchstatus.p (iBatchNr, 2).

  RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-FiksStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FiksStorl Procedure 
FUNCTION FiksStorl RETURNS CHARACTER
  ( INPUT wStorl AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Formaterer størrelsen korrekt etter SkoTex standard.
    Notes:  
------------------------------------------------------------------------------*/

 ASSIGN
    wStorl = TRIM(wStorl)
    wStorl = CAPS(wStorl)
    wStorl = IF (LENGTH(wStorl) = 1 OR 
                 length(wStorl) = 3
                 ) 
                then " " + wStorl
                else wStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  IF INDEX(wStorl,",") <> 0 THEN
    OVERLAY(wStorl, INDEX(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

