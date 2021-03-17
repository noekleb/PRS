&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : ovordre_retur.p

    Syntax      : 

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 3/12-19
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER cKOrdre_Id_Lst AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER bBytt          AS LOG NO-UNDO.

DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE iCL    AS INTEGER NO-UNDO. 
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE bSendReservasjon AS LOG NO-UNDO.
DEFINE VARIABLE cTekst           AS CHARACTER NO-UNDO.
DEFINE VARIABLE iFraButikk AS INTEGER NO-UNDO.
DEFINE VARIABLE iTilButikk AS INTEGER NO-UNDO.
DEFINE VARIABLE bVarebytte AS LOG NO-UNDO.
DEFINE VARIABLE bVareKorr AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEF VAR wEDB-System      AS CHARACTER  NO-UNDO.
DEF VAR wTabell          AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE ttt_OvBuffer LIKE OvBuffer.
DEFINE NEW SHARED TEMP-TABLE tt_OvBuffer NO-UNDO LIKE OvBuffer.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
ASSIGN 
  cLogg = 'ovordre_retur' + REPLACE(STRING(TODAY),'/','')
  .
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.'
    ).

/* Kode for låsing av artikkelnummer ved overføring. */
{syspara.i 1 2 3 wEDB-System}
IF wEDB-System = "" THEN
  wEDB-System = "OVERFOR-LOCK".

/* Skal det sendes eMail med reservasjonsmelding */
{syspara.i 150 1 5 cTekst}
IF CAN-DO('1,J,Y,Ja,Yes,True',cTekst) THEN
    bSendReservasjon = TRUE.
ELSE
    bSendReservasjon = FALSE.

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
{syspara.i 5 1 1 iCL INT}
IF NOT CAN-FIND(Butiker NO-LOCK WHERE
                  Butiker.Butik = iCL) THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  **Ukjent sentrallagerbutikk.'
        ).
                     
  RETURN '** Ukjent sentrallagerbutikk.'.
  END.
  
FOR EACH ttt_OvBuffer:
  DELETE ttt_OvBuffer.
END.

IF cKOrdre_Id_Lst = '' THEN
DO: 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ** Tom ordreliste.'
      ).
  RETURN 'ERROR'.
END. 

DO piLoop = 1 TO NUM-ENTRIES(cKOrdre_Id_Lst):
  FIND KOrdreHode NO-LOCK WHERE 
    KOrdreHode.KOrdre_Id = DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst)) NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ** Ukjent KordreHode: ' + ENTRY(piLoop,cKOrdre_Id_Lst) + '.'
        ).
    NEXT.
  END.
  /* Her SKAL returer og varebytte behandles. */
  IF (NOT KOrdreHode.EkstOrdreNr MATCHES '*RETUR*' AND 
      NOT KOrdreHode.EkstOrdreNr MATCHES '*BYTTE*') THEN
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ** Ordren er IKKE en RETUR eller BYTTE og skal ikke behandles her. KordreHode: ' + ENTRY(piLoop,cKOrdre_Id_Lst) + '.'
        ).
    NEXT.
  END.
  
  IF KOrdreHode.EkstOrdreNr MATCHES '*RETUR*' THEN 
    RUN opprett_temp_overforingsordre (DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst))).
  ELSE IF KOrdreHode.EkstOrdreNr MATCHES '*BYTTE*' THEN 
    RUN opprett_temp_overforingsordreBytte (DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst))).
END.

/* Her behandles temp tabellen og ordren opprettes. */
IF CAN-FIND(FIRST ttt_OvBuffer) THEN
  RUN opprett_ovordre.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.'
    ).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* **********************  Internal Procedures  *********************** */
 
&IF DEFINED(EXCLUDE-opprett_ovordre) = 0 &THEN
    
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprett_ovordre Procedure
PROCEDURE opprett_ovordre:

  /*------------------------------------------------------------------------------
      Purpose:                                      
      Notes:                                      
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE iBuntNr    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cBuntNrLst AS CHARACTER NO-UNDO.
  
  DEFINE BUFFER bOvBunt FOR OvBunt. 
  
  FOR EACH ttt_OvBuffer 
    BREAK 
      BY ttt_OvBuffer.ButikkNrTil
      BY ttt_OvBuffer.ButikkNrFra
      BY ttt_OvBuffer.ArtikkelNr
      BY ttt_OvBuffer.Storl:

    CREATE tt_OvBuffer.
    BUFFER-COPY ttt_OvBuffer TO tt_OvBuffer.

    IF LAST-OF(ttt_OvBuffer.ButikkNrFra) THEN 
      OPPRETT_BUNT:
      DO:
        ASSIGN iBuntNr = -1.
        /* Oppretter overføringsordre - oppdateres direkte. */
        RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,
                           0,
                           'J' + CHR(1) + "Ordrenr. nettbutikk (Varekorr): " + KOrdreHode.EkstOrdreNr,
                           wEDB-System,
                           wTabell,
                           8).
      EMPTY TEMP-TABLE TT_OvBuffer NO-ERROR.
        ASSIGN
          cBuntNrLst = cBuntNrLst + 
                       (IF cBuntNrLst = '' THEN '' ELSE ',') + 
                       STRING(iBuntNr).
      END. /* OPPRETT_BUNT */          
  END.
  
  IF (cKOrdre_Id_Lst <> '' AND cBuntNrLst <> '') AND bSendReservasjon THEN
    RUN webreservasjonKOrdrePDF.p (cKOrdre_Id_Lst, cBuntNrLst).

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
 
&IF DEFINED(EXCLUDE-opprett_temp_overforingsordre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprett_temp_overforingsordre Procedure
PROCEDURE opprett_temp_overforingsordre:
/*------------------------------------------------------------------------------
 Purpose: Overføringsordre som bare gjelder varelinjer hvor vare er bytttet.
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER lKOrdre_Id AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE piFraBut AS INTEGER NO-UNDO.
  DEFINE VARIABLE piTilBut AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
  FIND KOrdreHode NO-LOCK WHERE
    KOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN 
    RETURN.
    
  /* Her håndteres linjer hvor det ikke er byttet vare.                                                             */ 
  /* Varen er returnert inn på nettbutikk og skal tilbakeføres til butikk som har tatt imot retur, eller til lager. */
  LINJE:
  FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
    KOrdreLinje.Aktiv = TRUE AND
    KOrdreLinje.KopiKOrdreLinjeNr = 0 AND  
    KOrdreLinje.PlukkButikk > 0:
  
    
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT LINJE.
    
    IF NOT CAN-FIND(ArtBas WHERE
      ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN NEXT LINJE.

    ASSIGN 
      piFraBut = KOrdreHode.ButikkNr
      piTilBut = (IF KOrdreHode.iOpt1 > 0 THEN  KOrdreHode.iOpt1 ELSE KOrdreLinje.PlukkButikk) /* Retur kan komme fra butikk iOpt1 */ 
      .

    IF piFrabut = piTilBut THEN 
      NEXT LINJE.
    
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
    FIND ttt_OvBuffer WHERE
      ttt_OvBuffer.BuntNr = 999 AND
      ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND
      ttt_OvBuffer.ButikkNrFra = piFraBut AND
      ttt_OvBuffer.ButikkNrTil = piTilBut AND  
      ttt_OvBuffer.Storl       = KOrdreLinje.Storl NO-ERROR.
    IF NOT AVAILABLE ttt_OvBuffer THEN 
      DO:
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
        CREATE ttt_OvBuffer.
        ASSIGN iLinjeNr                = iLinjeNr + 1
               ttt_OvBuffer.BuntNr      = 999     
               ttt_OvBuffer.LinjeNr     = iLinjeNr
               ttt_OvBuffer.ButikkNrFra = piFraBut
               ttt_OvBuffer.ButikkNrTil = piTilBut
               ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) 
               
               ttt_OvBuffer.ButikkNrFra = piFraBut
               ttt_OvBuffer.ButikkNrTil = piTilBut

               ttt_OvBuffer.Vg          = ArtBas.Vg         
               ttt_OvBuffer.LopNr       = ArtBas.LopNr      
               ttt_OvBuffer.Merknad     = 'Nettbutikk retur(1)' + KOrdreHode.EkstOrdreNr    
               ttt_OvBuffer.Storl       = KOrdreLinje.Storl
               ttt_OvBuffer.TilStorl    = KOrdreLinje.Storl
               ttt_OvBuffer.Varekost    = KOrdreLinje.Varekost.
      END.   
    ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + ABS(KOrdreLinje.Antall).
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Overfører ' + KORdreLinje.VareNr + 
      ' LevKod ' + (IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '') + 
      ' Kode ' + KOrdreLinje.Kode + 
      ' fra ' + STRING(piFrabut) + 
      ' til ' + STRING(piTilBut) + 
      ' antall ' + STRING(KordreLinje.Antall)
      ).
         
  END. /* LINJE */

  /*
    To tilfeller:
      1. Varelinjer hvor det er byttet vare på den opprinnelige ordren
        - Vare på den aktive linjen returneres til lager eller butikk som har mottatt returen.
        - Disse varelinjene på den opprinnelige ordren, har KopiKOrdreLinjeNr > 0.
        - Returmelding til Phønix inneholder varen på den motsvarende passive linjen.
      2. Varelinjer hvor vare er byttet istedenfor at de er returnert. (Dette kan ikke gjøres i butikk, bare lager eCom.).
        - Vare på den aktive linjen må nå hentes fra lageret. 
        - Varen på den passive linjen leveres tilbake til lageret. 
        - Disse varelinjene på den opprinnelige ordren har KopiKOrdreLinjeNr > 0
        - Returmelding til Phønix skal ikke inneholde disse varelinjene.
  */
  LINJE:
  FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
    KOrdreLinje.Aktiv = TRUE AND
    KOrdreLinje.KopiKOrdreLinjeNr > 0 AND  
    KOrdreLinje.PlukkButikk > 0:
    
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT LINJE.
    
    IF NOT CAN-FIND(ArtBas WHERE
      ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN NEXT LINJE.

    /* Henter varelinje fra den opprinnelige ordren. */    
    FIND bufKOrdreLinje NO-LOCK WHERE 
      bufKOrdreLinje.KOrdre_Id     = KOrdreHode.RefKOrdre_Id AND 
      bufKOrdreLinje.KordreLinjeNr = KOrdreLinje.KopiKOrdreLinjeNr NO-ERROR.

    IF AVAILABLE bufKOrdreLinje AND bufKOrdreLinje.KopiKOrdreLinjeNr > 0 THEN
    /*
      Her returneres varen.
        - Varen på den aktive linjen returneres til lageret eller til den butikk som har tatt i mot returen.
    */
    TILFELLE1: 
    DO:
      ASSIGN 
        piFraBut = KOrdreHode.ButikkNr
        piTilBut = (IF KOrdreHode.iOpt1 > 0 THEN  KOrdreHode.iOpt1 ELSE KOrdreLinje.PlukkButikk) /* Retur kan komme fra butikk iOpt1 */ 
        .
  
      IF piFrabut = piTilBut THEN 
        NEXT LINJE.
      
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
      FIND ttt_OvBuffer WHERE
        ttt_OvBuffer.BuntNr = 999 AND
        ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND
        ttt_OvBuffer.ButikkNrFra = piFraBut AND
        ttt_OvBuffer.ButikkNrTil = piTilBut AND  
        ttt_OvBuffer.Storl       = KOrdreLinje.Storl NO-ERROR.
      IF NOT AVAILABLE ttt_OvBuffer THEN 
        DO:
          FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
          CREATE ttt_OvBuffer.
          ASSIGN iLinjeNr                = iLinjeNr + 1
                 ttt_OvBuffer.BuntNr      = 999     
                 ttt_OvBuffer.LinjeNr     = iLinjeNr
                 ttt_OvBuffer.ButikkNrFra = piFraBut
                 ttt_OvBuffer.ButikkNrTil = piTilBut
                 ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) 
                 
                 ttt_OvBuffer.ButikkNrFra = piFraBut
                 ttt_OvBuffer.ButikkNrTil = piTilBut
  
                 ttt_OvBuffer.Vg          = ArtBas.Vg         
                 ttt_OvBuffer.LopNr       = ArtBas.LopNr      
                 ttt_OvBuffer.Merknad     = 'Nettbutikk retur(2)' + KOrdreHode.EkstOrdreNr    
                 ttt_OvBuffer.Storl       = KOrdreLinje.Storl
                 ttt_OvBuffer.TilStorl    = KOrdreLinje.Storl
                 ttt_OvBuffer.Varekost    = KOrdreLinje.Varekost.
        END.   
      ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + ABS(KOrdreLinje.Antall).

      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Overfører ' + KORdreLinje.VareNr + 
        ' LevKod ' + (IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '') + 
        ' Kode ' + KOrdreLinje.Kode + 
        ' fra ' + STRING(piFrabut) + 
        ' til ' + STRING(piTilBut) + 
        ' antall ' + STRING(KordreLinje.Antall)
        ).
    END. /* TILFELLE1 */
    
    /*
      Her er det byttet vare istedenfor å returnere varen.
        - Vare på den aktive linjen skal hentes fra lageret. 
        - Varen på den passive linjen skal leveres tilbake til lageret.
    */
    ELSE IF AVAILABLE bufKOrdreLinje AND bufKOrdreLinje.KopiKOrdreLinjeNr = 0 THEN 
    TILFELLE2:
    DO:
      /* Henter varen fra lageret. */
      ASSIGN 
        piFraBut = KOrdreLinje.PlukkButikk
        piTilBut = KOrdreHode.ButikkNr 
        .
  
      IF piFrabut = piTilBut THEN 
        NEXT LINJE.
      
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
      FIND ttt_OvBuffer WHERE
        ttt_OvBuffer.BuntNr = 999 AND
        ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND
        ttt_OvBuffer.ButikkNrFra = piFraBut AND
        ttt_OvBuffer.ButikkNrTil = piTilBut AND  
        ttt_OvBuffer.Storl       = KOrdreLinje.Storl NO-ERROR.
      IF NOT AVAILABLE ttt_OvBuffer THEN 
        DO:
          FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
          CREATE ttt_OvBuffer.
          ASSIGN iLinjeNr                = iLinjeNr + 1
                 ttt_OvBuffer.BuntNr      = 999     
                 ttt_OvBuffer.LinjeNr     = iLinjeNr
                 ttt_OvBuffer.ButikkNrFra = piFraBut
                 ttt_OvBuffer.ButikkNrTil = piTilBut
                 ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) 
                 
                 ttt_OvBuffer.ButikkNrFra = piFraBut
                 ttt_OvBuffer.ButikkNrTil = piTilBut
  
                 ttt_OvBuffer.Vg          = ArtBas.Vg         
                 ttt_OvBuffer.LopNr       = ArtBas.LopNr      
                 ttt_OvBuffer.Merknad     = 'Nettbutikk retur(3)' + KOrdreHode.EkstOrdreNr    
                 ttt_OvBuffer.Storl       = KOrdreLinje.Storl
                 ttt_OvBuffer.TilStorl    = KOrdreLinje.Storl
                 ttt_OvBuffer.Varekost    = KOrdreLinje.Varekost.
        END.   
      ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + ABS(KOrdreLinje.Antall).
      
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Overfører ' + KORdreLinje.VareNr + 
        ' LevKod ' + (IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '') + 
        ' Kode ' + KOrdreLinje.Kode + 
        ' fra ' + STRING(piFrabut) + 
        ' til ' + STRING(piTilBut) + 
        ' antall ' + STRING(KordreLinje.Antall)
        ).

      /* Leverer varen som er byttet tilbake til lageret. */
      FIND bufKOrdreLinje NO-LOCK WHERE 
        bufKOrdreLinje.KOrdre_Id     = KOrdreLinje.KOrdre_Id AND 
        bufKOrdreLinje.KORdreLinjeNr = KORdreLinje.KopiKOrdreLinjeNr NO-ERROR.
      IF AVAILABLE bufKOrdreLinje THEN
      TILBAKETILLAGER: 
      DO:        
        /* Tilbake til lageret. */
        ASSIGN 
          piFraBut = KOrdreHode.ButikkNr
          piTilBut = bufKOrdreLinje.PlukkButikk 
          .
    
        IF piFrabut = piTilBut THEN 
          NEXT LINJE.
        
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = DEC(bufKOrdreLinje.VareNr) NO-ERROR.
        FIND ttt_OvBuffer WHERE
          ttt_OvBuffer.BuntNr = 999 AND
          ttt_OvBuffer.ArtikkelNr  = DECIMAL(bufKOrdreLinje.VareNr) AND
          ttt_OvBuffer.ButikkNrFra = piFraBut AND
          ttt_OvBuffer.ButikkNrTil = piTilBut AND  
          ttt_OvBuffer.Storl       = bufKOrdreLinje.Storl NO-ERROR.
        IF NOT AVAILABLE ttt_OvBuffer THEN 
          DO:
            FIND ArtBas NO-LOCK WHERE
              ArtBas.ArtikkelNr = DECIMAL(bufKOrdreLinje.VareNr) NO-ERROR.
            CREATE ttt_OvBuffer.
            ASSIGN iLinjeNr                = iLinjeNr + 1
                   ttt_OvBuffer.BuntNr      = 999     
                   ttt_OvBuffer.LinjeNr     = iLinjeNr
                   ttt_OvBuffer.ButikkNrFra = piFraBut
                   ttt_OvBuffer.ButikkNrTil = piTilBut
                   ttt_OvBuffer.ArtikkelNr  = DECIMAL(bufKOrdreLinje.VareNr) 
                   
                   ttt_OvBuffer.ButikkNrFra = piFraBut
                   ttt_OvBuffer.ButikkNrTil = piTilBut
    
                   ttt_OvBuffer.Vg          = ArtBas.Vg         
                   ttt_OvBuffer.LopNr       = ArtBas.LopNr      
                   ttt_OvBuffer.Merknad     = 'Nettbutikk retur(4)' + KOrdreHode.EkstOrdreNr    
                   ttt_OvBuffer.Storl       = bufKOrdreLinje.Storl
                   ttt_OvBuffer.TilStorl    = bufKOrdreLinje.Storl
                   ttt_OvBuffer.Varekost    = bufKOrdreLinje.Varekost.
          END.   
        ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + ABS(bufKOrdreLinje.Antall).
        
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Overfører ' + bufKORdreLinje.VareNr + 
          ' LevKod ' + (IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '') + 
          ' Kode ' + bufKOrdreLinje.Kode + 
          ' fra ' + STRING(piFrabut) + 
          ' til ' + STRING(piTilBut) + 
          ' antall ' + STRING(bufKordreLinje.Antall)
          ).

      END. /* TILBAKETILLAGER */
    END. /* TILFELLE2 */
    ELSE DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  ** Feil. Fant ikke opprinnelig ordrerad. '
          ).
    END.
    

    
  END. /* LINJE */

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-opprett_temp_overforingsordreBytte) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprett_temp_overforingsordreBytte Procedure
PROCEDURE opprett_temp_overforingsordreBytte:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER lKOrdre_Id AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE piFraBut AS INTEGER NO-UNDO.
  DEFINE VARIABLE piTilBut AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
  FIND KOrdreHode NO-LOCK WHERE
    KOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN 
    RETURN.
    
  /* Utleverte varer skal hentes fra nettbutikkens lager. */
  LINJE:
  FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
    KOrdreLinje.Aktiv = TRUE AND
    KOrdreLinje.ByttetKOrdreLinjeNr > 0 AND 
    KOrdreLinje.antall > 0:
    
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT LINJE.
    
    IF NOT CAN-FIND(ArtBas WHERE
      ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN NEXT LINJE.

    ASSIGN 
      piFraBut = KOrdreLinje.PlukkButikk
      piTilBut = KOrdreHode.ButikkNr 
      .

    IF piFrabut = piTilBut THEN 
      NEXT LINJE.
    
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
    FIND ttt_OvBuffer WHERE
      ttt_OvBuffer.BuntNr = 999 AND
      ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND
      ttt_OvBuffer.ButikkNrFra = piFraBut AND
      ttt_OvBuffer.ButikkNrTil = piTilBut AND  
      ttt_OvBuffer.Storl       = KOrdreLinje.Storl NO-ERROR.
    IF NOT AVAILABLE ttt_OvBuffer THEN 
      DO:
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
        CREATE ttt_OvBuffer.
        ASSIGN iLinjeNr                = iLinjeNr + 1
               ttt_OvBuffer.BuntNr      = 999     
               ttt_OvBuffer.LinjeNr     = iLinjeNr
               ttt_OvBuffer.ButikkNrFra = piFraBut
               ttt_OvBuffer.ButikkNrTil = piTilBut
               ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) 
               
               ttt_OvBuffer.ButikkNrFra = piFraBut
               ttt_OvBuffer.ButikkNrTil = piTilBut

               ttt_OvBuffer.Vg          = ArtBas.Vg         
               ttt_OvBuffer.LopNr       = ArtBas.LopNr      
               ttt_OvBuffer.Merknad     = 'Nettbutikk bytte(1)' + KOrdreHode.EkstOrdreNr    
               ttt_OvBuffer.Storl       = KOrdreLinje.Storl
               ttt_OvBuffer.TilStorl    = KOrdreLinje.Storl
               ttt_OvBuffer.Varekost    = KOrdreLinje.Varekost.
      END.   
    ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + ABS(KOrdreLinje.Antall).
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Overfører ' + KORdreLinje.VareNr + 
      ' LevKod ' + (IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '') + 
      ' Kode ' + KOrdreLinje.Kode + 
      ' fra ' + STRING(piFrabut) + 
      ' til ' + STRING(piTilBut) + 
      ' antall ' + STRING(KordreLinje.Antall)
      ).
         
  END. /* LINJE */

  /* Utleverte varer skal hentes fra nettbutikkens lager. */
  LINJE:
  FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
    KOrdreLinje.Aktiv = TRUE AND
    KOrdreLinje.ByttetKOrdreLinjeNr > 0 AND 
    KOrdreLinje.antall < 0:
    
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT LINJE.
    
    IF NOT CAN-FIND(ArtBas WHERE
      ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN NEXT LINJE.

    ASSIGN 
      piFraBut = KOrdreHode.ButikkNr
      piTilBut = KOrdreLinje.PlukkButikk 
      .

    IF piFrabut = piTilBut THEN 
      NEXT LINJE.
    
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
    FIND ttt_OvBuffer WHERE
      ttt_OvBuffer.BuntNr = 999 AND
      ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND
      ttt_OvBuffer.ButikkNrFra = piFraBut AND
      ttt_OvBuffer.ButikkNrTil = piTilBut AND  
      ttt_OvBuffer.Storl       = KOrdreLinje.Storl NO-ERROR.
    IF NOT AVAILABLE ttt_OvBuffer THEN 
      DO:
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
        CREATE ttt_OvBuffer.
        ASSIGN iLinjeNr                = iLinjeNr + 1
               ttt_OvBuffer.BuntNr      = 999     
               ttt_OvBuffer.LinjeNr     = iLinjeNr
               ttt_OvBuffer.ButikkNrFra = piFraBut
               ttt_OvBuffer.ButikkNrTil = piTilBut
               ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) 
               
               ttt_OvBuffer.ButikkNrFra = piFraBut
               ttt_OvBuffer.ButikkNrTil = piTilBut

               ttt_OvBuffer.Vg          = ArtBas.Vg         
               ttt_OvBuffer.LopNr       = ArtBas.LopNr      
               ttt_OvBuffer.Merknad     = 'Nettbutikk bytte(1)' + KOrdreHode.EkstOrdreNr    
               ttt_OvBuffer.Storl       = KOrdreLinje.Storl
               ttt_OvBuffer.TilStorl    = KOrdreLinje.Storl
               ttt_OvBuffer.Varekost    = KOrdreLinje.Varekost.
      END.   
    ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + ABS(KOrdreLinje.Antall).
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Overfører ' + KORdreLinje.VareNr + 
      ' LevKod ' + (IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '') + 
      ' Kode ' + KOrdreLinje.Kode + 
      ' fra ' + STRING(piFrabut) + 
      ' til ' + STRING(piTilBut) + 
      ' antall ' + STRING(KordreLinje.Antall)
      ).
         
  END. /* LINJE */

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

