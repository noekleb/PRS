&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : ovordre_reservervarer.p
    Purpose     : Reserverer varer fra nettbutikk lager ved å overføre dem fra nettbutikks lager til nettbutikk. 

    Syntax      : RUN ovordre_reservervarer.p (cKOrdre_Id_Lst,TRUE).

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 1/12-19
    Notes       : 
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
DEFINE VARIABLE  bVarebytte AS LOG NO-UNDO.
DEFINE VARIABLE bVareKorr AS LOG NO-UNDO.

DEF VAR wEDB-System      AS CHARACTER  NO-UNDO.
DEF VAR wTabell          AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE ttt_OvBuffer LIKE OvBuffer.
DEFINE NEW SHARED TEMP-TABLE tt_OvBuffer NO-UNDO LIKE OvBuffer.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

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
  RETURN 'ERROR Ukjent sentrallagerbutikk i procedure opprett_overforingsordre.p.'.

FOR EACH ttt_OvBuffer:
  DELETE ttt_OvBuffer.
END.

IF cKOrdre_Id_Lst = '' THEN 
  RETURN 'ERROR'.
 
DO piLoop = 1 TO NUM-ENTRIES(cKOrdre_Id_Lst):
  FIND KOrdreHode NO-LOCK WHERE 
    KOrdreHode.KOrdre_Id = DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst)) NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN 
    NEXT.

  /* Flytter varer fra lager til nettbutikk. */  
  RUN opprett_temp_overforingsordre (DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst))).    
END.

/* Her behandles temp tabellen og ordren opprettes. */
IF CAN-FIND(FIRST ttt_OvBuffer) THEN
  RUN opprett_ovordre.


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
                           'J' + CHR(1) + "Importert fra nettbutikk: " + KOrdreHode.EkstOrdreNr,
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
  /* IMPORT AV NYE ORDRE FRA NETTBUTIKK.                                      */
  /* Varene skal flyttes fra lager til nettbutikk. Lager er angitt i Plukkbutikk. */
	------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER lKOrdre_Id AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
  DEFINE VARIABLE piFrabut AS INTEGER NO-UNDO.
  DEFINE VARIABLE piTilbut AS INTEGER NO-UNDO.
  
  FIND KOrdreHode NO-LOCK WHERE
    KOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN 
    RETURN.
    
  LINJE:
  FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK:
  
    /* Skipper betalingslinjen. */
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT LINJE.
    
    IF NOT CAN-FIND(ArtBas WHERE
      ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN NEXT LINJE.

    /* Kanselering av ordre. Flytt varer fra nettbutikk til nettbutikks lager. */
    IF bBytt THEN 
      ASSIGN 
        piFraBut = KOrdreHode.ButikkNr
        piTilBut = KOrdreLinje.PlukkButikk
        .
    /* Ny ordre importeres. Flytt varer fra nettbutikks lager til nettbutikk. */
    ELSE 
      ASSIGN 
        piFraBut = KOrdreLinje.PlukkButikk
        piTilBut = KOrdreHode.ButikkNr
        .

    IF piFrabut = piTilBut THEN 
      NEXT LINJE.
      
    FIND ttt_OvBuffer WHERE
      ttt_OvBuffer.BuntNr      = 999 AND
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
               ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) 
               ttt_OvBuffer.Vg          = ArtBas.Vg         
               ttt_OvBuffer.LopNr       = ArtBas.LopNr      
               ttt_OvBuffer.ButikkNrFra = piFraBut
               ttt_OvBuffer.ButikkNrTil = piTilBut
               ttt_OvBuffer.Merknad     = 'Byttet vare på linje på ordre ' + KOrdreHode.EkstOrdreNr    
               ttt_OvBuffer.Storl       = KOrdreLinje.Storl
               ttt_OvBuffer.TilStorl    = KOrdreLinje.Storl
               ttt_OvBuffer.Varekost    = KOrdreLinje.Varekost
               .
      END.   
    ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + ABS(KOrdreLinje.Antall).     
  END. /* LINJE */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF







