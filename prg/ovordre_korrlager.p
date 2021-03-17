&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : opprett_Overforingsordre.p
    Purpose     : Opprettes en overføringsordre for alle linjer på de aktuelle ordrene som skal overføres fra en butikk.
                  Skal det overføres fra flere butikker på en ordre, opprettes flere overføringsordre. 

    Syntax      : RUN opprett_Overforingsordre.p (cKOrdre_Id_Lst,TRUE).

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 3/7-09
    Notes       : 3/12-16 TN
                  Sendes det inn ? i variabel bBytt, skal fra og til 
                  butikk hentes via publish statment.
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
  cLogg = 'ovordre_korrlager' + REPLACE(STRING(TODAY),'/','')
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
  /* Her skal IKKE returer behandles. */
  IF KOrdreHode.SendingsNr = 'RETUR' THEN
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ** RETUR skal ikke behandles her. KordreHode: ' + ENTRY(piLoop,cKOrdre_Id_Lst) + '.'
        ).
    NEXT.
  END.
  
  RUN opprett_temp_overforingsordre (DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst))).
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
    
  /* Behandler BARE linjer hvor vare er byttet.  KOrdreLinje.KopiKOrdreLinjeNr > 0 */ 
  /* Aktive linjer - Hente varer fra lageret.                                      */
  /* Passeive linjer - Legge varer tilbake på lageret.                             */ 
  LINJE:
  FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
    KOrdreLinje.PlukkButikk > 0 AND 
    KOrdreLinje.KopiKOrdreLinjeNr > 0: /* Bare varelinjer hvor vare byttet på linje */
  
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT LINJE.
    
    IF NOT CAN-FIND(ArtBas WHERE
      ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN NEXT LINJE.
    
    /* Henter fra lageret. */
    IF KOrdreLinje.Aktiv THEN 
      ASSIGN 
      piFraBut = KOrdreLinje.PlukkButikk
      piTilBut = KOrdreHode.ButikkNr 
      .
    /* Legger tilbake på lageret. */
    ELSE 
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
               ttt_OvBuffer.Merknad     = 'Nettbutikk varekorr' + KOrdreHode.EkstOrdreNr    
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
