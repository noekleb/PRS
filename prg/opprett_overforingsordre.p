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
 
/* Er bBytt = ?, skal fra og til butikk hentes med et publish statment. */
IF bBytt = ? THEN 
    PUBLISH "getFraTilbutikkReturKOrdre" (OUTPUT iFraButikk, OUTPUT iTilButikk).

DO piLoop = 1 TO NUM-ENTRIES(cKOrdre_Id_Lst):
  FIND KOrdreHode NO-LOCK WHERE 
    KOrdreHode.KOrdre_Id = DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst)) NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN 
    NEXT.
  
  IF KOrdreHode.SendingsNr = 'RETUR' THEN 
  BEHANDLE_RETUR:
  DO:
    
  END.
  
  ELSE 
  BEHANDLE_UTLEVERING:
  DO:
    /* Her skal BARE linjer hvor vare er byttet behandles. Slike linjer har KopiKOrdreLinjeNr > 0.    */
    /*   - Den opprinnelige varen skal tilbake til der hvor den kom fra. Plukkbutikk og PASSIV linje. */
    /*   - Den nye varen skal hentes fra plukkbutikk og AKTIV linje .                                 */
    RUN opprett_temp_overforingsordre (DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst))).    
  END.
  
    
/*-------------
  /* Utlevering av RETUR ordre hvor det ikke er byttet vare på linjene.                     */
  /* Varer skal tilbake til lager (16 fra 15), eller til den butikk hvor retur ble mottatt. */
  IF KOrdreHode.SendingsNr = 'RETUR' AND 
    NOT CAN-FIND(FIRST KOrdreLinje WHERE 
              KOrdreLinje.KOrdre_Id = DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst)) AND 
              KOrdreLinje.Aktiv = FALSE) THEN
  DO: 
    RUN opprett_temp3_overforingsordre (DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst))).
  END.
  /* Utlevering av RETUR ordre hvor det er byttet vare på linjene.                           */
  /* Varer skal tilbake til lager (16 fra 15), eller til den butikk hvor retur ble mottatt.  */
  /* Behandling av AKTIVE linjer:*/
  /*     -AKTIVE linjer skal overføres til butikk som har mottatt returen.*/
  /* Behandling av PASSIVE linjer:*/
  /*     -Passive linjer som er byttet på retur ordren skal returneres til */ 
  ELSE IF KOrdreHode.SendingsNr = 'RETUR' THEN   
  DO:
    
  END.
  
  /* Ved import fra Nettbutikk.                                                   */
  /* Kundeordre importeres og varer flyttes fra nettbutikks lager til nettbutikk. */
  /* Her forekommer ikke at linjene er korrigert.                                 */
  ELSE IF bBytt = FALSE THEN 
  DO:
    /* Her forekommer ikke korreksjon av ordre, men bare aktive linjer behandles. */
    RUN opprett_temp_overforingsordre (DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst))).    
  END.
  
  /* Ved MAKULERING - varene returneres - Til og fra skal byttes om. */
  /* Fra ordreimport PHX kan det komme Cancel ordre her.             */
  ELSE IF bBytt = TRUE THEN 
  DO:
    /* Her kan ordren være korrigert. Bare aktive linjer skal føres tilbake til lager. */
    RUN opprett_temp_overforingsordre (DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst))).    
  END.
  
  /* ORDRE KORREKSJON NB: bBytt = ? - det er byttet varer på en eller flere ordrelinjer. */
  ELSE IF CAN-FIND(FIRST KOrdreLinje WHERE 
              KOrdreLinje.KOrdre_Id = DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst)) AND 
              KOrdreLinje.Aktiv = FALSE) THEN
  DO: 
    /* Bare de linjene som er korrigert skal behandles.         */
    /* Her opprettes to sett med linjer (To overføringsordrer). */
    /*  -- Varer som skal til lager.                            */
    /*  -- Varer som skal hentes fra lager.                     */
    RUN opprett_temp2_overforingsordre (DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst))).
    bVareKorr = TRUE.
  END.
  ELSE DO:
  END.
--------------*/
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
        IF KOrdreHode.SendingsNr = 'RETUR' THEN 
          RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,
                             0,
                             'J' + CHR(1) + "Ordrenr. nettbutikk (RETUR)" + (IF bBytt THEN " CANCEL" ELSE "") + ": " + KOrdreHode.EkstOrdreNr,
                             wEDB-System,
                             wTabell,
                             8).
        ELSE 
          RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,
                             0,
                             'J' + CHR(1) + "Ordrenr. nettbutikk (Varekorr)" + (IF bBytt THEN " CANCEL" ELSE "") + ": " + KOrdreHode.EkstOrdreNr,
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
 
&IF DEFINED(EXCLUDE-opprett_temp2_overforingsordre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprett_temp2_overforingsordre Procedure
PROCEDURE opprett_temp2_overforingsordre:
/*------------------------------------------------------------------------------
 Purpose: Overføringsordre som bare gjelder varelinjer hvor vare er bytttet.
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER lKOrdre_Id AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
  FIND KOrdreHode NO-LOCK WHERE
    KOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN 
    RETURN.
    
  LINJE:
  FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
    KOrdreLinje.PlukkButikk > 0 AND 
    KOrdreLinje.Aktiv = FALSE:
  
    IF KOrdreLinje.Plukkbutikk = KOrdreHode.ButikkNr THEN NEXT LINJE.
    
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT LINJE.
    
    IF NOT CAN-FIND(ArtBas WHERE
      ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN NEXT LINJE.
    
    /* ------------- Tilbakfører original linje. Fra 15 - til 16 --------- */
    FIND ttt_OvBuffer WHERE
      ttt_OvBuffer.BuntNr = 999 AND
      ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND
      ttt_OvBuffer.ButikkNrFra = KOrdreLinje.PlukkButikk AND
      ttt_OvBuffer.ButikkNrTil = KOrdreHode.ButikkNr AND  
      ttt_OvBuffer.Storl       = KOrdreLinje.Storl NO-ERROR.
    IF NOT AVAILABLE ttt_OvBuffer THEN 
      DO:
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
        CREATE ttt_OvBuffer.
        ASSIGN iLinjeNr                = iLinjeNr + 1
               ttt_OvBuffer.BuntNr      = 999     
               ttt_OvBuffer.LinjeNr     = iLinjeNr
               ttt_OvBuffer.ButikkNrFra = KOrdreHode.ButikkNr
               ttt_OvBuffer.ButikkNrTil = KOrdreLinje.PlukkButikk
/*        ttt_OvBuffer.ButikkNrFra = (IF bBytt = ? THEN            */
/*                                        iFraButikk               */
/*                                    ELSE IF bBytt = FALSE THEN   */
/*                                        KOrdreLinje.PlukkButikk  */
/*                                    ELSE KOrdreHode.ButikkNr)    */
/*        ttt_OvBuffer.ButikkNrTil = (IF bBytt = ? THEN            */
/*                                        iTilbutikk               */
/*                                    ELSE IF bBytt = FALSE THEN   */
/*                                        KOrdreHode.ButikkNr      */
/*                                    ELSE KOrdreLinje.PlukkButikk)*/
                  
               ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) 
               ttt_OvBuffer.Vg          = ArtBas.Vg         
               ttt_OvBuffer.LopNr       = ArtBas.LopNr      
               ttt_OvBuffer.Merknad     = 'Nettbutikk varekorr' + KOrdreHode.EkstOrdreNr    
               ttt_OvBuffer.Storl       = KOrdreLinje.Storl
               ttt_OvBuffer.TilStorl    = KOrdreLinje.Storl
               ttt_OvBuffer.Varekost    = KOrdreLinje.Varekost.
      END.   
    ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + ABS(KOrdreLinje.Antall).     
    /* ------------------- Ferdig original ----------------------- */
    
    /* ------------------ Overfører den nye varen Fra 16 til 15 ---- */
    FIND bufKOrdreLinje NO-LOCK WHERE 
      bufKOrdreLinje.KOrdre_Id     = KOrdreLinje.KOrdre_Id AND 
      bufKOrdreLinje.KOrdreLinjeNr = KOrdreLinje.KopiKOrdreLinjeNr NO-ERROR.
    IF AVAILABLE bufKORdreLinje THEN 
    DO:
      FIND ttt_OvBuffer WHERE
        ttt_OvBuffer.BuntNr = 999 AND
        ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND
        ttt_OvBuffer.ButikkNrFra = bufKOrdreLinje.PlukkButikk AND
        ttt_OvBuffer.ButikkNrTil = KOrdreHode.ButikkNr AND  
        ttt_OvBuffer.Storl       = bufKOrdreLinje.Storl NO-ERROR.
      IF NOT AVAILABLE ttt_OvBuffer THEN 
        DO:
          FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = DECIMAL(bufKOrdreLinje.VareNr) NO-ERROR.
          CREATE ttt_OvBuffer.
          ASSIGN iLinjeNr                = iLinjeNr + 1
                 ttt_OvBuffer.BuntNr      = 999     
                 ttt_OvBuffer.LinjeNr     = iLinjeNr
                 ttt_OvBuffer.ButikkNrFra = KOrdreLinje.PlukkButikk
                 ttt_OvBuffer.ButikkNrTil = KOrdreHode.ButikkNr
/*          ttt_OvBuffer.ButikkNrFra = (IF bBytt = ? THEN               */
/*                                          iFraButikk                  */
/*                                      ELSE IF bBytt = FALSE THEN      */
/*                                          bufKOrdreLinje.PlukkButikk  */
/*                                      ELSE KOrdreHode.ButikkNr)       */
/*          ttt_OvBuffer.ButikkNrTil = (IF bBytt = ? THEN               */
/*                                          iTilbutikk                  */
/*                                      ELSE IF bBytt = FALSE THEN      */
/*                                          KOrdreHode.ButikkNr         */
/*                                      ELSE bufKOrdreLinje.PlukkButikk)*/
                    
                 ttt_OvBuffer.ArtikkelNr  = DECIMAL(bufKOrdreLinje.VareNr) 
                 ttt_OvBuffer.Vg          = ArtBas.Vg         
                 ttt_OvBuffer.LopNr       = ArtBas.LopNr      
                 ttt_OvBuffer.Merknad     = 'Nettbutikk varekorr' + KOrdreHode.EkstOrdreNr    
                 ttt_OvBuffer.Storl       = bufKOrdreLinje.Storl
                 ttt_OvBuffer.TilStorl    = bufKOrdreLinje.Storl
                 ttt_OvBuffer.Varekost    = bufKOrdreLinje.Varekost.
        END.   
      ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + ABS(KOrdreLinje.Antall).     
    END.
    /* ----------------------- Ferdig ny ------------------------- */
    
  END. /* LINJE */


END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-opprett_temp3_overforingsordre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprett_temp3_overforingsordre Procedure
PROCEDURE opprett_temp3_overforingsordre:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER lKOrdre_Id AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
  
  FIND KOrdreHode NO-LOCK WHERE
    KOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN 
    RETURN.
    
  /* 
    TN 6/6-19 Her leses alle aktive linjer. Det er disse som er utlevert, og det er disse som skal returneres.
    Ordren er allerede utlevert, og lager 15 ble da korrigert. Ved retur er det bare de aktive linjene som skal returneres. 
  */  
  LINJE:
  FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
    KOrdreLinje.PlukkButikk > 0 AND 
    KOrdreLinje.Aktiv = TRUE:
  
    IF KOrdreLinje.Plukkbutikk = KOrdreHode.ButikkNr THEN NEXT LINJE.
    
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT LINJE.
    
    IF NOT CAN-FIND(ArtBas WHERE
      ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN NEXT LINJE.
    
    FIND ttt_OvBuffer WHERE
      ttt_OvBuffer.BuntNr = 999 AND
      ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND
      ttt_OvBuffer.ButikkNrFra = KOrdreLinje.PlukkButikk AND
      ttt_OvBuffer.ButikkNrTil = KOrdreHode.ButikkNr AND  
      ttt_OvBuffer.Storl       = KOrdreLinje.Storl NO-ERROR.
    IF NOT AVAILABLE ttt_OvBuffer THEN 
      DO:
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
        CREATE ttt_OvBuffer.
        ASSIGN iLinjeNr                = iLinjeNr + 1
               ttt_OvBuffer.BuntNr      = 999     
               ttt_OvBuffer.LinjeNr     = iLinjeNr
               ttt_OvBuffer.ButikkNrFra = KOrdreHode.ButikkNr
               ttt_OvBuffer.ButikkNrTil = (IF KOrdreHode.iOpt1 > 0 THEN KOrdreHode.iOpt1 ELSE KOrdreLinje.PlukkButikk)       
                  
               ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) 
               ttt_OvBuffer.Vg          = ArtBas.Vg         
               ttt_OvBuffer.LopNr       = ArtBas.LopNr      
               ttt_OvBuffer.Merknad     = 'Nettbutikk retur ' + KOrdreHode.EkstOrdreNr    
               ttt_OvBuffer.Storl       = KOrdreLinje.Storl
               ttt_OvBuffer.TilStorl    = KOrdreLinje.Storl
               ttt_OvBuffer.Varekost    = KOrdreLinje.Varekost.
      END.   
    ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + ABS(KOrdreLinje.Antall).     
    
  END. /* LINJE */

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-opprett_temp4_overforingsordre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprett_temp4_overforingsordre Procedure
PROCEDURE opprett_temp4_overforingsordre:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER lKOrdre_Id AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
  
  FIND KOrdreHode NO-LOCK WHERE
    KOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN 
    RETURN.
    
  LINJE:
  FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
    KOrdreLinje.PlukkButikk > 0 AND 
    KOrdreLinje.Aktiv = TRUE:
  
    IF KOrdreLinje.Plukkbutikk = KOrdreHode.ButikkNr THEN NEXT LINJE.
    
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT LINJE.
    
    IF NOT CAN-FIND(ArtBas WHERE
      ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN NEXT LINJE.
    
    FIND ttt_OvBuffer WHERE
      ttt_OvBuffer.BuntNr = 999 AND
      ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND
      ttt_OvBuffer.ButikkNrFra = KOrdreLinje.PlukkButikk AND
      ttt_OvBuffer.ButikkNrTil = KOrdreHode.ButikkNr AND  
      ttt_OvBuffer.Storl       = KOrdreLinje.Storl NO-ERROR.
    IF NOT AVAILABLE ttt_OvBuffer THEN 
      DO:
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
        CREATE ttt_OvBuffer.
        ASSIGN iLinjeNr                = iLinjeNr + 1
               ttt_OvBuffer.BuntNr      = 999     
               ttt_OvBuffer.LinjeNr     = iLinjeNr
               ttt_OvBuffer.ButikkNrFra = KOrdreHode.ButikkNr
               ttt_OvBuffer.ButikkNrTil = (IF KOrdreHode.iOpt1 > 0 THEN KOrdreHode.iOpt1 ELSE KOrdreLinje.PlukkButikk)       
                  
               ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) 
               ttt_OvBuffer.Vg          = ArtBas.Vg         
               ttt_OvBuffer.LopNr       = ArtBas.LopNr      
               ttt_OvBuffer.Merknad     = 'Nettbutikk retur ' + KOrdreHode.EkstOrdreNr    
               ttt_OvBuffer.Storl       = KOrdreLinje.Storl
               ttt_OvBuffer.TilStorl    = KOrdreLinje.Storl
               ttt_OvBuffer.Varekost    = KOrdreLinje.Varekost.
      END.   
    ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + ABS(KOrdreLinje.Antall).     
    
  END. /* LINJE */
END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-opprett_temp_overforingsordre) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprett_temp_overforingsordre Procedure
PROCEDURE opprett_temp_overforingsordre:

	/*------------------------------------------------------------------------------
  /* UTLEVERING AV ORDRE HVOR DET ER BYTTET VARE PÅ VARELINJE.                                      */
  /* Her skal BARE linjer hvor vare er byttet behandles. Slike linjer har KopiKOrdreLinjeNr > 0.    */
  /*   - Den opprinnelige varen skal tilbake til der hvor den kom fra. Plukkbutikk og PASSIV linje. */
  /*   - Den nye varen skal hentes fra plukkbutikk og AKTIV linje .                                 */
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
  FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
    KOrdreLinje.KopiKOrdreLinjeNr > 0 AND 
    KOrdreLinje.PlukkButikk > 0:
  
    IF KOrdreLinje.Plukkbutikk = KOrdreHode.ButikkNr THEN NEXT LINJE.
    
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT LINJE.
    
    IF NOT CAN-FIND(ArtBas WHERE
      ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN NEXT LINJE.

    /* Linjer med nye varer skal hentes fra lageret. */
    IF KOrdreLinje.Aktiv = TRUE THEN 
      ASSIGN                
         piFraBut = KOrdreLinje.PlukkButikk
         piTilBut = KOrdreHode.ButikkNr
         .
    /* De opprinnelige linjene skal tilbake til lageret. */
    ELSE ASSIGN                
         piFrabut = KOrdreHode.ButikkNr
         piTilBut = KOrdreLinje.PlukkButikk
         .
    
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
               ttt_OvBuffer.ButikkNrTil = piTilbut
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







