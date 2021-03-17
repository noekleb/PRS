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
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER rKOrdre_linje AS ROWID NO-UNDO.

DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE iCL    AS INTEGER NO-UNDO. 
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE bSendReservasjon AS LOG NO-UNDO.
DEFINE VARIABLE cTekst           AS CHARACTER NO-UNDO.

DEF VAR wEDB-System      AS CHARACTER  NO-UNDO.
DEF VAR wTabell          AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE ttt_OvBuffer LIKE OvBuffer.
DEFINE NEW SHARED TEMP-TABLE tt_OvBuffer NO-UNDO LIKE OvBuffer.

/* Kode for låsing av artikkelnummer ved overføring. */
{syspara.i 1 2 3 wEDB-System}
IF wEDB-System = "" THEN
  wEDB-System = "OVERFOR-LOCK".

/* Skal det sendes eMail med reservasjonsmelding */
/* {syspara.i 150 1 5 cTekst}                 */
/* IF CAN-DO('1,J,Y,Ja,Yes,True',cTekst) THEN */
/*     bSendReservasjon = TRUE.               */
/* ELSE                                       */
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

FIND KOrdrelinje WHERE ROWID(KOrdrelinje) = rKOrdre_linje NO-LOCK NO-ERROR.
IF NOT AVAIL KOrdrelinje THEN
  RETURN 'ERROR'.
RUN opprett_temp_overforingsordre.

IF CAN-FIND(FIRST ttt_OvBuffer) THEN DO:
    RUN opprett_overforingsordre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-opprett_overforingsordre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprett_overforingsordre Procedure 
PROCEDURE opprett_overforingsordre :
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
                           'J' + CHR(1) + "Reversering: " + string(KOrdreHode.KOrdre_Id),
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
/*   IF (cKOrdre_Id_Lst <> '' AND cBuntNrLst <> '') AND bSendReservasjon THEN */
/*     RUN webreservasjonKOrdrePDF.p (cKOrdre_Id_Lst, cBuntNrLst).            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-opprett_temp_overforingsordre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprett_temp_overforingsordre Procedure 
PROCEDURE opprett_temp_overforingsordre :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
/*   DEFINE INPUT PARAMETER lKOrdre_Id AS DECIMAL NO-UNDO. */
/*                                                         */
/*   DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.              */
  
  FIND KOrdreHode OF KOrdrelinje NO-LOCK NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN 
    RETURN.
    
  DO:
      FIND ArtBas WHERE ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) NO-LOCK NO-ERROR.
      CREATE ttt_OvBuffer.
      ASSIGN iLinjeNr                = iLinjeNr + 1
             ttt_OvBuffer.BuntNr      = 999     
             ttt_OvBuffer.LinjeNr     = iLinjeNr
             ttt_OvBuffer.ButikkNrFra = KOrdreHode.ButikkNr
             ttt_OvBuffer.ButikkNrTil = KOrdreLinje.PlukkButikk
             ttt_OvBuffer.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) 
             ttt_OvBuffer.Vg          = ArtBas.Vg         
             ttt_OvBuffer.LopNr       = ArtBas.LopNr      
             ttt_OvBuffer.Merknad     = 'Nettbutikk'    
             ttt_OvBuffer.Storl       = KOrdreLinje.Storl
             ttt_OvBuffer.TilStorl    = KOrdreLinje.Storl
             ttt_OvBuffer.Varekost    = KOrdreLinje.Varekost.
      ASSIGN ttt_OvBuffer.Antall = ttt_OvBuffer.Antall + KOrdreLinje.Antall.     
    
  END. /* LINJE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

