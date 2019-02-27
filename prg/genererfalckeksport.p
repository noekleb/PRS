/* Genererer Falck på bakgrunn av Falck_Sykkelregister.

  RUN genererfalckeksport.p
                          (DATE(FraDato:SCREEN-VALUE),
                           DATE(TilDato:SCREEN-VALUE),
                           cButiLst,
                           INPUT-OUTPUT iAntLest,
                           INPUT-OUTPUT iAntPostert,
                           OUTPUT cMsgs).
*/

DEF INPUT PARAMETER dFraDato AS DATE      NO-UNDO.
DEF INPUT PARAMETER dTilDato AS DATE      NO-UNDO.
DEF INPUT PARAMETER cButLst  AS CHARACTER NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iAntLest     AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iAntPostert  AS INT NO-UNDO.
DEF OUTPUT PARAMETER cMsgs AS CHAR NO-UNDO.

DEF VAR iCL                AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop      AS INTEGER NO-UNDO.
DEFINE VARIABLE dDato      AS DATE    NO-UNDO.
DEFINE VARIABLE iButikkNr  AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnsattNr  AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntCust   AS INTEGER NO-UNDO.
DEFINE VARIABLE dSalesDate AS INTEGER NO-UNDO.
DEFINE VARIABLE dLinjeSum  AS DECIMAL NO-UNDO.
DEFINE VARIABLE dSelgerNr  AS INT     NO-UNDO.
DEFINE VARIABLE iKasSelg   AS INTEGER NO-UNDO.
DEFINE VARIABLE iBranVen   AS INTEGER NO-UNDO.

{syspara.i  5 1 1 iCL INT}
{syspara.i 50 21 2 iBranVen INT}
{syspara.i 50 21 1 iKasSelg INT}

/* Kontroll av butikkliste */
ASSIGN 
  cButLst = TRIM(TRIM(cButLst),',')
  cButLst = REPLACE(cButLst,'|',',').
IF cButLst = '' THEN 
  DO:
    cMsgs = '** Tom butikkliste mottatt i genererfalckeksport.p.'.
    RETURN.
  END.

/* Leser data for butikker i angitt periode. */
RUN LesButikkerOgDato.

/* **********************  Internal Procedures  *********************** */

PROCEDURE LesButikkerOgDato:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

BUTIKK_LOOP:
DO iLoop = 1 TO NUM-ENTRIES(cbutLst):
  /* Kontroll av butikknr. */
  iButikkNr = INT(ENTRY(iLoop,cButLst)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN NEXT BUTIKK_LOOP.
  IF NOT CAN-FIND(Butiker WHERE Butiker.Butik = iButikkNr) THEN NEXT BUTIKK_LOOP.
  
  FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iButikkNr NO-ERROR.
  IF AVAILABLE Butiker THEN
  BUTIKK: 
  DO:
    /* Sjekker omd et er noe å eksportere. */
    IF NOT CAN-FIND(FIRST Falck_Sykkelregister WHERE
                    Falck_Sykkelregister.ButikkNr  = Butiker.Butik AND
                    Falck_Sykkelregister.EksportId = 0 AND
                    Falck_Sykkelregister.Dato_Solgt >= dFraDato AND
                    Falck_SykkelRegister.Dato_Solgt <= dTilDato) THEN
        LEAVE BUTIKK.
    DO TRANSACTION:
      /* Id og dato/tid tildeles i trigger. */
      CREATE FalckEksport.
      ASSIGN
        FalckEksport.ButikkNr = iButikkNr 
        FalckEksport.Merknad  = 'Eksportdato ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS")
        FalckEksport.ETid     = TIME 
        FalckEksport.EDato    = TODAY 
        .
      FIND CURRENT FalckEksport NO-LOCK.
    END. /* TRANSACTION */
      
    REGISTER_LOOP:
    FOR EACH Falck_Sykkelregister EXCLUSIVE-LOCK WHERE
      Falck_Sykkelregister.ButikkNr    = iButikkNr AND 
      Falck_Sykkelregister.EksportId   = 0 AND
      Falck_Sykkelregister.Dato_Solgt >= dFraDato AND
      Falck_SykkelRegister.Dato_Solgt <= dTilDato:
    
      /* Antall leste poster. */
      ASSIGN
        iAntCust = iAntCust + 1
        iAntLest = iAntLest + 1.

      ASSIGN
          Falck_Sykkelregister.EksportId      = FalckEksport.EksportId
          Falck_Sykkelregister.SendtDato      = TODAY 
          Falck_Sykkelregister.SendtTid       = TIME  
          Falck_Sykkelregister.EksportertDato = TODAY 
          Falck_Sykkelregister.EksportertTid  = TIME  
          Falck_Sykkelregister.Kampanjekode   = Butiker.FalckMedlNr
          .

      /* Antall posterte bonger. */        
      iAntPostert = iAntPostert + 1.
      
    END. /*REGISTER_LOOP */

  END. /* BUTIKK */
END. /* BUTIKK_LOOP */

END PROCEDURE.

