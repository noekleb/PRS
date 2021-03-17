&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : sbudmalhode_generer.p 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
    RUN sbudmalhode_generer.p (hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('MalId'):BUFFER-VALUE,
                               hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ButikkNr'):BUFFER-VALUE,
                               piButikkNr,
                               hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Aar'):BUFFER-VALUE,
                               piAar) NO-ERROR.
        
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN    
    DEFINE VARIABLE iMalId     AS INTEGER INIT 1    NO-UNDO.
    DEFINE VARIABLE iMalButNr  AS INTEGER INIT 4    NO-UNDO.
    DEFINE VARIABLE iStatButNr AS INTEGER INIT 4 NO-UNDO.
    DEFINE VARIABLE iTilAar    AS INTEGER INIT 2012 NO-UNDO.
    DEFINE VARIABLE iFraAar    AS INTEGER INIT 2011 NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER iMalId     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iMalButNr  AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iStatButNr AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iTilAar    AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iFraAar    AS INTEGER NO-UNDO.
&ENDIF 

DEFINE VARIABLE iwdFra  AS INTEGER NO-UNDO.
DEFINE VARIABLE iwdTil  AS INTEGER NO-UNDO.
DEFINE VARIABLE iOffset AS INTEGER NO-UNDO.
DEFINE VARIABLE iPerLinNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop   AS INTEGER NO-UNDO.
DEFINE VARIABLE dDato   AS DATE    NO-UNDO.
DEFINE VARIABLE iAntDag AS INT     NO-UNDO.
DEFINE VARIABLE dFraDato   AS DATE    NO-UNDO.
DEFINE VARIABLE dTilDato   AS DATE    NO-UNDO.
DEFINE VARIABLE iAarMnd    AS INTEGER NO-UNDO.
DEFINE VARIABLE iAarMndDag AS INTEGER NO-UNDO.      
DEFINE VARIABLE lTotSalgBudsjett AS DECIMAL NO-UNDO.
DEFINE VARIABLE lTotDbBudsjett   AS DECIMAL NO-UNDO.
DEFINE VARIABLE lTotMndSalgBudsjett AS DECIMAL NO-UNDO.
DEFINE VARIABLE lTotMndDbBudsjett   AS DECIMAL NO-UNDO.
DEFINE VARIABLE lSalgSum% AS DECIMAL NO-UNDO.
DEFINE VARIABLE lDbSnitt% AS DECIMAL NO-UNDO.
DEFINE VARIABLE lMndSalgSum% AS DECIMAL NO-UNDO.

DEFINE TEMP-TABLE tmpSBudMalDag NO-UNDO LIKE SBudMalDag 
  FIELD SalgBudsjett AS DECIMAL 
  FIELD DbBudsjett   AS DECIMAL 
  FIELD Dato         AS DATE 
  .

DEFINE TEMP-TABLE tmpSBudMalManed NO-UNDO LIKE SBudMalManed 
  FIELD SalgBudsjett AS DECIMAL 
  FIELD DbBudsjett   AS DECIMAL 
  .

FIND SBudMalHode NO-LOCK WHERE 
    SBudMalHode.MalId    = iMalId AND 
    SBudMalHode.ButikkNr = iMalButNr AND
    SBudMalHode.Aar      = iTilAar NO-ERROR.
IF NOT AVAILABLE SBudMalHode THEN
 DO TRANSACTION:
     CREATE SBudMalHode.
     ASSIGN SBudMalHode.MalId          = iMalId
            SBudMalHode.ButikkNr       = iMalButNr
            SBudMalHode.Aar            = iTilAar
            SBudMalHode.MalBeskrivelse = "Generert tomt budsjett"
            SBudMalHode.MalNotat       = "Generert tomt budsjett".
     FIND CURRENT SBudMalHode NO-LOCK.
 END.

RUN SetStartUkedag.
RUN byggBudsjettMal.
RUN lagreBudsjettMal.


/* **********************  Internal Procedures  *********************** */

 
&IF DEFINED(EXCLUDE-byggBudsjettMal) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggBudsjettMal Procedure
PROCEDURE byggBudsjettMal:
    /*------------------------------------------------------------------------------
            Purpose:  																	  
            Notes:  																	  
    ------------------------------------------------------------------------------*/
	
    /*      Beregningsmåte  - Leser alle dager i det gamle året.                    */
    /*      - Klarlegge ukedagsnr på 1/1 året vi leser statistikk fra.              */
    /*      - Klarlegge ukedagsnr på 1/1 i det året vi bygger mal FOR.              */
    /*      - Finne offsett som gjør AT vi kan postere salg på riktig ukedag.       */
    /*      - Ved hjelp av offset, gå igjennom hele året og overføre salgKr og dbkr.*/
    /*      - Etter AT alle dagene er overført                                      */
    /*        - Summer opp salg og db kroner i TO totaler.                          */
    /*        - Gå igjennom alle dagene i den genererte malen, og regn ut % satsene.*/
    /*        Utfordringer                    */
    /*        - Fra / til skuddår/ikke skuddår*/
    /*        - bevegelige helligdager        */
    
         
/* Leser hele året og bygger SBudMalDag. */      
STLOOP:      
FOR EACH StLinje WHERE 
    StLinje.Butik    = iStatButNr AND 
    StLinje.StTypeId = "BUTSTAT" AND 
    StLinje.PerId    = "DAG" AND
    INT(SUBSTRING(STRING(StLinje.AarPerLin),1,4)) = iFraAar AND 
    StLinje.PerLinNr >= iPerLinNr
    BREAK BY StLinje.Butik
          BY StLinje.StTypeId
          BY StLinje.PerId
          BY StLinje.AarPerLinNr:

    /* Feiler tildato pga. 29 feb (Skuddår), så hopper vi over denne dagen */
    ASSIGN
        dFraDato   = (DATE(1,1,StLinje.Aar) - 1) + StLinje.PerLinNr
        dTilDato   = dFraDato - iOffset
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      NEXT STLOOP.
        
    /* NB: Legg inn logikk her for manipulering av tildato i forhold til helligdager. */
    /* Søndagene fikses av Offset beregningen.                                        */    
        
    ASSIGN 
        iAarMnd    = INT(STRING(iTilAar) + STRING(MONTH(dTilDato),"99")) 
        iAarMndDag = INT(STRING(iTilAar) + STRING(MONTH(dTilDato),"99") + STRING(DAY(dTilDato),"99")) 
        .      
    FIND tmpSBudMalDag EXCLUSIVE-LOCK WHERE 
            tmpSBudMalDag.MalId     = iMalId
        AND tmpSBudMalDag.AarMnd    = iAarMnd 
        AND tmpSBudMalDag.AarMndDag = iAarMndDag NO-ERROR.
    IF NOT AVAILABLE tmpSBudMalDag THEN
    DO:
        CREATE tmpSBudMalDag.
        ASSIGN 
            tmpSBudMalDag.MalId        = iMalId
            tmpSBudMalDag.AarMnd       = iAarMnd
            tmpSBudMalDag.AarMndDag    = iAarMndDag
            tmpSBudMalDag.SalgBudsjett = StLinje.VerdiSolgt
            tmpSBudMalDag.DbBudsjett   = StLinje.VerdiSolgt - StLinje.VVarekost /*StLinje.DbKr*/
            tmpSBudMalDag.Dato         = dTilDato
            .
    END.
END. /* STLOOP */

/* Summerer totalen for året. */
FOR EACH tmpSBudMalDag:
    ASSIGN
        lTotSalgBudsjett = lTotSalgBudsjett + tmpSBudMalDag.SalgBudsjett
        lTotDbBudsjett   = lTotDbBudsjett   + tmpSBudMalDag.DbBudsjett
        .
END.

/* Beregner Salg% og db% for måned og oppretter SBudMalMnd. */
FOR EACH tmpSBudMalDag EXCLUSIVE-LOCK WHERE 
    tmpSBudMalDag.MalId     = iMalId 
    BREAK BY tmpSBudMalDag.MalId
          BY tmpSBudMalDag.AarMnd
          BY tmpSBudMalDag.AarMndDag:

    ASSIGN
        lTotMndSalgBudsjett = lTotMndSalgBudsjett + tmpSBudMalDag.SalgBudsjett 
        lTotMndDbBudsjett   = lTotMndDbBudsjett   + tmpSBudMalDag.DbBudsjett.
   
    IF LAST-OF(tmpSBudMalDag.AarMnd) THEN 
    DO:
      /* Oppretter og regner ut månedsrecord. */
      CREATE tmpSBudMalManed.
      ASSIGN 
          tmpSBudMalManed.MalId        = iMalId
          tmpSBudMalManed.AarMnd       = tmpSBudMalDag.AarMnd
          tmpSBudMalManed.SalgBudsjett = lTotMndSalgBudsjett
          tmpSBudMalManed.Dbbudsjett   = lTotMndDbBudsjett
          tmpSBudMalManed.Prosent      = ROUND((lTotMndSalgBudsjett / lTotSalgBudsjett) * 100,2)
          tmpSBudMalManed.Prosent      = IF tmpSBudMalManed.Prosent = ? THEN 0 ELSE tmpSBudMalManed.Prosent
          tmpSBudMalManed.DbProsent    = ROUND((tmpSBudMalManed.Dbbudsjett / tmpSBudMalManed.SalgBudsjett) * 100,2)
          tmpSBudMalManed.DbProsent    = IF tmpSBudMalManed.DbProsent = ? THEN 0 ELSE tmpSBudMalManed.DbProsent
          .
      ASSIGN
          lTotMndSalgBudsjett = 0
          lTotMndDbBudsjett   = 0.
    END. 
END.

ASSIGN
  lMndSalgSum% = 0.
/* Beregner Salg% og db% for dag og oppretter SBudMalDag. */
FOR EACH tmpSBudMalManed NO-LOCK WHERE
    tmpSBudMalManed.MalId = iMalId
    BREAK BY tmpSBudMalManed.MalId
          BY tmpSBudMalManed.AarMnd:

    ASSIGN 
        lSalgSum% = 0
        lDbSnitt% = 0
        iAntDag   = 0
        tmpSBudMalManed.DbBudsjett = 0
        .
              
    FOR EACH tmpSBudMalDag WHERE 
        tmpSBudMalDag.MalId  = iMalId AND 
        tmpSBudMalDag.AarMnd = tmpSBudMalManed.AarMnd
        BREAK BY tmpSBudMalDag.MalId
              BY tmpSBudMalDag.AarMnd
              BY tmpSBudMalDag.AarMndDag:
        ASSIGN
            tmpSBudMalDag.Prosent   = ROUND((tmpSBudMalDag.SalgBudsjett / tmpSBudMalManed.SalgBudsjett) * 100,2)
            tmpSBudMalDag.Prosent   = IF tmpSBudMalDag.Prosent = ? THEN 0 ELSE tmpSBudMalDag.Prosent 
            tmpSBudMalDag.DbProsent = ROUND((tmpSBudMalDag.DbBudsjett / tmpSBudMalDag.SalgBudsjett) * 100,2) 
            tmpSBudMalDag.DbProsent = IF tmpSBudMalDag.DbProsent = ? THEN 0 ELSE tmpSBudMalDag.DbProsent
            lSalgSum%               = lSalgSum% + tmpSBudMalDag.Prosent
            lDbSnitt%               = lDbSnitt% + tmpSBudMalDag.DbProsent
            iAntDag                 = iAntDag   + (IF tmpSBudMalDag.Prosent > 0 THEN 1 ELSE 0)
            tmpSBudMalManed.DbBudsjett = tmpSBudMalManed.DbBudsjett + tmpSBudMalDag.DbBudsjett   
            .    
        /* Korrigerer for avrundingsfeil. Belaster siste post. */
        IF LAST(tmpSBudMalDag.AarMnd) THEN
        DO:
          ASSIGN           
              lDbSnitt%               = ROUND(lDbSnitt% / iAntDag,2)
              lDbSnitt%               = IF lDbSnitt% = ? THEN 0 ELSE lDbSnitt%
              . 
          IF lSalgSum% > 0 THEN 
              tmpSBudMalDag.Prosent   = tmpSBudMalDag.Prosent   + (100 - lSalgSum%).
        END.  
    END. 
    IF lDbSnitt% > 0 THEN   
        tmpSBudMalManed.DbProsent = lDbSnitt%.

    ASSIGN
        lMndSalgSum% = lMndSalgSum% + tmpSBudMalManed.Prosent.
        
    IF LAST(tmpSBudMalManed.AarMnd) AND lMndSalgSum% > 0 THEN 
        tmpSBudMalManed.Prosent = tmpSBudMalManed.Prosent + (100 - lMndSalgSum%). 
        
END. 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-DelMalManODag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelMalManODag Procedure 
PROCEDURE DelMalManODag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH SBudMalManed WHERE SBudMalManed.MalId = iMalId:
    DELETE SBudMalManed.
END.
FOR EACH SBudMalDag WHERE SBudMalDag.MalId = iMalId:
    DELETE SBudMalDag.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
        
 
 
&IF DEFINED(EXCLUDE-lagreBudsjettMal) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagreBudsjettMal Procedure
PROCEDURE lagreBudsjettMal:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
RUN DelMalManODag.	
RUN opprettTommePoster.
FOR EACH tmpSBudMalManed:
    CREATE SBudMalManed.
    BUFFER-COPY tmpSBudMalManed TO SBudMalManed.
END.
FOR EACH tmpSBudMalDag:
    CREATE SBudMalDag.
    BUFFER-COPY tmpSBudMalDag TO SBudMalDag.
END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
 
&IF DEFINED(EXCLUDE-opprettTommePoster) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettTommePoster Procedure
PROCEDURE opprettTommePoster:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
ASSIGN 
    iAntDag = DATE(12,31,iTilAar) - DATE(12,31,iTilAar - 1).

DO iLoop = 1 TO iAntDag: 
    ASSIGN 
        dDato      = DATE(1,1,iTilAar)   + iLoop - 1
        iAarMnd    = INT(STRING(iTilAar) + STRING(MONTH(dDato),"99")) 
        iAarMndDag = INT(STRING(iTilAar) + STRING(MONTH(dDato),"99") + STRING(DAY(dDato),"99")) 
        .      
    
    IF NOT CAN-FIND(tmpSBudMalDag WHERE 
        tmpSBudMalDag.MalId     = iMalId AND
        tmpSBudMalDag.AarMnd    = iAarMnd AND 
        tmpSBudMalDag.AarMndDag = iAarMndDag) THEN 
    DO:
        CREATE tmpSBudMalDag.
        ASSIGN 
            tmpSBudMalDag.MalId     = iMalId
            tmpSBudMalDag.AarMnd    = iAarMnd
            tmpSBudMalDag.AarMndDag = iAarMndDag.
    END.
    IF NOT CAN-FIND(tmpSBudMalManed WHERE 
        tmpSBudMalManed.MalId     = iMalId AND
        tmpSBudMalManed.AarMnd    = iAarMnd) THEN 
    DO:
        CREATE tmpSBudMalManed.
        ASSIGN 
            tmpSBudMalManed.MalId     = iMalId
            tmpSBudMalManed.AarMnd    = iAarMnd.
    END.
END. 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-SetStartUkedag) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetStartUkedag Procedure
PROCEDURE SetStartUkedag:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
    DEFINE VARIABLE iwdFra  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iwdTil  AS INTEGER NO-UNDO. 
    DEFINE VARIABLE iOffset AS INTEGER NO-UNDO.
    define var      cDagLst as char no-undo.
    def var iFraAar as int initial 2011 no-undo.
    def var iTilAar as int initial 2014 no-undo.
	------------------------------------------------------------------------------*/
  DEFINE VARIABLE cDagLst AS CHAR NO-UNDO.

  ASSIGN      /* 1 2 3 4 5 6 7 */
      cDagLst = '7,1,2,3,4,5,6'
      iwdFra  = INT(ENTRY(WEEKDAY(DATE(1,1,iFraAar)),cDagLst))
      iwdTil  = INT(ENTRY(WEEKDAY(DATE(1,1,iTilAar)),cDagLst))
      iOffset = iwdTil - iwdFra.
  
  /* Finner hvor mange dager vi må gå inn i fra året for å få samme ukedag som 1/1 i til året. */    
  DO dDato = DATE(1,1,iFraAar) TO DATE(1,1,iFraAar) + 10:
    iwdFra = INT(ENTRY(WEEKDAY(dDato),cDagLst)).
    IF iwdFra = iwdTil THEN 
    DO:
        iPerLinNr = dDato - DATE(1,1,iFraAar).
    END.
  END.       
      
/*MESSAGE DATE(1,1,iFraAar) 'iwdFra' iwdFra SKIP*/
/*        DATE(1,1,iTilAar) 'iwdTil' iwdTil SKIP*/
/*        'iOffset' iOffset                     */
/*VIEW-AS ALERT-BOX.                            */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


