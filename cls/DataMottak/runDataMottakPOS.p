DEFINE VARIABLE dDato      AS DATE NO-UNDO.
DEFINE VARIABLE cLogg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk        AS LOG       NO-UNDO.
DEFINE VARIABLE cReturn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest      AS LOG NO-UNDO.
DEFINE VARIABLE cTimeLst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTime      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTime      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cStoppFil  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iKjoreTid  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLogKat  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAntfiler AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rDataMottakPOS AS cls.DataMottak.DataMottakPOS NO-UNDO.

DEFINE TEMP-TABLE ttDataSett LIKE DataSett.

{cls\Stdfunk\File.i}  

HOVEDBLOKK:
DO  ON ERROR  UNDO, LEAVE
    ON ENDKEY UNDO, LEAVE
    ON STOP   UNDO, LEAVE
    ON QUIT   UNDO, LEAVE:

    ASSIGN 
        cLogKat  = 'log\' 
        dDato     = TODAY 
        iTime     = TIME
        cLogg     = 'DataMottakPOS' + REPLACE(STRING(TODAY),'/','')
        cTimeLst  = '' /*'23,01'*/
        bTest     = TRUE 
        cStoppFil = 'DataMottakPOS_stop.txt'
        iKjoreTid = 60 * 60 * 2 /* Kjører i to timers intervaller. */
        .
    rStandardFunksjoner  = NEW cls.Stdfunk.StandardFunksjoner( INPUT cLogg ).    
    /* Starter med tom linje i loggen. */
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Start.' 
        ).    

    /* Starter klassene. */
    rDataMottakPOS  = NEW cls.DataMottak.DataMottakPOS( INPUT cLogg ).
    rDataMottakPOS:setParent(THIS-PROCEDURE).

    cReturn = rDatamottakPOS:sjekkStopp(cStoppFil).
    IF CAN-DO('FALSE,STOPP',cReturn) THEN
    DO:
      /* Logg kjøring av datamottak. */
      RUN loggKjoringDatamottak.
      /* Avslutter */
      LEAVE HOVEDBLOKK.
    END.

    EVIGHETEN:
    DO WHILE TRUE:
        /* Bytte navn på loggfil ved datoskifte. */
        cLogg = 'DataMottakPOS' + REPLACE(STRING(TODAY),'/','').
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Ny runde for sjekk av bonger.'  
            ).    

        /* Kjører i intervaller */
        IF (iTime - TIME) > iKjoretid OR SEARCH(cStoppFil) <> ? THEN 
        DO: 
          IF SEARCH(cStoppFil) <> ? THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Stoppfil finnes. Kjøring avbrutt (' + cStoppFil +  ').' 
                ).    
          ELSE
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Intervallstopp (Kjøretid: ' + STRING(iTime - TIME,"HH:MM:SS") + ').' 
                ).    
          LEAVE EVIGHETEN.
        END.

        /* Henter liste med filer fra POS. */
        rDataMottakPOS:byggFilListe( INPUT-OUTPUT DATASET dsFile ).
        /* Tar bort poster fra listen som ikke fyller kravene. */
        rDataMottakPOS:rensFilListe( INPUT-OUTPUT DATASET dsFile ).

        IF bTest AND SEARCH('tnc.txt') <> ? THEN 
          DATASET dsFile:WRITE-JSON ("file", cLogKat + 'PRSJournalLst.' + REPLACE(STRING(TODAY),'/','') + 'JSon',TRUE).

        IF CAN-FIND(FIRST ttFile) THEN
        DO: 
          /* Teller og loggger antall filer som skal leses inn. */
          RUN tellAntallFiler(OUTPUT iAntfiler).

          /* Leser inn filene i listen. */
          rDataMottakPOS:lesInnFiler( INPUT-OUTPUT DATASET dsFile ).
        END.
        /* Logg kjøring av datamottak. */
        RUN loggKjoringDatamottak.
        
        /* Bygg liste med ikke overførte datasett. */
        rDataMottakPOS:byggListeDatasett( INPUT-OUTPUT TABLE ttDataSett ).

        /* Overfører datasettene og flagger filen det kom fra som overført. */
        rDataMottakPOS:overforDataSett ( INPUT-OUTPUT TABLE ttDataSett ).

        /* Sjekker om jobben skal stoppes, eller om den kjører via AppServer.        */ 
        /* NB: Her skal AppSrv være med i listen. Loopen ligger nå kallende program. */
        cReturn = rDatamottakPOS:sjekkStopp(cStoppFil).
        IF CAN-DO('FALSE,STOPP,AppSrv,TEST',cReturn) THEN 
          LEAVE HOVEDBLOKK.

        /* Pauser litt for å la server puste litt. */
        PAUSE 1 NO-MESSAGE.
    END. /* EVIGHETEN */

	CATCH e1 AS Progress.Lang.AppError:
    	DO ix = 1 TO e1:NumMessages:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  AppError: ' + e1:GetMessage(ix) 
                ).    
    	END.
    
    	IF e1:ReturnValue > "" THEN
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Returverdi: ' + e1:ReturnValue 
                ).    
	END CATCH.
	CATCH e2 AS Progress.Lang.Error:
		DO ix = 1 TO e2:NumMessages:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Lang.Error: ' + e2:GetMessage(ix) 
                ).    
		END.
    END CATCH.
END. /* HOVEDBLOKK */

FINALLY.
    /*    IF VALID-OBJECT(rTemp) THEN DELETE OBJECT rTemp NO-ERROR.*/
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Tidsbruk ' + STRING(TIME - iTime,'HH:MM:SS') 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Ferdig.' 
        ).    
    RETURN cReturn.
END FINALLY.


/* **********************  Internal Procedures  *********************** */

PROCEDURE tellAntallFiler:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER piAntFiler AS INTEGER NO-UNDO.
  
  FOR EACH ttFile:
    piAntFiler = piantfiler + 1.
  END.
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Antall filer ' + STRING(piantFiler) 
      ).    

END PROCEDURE.

PROCEDURE bongFeilVedImport:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcErrTekst AS CHARACTER NO-UNDO.
  
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      ' + pcErrTekst   
      ).    

END PROCEDURE.

PROCEDURE NyFilLogg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER plFilId AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pcTekst AS CHARACTER NO-UNDO. 
  
  RUN bongFeilVedImport ('FilId: ' + STRING(plFilId) + ' ' + pcTekst).

END PROCEDURE.

PROCEDURE Telleverk:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcTekst AS CHARACTER NO-UNDO.
  
  RUN bongFeilVedImport (pcTekst).

END PROCEDURE.

PROCEDURE loggKjoringDatamottak:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE BUFFER bufSyspara FOR sysPara.

  DO FOR bufSysPara TRANSACTION:
    FIND bufSysPara EXCLUSIVE-LOCK WHERE
      bufSysPara.SysHId = 200 AND
      bufSysPara.SysGr  = 1 AND
      bufSysPara.ParaNr = 3 NO-WAIT NO-ERROR.
    IF AVAILABLE bufSysPara AND NOT LOCKED bufsysPara THEN
    DO:
      ASSIGN 
        bufSysPara.Parameter1 = STRING(NOW,"99/99/9999 HH:MM:SS")
        .
      RELEASE bufSysPara.      
    END.
    ELSE DO:
      IF LOCKED bufSysPara THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '      **Får ikke logget siste kjøring i SysPara.'   
            ).    
      ELSE  
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '      **Finner ikke syspara 200 1 3. Får ikke logget siste kjøring.'   
            ).    
    END.
  END.

END PROCEDURE.
