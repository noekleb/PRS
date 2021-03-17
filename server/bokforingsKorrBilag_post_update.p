/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: Ovbuffer */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Delete, Create or Update  */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cFields       AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR cFieldValues  AS CHAR  NO-UNDO.   
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cFelt AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVerdi AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iKontoNr AS INTEGER NO-UNDO.

cFields      = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE).
cFieldValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE).

IF (ihBuffer:AVAIL OR cFields = '') AND CAN-DO('Create,Update',icAction) THEN 
DO:
  IF CAN-DO('61,62',STRING(ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE)) AND ihBuffer:BUFFER-FIELD("KontoNr"):BUFFER-VALUE = 0 THEN 
  DO:
    ocValue = '**Kontonr må angis på innbetaling og utleggsbilag.'.
    RETURN.
  END.
  IF NOT CAN-DO('61,62',STRING(ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE)) AND ihBuffer:BUFFER-FIELD("KontoNr"):BUFFER-VALUE <> 0 THEN 
  DO:
    ocValue = '**Kontonr kan bare angis på innbetaling og utleggsbilag.'.
    RETURN.
  END.
  IF ihBuffer:BUFFER-FIELD("Belop"):BUFFER-VALUE = 0 THEN
  DO:
    ocValue = '**Beløp må angis.'.
    RETURN.
  END.
  
  FIND BokforingsBilag NO-LOCK WHERE 
    BokforingsBilag.BokforingsID = ihBuffer:BUFFER-FIELD("BokforingsId"):BUFFER-VALUE NO-ERROR.
  
  DO iLoop = 1 TO NUM-ENTRIES(cFields):
    ASSIGN 
      cFelt  = ENTRY(iLoop,cFields)
      cVerdi = ENTRY(iLoop,cFieldValues,'|')
      .

    CASE cFelt:                                      
      WHEN 'BokforingsId'  THEN 
      DO:
        FIND LAST BokforingsKorrBilag NO-LOCK WHERE 
          BokforingsKorrBilag.BokForingsId = DEC(cVerdi) NO-ERROR.
        IF AVAILABLE BokforingsKorrbilag THEN 
          iLinjeNr = BokforingsKorrbilag.LinjeNr + 1.
        ELSE 
          iLinjeNr = 1.
        ihBuffer:BUFFER-FIELD("BokforingsId"):BUFFER-VALUE = DEC(cVerdi).
        ihBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE = iLinjeNr.
      END.
      WHEN 'TTId' THEN ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE = INT(cVerdi).
      WHEN 'TBId' THEN ihBuffer:BUFFER-FIELD("TBId"):BUFFER-VALUE = INT(cVerdi).
      WHEN 'EBrukerid' THEN ihBuffer:BUFFER-FIELD("EBrukerid"):BUFFER-VALUE = cVerdi.
    END CASE.
    IF AVAILABLE BokforingsBilag THEN 
    DO:
      IF ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE = 900 AND CAN-DO('28,29',STRING(ihBuffer:BUFFER-FIELD("TBId"):BUFFER-VALUE)) THEN
      DO:
        RUN settTekstOgKonto (BokforingsBilag.ButikkNr, 
                              ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE, 
                              27, /* Bruker konto for kassadiff pr. butikk */
                              OUTPUT iKontoNr
                              ).
        ASSIGN 
          ihBuffer:BUFFER-FIELD("KontoNr"):BUFFER-VALUE = iKontoNr
          .
      END. 
      IF ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE = 1 AND CAN-DO('1',STRING(ihBuffer:BUFFER-FIELD("TBId"):BUFFER-VALUE)) THEN
      DO:
        RUN settTekstOgKonto (BokforingsBilag.ButikkNr, 
                              ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE, 
                              1, 
                              OUTPUT iKontoNr
                              ).
        ASSIGN 
          ihBuffer:BUFFER-FIELD("KontoNr"):BUFFER-VALUE = iKontoNr
          .
      END. 
      /* Registrering av ekstra omsetning.*/
      IF CAN-DO('50,52,53,58',STRING(ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE)) THEN
      DO:
        RUN settTekstOgKonto (BokforingsBilag.ButikkNr, 
                              ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE, 
                              1, 
                              OUTPUT iKontoNr
                              ).
        ASSIGN 
          ihBuffer:BUFFER-FIELD("KontoNr"):BUFFER-VALUE = iKontoNr
          .
      END.
      /* Ekstra dropp*/
      IF CAN-DO('59',STRING(ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE)) THEN
      DO:
        RUN settTekstOgKonto (BokforingsBilag.ButikkNr, 
                              900, 
                              30, 
                              OUTPUT iKontoNr
                              ).
        ASSIGN 
          ihBuffer:BUFFER-FIELD("KontoNr"):BUFFER-VALUE = iKontoNr
          .
      END.
    END. 
  END.
END.

PROCEDURE settTekstOgKonto :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:  RUN settTekstOgKonto(tmpKas_Rap.Butikk,52,tmpKort_Spes.KortType,lBelop,output cChar, OUTPUT cKonto).      
                                                                                                                                                                  
        ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piButikkNr AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER piTTId     AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER piTBId     AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER piKonto    AS INTEGER   NO-UNDO. 

  ASSIGN
    piKonto = 0.
    
  RUN setSieKontoNr.p (piButikkNr, piTTId, piTbId, OUTPUT piKonto). 

/*  /* Sjekker SIETransType for butikken. */          */
/*  FIND SIETransType NO-LOCK WHERE                   */
/*    SIETRansType.ButikkNr = piButikkNr AND          */
/*    SIETransType.TTId     = piTTId AND              */
/*    SIETransType.TBId     = piTBId NO-ERROR.        */
/*  IF AVAILABLE SIETRansType THEN                    */
/*       ASSIGN                                       */
/*         piKonto = SIETRansType.KontoNr.            */
/*                                                    */
/*  /* Sjekker SIETransType for standardbutikk = 0. */*/
/*  IF (piKonto = 0) THEN                             */
/*  DO:                                               */
/*    FIND SIETransType NO-LOCK WHERE                 */
/*      SIETRansType.ButikkNr = 0 AND                 */
/*      SIETransType.TTId     = piTTId AND            */
/*      SIETransType.TBId     = piTBId NO-ERROR.      */
/*    IF AVAILABLE SIETRansType THEN                  */
/*         ASSIGN                                     */
/*         piKonto = SIETRansType.KontoNr.            */
/*  END.                                              */
/*                                                    */
/*  /* Standard kontering. */                         */
/*  IF (piKonto = 0) THEN                             */
/*  DO:                                               */
/*      FIND TransBeskr NO-LOCK WHERE                 */
/*           TransBeskr.TTId = piTTId AND             */
/*           TransBeskr.TBId = piTBId NO-ERROR.       */
/*      IF AVAILABLE TransBeskr THEN                  */
/*           ASSIGN                                   */
/*         piKonto = TransBeskr.KontoNr.              */
/*  END.                                              */
END PROCEDURE.
