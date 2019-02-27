/* Oppretter størrelsestype på ArtikkelNrSerie.                               */
/* Syntaks: run opprett_strtype2.p (cEkstStrTypeNavn, cStr, cVg, OUTPUT iStrTypeId). */ 

DEFINE INPUT  PARAMETER cEkstStrTypeNavn AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER cStr             AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER cVg              AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER iStrTypeId       AS INTEGER   NO-UNDO.

DEFINE VARIABLE iSeqNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnt   AS INTEGER NO-UNDO.
DEFINE VARIABLE bNy    AS LOG     NO-UNDO.
DEFINE VARIABLE iVg    AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttStr 
  FIELD Storl   AS CHARACTER 
  FIELD SeqNr   AS INTEGER
  INDEX Str Storl.

{incl/devmode.i}
{incl/custdevmode.i}

/* Renser temp-tabell. */
FOR EACH ttSTr:
  DELETE ttSTr.
END.

/* Er det ugyldig varegruppe, settes 0. */
ASSIGN
  iVg = DECIMAL(cVg) NO-ERROR.

/* Renser for Space */
ASSIGN
  iSeqNr           = 0 
  cEkstStrTypeNavn = TRIM(cEkstStrTypeNavn).
  
/* Blanke størrelsestyper skal ikke gjøres noe med. */
IF cEkstStrTypeNavn = '' THEN RETURN. 

DYNAMIC-FUNCTION("runproc","bibl_fixstorl.p",cSTr,?).
cSTr = CAPS(DYNAMIC-FUNCTION("getTransactionMessage")).

/* Blanke størrelser skal det ikke gjøres noe med */
IF TRIM(cSTr) = '' THEN RETURN.

/* Finnes størrelsestypen fra før, skal denne benyttes. */
FIND FIRST StrType NO-LOCK WHERE StrType.Beskrivelse = cEkstStrTypeNavn NO-ERROR.
IF AVAILABLE StrType THEN iStrTypeId = StrType.StrTypeId.
ELSE RUN OpprettStrType.

/* Ligger størrelsen ikke i størrelsestypen, skal den legges inn. */
IF NOT CAN-FIND(StrTStr WHERE
                StrTStr.StrTypeId = iStrTypeId AND 
                StrTSTr.SoStorl   = cStr) THEN 
BYGG_TMPTAB:
DO:
  FOR EACH StrTStr NO-LOCK WHERE 
    StrTStr.StrTypeId = iStrTypeId 
    BY StrTStr.StrTypeId 
    BY StrTStr.SeqNr:
  
    ASSIGN iSeqNr = StrTStr.SeqNr.
    
    FIND FIRST ttStr WHERE ttStr.SeqNr = StrTStr.SeqNr NO-ERROR.
    IF NOT AVAILABLE ttStr THEN 
    DO:
      CREATE ttStr.
      ASSIGN
        ttStr.Storl   = cStr
        ttStr.SeqNr   = StrTStr.SeqNr.
    END.
  END. 
  CREATE ttStr.
  ASSIGN
    iSeqNr      = iSeqNr + 1
    ttStr.Storl = cStr
    ttStr.SeqNr = iSeqNr.
END.  /* BYGG_TMPTAB */              

/* Legger inn størrelser som mangler i størrelsestypen. */
IF CAN-FIND(FIRST ttStr) THEN 
  RUN opprettStrTStr.

IF bNy AND iStrTypeId > 0 AND CAN-FIND(StrType WHERE StrType.StrTypeId = iStrTypeId) THEN 
  RUN settStrTypeFelt.p (iStrTypeID).
RETURN 'OK'.


/* **********************  Internal Procedures  *********************** */

PROCEDURE opprettStrTStr:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

/* Oppretter strtype */
OPPRETT:
FOR EACH ttStr BREAK 
  BY ttSTr.SeqNr:
  IF NOT CAN-FIND(FIRST StrTStr WHERE
                  StrTStr.StrTypeId = iStrTypeId AND
                  StrTStr.SeqNr     = ttSTr.SeqNr) THEN 
  DO:
    CREATE StrTStr.
    ASSIGN
      bNy               = TRUE 
      StrTStr.STrTypeId = iStrTypeId
      StrTStr.SeqNr     = ttSTr.SeqNr
      StrTStr.SoStorl   = ttStr.Storl
      .
  END.
END. /* OPPRETT */
END PROCEDURE.

PROCEDURE OpprettStrType:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

DO ON ERROR UNDO, LEAVE TRANSACTION:  
  CREATE StrType.
  ASSIGN
    bNy                 = TRUE
    StrType.Beskrivelse = cEkstStrTypeNavn
    iStrTypeId          = StrType.StrTypeId.
    
  IF iVg > 0 AND CAN-FIND(VarGr WHERE VarGr.Vg = iVg) THEN 
  DO:
    FIND VarGr NO-LOCK WHERE VarGr.Vg = iVg NO-ERROR.
    IF AVAILABLE VarGr THEN 
      ASSIGN StrType.Hg = VarGr.Hg.

    IF AVAILABLE VarGr THEN FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
    IF AVAILABLE HuvGr THEN 
      ASSIGN StrType.AvdelingNr = HuvGr.AvdelingNr.
  END.  
  FIND CURRENT StrType NO-LOCK.
END. /* TRANSACTION*/

END PROCEDURE.
