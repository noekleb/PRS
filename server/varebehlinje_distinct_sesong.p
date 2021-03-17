/* Finner unike sesonger i varehåndteringsbok 
   Parametere:  <varebehnr>
                
   Opprettet: 30.03.07 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fVarebehNr AS DEC NO-UNDO.

fVarebehNr = DEC(icParam) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

FOR EACH VarebehLinje FIELDS() NO-LOCK
    WHERE VareBehLinje.VareBehNr = fVarebehNr
   ,FIRST ArtBas FIELDS(Sasong) NO-LOCK
          OF VarebehLinje:
  IF NOT CAN-DO(ocReturn,STRING(ArtBas.SaSong)) THEN
    ocReturn = ocReturn + STRING(ArtBas.SaSong) + ",".
END.

ASSIGN ocReturn = TRIM(ocReturn,",")
       obOk     = ocReturn NE "".


