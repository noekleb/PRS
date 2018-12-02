/* Registrer innleveranse fra pakkseddel
   Parameter:  Artikkelnr|VarebehNr
   Opprettet: 31.10.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fArtikkelNr     AS DEC    NO-UNDO.
DEF VAR fVarebehNr      AS DEC    NO-UNDO.
DEF VAR rRowIdLinje     AS ROWID  NO-UNDO.

ASSIGN fArtikkelNr   = DEC(ENTRY(1,icParam,"|"))
       fVarebehNr    = DEC(ENTRY(2,icParam,"|"))
       .

RUN newVarebehLinje.p (fArtikkelNr,fVarebehNr,OUTPUT rRowIdLinje) NO-ERROR.
ocReturn = RETURN-VALUE.

obOK = ocReturn = "".
IF obOk THEN
  ocReturn = STRING(rRowIdLinje).
