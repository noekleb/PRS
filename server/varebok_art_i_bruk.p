/* Sjekk om artikkel er bestilt på i varehåndteringsbok
   Parametere:  <ArtikkelNr>|Vareboknr
   Opprettet: 01.09.08 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fArtikkelNr AS DEC    NO-UNDO.
DEF VAR fVarebokNr  AS DEC    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cDeleteRows AS CHAR   NO-UNDO.

ASSIGN fArtikkelNr = DEC(ENTRY(1,icParam,"|"))
       fVarebokNr  = DEC(ENTRY(2,icParam,"|"))
       .

FOR EACH VareBehHode NO-LOCK
    WHERE VareBehHode.Kilde = fVarebokNr
   ,EACH VareBehLinjeTrans NO-LOCK
         OF VareBehHode
         WHERE VareBehLinjeTrans.ArtikkelNr = fArtikkelNr
   :
  IF VareBehLinjeTrans.Bestilt1 > 0 OR
     VareBehLinjeTrans.Bestilt2 > 0 OR
     VareBehLinjeTrans.Bestilt3 > 0 OR
     VareBehLinjeTrans.Bestilt4 > 0 THEN
    ocReturn = ocReturn + STRING(VareBehLinjeTrans.ButikkNr) + CHR(10).
  cDeleteRows = cDeleteRows + (IF cDeleteRows NE "" THEN "," ELSE "") + STRING(ROWID(VareBehLinjeTrans)).
END.

IF cDeleteRows NE "" THEN
  ocReturn = cDeleteRows + (IF ocReturn NE "" THEN "|" + ocReturn ELSE "").

