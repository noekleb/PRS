/* Sjekk om leverandørinndelinger til artikkel er bestilt på i varehåndteringsbok
   Parametere:  <ArtikkelNr>|Vareboknr|<Liste over rowid'er for leverandørinndelinger>
            - Parameter 2 (Y/N): Skal inndelinger som ikke ligger i listen fjernes
   Opprettet: 25.07.05 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fArtikkelNr AS DEC    NO-UNDO.
DEF VAR fVarebokNr  AS DEC    NO-UNDO.
DEF VAR cRowIdList  AS CHAR   NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cDeleteRows AS CHAR   NO-UNDO.

ASSIGN fArtikkelNr = DEC(ENTRY(1,icParam,"|"))
       fVarebokNr  = DEC(ENTRY(2,icParam,"|"))
       cRowIdList  = ENTRY(3,icParam,"|")
       .

FOR EACH ArtSort EXCLUSIVE-LOCK
    WHERE ArtSort.ArtikkelNr = fArtikkelNr
   ,FIRST LevSort OF ArtSort NO-LOCK:
  IF CAN-DO(cRowIdlist,STRING(ROWID(LevSort))) THEN DO:
    FOR EACH VareBehHode NO-LOCK
        WHERE VareBehHode.Kilde = fVarebokNr
       ,EACH VareBehLinjeTrans NO-LOCK
             OF VareBehHode
             WHERE VareBehLinjeTrans.ArtikkelNr = fArtikkelNr
               AND VareBehLinjeTrans.Kode = LevSort.SortID
       :
      IF VareBehLinjeTrans.Bestilt1 > 0 OR
         VareBehLinjeTrans.Bestilt2 > 0 OR
         VareBehLinjeTrans.Bestilt3 > 0 OR
         VareBehLinjeTrans.Bestilt4 > 0 THEN
        ocReturn = ocReturn + STRING(VareBehLinjeTrans.ButikkNr) + CHR(10).
      cDeleteRows = cDeleteRows + (IF cDeleteRows NE "" THEN "," ELSE "") + STRING(ROWID(VareBehLinjeTrans)).
    END.
  END.
END.

IF cDeleteRows NE "" THEN
  ocReturn = cDeleteRows + (IF ocReturn NE "" THEN "|" + ocReturn ELSE "").

