/* Endre leveringsuker for mange artikler i Varebehlinje 
   Parametere:  <butnr> (blank eller en butikk);<pipe-sep liste over nye LevBas.uker>
              
   Opprettet: 30.08.07 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iButikkNr    AS INT    NO-UNDO.
DEF VAR cLevUkeListe AS CHAR   NO-UNDO.
DEF VAR iUke         AS INT    NO-UNDO.

ASSIGN iButikkNr    = INT(ENTRY(1,icParam,";"))
       cLevUkeListe = ENTRY(2,icParam,";")
       .
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

DO TRANSACTION ON ERROR UNDO,LEAVE:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FOR EACH VareBehLinjeTrans EXCLUSIVE-LOCK 
        WHERE VareBehLinjeTrans.VareBehNr  = DEC(ihBuffer:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
          AND VareBehLinjeTrans.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
          AND VareBehLinjeTrans.RegistrertBestilling
          AND (IF iButikkNr NE 0 THEN VareBehLinjeTrans.ButikkNr = iButikkNr ELSE TRUE)
        :
      DO ix = 1 TO NUM-ENTRIES(cLevUkeListe,"|"):
        IF ENTRY(ix,cLevUkeListe,"|") NE "" THEN DO:
          iUke = INT("20" + SUBSTR(ENTRY(ix,cLevUkeListe,"|"),3) + SUBSTR(ENTRY(ix,cLevUkeListe,"|"),1,2)).  
          IF ix = 1 AND VareBehLinjeTrans.Bestilt1 > 0 THEN
            VareBehLinjeTrans.Levuke1 = iUke.
          ELSE IF ix = 2 AND VareBehLinjeTrans.Bestilt2 > 0 THEN
            VareBehLinjeTrans.Levuke2 = iUke.
          ELSE IF ix = 3 AND VareBehLinjeTrans.Bestilt3 > 0 THEN
            VareBehLinjeTrans.Levuke3 = iUke.
          ELSE IF ix = 4 AND VareBehLinjeTrans.Bestilt4 > 0 THEN
            VareBehLinjeTrans.Levuke4 = iUke.
        END.
      END.
    END.
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

obOk = ocReturn = "".

