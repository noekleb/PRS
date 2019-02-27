/* Beregner sum verdi (så langt) for en vareh.bok, messe, for en butikk 
   Parametere:  <varebehnr>|<butikknr>
                
   Opprettet: 26.07.05 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR fTotal      AS DEC    NO-UNDO.
DEF VAR iAntFord    AS INT    NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER VareBehLinjeTrans:HANDLE
                  ,BUFFER VarebehLinje:HANDLE
                  ).
hQuery:QUERY-PREPARE("FOR EACH VareBehLinjeTrans NO-LOCK " 
                     + " WHERE VareBehLinjeTrans.VarebehNr = " + ENTRY(1,icParam,"|") 
                     + (IF ENTRY(2,icParam,"|") NE "" THEN
                         " AND VareBehLinjeTrans.ButikkNr = " + ENTRY(2,icParam,"|")
                        ELSE "")
                     + " AND RegistrertBestilling"
                     + " AND GodkjentBestilling"
/*                      + " AND (Bestilt1 NE 0 OR Bestilt2 NE 0 OR Bestilt3 NE 0 OR Bestilt4 NE 0)" */
                     + ",FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK"
                     + (IF ENTRY(3,icParam,"|") NE "*" THEN 
                         " WHERE CAN-DO('" + ENTRY(3,icParam,"|") + "',STRING(VarebehLinje.Levnr))"
                        ELSE "")
                     ).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  iAntFord = 0.
  FOR EACH ArtSort NO-LOCK
      WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
        AND ArtSort.SortId     = VarebehLinjeTrans.Kode
     ,FIRST LevSort OF ArtSort NO-LOCK:

    FOR EACH LevSAnt OF LevSort NO-LOCK:
      iAntFord      = iAntFord + LevSAnt.SoAnt.
    END.
  END.

  fTotal = fTotal + (VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * VareBehLinje.VareKost * MAX(1,iAntFord).

  hQuery:GET-NEXT().
END.

ASSIGN ocReturn = STRING(fTotal)
       obOk     = TRUE.

DELETE OBJECT hQuery.
