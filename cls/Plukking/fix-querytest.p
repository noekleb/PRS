    DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO.
    DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
    DEFINE VARIABLE hBuffArtBas  AS HANDLE NO-UNDO.
    DEFINE VARIABLE hBuffStrKonv AS HANDLE NO-UNDO.
    DEFINE VARIABLE hBuffStrTStr AS HANDLE NO-UNDO.
    DEFINE VARIABLE hBuffPlListeHode AS HANDLE NO-UNDO.
    DEF VAR hBuffStrekkode AS HANDLE NO-UNDO.
    DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
    
DEF VAR cSortering AS CHAR NO-UNDO.

cSortering = " By PlListeLinje.PlListeId By PlListeLinje.VarGr By PlListeLinje.LopNr By StrTStr.SeqNr".


CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE "PlListeLinje".
CREATE BUFFER hBuffArtBas FOR TABLE "ArtBas".
CREATE BUFFER hBuffStrKonv FOR TABLE "StrKonv".
CREATE BUFFER hBuffStrTStr FOR TABLE "StrTStr".
CREATE BUFFER hBuffStrekkode FOR TABLE "Strekkode".

hQuery:SET-BUFFERS(hBuffer,hBuffArtBas,hBuffStrKonv,hBuffStrTStr,hBuffStrekkode).
        
        
DEFINE VARIABLE cPlListeIdLst AS CHARACTER NO-UNDO.

FOR EACH PlListeHode NO-LOCK WHERE 
    PlListeHode.PlListeId > 0 AND 
    PlListeHode.PlListeStatus  = 10 AND 
    PlListeHode.SendtPda      >= 01/01/2017 AND 
    PlListeHode.DatoPlukket    = ?:
    
    cPlListeIdLst = cPlListeIdLst + 
                    (IF cPlListeIdLst <> '' THEN ',' ELSE '') + 
                    STRING(PlListeHode.PlListeId).                  
END. 


IF cPlListeIdLst > '' THEN 
LOOPBLOCK:
DO iLoop = 1 TO NUM-ENTRIES(cPlListeIdLst):
    hQuery:QUERY-PREPARE("For each PlListeLinje no-lock where PlListeLinje.PlListeId = '" + ENTRY(iLoop,cPlListeIdLst) + "' " 
        + ",first Artbas no-lock where ArtBas.ArtikkelNr = PlListeLinje.ArtikkelNr OUTER-JOIN"
        + ",first StrKonv no-lock where StrKonv.StrKode = PlListeLinje.StrKode  OUTER-JOIN"
        + ",first StrTStr no-lock where StrTStr.StrTypeId = ArtBas.StrTypeId and StrTStr.SoStorl = StrKonv.Storl OUTER-JOIN"
        + ",first Strekkode no-lock where Strekkode.Artikkelnr = PlListeLinje.ArtikkelNr and Strekkode.StrKode = PlListeLinje.StrKode OUTER-JOIN"
        + cSortering
        ).

                MESSAGE 'test-1'
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    
    hQuery:QUERY-OPEN().

    MESSAGE 'test-2'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    hQuery:GET-FIRST().

    MESSAGE hBuffer:AVAIL
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.        
