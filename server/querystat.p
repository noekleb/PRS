/* For å teste på appserver, sport1:
 
   Kompiler programmet og kjør dette fra editoren: 
   
def var hserver as handle.

create server hserver.
hserver:connect("-H 192.168.200.1 -S 20032 -AppService AppsSkoTex -DirectConnect").

run querystat.p on hserver.

*/


DEF VAR hq AS HANDLE NO-UNDO.
DEF VAR ix AS INT NO-UNDO.
DEF VAR cMsg AS CHAR NO-UNDO.

CREATE QUERY hq.

/* Ordrebekreftelse, leverandør - messebok */
/* hq:SET-BUFFERS(BUFFER VareBehLinje:HANDLE,      */
/*                BUFFER VareBehLinjeTrans:HANDLE, */
/*                BUFFER StrKonv:HANDLE,           */
/*                BUFFER artbas:HANDLE,            */
/*                BUFFER butiker:HANDLE).          */

/* hq:query-prepare("FOR EACH VareBehLinje NO-LOCK  WHERE VareBehLinje.VarebehNr = 90000027   AND VareBehLinje.levnr = 1,EACH VarebehlinjeTrans OF VarebehLinje NO-LOCK WHERE VarebehLinjeTrans.ArtikkelNr > 0   AND RegistrertBestilling AND GodkjentBestilling,FIRST StrKonv OF VareBehLinjeTrans NO-LOCK OUTER-JOIN,FIRST ArtBas OF VarebehLinje NO-LOCK,FIRST Butiker WHERE Butiker.Butik = VareBehLinjeTrans.ButikkNr NO-LOCK"). */
/* hq:query-prepare("FOR EACH VareBehLinje NO-LOCK  WHERE VareBehLinje.VarebehNr = 90000027   AND VareBehLinje.levnr = 1,EACH VarebehlinjeTrans OF VarebehLinje NO-LOCK WHERE VarebehLinjeTrans.ArtikkelNr > 0   AND RegistrertBestilling AND GodkjentBestilling,FIRST StrKonv OF VareBehLinjeTrans NO-LOCK OUTER-JOIN,FIRST ArtBas OF VarebehLinje NO-LOCK,FIRST Butiker WHERE Butiker.Butik = VareBehLinjeTrans.ButikkNr NO-LOCK BY VarebehLinjeTrans.ButikkNr BY VarebehLinje.LevKod"). */


/* Alle registreringer - messebok */
hq:SET-BUFFERS(BUFFER VareBehLinjeTrans:HANDLE,
               BUFFER StrKonv:HANDLE,
               BUFFER VareBehLinje:HANDLE,
               BUFFER artbas:HANDLE, 
               BUFFER varemerke:HANDLE, 
               BUFFER butiker:HANDLE).

/*hq:query-prepare("FOR EACH VareBehLinjeTrans NO-LOCK  WHERE VareBehLinjeTrans.VarebehNr = 90000027 AND VarebehLinjeTrans.ArtikkelNr > 0 AND RegistrertBestilling AND GodkjentBestilling,FIRST StrKonv OF VareBehLinjeTrans NO-LOCK OUTER-JOIN,FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK,FIRST ArtBas OF VarebehLinje NO-LOCK,FIRST Varemerke OF ArtBas NO-LOCK OUTER-JOIN,FIRST Butiker WHERE Butiker.Butik = VareBehLinjeTrans.ButikkNr NO-LOCK").*/
hq:query-prepare("FOR EACH VareBehLinjeTrans NO-LOCK  WHERE VareBehLinjeTrans.VarebehNr = 90000027 AND VarebehLinjeTrans.ArtikkelNr > 0 AND RegistrertBestilling AND GodkjentBestilling,FIRST StrKonv OF VareBehLinjeTrans NO-LOCK OUTER-JOIN,FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK,FIRST ArtBas OF VarebehLinje NO-LOCK,FIRST Varemerke OF ArtBas NO-LOCK OUTER-JOIN,FIRST Butiker WHERE Butiker.Butik = VareBehLinjeTrans.ButikkNr NO-LOCK BY VarebehLinje.AvdelingNr BY VarebehLinje.Hg BY VarebehLinje.Artikkelnr BY VarebehLinjeTrans.SeqNr"). 

DO  ix = 1 TO hq:NUM-BUFFERS:
  cMsg = cMsg + hq:get-buffer-handle(ix):NAME + " " + hq:INDEX-INFORMATION(ix) + CHR(10).
END.


ETIME(TRUE).
hq:QUERY-OPEN().
hq:GET-FIRST().
REPEAT WHILE NOT hq:QUERY-OFF-END:
  hq:GET-NEXT().
END.

output to c:\tmp\querystat.txt.
put unformatted PROGRAM-NAME(1) SKIP
        ETIME / 1000 SKIP
        cMsg SKIP(1)
        hq:prepare-string.
output close.        

