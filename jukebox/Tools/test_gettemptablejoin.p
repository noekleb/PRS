DEF VAR ohTT      AS HANDLE NO-UNDO.
DEF VAR ocStat    AS CHAR   NO-UNDO.
DEF VAR ocReturn  AS CHAR   NO-UNDO.
DEF VAR hQuery    AS HANDLE NO-UNDO.

RUN jbserv_gettemptablejoin.p ("validsession",100,0,"",
                               "VareBehLinjeTrans;ArtikkelNr|InfoPOS SE Art.nr"
                            + ";LevUke1|L.uke1"
                            + ";+KalkBestilt1|INTEGER|>>>>9|varebehlinjetrans_bestilt1(ROWIDartikkelnr_butikknr)|Best1"
                            + ";LevUke2|L.uke2;+KalkBestilt2|INTEGER|>>>>9|varebehlinjetrans_bestilt2(ROWIDartikkelnr_butikknr)|Best2"
                            + ";LevUke3|L.uke3;+KalkBestilt3|INTEGER|>>>>9|varebehlinjetrans_bestilt3(ROWIDartikkelnr_butikknr)|Best3"
                            + ";LevUke4|L.uke4;+KalkBestilt4|INTEGER|>>>>9|varebehlinjetrans_bestilt4(ROWIDartikkelnr_butikknr)|Best4"
                            + ";ButikkNr;+SumAntall|INTEGER|>>>>9|varebehlinjetrans_totbestilt(ROWID)|Sum antall"
                            + ";+OrdreTot|DECIMAL|>>>>>>9.99|varebehlinjetrans_ordretot(ROWIDartikkelnr_butikknr)|Sum ordre"
                            + ";EDato;VarebehNr;SeqNr"
                            + ";Kode;+Distinct|DECIMAL|>>>>>>>>9|varebehlinjetrans_distinct(ROWIDartikkelnr_butikknr)"
                            + ";+GodkjentBest|LOGICAL|*/|varebehlinjetrans_bestforslag(ROWID)|Godkj"
                            + ",Strekkode;StrKode,StrKonv;Storl|Str,VareBehLinje;levKod;Beskr|Artikkelnavn;LevFargKod|Fargetekst;levnamn|Leverandør;VareKost|Nettoforh.pris;DB%;supVareKost|Nettosup.pris;Pris|Veil.uts.pris;KampanjePris|Kampanjepris;Sortering|Art.sekv;Vg;VgBeskr;Hg;HgBeskr;AvdelingNr;AvdelingNavn,ArtBas;VMid,Varemerke;Beskrivelse|Varemerke",

                              "WHERE VarebehNr = DEC('10000029') AND ButikkNr = 260 AND (Bestilt1 NE 0 OR Bestilt2 NE 0 OR Bestilt3 NE 0 OR Bestilt4 NE 0)"
                            + ",FIRST Strekkode OF VareBehLinjeTrans NO-LOCK OUTER-JOIN"
                            + ",FIRST StrKonv OF Strekkode NO-LOCK OUTER-JOIN"
                            + ",FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK"
                            + ",FIRST ArtBas OF VarebehLinje NO-LOCK"
                            + ",FIRST Varemerke OF ArtBas NO-LOCK"

/*                             + " BY Varekost BY StrKode", */
                               + " BY KalkBestilt1 BY SeqNr BY OrdreTot",

                            "",
                            "logfile;serverquery.log,calcfieldproc;varebehlinjetrans_browsekalk.p",
                            OUTPUT TABLE-HANDLE ohTT,
                            OUTPUT ocStat,
                            OUTPUT ocReturn).
IF ocReturn NE "" THEN DO:
  MESSAGE PROGRAM-NAME(1) SKIP
          ocReturn SKIP
          VIEW-AS ALERT-BOX.
  RETURN.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ohTT:DEFAULT-BUFFER-HANDLE).
hQuery:QUERY-PREPARE("FOR EACH " + ohTT:DEFAULT-BUFFER-HANDLE:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
OUTPUT TO c:\temp\RESULT.txt.
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  PUT hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
      hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Kalkbestilt1"):BUFFER-VALUE
      SKIP.       
  hQuery:GET-NEXT().
END.
OUTPUT CLOSE.

