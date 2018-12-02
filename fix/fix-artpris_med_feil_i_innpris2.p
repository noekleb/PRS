CURRENT-WINDOW:WIDTH = 300.

DEF VAR wBatchNr AS INT NO-UNDO.
DEF VAR iSeqNr AS INT NO-UNDO.
DEFINE VARIABLE dArtikkelNr AS DECIMAL NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cEAN AS CHAR NO-UNDO.
DEF STREAM Ut.

FUNCTION hentSeqNr RETURNS INTEGER 
    (  ) FORWARD.

DEF BUFFER bArtPris FOR ArtPris.
DEF BUFFER bTransLogg FOR TransLogg.

ASSIGN
    cFilNavn = 'Time_Artpris_med_avvikende_innpris.txt'.

OUTPUT STREAM Ut TO VALUE(cFilNavn) NO-ECHO.

RUN batchlogg.p (PROGRAM-NAME(1), 'KORR av innpris 16 aug. En artikkel', OUTPUT wBatchNr).
RUN batchstatus.p (wBatchNr, 1).

PUT STREAM ut UNFORMATTED
    'ProfilNr;Strekkode;ArtikkelNr;Lev.art.nr;Varetekst;HK innkj.pris;Innkj.pris;Varekost;Pris;Db%' SKIP.

ARTIKKELLOOP:
FOR EACH ArtBas NO-LOCK:
    FIND FIRST strekkode OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE Strekkode 
        THEN cEAN = Strekkode.Kode.
        ELSE cEAN = ''.

    /* Henter HK kalkylen */
    FIND bArtPris NO-LOCK WHERE
        bArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
        bArtPris.ProfilNr   = 1 NO-ERROR.
    IF NOT AVAILABLE bArtPris THEN
        NEXT ARTIKKELLOOP.

    FOR EACH ArtPris OF ArtBas EXCLUSIVE-LOCK WHERE 
      ArtPris.ProfilNr > 1:
      IF bArtPris.ValPris[1] <> ArtPris.ValPris[1] THEN
      DO:   
          PUT STREAM Ut  UNFORMATTED 
            ArtPris.ProfilNr ';'
            cEAN ';'
            ArtPris.ArtikkelNr ';'
            ArtBas.LevKod ';'
            ArtBas.Beskr ';'
            bArtPris.InnkjopsPris[1] ';'
            ArtPris.InnkjopsPris[1] ';'
            ArtPris.VareKost[1]  ';'
            ArtPris.Pris[1] ';'
            (((ArtPris.Pris[1] - ArtPris.MvaKr[1]) - ArtPris.VareKost[1]) * 100) / (ArtPris.Pris[1] - ArtPris.MvaKr[1])
          SKIP.
          
          ASSIGN
              ArtPris.ValPris[1]      = bArtPris.ValPris[1]
              ArtPris.InnkjopsPris[1] = bArtPris.InnkjopsPris[1]
              ArtPris.Rab1Kr[1]       = bArtPris.Rab1Kr[1]
              ArtPris.Rab1%[1]        = bArtPris.Rab1%[1]
              ArtPris.VareKost[1]     = bArtPris.VareKost[1]
              ArtPris.DbKr[1]         = (ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1])
              ArtPris.Db%[1]          = (((ArtPris.Pris[1] - ArtPris.MvaKr[1]) - ArtPris.VareKost[1]) * 100) / (ArtPris.Pris[1] - ArtPris.MvaKr[1])
              .
      END.
    END. /* ARTPRIS */
      
END. /* ARTIKKELLOOP */
OUTPUT STREAM Ut CLOSE.

/* ************************  Function Implementations ***************** */

FUNCTION hentSeqNr RETURNS INTEGER 
	    (  ):
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/	

  DEFINE VARIABLE piSeqNr AS INTEGER NO-UNDO.
            
          piSeqNr = 0.
          FOR EACH bTransLogg NO-LOCK WHERE
              bTransLogg.Butik    = TransLogg.Butik AND
              bTransLogg.TransNr  = TransLogg.TransNr
              BY bTransLogg.Butik
              BY bTransLogg.TransNr
              BY bTransLogg.SeqNr:
              piSeqNr = bTransLogg.SeqNr + 1.
          END.

		RETURN piSeqNr.


		
END FUNCTION.
