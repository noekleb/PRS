&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : sett_plukkbutikk_kundeordre.p
    Purpose     : Bestemmer fra hvilke butikker varene skal plukkes fra.

    Syntax      : run sett_plukkbutikk_kundeordre.p (cKORdre_Id_Lst).

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 3/7-09
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER cKOrdre_Id_Lst AS CHARACTER NO-UNDO.

DEFINE VARIABLE piLoop         AS INTEGER NO-UNDO.
DEFINE VARIABLE iCL            AS INTEGER NO-UNDO.
DEFINE VARIABLE lDec           AS DECIMAL NO-UNDO.
DEFINE VARIABLE iNettbutikk    AS INTEGER NO-UNDO.
DEFINE VARIABLE cNettButLst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLagerListe    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDefaultButikk AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFrakt         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevNrListe    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevButikk     AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tmpArtLag LIKE ArtLag.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND FIRST SysPara NO-LOCK WHERE
  SysPara.SysHId       = 150 AND  
  SysPara.SysGr        = 10 AND  
  SysPara.Beskrivelse  = "Posten" NO-ERROR.
IF AVAILABLE SysPara THEN 
  cFrakt = SysPara.Parameter1.
ELSE 
cFrakt = ''.

{syspara.i 5 1 1 iCL INT}
IF NOT CAN-FIND(Butiker NO-LOCK WHERE
                  Butiker.Butik = iCL) THEN
  RETURN 'ERROR Ukjent sentrallagerbutikk.'.

/* Nettbutikk. Er denen angitt, skal ikke nettbutikkens eget lager kunne plukkes fra. */
{syspara.i 150 1 2 iNettbutikk INT}
IF iNettbutikk <> 0 THEN 
DO:
  IF NOT CAN-FIND(Butiker WHERE
                  Butiker.Butik = iNettbutikk) THEN 
    iNettbutikk = 0.
END.

/* Angivelse av primærlagre for nettbutikk. Det er de lagerne som skal sjekkes først. */
{syspara.i 150 1 3 cNettButLst}
IF cNettButLst <> '' THEN 
DO piLoop = 1 TO NUM-ENTRIES(cNettButLst):
  IF NOT CAN-FIND(Butiker WHERE
                  Butiker.Butik = int(ENTRY(piLoop,cNettButLst))) THEN 
    RETURN 'ERROR Ukjent butikknr i prioritert butikkliste 150 1 3.'.
END.

/* Angivelse av primærlagre for leverandører. */
{syspara.i 150 1 10 cLevNrListe}
{syspar2.i 150 1 10 cLevButikk}
ASSIGN
  cLevNrListe = TRIM(cLevNrListe)
  cLevButikk  = TRIM(cLevButikk)
  .
IF cLevButikk <> '' THEN 
DO piLoop = 1 TO NUM-ENTRIES(cLevButikk):
  IF NOT CAN-FIND(Butiker WHERE
                  Butiker.Butik = int(ENTRY(piLoop,cLevButikk))) THEN 
    RETURN 'ERROR Ukjent butikknr i prioritert leverandør/butikkliste 150 1 10.'.
END.

/* Liste over lagre som nettbutikken kan plukke fra. Disse lagrene + primærbutikk, er de tilgjengelige lagrene */
{syspara.i 150 1 4 cLagerListe}
IF cLagerListe <> '' THEN 
DO piLoop = 1 TO NUM-ENTRIES(cLagerListe):
  IF NOT CAN-FIND(Butiker WHERE
                  Butiker.Butik = int(ENTRY(piLoop,cLagerListe))) THEN 
    RETURN 'ERROR Ukjent butikknr i øvrige butikkliste 150 1 4.'.
END.

/* Default plukklager. Benyttes hvis ingen andre lager blir funnet */
{syspara.i 150 1 6 cDefaultButikk}
IF cDefaultButikk <> '' THEN 
DO piLoop = 1 TO NUM-ENTRIES(cDefaultButikk):
  IF NOT CAN-FIND(Butiker WHERE
                  Butiker.Butik = int(ENTRY(piLoop,cDefaultButikk))) THEN 
    RETURN 'ERROR Ukjent butikknr satt som defaultlager 150 1 6.'.
END.

IF cKOrdre_Id_Lst = '' THEN 
  RETURN 'ERROR'.
DO piLoop = 1 TO NUM-ENTRIES(cKOrdre_Id_Lst):
  RUN sett_plukk_butikk (DECIMAL(ENTRY(piLoop,cKOrdre_Id_Lst))).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* **********************  Internal Procedures  *********************** */
 
&IF DEFINED(EXCLUDE-sett_plukk_butikk) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sett_plukk_butikk Procedure
PROCEDURE sett_plukk_butikk:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER lKOrdre_Id AS DECIMAL NO-UNDO.
  DEFINE VARIABLE iAnt      AS DECIMAL NO-UNDO.
  DEFINE VARIABLE piLinjeNr AS DECIMAL NO-UNDO.

  DEFINE BUFFER b1KOrdreHode  FOR KOrdreHode.
  DEFINE BUFFER b1KOrdreLinje FOR KORdreLinje.
  DEFINE BUFFER b1Butiker     FOR Butiker. 
  
  /* Temporær fil som benyttes hvis ordrelinjer må splittes. */
  FOR EACH tmpArtLag:
    DELETE tmpArtLag.
  END.
  
  FIND KOrdreHode NO-LOCK WHERE
    KOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN 
    RETURN.
  
  /* Henter siste linjenr og gjør klart til splitting av ordrerader. */
  FIND LAST KORdreLinje OF KOrdreHode NO-LOCK 
    USE-INDEX FaktLinje NO-ERROR.
  IF AVAILABLE KOrdreLinje 
    THEN piLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
    ELSE piLinjeNr = 1. 
    
  /* Leser linjene i stigende linjenummerordning. Ved eventuell splitting av rader, */
  /* vil de nye radene bli lest til slutt.                                          */  
  SETT_PLUKKBUTIKK:
  FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK
    BREAK BY KOrdreLinje.KOrdre_Id
          BY KOrdreLinje.KOrdreLinjeNr:
    
    /* Betaling m.m. skal ikke behandles for plukkbutikk */
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.

    /* Tømmer tempLager */
    FOR EACH tmpArtLag:
      DELETE tmpArtLag.
    END.
    
    /* Legger opp lageret i tmpFile. */  
    IF NOT CAN-FIND(FIRST tmpArtLag WHERE
                      tmpArtLag.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN 
    FOR EACH ArtLag NO-LOCK WHERE
      ArtLag.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND 
      TRIM(ArtLag.storl) = TRIM(KOrdreLinje.Storl) AND
      ArtLag.Butik       > 0 AND  
      ArtLag.Lagant      > 0:
      
      CREATE tmpArtLag.
      BUFFER-COPY ArtLag TO tmpArtLag.
    END.
    
    /* Linjer med frakt skal ikke behandles for plukkbutikk. */
    IF cFrakt <> '' THEN 
    DO:
      IF cFrakt = KOrdreLinje.VareNr THEN NEXT.
    END.
    
    IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)) THEN NEXT.
    FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    
    /* Nuller og behandler linjen pånytt */
    ASSIGN
      KOrdreLinje.Plukkbutikk = 0.

    /**********************************************************************************/
    /*               Varer som skal trekkes fra leverandørlager                       */
    /*               Dette overstyrer behandling av ikke lagerstyrte varer.           */
    /**********************************************************************************/
    IF (cLevNrListe <> '' AND cLevButikk <> '' AND CAN-DO(cLevNrListe,STRING(ArtBas.LevNr)) AND KordreLinje.Plukkbutikk = 0) THEN
    LEVLAGER:
    DO:
      SETT_LEVLAGER:
      FOR EACH tmpArtLag NO-LOCK WHERE
        tmpArtLag.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND 
        TRIM(tmpArtLag.storl) = TRIM(KOrdreLinje.Storl) AND
        tmpArtLag.Butik       = INTEGER(ENTRY(1,cLevButikk)) AND /* Bare butikk som er satt opp for leverandørlager */  
        tmpArtLag.Lagant      > 0 
        BREAK BY tmpArtLag.Lagant DESCENDING:
        ASSIGN
          KOrdreLinje.PlukkButikk = tmpArtLag.Butik.
        LEAVE SETT_LEVLAGER.
      END. /* LEVLAGER */
    END. /* LEVLAGER */
 
    /**********************************************************************************/
    /*                          IKKE LAGERSTYRTE VARER                                */
    /**********************************************************************************/
    /* Ikke lagerstyrte varer skal alltid plukkes fra den første prioriterte butikken */
    IF ArtBas.Lager = FALSE AND KordreLinje.Plukkbutikk = 0 THEN 
      KordreLinje.Plukkbutikk = (IF cNettButLst <> '' 
                                   THEN int(ENTRY(1,cNettButLst))
                                   ELSE int(ENTRY(1,cLagerListe))
                                 ).
 
    /**********************************************************************************/
    /*                    SJEKK MOT PRIORITERTE LAGERSTEDER                           */
    /**********************************************************************************/
    /* Er det satt opp et prioritert nettbutikklager, skal dette sjekeks først. */
    /* De prioriterte butikkene skal leses i den rekkefølge de er stillt opp    */
    /* i systemparameteren.                                                     */
    IF cNettButLst <> '' AND ArtBas.Lager = TRUE AND KordreLinje.Plukkbutikk = 0 THEN
    LISTE: 
    DO piLoop = 1 TO NUM-ENTRIES(cNettButLst):
      SETT_NETTBUTIKK_LINJE:
      FOR EACH tmpArtLag NO-LOCK WHERE
        tmpArtLag.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND 
        TRIM(tmpArtLag.storl) = TRIM(KOrdreLinje.Storl) AND
        tmpArtLag.Butik       = INTEGER(ENTRY(piLoop,cNettButLst)) /*CAN-DO(cNettButLst,STRING(ArtLag.Butik))*/ AND  
        tmpArtLag.Lagant      > 0
        BREAK BY tmpArtLag.Lagant DESCENDING:
        
        /* Sjekker og eventuelt splitter raden. */
        IF KOrdreLinje.Antall > 0 AND (tmpArtLag.LagAnt < KOrdreLinje.Antall) THEN 
        SPLITT:
        DO:
          CREATE b1KOrdreLinje.
          BUFFER-COPY KOrdreLinje
            EXCEPT KOrdreLinjeNr PlukkButikk
            TO b1KOrdreLinje
            ASSIGN
              b1KOrdreLinje.KOrdreLinjeNr = piLinjeNr
              b1KOrdreLinje.Antall        = KOrdreLinje.Antall - tmpArtLag.LagAnt
              b1KOrdreLinje.PlukkButikk   = IF INTEGER(cDefaultButikk) > 0 THEN INTEGER(cDefaultButikk) ELSE iCL
              piLinjeNr                   = piLinjeNr + 1
              .
          ASSIGN
            /* Regner om verdiene i feltene på gammel ordrelinje. */
            KOrdreLinje.NettoLinjesum     = KOrdreLinje.NettoPris * tmpArtLag.Lagant
            KOrdreLinje.LinjeRabattKr     = (KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall) * tmpArtLag.Lagant
            KOrdreLinje.LinjeRabattKr     = IF (KOrdreLinje.LinjeRabattKr < 0 OR KOrdreLinje.LinjeRabattKr = ?) THEN 0 ELSE KOrdreLinje.LinjeRabattKr 
            /*KOrdreLinje.MvaKr             = (KOrdreLinje.MvaKr / KOrdreLinje.Antall) * tmpArtLag.Lagant*/
            KOrdreLinje.MvaKr             = ((KOrdreLinje.NettoPris - (KOrdreLinje.NettoPris / (1 + (KOrdreLinje.Mva% / 100)))) * tmpArtLag.Lagant)
            KOrdreLinje.MvaKr             = IF (KOrdreLinje.MvaKr < 0 OR KOrdreLinje.MvaKr = ?) THEN 0 ELSE KOrdreLinje.MvaKr 

            KOrdreLinje.BruttoPris        = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / tmpArtLag.Lagant)  
            KOrdreLinje.Pris              = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / tmpArtLag.Lagant)
            
            KOrdreLinje.Linjesum          = KOrdreLinje.NettoLinjesum + KOrdreLinje.LinjeRabattKr 
            KOrdreLinje.DbKr              = KOrdreLinje.NettoLinjesum - (KOrdreLinje.VareKost * tmpArtLag.Lagant)

            /* Regner om verdiene i feltene på gammel ordrelinje.                                           */
            /* Her ligger nå opprinnelig antall i KOrdreLinje.Antall og nytt antall i b1KOrdreLinje.Antall. */
            b1KOrdreLinje.NettoLinjesum     = b1KOrdreLinje.NettoPris * b1KOrdreLinje.Antall
            b1KOrdreLinje.LinjeRabattKr     = (b1KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall) * b1KOrdreLinje.Antall
            b1KOrdreLinje.LinjeRabattKr     = IF (b1KOrdreLinje.LinjeRabattKr < 0 OR b1KOrdreLinje.LinjeRabattKr = ?) THEN 0 ELSE b1KOrdreLinje.LinjeRabattKr 
            /*b1KOrdreLinje.MvaKr             = (b1KOrdreLinje.MvaKr / KOrdreLinje.Antall) * KOrdreLinje.Antall*/
            b1KOrdreLinje.MvaKr             = ((b1KOrdreLinje.NettoPris - (b1KOrdreLinje.NettoPris / (1 + (b1KOrdreLinje.Mva% / 100)))) * b1KOrdreLinje.Antall)
            b1KOrdreLinje.MvaKr             = IF (b1KOrdreLinje.MvaKr < 0 OR b1KOrdreLinje.MvaKr = ?) THEN 0 ELSE b1KOrdreLinje.MvaKr 

            b1KOrdreLinje.BruttoPris        = b1KOrdreLinje.NettoPris + (b1KOrdreLinje.LinjeRabattKr / b1KOrdreLinje.Antall)  
            b1KOrdreLinje.Pris              = b1KOrdreLinje.NettoPris + (b1KOrdreLinje.LinjeRabattKr / b1KOrdreLinje.Antall)
            
            b1KOrdreLinje.Linjesum          = b1KOrdreLinje.NettoLinjesum + b1KOrdreLinje.LinjeRabattKr 
            b1KOrdreLinje.DbKr              = b1KOrdreLinje.NettoLinjesum - (b1KOrdreLinje.VareKost * b1KOrdreLinje.Antall)
             
            /* Korrigerer antall i temp lager og på gammel ordrelinje. */
            KOrdreLinje.Antall            = tmpArtLag.Lagant
            tmpArtLag.LagAnt              = 0.
          RELEASE b1KOrdreLinje.
        END. /* SPLITT */
        
        ASSIGN
          KOrdreLinje.PlukkButikk = tmpArtLag.Butik.
        LEAVE SETT_NETTBUTIKK_LINJE.
      END. /* SETT_NETTBUTIKK_LINJE */
      
      IF KOrdreLinje.PlukkButikk > 0 THEN 
        LEAVE LISTE.
      
    END. /* LISTE */

    /**********************************************************************************/
    /*                    SJEKK AV ØVRIGE BUTIKKERS LAGER                             */
    /**********************************************************************************/
    /* Sjekker de øvrige butikkenes lager.            */
    /* Her plukkes det fra den som har mest på lager. */
    IF KOrdreLinje.PlukkButikk = 0 AND cLagerListe <> '' THEN
    LISTE: 
    DO:
      SETT_NETTBUTIKK_LINJE:
      FOR EACH tmpArtLag NO-LOCK WHERE
        tmpArtLag.ArtikkelNr  = DECIMAL(KOrdreLinje.VareNr) AND 
        TRIM(tmpArtLag.storl) = TRIM(KOrdreLinje.Storl) AND
        CAN-DO(cLagerListe,STRING(tmpArtLag.Butik)) AND
        tmpArtLag.Lagant     > 0
        BREAK BY tmpArtLag.Lagant DESCENDING:

        /* Sjekker og eventuelt splitter raden. */
        IF KOrdreLinje.Antall > 0 AND (tmpArtLag.LagAnt < KOrdreLinje.Antall) THEN         
        SPLITT:
        DO:
          CREATE b1KOrdreLinje.
          BUFFER-COPY KOrdreLinje
            EXCEPT KOrdreLinjeNr PlukkButikk
            TO b1KOrdreLinje
            ASSIGN
              b1KOrdreLinje.KOrdreLinjeNr = piLinjeNr
              b1KOrdreLinje.Antall        = KOrdreLinje.Antall - tmpArtLag.LagAnt
              b1KOrdreLinje.PlukkButikk   = IF INTEGER(cDefaultButikk) > 0 THEN INTEGER(cDefaultButikk) ELSE iCL
              piLinjeNr                   = piLinjeNr + 1
              .
          ASSIGN
            /* Regner om verdiene i feltene på gammel ordrelinje. Nytt antall tmpArtLag.Lagant, opprinnelig antall KOrdreLinje.Antall. */
            KOrdreLinje.NettoLinjesum     = KOrdreLinje.NettoPris * tmpArtLag.Lagant
            KOrdreLinje.LinjeRabattKr     = (KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall) * tmpArtLag.Lagant
            KOrdreLinje.LinjeRabattKr     = IF (KOrdreLinje.LinjeRabattKr < 0 OR KOrdreLinje.LinjeRabattKr = ?) THEN 0 ELSE KOrdreLinje.LinjeRabattKr 
            /*KOrdreLinje.MvaKr             = (KOrdreLinje.MvaKr / KOrdreLinje.Antall) * tmpArtLag.Lagant*/
            KOrdreLinje.MvaKr             = ((KOrdreLinje.NettoPris - (KOrdreLinje.NettoPris / (1 + (KOrdreLinje.Mva% / 100)))) * tmpArtLag.Lagant)
            KOrdreLinje.MvaKr             = IF (KOrdreLinje.MvaKr < 0 OR KOrdreLinje.MvaKr = ?) THEN 0 ELSE KOrdreLinje.MvaKr 

            KOrdreLinje.BruttoPris        = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / tmpArtLag.Lagant)  
            KOrdreLinje.Pris              = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / tmpArtLag.Lagant)
            
            KOrdreLinje.Linjesum          = KOrdreLinje.NettoLinjesum + KOrdreLinje.LinjeRabattKr 
            KOrdreLinje.DbKr              = KOrdreLinje.NettoLinjesum - (KOrdreLinje.VareKost * tmpArtLag.Lagant)

            /* Regner om verdiene i feltene på gammel ordrelinje.                                           */
            /* Her ligger nå opprinnelig antall i KOrdreLinje.Antall og nytt antall i b1KOrdreLinje.Antall. */
            b1KOrdreLinje.NettoLinjesum     = b1KOrdreLinje.NettoPris * b1KOrdreLinje.Antall
            b1KOrdreLinje.LinjeRabattKr     = (b1KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall) * b1KOrdreLinje.Antall
            b1KOrdreLinje.LinjeRabattKr     = IF (b1KOrdreLinje.LinjeRabattKr < 0 OR b1KOrdreLinje.LinjeRabattKr = ?) THEN 0 ELSE b1KOrdreLinje.LinjeRabattKr 
            /*b1KOrdreLinje.MvaKr             = (b1KOrdreLinje.MvaKr / KOrdreLinje.Antall) * KOrdreLinje.Antall*/
            b1KOrdreLinje.MvaKr             = ((b1KOrdreLinje.NettoPris - (b1KOrdreLinje.NettoPris / (1 + (b1KOrdreLinje.Mva% / 100)))) * b1KOrdreLinje.Antall)
            b1KOrdreLinje.MvaKr             = IF (b1KOrdreLinje.MvaKr < 0 OR b1KOrdreLinje.MvaKr = ?) THEN 0 ELSE b1KOrdreLinje.MvaKr 

            b1KOrdreLinje.BruttoPris        = b1KOrdreLinje.NettoPris + (b1KOrdreLinje.LinjeRabattKr / b1KOrdreLinje.Antall)  
            b1KOrdreLinje.Pris              = b1KOrdreLinje.NettoPris + (b1KOrdreLinje.LinjeRabattKr / b1KOrdreLinje.Antall)
            
            b1KOrdreLinje.Linjesum          = b1KOrdreLinje.NettoLinjesum + b1KOrdreLinje.LinjeRabattKr 
            b1KOrdreLinje.DbKr              = b1KOrdreLinje.NettoLinjesum - (b1KOrdreLinje.VareKost * b1KOrdreLinje.Antall)
             
            /* Korrigerer antall i temp lager og på gammel ordrelinje. */
            KOrdreLinje.Antall            = tmpArtLag.Lagant
            tmpArtLag.LagAnt              = 0.
            /* Omregning ferdig.            */
            
          RELEASE b1KOrdreLinje.
        END. /* SPLITT */

        ASSIGN
          KOrdreLinje.PlukkButikk = tmpArtLag.Butik.
        LEAVE SETT_NETTBUTIKK_LINJE.
      END. /* SETT_NETTBUTIKK_LINJE */
    END. /* LISTE */
    
    /* Hvis plukkbutikk ikke er satt, benyttes default butikk */
    IF KOrdreLinje.PlukkButikk = 0 THEN
      KOrdreLinje.PlukkButikk = INTEGER(cDefaultButikk).     
       
    /* Hvis plukkbutikk ennå ikke er satt, skal sentrallager benyttes */
    IF KOrdreLinje.PlukkButikk = 0 THEN
      KOrdreLinje.PlukkButikk = iCL.
      
  END. /* SETT_PLUKKBUTIKK */ 

  /* Setter inn oversikt over utleveringsteder i VerkstedMerknad. */
  FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
    KOrdreLinje.Plukkbutikk > 0
    BREAK BY KOrdreLinje.PlukkButikk:
    iAnt = iAnt + KOrdreLinje.Antall.
    IF FIRST (KOrdreLinje.Plukkbutikk) THEN
      DO:
        FIND b1KOrdreHode EXCLUSIVE-LOCK WHERE
          b1KOrdreHode.KOrdre_Id = KOrdreHode.KOrdre_Id.
        ASSIGN
          b1KOrdreHode.VerkstedMerknad = b1KOrdreHode.VerkstedMerknad + 
                                         (IF TRIM(b1KOrdreHode.VerkstedMerknad) = '' THEN '' ELSE CHR(10)) + 
                                         '    Antall:   Plukk butikk:'.
      END.
    IF LAST-OF(KOrdreLinje.Plukkbutikk) THEN 
      DO:
        FIND b1KOrdreHode EXCLUSIVE-LOCK WHERE
          b1KOrdreHode.KOrdre_Id = KOrdreHode.KOrdre_Id.
        FIND b1Butiker NO-LOCK WHERE
          b1Butiker.Butik = KOrdreLinje.PlukkButikk.
        ASSIGN
          b1KOrdreHode.VerkstedMerknad = b1KOrdreHode.VerkstedMerknad + 
                                         (IF TRIM(b1KOrdreHode.VerkstedMerknad) = '' THEN '' ELSE CHR(10)) + 
                                         string(iant,'->>,>>>,>>9') + '    ' + b1Butiker.ButNamn.
        iAnt = 0.
      END.
    
  END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF




