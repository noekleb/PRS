&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

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


run ByttButikknr(261,309).
run ByttButikknr(323,353).
run ByttButikknr(324,354).
run ByttButikknr(325,355).
run ByttButikknr(326,356).
run ByttButikknr(327,327).

/*
OK 261
OK 323	Anton Sport Moss AS	*353*
324	Anton Sport Røa	*354*
325	Anton Sport Madla	*355*
326	Anton Sport Ila	*356*
327	Anton Sport Kolbotn	327
365	Anton Sport Sandvika Storsenter	365
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByttButikknr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttButikknr Procedure 
PROCEDURE ByttButikknr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input parameter igmlButikk as int no-undo.
    def input parameter inyButikk as int no-undo.

    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iButik AS INTEGER    NO-UNDO.

    
    find butiker no-lock where
      Butiker.Butik = igmlButikk no-error.
    if not available Butiker then
    return.

    ASSIGN iButik = inyButikk.
    
    FOR EACH akt_rapp WHERE akt_rapp.butik = Butiker.Butik:
        ASSIGN akt_rapp.butik = iButik.
    END.
    FOR EACH ApnSkjema WHERE ApnSkjema.ButikkNr = Butiker.Butik:
        ASSIGN ApnSkjema.ButikkNr = iButik. 
    END.
    FOR EACH ArtLag WHERE ArtLag.butik = Butiker.Butik:
        ASSIGN ArtLag.butik = iButik. 
    END.
    FOR EACH ArtLok WHERE ArtLok.ButikkNr = Butiker.Butik:
        ASSIGN ArtLok.ButikkNr = iButik. 
    END.
    FOR EACH BestKasse WHERE BestKasse.Butik = Butiker.Butik:
        ASSIGN BestKasse.Butik = iButik. 
    END.
    FOR EACH BestLevert WHERE BestLevert.Butik = Butiker.Butik:
        ASSIGN BestLevert.Butik = iButik. 
    END.
    FOR EACH BestLinje WHERE BestLinje.Butik = Butiker.Butik:
        ASSIGN BestLinje.Butik = iButik. 
    END.
    FOR EACH BestStr WHERE BestStr.Butik = Butiker.Butik:
        ASSIGN BestStr.Butik = iButik. 
    END.
    FOR EACH BokforingsBilag WHERE BokforingsBilag.ButikkNr = Butiker.Butik:
        ASSIGN BokforingsBilag.ButikkNr = iButik. 
    END.
    
    FOR EACH forsalj WHERE forsalj.forsnr > Butiker.Butik * 1000 AND forsalj.forsnr <= Butiker.Butik * 1999:
        ASSIGN forsalj.forsnr = iButik * 1000 + INT(SUBSTR(STRING(forsalj.Forsnr),LENGTH(STRING(forsalj.Forsnr)) - 2)).
    END.
    FOR EACH butikkforsalj WHERE butikkforsalj.Butik = Butiker.Butik:
        ASSIGN butikkforsalj.Forsnr = iButik * 1000 + INT(SUBSTR(STRING(butikkforsalj.Forsnr),LENGTH(STRING(butikkforsalj.Forsnr)) - 2))
               butikkforsalj.Butik  = iButik.
    END.
    FOR EACH Selger WHERE Selger.Selgernr > Butiker.Butik * 100000 AND Selger.Selgernr <= Butiker.Butik * 199999:
        ASSIGN Selger.SelgerNr = iButik * 100000 + INT(SUBSTR(STRING(Selger.SelgerNr),LENGTH(STRING(Selger.SelgerNr)) - 4)).
    END.
    FOR EACH ButikkSelger WHERE ButikkSelger.ButikkNr = Butiker.Butik:
        ASSIGN ButikkSelger.SelgerNr = iButik * 100000 + INT(SUBSTR(STRING(ButikkSelger.SelgerNr),LENGTH(STRING(ButikkSelger.SelgerNr)) - 4))
               ButikkSelger.ButikkNr = iButik. 
    END.
    FOR EACH ButLok WHERE ButLok.ButikkNr = Butiker.Butik:
        ASSIGN ButLok.ButikkNr = iButik. 
    END.
    
    FOR EACH dags_rap WHERE dags_rap.butikk = Butiker.Butik:
        ASSIGN dags_rap.butikk = iButik. 
    END.
    FOR EACH etikett WHERE etikett.butik = Butiker.Butik:
        ASSIGN etikett.butik = iButik. 
    END.
/*     FOR EACH Faktura WHERE Faktura.ButikkNr = Butiker.Butik: */
/*         ASSIGN Faktura.ButikkNr = iButik.                    */
/*     END.                                                     */
    FOR EACH Fributik WHERE Fributik.Butik = Butiker.Butik:
        ASSIGN Fributik.Butik = iButik. 
    END.
    FOR EACH Gavekort WHERE Gavekort.butnr = Butiker.Butik:
        ASSIGN Gavekort.butnr = iButik. 
    END.
    FOR EACH Gavekort WHERE Gavekort.BruktButNr = Butiker.Butik:
        ASSIGN Gavekort.BruktButNr = iButik. 
    END.
    
    
    FOR EACH KassererBilag WHERE KassererBilag.ButikkNr = Butiker.Butik:
        ASSIGN KassererBilag.ButikkNr  = iButik.  
    END.
    FOR EACH KassererDag WHERE KassererDag.ButikkNr = Butiker.Butik:
        ASSIGN KassererDag.ButikkNr  = iButik.  
    END.
    FOR EACH KassererKontanter WHERE KassererKontanter.ButikkNr = Butiker.Butik:
        ASSIGN KassererKontanter.ButikkNr  = iButik.  
    END.
    FOR EACH KassererOppgj WHERE KassererOppgj.ButikkNr = Butiker.Butik:
        ASSIGN KassererOppgj.ButikkNr  = iButik.  
    END.
    FOR EACH KassererValuta WHERE KassererValuta.ButikkNr = Butiker.Butik:
        ASSIGN KassererValuta.ButikkNr  = iButik.  
    END.
    FOR EACH kas_konter WHERE kas_konter.butikk = Butiker.Butik:
        ASSIGN kas_konter.butikk  = iButik.  
    END.
    FOR EACH kas_logg WHERE kas_logg.butikk = Butiker.Butik:
        ASSIGN kas_logg.butikk  = iButik.  
    END.
    FOR EACH kas_rap WHERE kas_rap.butikk = Butiker.Butik:
        ASSIGN kas_rap.butikk  = iButik.  
    END.
    
    
    FOR EACH konto WHERE konto.butikk = Butiker.Butik:
        ASSIGN konto.butikk = iButik.  
    END.
    FOR EACH kont_mal WHERE kont_mal.butikk = Butiker.Butik:
        ASSIGN kont_mal.butikk = iButik.  
    END.
    FOR EACH Kort_Spes WHERE Kort_Spes.butikk = Butiker.Butik:
        ASSIGN Kort_Spes.butikk = iButik.  
    END.
    FOR EACH Kunde WHERE Kunde.ButikkNr = Butiker.Butik:
        ASSIGN Kunde.ButikkNr = iButik.  
    END.
    FOR EACH KundeBetTrans WHERE KundeBetTrans.betButik = Butiker.Butik:
        ASSIGN KundeBetTrans.betButik = iButik.  
    END.
    FOR EACH KundeBetTrans WHERE KundeBetTrans.Butik = Butiker.Butik:
        ASSIGN KundeBetTrans.Butik = iButik.  
    END.
    FOR EACH KundeSaldo WHERE KundeSaldo.ButikkNr = Butiker.Butik:
        ASSIGN KundeSaldo.ButikkNr = iButik.  
    END.
    FOR EACH KundeTrans WHERE KundeTrans.Butik = Butiker.Butik:
        ASSIGN KundeTrans.Butik = iButik.  
    END.
    FOR EACH Lager WHERE Lager.Butik = Butiker.Butik:
        ASSIGN Lager.Butik = iButik.  
    END.
    FOR EACH Medlem WHERE Medlem.ButikkNr = Butiker.Butik:
        ASSIGN Medlem.ButikkNr = iButik.  
    END.
    FOR EACH MedlemBetTrans WHERE MedlemBetTrans.betButik = Butiker.Butik:
        ASSIGN MedlemBetTrans.betButik = iButik.  
    END.
    FOR EACH MedlemBetTrans WHERE MedlemBetTrans.Butik = Butiker.Butik:
        ASSIGN MedlemBetTrans.Butik = iButik.  
    END.
    FOR EACH MedlemSaldo WHERE MedlemSaldo.ButikkNr = Butiker.Butik:
        ASSIGN MedlemSaldo.ButikkNr = iButik.  
    END.
    FOR EACH MedTrans WHERE MedTrans.Butik = Butiker.Butik:
        ASSIGN MedTrans.Butik = iButik.  
    END.
    FOR EACH OvBuffer WHERE OvBuffer.ButikkNrFra = Butiker.Butik:
        ASSIGN OvBuffer.ButikkNrFra = iButik.  
    END.
    FOR EACH OvBuffer WHERE OvBuffer.ButikkNrTil = Butiker.Butik:
        ASSIGN OvBuffer.ButikkNrTil = iButik.  
    END.
    FOR EACH OvLinje WHERE OvLinje.FraButik = Butiker.Butik:
        ASSIGN OvLinje.FraButik = iButik.  
    END.
    FOR EACH OvLinje WHERE OvLinje.TilButik = Butiker.Butik:
        ASSIGN OvLinje.TilButik = iButik.  
    END.
    FOR EACH OvOrdre WHERE OvOrdre.FraButik = Butiker.Butik:
        ASSIGN OvOrdre.FraButik = iButik.  
    END.
    FOR EACH OvOrdre WHERE OvOrdre.TilButik = Butiker.Butik:
        ASSIGN OvOrdre.TilButik = iButik.  
    END.
    FOR EACH ReklamasjonsLinje WHERE ReklamasjonsLinje.Butik = Butiker.Butik:
        ASSIGN ReklamasjonsLinje.Butik = iButik.  
    END.
    FOR EACH ReklamasjonsLinje WHERE ReklamasjonsLinje.SolgtIButikk = Butiker.Butik:
        ASSIGN ReklamasjonsLinje.SolgtIButikk = iButik.  
    END.
    FOR EACH StLager WHERE StLager.Butik = Butiker.Butik:
        ASSIGN StLager.Butik = iButik.  
    END.
    FOR EACH StLinje WHERE StLinje.Butik = Butiker.Butik:
        ASSIGN StLinje.Butik = iButik.  
    END.
    FOR EACH StLinje WHERE StLinje.StTypeId = "BUTSTAT" AND StLinje.DataObjekt = STRING(Butiker.Butik,"999999"):
        ASSIGN StLinje.DataObjekt = STRING(iButik,"999999").
    END.
    
    /*        TG-StLinje-DataObj:CHECKED = CAN-FIND(FIRST StLinje WHERE StLinje.Butik = Butiker.Butik) */
    
    FOR EACH TelleHode WHERE CAN-DO(TelleHode.ButikkListe,STRING(Butiker.Butik)):
        DO iCount = 1 TO NUM-ENTRIES(TelleHode.ButikkListe):
            IF ENTRY(iCount,TelleHode.ButikkListe) = STRING(Butiker.Butik) THEN
                ASSIGN ENTRY(iCount,TelleHode.ButikkListe) = STRING(iButik).
        END.
    END.
    FOR EACH TelleLinje WHERE TelleLinje.Butik = Butiker.Butik:
        ASSIGN TelleLinje.Butik = iButik.  
    END.
    FOR EACH Tilgode WHERE Tilgode.butnr = Butiker.Butik:
        ASSIGN Tilgode.butnr = iButik.  
    END.
    FOR EACH Tilgode WHERE Tilgode.BruktButNr = Butiker.Butik:
        ASSIGN Tilgode.BruktButNr = iButik.  
    END.
    FOR EACH TransLogg WHERE TransLogg.Butik = Butiker.Butik:
        ASSIGN TransLogg.Butik = iButik.  
    END.
    FOR EACH TransLogg WHERE TransLogg.OvButik = Butiker.Butik:
        ASSIGN TransLogg.OvButik = iButik.  
    END.
    FOR EACH z_nummer WHERE z_nummer.butikk = Butiker.Butik:
        ASSIGN z_nummer.butikk = iButik.  
    END.
    FOR EACH AnalyseLogg WHERE AnalyseLogg.ButikkNr = Butiker.Butik:
        ASSIGN AnalyseLogg.ButikkNr = iButik.  
    END.
    FOR EACH BongHode WHERE BongHode.ButikkNr = Butiker.Butik:
        ASSIGN BongHode.ButikkNr = iButik.  
    END.
    FOR EACH BongLinje WHERE BongLinje.ButikkNr = Butiker.Butik:
        ASSIGN BongLinje.ButikkNr = iButik.  
    END.
    FOR EACH BongLinje WHERE BongLinje.MButikkNr = Butiker.Butik:
        ASSIGN BongLinje.MButikkNr = iButik.  
    END.
    FOR EACH BongLinje WHERE BongLinje.ReturButikk = Butiker.Butik:
        ASSIGN BongLinje.ReturButikk = iButik.  
    END.
    FOR EACH Datasett WHERE Datasett.ButikkNr = Butiker.Butik:
        ASSIGN Datasett.ButikkNr = iButik.  
    END.
    FOR EACH hgrdag WHERE hgrdag.butnr = Butiker.Butik:
        ASSIGN hgrdag.butnr = iButik.  
    END.
    FOR EACH timedag WHERE timedag.butnr = Butiker.Butik:
        ASSIGN timedag.butnr = iButik.  
    END.
    FOR EACH varedag WHERE varedag.butnr = Butiker.Butik:
        ASSIGN varedag.butnr = iButik.  
    END.

    FOR EACH Bruker WHERE Bruker.BrGrpNr = Butiker.Butik:
        ASSIGN Bruker.BrGrpNr = iButik.
    END.
    FOR EACH ButikkKobling WHERE ButikkKobling.Butik = Butiker.Butik:
        ASSIGN ButikkKobling.BrGrpNr = IF ButikkKobling.BrGrpNr = Butiker.Butik THEN iButik ELSE ButikkKobling.BrGrpNr
               ButikkKobling.TeamNr  = IF ButikkKobling.TeamNr = Butiker.Butik THEN iButik ELSE ButikkKobling.TeamNr
               ButikkKobling.Butik   = iButik.
    END.
/*     FOR EACH ButikkKobling WHERE ButikkKobling.Butik = Butiker.Butik: */
/*         ASSIGN ButikkKobling.Butik = iButik.                          */
/*     END.                                                              */
    FOR EACH ButikkTilgang WHERE ButikkTilgang.Butik = Butiker.Butik:
        ASSIGN ButikkTilgang.BrGrpNr = IF ButikkTilgang.BrGrpNr = Butiker.Butik THEN iButik ELSE ButikkTilgang.BrGrpNr
               ButikkTilgang.Butik = iButik.
    END.
    FOR EACH Butikkteam:
        ASSIGN Butikkteam.Beskrivelse = Butikkteam.Beskrivelse
               Butikkteam.BrGrpNr     = IF Butikkteam.BrGrpNr = Butiker.butik THEN iButik ELSE Butikkteam.BrGrpNr
               Butikkteam.TeamNr      = IF Butikkteam.TeamNr = Butiker.Butik THEN iButik ELSE Butikkteam.TeamNr.
    END.
    FOR EACH BrukerGrp WHERE BrukerGrp.BrGrpNr = Butiker.Butik:
        FOR EACH ProgBrGrp OF Brukergrp:
              ProgBrGrp.BrGrpNr     = iButik.
        END.
        ASSIGN BrukerGrp.BrGrpNr     = iButik.
    END.
    FOR EACH Gruppe WHERE Gruppe.ButikkNr = Butiker.Butik:
        ASSIGN Gruppe.ButikkNr = iButik.
    END.
    
    
    FOR EACH Kasse WHERE Kasse.ButikkNr = Butiker.Butik:
        ASSIGN Kasse.ButikkNr = iButik
               Kasse.ElJournal[2] = STRING(iButik)
               ENTRY(1,Kasse.ElJournalId,";")  = STRING(iButik).
      FIND CURRENT Butiker EXCLUSIVE.
      ASSIGN Butiker.Butik = iButik.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

