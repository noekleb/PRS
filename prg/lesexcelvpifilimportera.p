/* lesexcelvpifilimportera.p */
DEFINE INPUT PARAMETER cExcelFilNavn AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iEkstVPILevNr AS INTEGER   NO-UNDO.

/* -------------------------------------------------------------------------- */
/* Init fields from database - depends on global varibale setting             */ 
/* By : CHO - 2012                                                            */
/* -------------------------------------------------------------------------- */

DEFINE VARIABLE getFieldFromDB AS LOGICAL EXTENT 18 NO-UNDO. 
DEFINE VARIABLE UpdateArtikkel AS LOGICAL INIT FALSE NO-UNDO. 
DEFINE VARIABLE cEanCode AS CHAR NO-UNDO. 
DEFINE VARIABLE DBAssignedField AS LOGICAL EXTENT 18 NO-UNDO. 
DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.

FUNCTION getStrekkodeArtikkelNr RETURNS DEC (INPUT ipcEan AS CHAR):
    DEFINE BUFFER strekkode FOR strekkode. 
    RUN bibl_chkean.p (INPUT-OUTPUT ipcEan).
    FIND strekkode WHERE strekkode.kode = ipcEan NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Strekkode AND LENGTH(ipcEan) > 6 THEN 
      FIND Strekkode NO-LOCK WHERE Strekkode.Kode = FILL('0',13 - LENGTH(ipcEan)) + ipcEan NO-ERROR. 
    IF NOT AVAIL strekkode THEN RETURN ?.
    ELSE RETURN strekkode.ArtikkelNr. 
END. 

FUNCTION getStrekkodeClean RETURNS CHAR (INPUT ipcEan AS CHAR):
    DEFINE BUFFER strekkode FOR strekkode. 
    RUN bibl_chkean.p (INPUT-OUTPUT ipcEan).
    FIND strekkode WHERE strekkode.kode = ipcEan NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Strekkode AND LENGTH(ipcEan) > 6 THEN 
      FIND Strekkode NO-LOCK WHERE Strekkode.Kode = FILL('0',13 - LENGTH(ipcEan)) + ipcEan NO-ERROR. 
    IF NOT AVAIL strekkode THEN RETURN ?.
    ELSE RETURN strekkode.kode. 
END. 

FUNCTION getUpdateArtikkelStatus RETURNS LOGICAL (INPUT ipcEan AS CHAR):
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO.  
    dArtikkelNr = getStrekkodeArtikkelNr(ipcEan). 

    IF dArtikkelNr = 0  OR dArtikkelNr = ? THEN RETURN FALSE. 
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF AVAIL Artbas AND ArtBas.ArtikkelNr NE ? THEN RETURN TRUE. 
    ELSE RETURN FALSE.
END. 

/* Felt #1 Artbaslevnr */
FUNCTION getArtBasLevNr RETURNS INT (INPUT ipcStrekkode AS CHAR):
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF AVAIL Artbas AND ArtBas.ArtikkelNr NE ? THEN RETURN artbas.LevNr. 
    ELSE RETURN ?.
END. 

/* Felt #2 Varunr */
FUNCTION getArtBasLevKod RETURNS INT (INPUT ipcStrekkode AS CHAR):
    DEFINE VARIABLE iLevKod AS INT NO-UNDO. 
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF AVAIL Artbas AND ArtBas.ArtikkelNr NE ? THEN 
    DO:
        iLevKod = INT(artbas.LevKod) NO-ERROR. 
        IF ERROR-STATUS:ERROR THEN RETURN ?. 
        ELSE RETURN ilevKod. 
    END. 
    ELSE RETURN ?.
END. 

/* Felt #3 EAN  mandatory */

/* Felt #4 Bestnr */
FUNCTION getStrekkodeBestillingsnummer RETURNS CHAR (INPUT ipcStrekkode AS CHAR):
    DEFINE VARIABLE iBestillingsNummer AS INT NO-UNDO. 
    DEFINE BUFFER strekkode FOR strekkode. 
    DEFINE VARIABLE cKode AS CHAR NO-UNDO. 
    cKode = getStrekkodeClean(ipcStrekkode).
    FIND strekkode WHERE strekkode.kode = cKode NO-LOCK NO-ERROR.
    IF NOT AVAIL strekkode THEN RETURN ?.
    RETURN Strekkode.Bestillingsnummer. 
END. 

/* Felt #5 beskrivelse */
FUNCTION getArtBasBesk RETURNS CHAR (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER artbas FOR artbas. 
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF AVAIL Artbas AND ArtBas.ArtikkelNr NE ? THEN RETURN artbas.beskr. 
    ELSE RETURN ?.
END. 

/* Felt #6 Mengde  */
FUNCTION getArtBasMengde RETURNS DEC (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER artbas FOR artbas. 
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelnr NO-LOCK NO-ERROR.
    IF AVAIL Artbas AND ArtBas.ArtikkelNr NE ? THEN RETURN artbas.mengde.
    ELSE RETURN ?.
END. 

/* Felt #7 Salgsenhet  */
FUNCTION getArtBasSalgsEnhet RETURNS CHAR (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER artbas FOR artbas. 
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF AVAIL Artbas AND ArtBas.ArtikkelNr NE ? THEN RETURN artbas.JamforEnhet. 
    ELSE RETURN ?.
END. 

/* Felt #8 VareGruppe artBas.vg  */
FUNCTION getArtBasVG RETURNS INT (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER artbas FOR artbas. 
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF AVAIL Artbas AND ArtBas.ArtikkelNr NE ? THEN RETURN ArtBas.vg. 
    ELSE RETURN ?.
END. 

/* Felt #9 InnPris artBas.Innkjøpspris [1]  */
FUNCTION getArtBasInnkjopspris RETURNS DEC (INPUT ipcStrekkode AS CHAR):
   DEFINE BUFFER artPris FOR artPris. 
   DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
   dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
   FIND artpris WHERE 
        artpris.artikkelnr = dArtikkelNr AND
        artpris.profilnr   = 1 NO-LOCK NO-ERROR.
   IF AVAIL ArtPris THEN RETURN ArtPris.InnkjopsPris[1]. ELSE RETURN ?.
END. 

/* Felt #10 DO-NOTHING */

/* Felt #11 InnPris artBas.Innkjøpspris [1]  */
FUNCTION getArtBasPris RETURNS DEC (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER artPris FOR artPris. 
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artpris WHERE 
         artpris.artikkelnr = dArtikkelNr AND
         artpris.profilnr   = 1 NO-LOCK NO-ERROR.
    IF AVAIL ArtPris THEN RETURN ArtPris.Pris[1]. ELSE RETURN ?.
END. 

/* Felt #12  artBas.anbefaltspris  */
FUNCTION getArtBasAnbefaltPris RETURNS DEC (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER artbas FOR artbas. 
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF AVAIL Artbas AND ArtBas.ArtikkelNr NE ? THEN RETURN artbas.AnbefaltPris. 
    ELSE RETURN ?.
END. 

/* Felt #13  Mva  */
FUNCTION getMva RETURNS DEC (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER artbas FOR artbas. 
    DEFINE BUFFER vargr FOR vargr. 
    DEFINE BUFFER moms FOR moms. 

    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    FIND vargr OF artbas NO-LOCK NO-ERROR.
    IF AVAILABLE VarGr THEN 
       FIND Moms WHERE Moms.MomsKod = vargr.MomsKod NO-LOCK NO-ERROR.

    IF AVAIL Moms THEN RETURN Moms.MomsProc. 
    ELSE RETURN ?. 
END. 

/* Felt #14  artBas.getArtBasAntIPakn  */
FUNCTION getArtBasAntIPakn RETURNS INT (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER artbas FOR artbas. 
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF AVAIL Artbas AND ArtBas.ArtikkelNr NE ? THEN RETURN artbas.AntIPakn. 
    ELSE RETURN ?.
END. 

/* Felt #15  artBas.LinkVareNr  */
FUNCTION getArtBasLinkVareNr RETURNS DEC (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER artbas FOR artbas. 
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF AVAIL Artbas AND ArtBas.ArtikkelNr NE ? THEN RETURN artbas.LinkVareNr. 
    ELSE RETURN ?.
END. 

/* Felt #16  artBas.ProdNr  */
FUNCTION getProdusentBeskrivelse RETURNS CHAR (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER artbas FOR artbas. 
    DEFINE BUFFER Produsent FOR Produsent. 
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF NOT AVAIL Artbas THEN RETURN ?. 
    FIND Produsent WHERE Produsent.ProdNr = ArtBas.ProdNr NO-LOCK NO-ERROR. 
    IF AVAIL Produsent THEN RETURN Produsent.Beskrivelse. ELSE RETURN ?. 
END. 

/* Felt #17  artBas.VMid  */
FUNCTION getVaremerkeBeskrivelse RETURNS CHAR (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER artbas FOR artbas. 
    DEFINE BUFFER varemerke FOR varemerke. 
    DEFINE VARIABLE dArtikkelNr AS DEC NO-UNDO. 
    dArtikkelNr = getStrekkodeArtikkelNr(ipcStrekkode).
    FIND artbas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    IF NOT AVAIL Artbas THEN RETURN ?. 
    FIND Varemerke WHERE VareMerke.vmid = ArtBas.vmid NO-LOCK NO-ERROR. 
    IF AVAIL Varemerke THEN RETURN Varemerke.Beskrivelse. ELSE RETURN ?. 
END. 

/* Felt #18  Butikknr  */
/*
FUNCTION getButikerButik RETURNS INT (INPUT ipcStrekkode AS CHAR):
    DEFINE BUFFER bufButiker FOR Butiker. 

    FIND bufButiker WHERE Butiker.Butik = INT(ipcStrekkode) NO-LOCK NO-ERROR.
    IF AVAIL bufButiker THEN RETURN bufButiker.Butik. ELSE RETURN ?.
END. 
*/

FUNCTION getFieldFromDb RETURNS LOGICAL EXTENT 18 (INPUT ipcExcludeImportFields AS CHAR):
    DEFINE VARIABLE iCnt AS INT NO-UNDO. 
    DEFINE VARIABLE getFieldFromDb  AS LOGICAL EXTENT 18 NO-UNDO. 
    
    DO iCnt = 1 TO EXTENT(getFieldFromDb): 
        getFieldFromDb[iCnt] = CAN-DO(ipcExcludeImportFields,STRING(iCnt)).
    END.
    
    RETURN getFieldFromDB. 
END. 
/* -------------------------------------------------------------------------- */



{lesexcelvpifil.i &SHARED = "SHARED"}


DEF VAR cStr AS CHARACTER   NO-UNDO.
DEF VAR ii AS INTEGER     NO-UNDO.
DEF VAR cTmp AS CHARACTER   NO-UNDO.
DEF VAR dTst AS DECIMAL     NO-UNDO.
DEF VAR iAntRows AS INTEGER     NO-UNDO.
DEF VAR cEntryList  AS CHARACTER   NO-UNDO.
DEF VAR cError  AS CHARACTER   NO-UNDO.
DEF VAR cErrorFelt AS CHARACTER   NO-UNDO.
DEF VAR dartbas_levnr   AS INTE NO-UNDO.
DEF VAR dartbas_artnr     AS CHAR NO-UNDO.
DEF VAR dstrekkode_kode       AS DECI NO-UNDO.
DEF VAR cstrekkode_kode       AS CHAR NO-UNDO.
DEF VAR cOrgstrekkode_kode    AS CHAR NO-UNDO.
DEF VAR ipris_bestnr    AS CHAR NO-UNDO.
DEF VAR cvare_varetekst AS CHAR NO-UNDO.
DEF VAR dvare_mengde    AS DECI NO-UNDO.
DEF VAR ivare_enhet     AS INTE NO-UNDO.
DEF VAR cvare_Cenhet    AS CHAR NO-UNDO.
DEF VAR ivare_hgr       AS INTE NO-UNDO.
DEFINE VARIABLE iVareGr AS INTEGER NO-UNDO.
DEF VAR dpris_engrosn   AS DECI NO-UNDO.
DEF VAR drabatt         AS DECI NO-UNDO.
DEF VAR dpris_utprisn   AS DECI NO-UNDO.
DEF VAR dpris_veilpris  AS DECI NO-UNDO.
DEF VAR dvare_mva       AS DECI NO-UNDO.
DEF VAR ivare_mvagr     AS INTE NO-UNDO.
DEF VAR dvare_antpkn    AS DECI NO-UNDO.
DEF VAR cVare_Varemerke AS CHAR NO-UNDO.
DEF VAR cVare_Produsent AS CHAR NO-UNDO.
DEF VAR iButikk_ButNr   AS INT  NO-UNDO.
DEF VAR iRad            AS INTE NO-UNDO.
DEF VAR cColNamn        AS CHAR NO-UNDO.
DEF VAR dInExMva        AS DECI NO-UNDO.
DEF VAR dOrigVareEAN    AS DECI NO-UNDO.
DEF VAR dvare_link      AS DECI NO-UNDO.
DEF VAR cvare_link      AS CHARACTER NO-UNDO.
DEF VAR dTandemEAN      AS DECI NO-UNDO.
DEF VAR dUtNetto AS DECI NO-UNDO.
DEF VAR dTBkr    AS DECI NO-UNDO.
DEF VAR dOren    AS DECI NO-UNDO.
DEF VAR dhgrprof_brutto% AS DECI NO-UNDO.
DEF VAR iNumEntries AS INT NO-UNDO.
DEF VAR cJamforenhLst   AS CHAR NO-UNDO.
DEFINE VARIABLE bDistribIPS   AS LOG NO-UNDO.
DEF VAR c2Tekst AS CHAR NO-UNDO.
DEF VAR cEDB-System        LIKE ImpKonv.EDB-System NO-UNDO.
DEFINE VARIABLE iLevNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iCL    AS INTEGER NO-UNDO.



/* Henter ID for konvertering */
FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = iEkstVPILevNr NO-ERROR.
IF NOT AVAILABLE EkstVPILev OR EkstVPILevNr = 0 THEN
DO:
    MESSAGE "Ingen ekstern VPI leverandør tilgjengelig." SKIP
            "Id: " + STRING(iEkstVPILevNr) + "."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ASSIGN
      cEDB-System = ''.
END.
ASSIGN
    cEDB-System = EkstVPILev.EDB-System
    iLevNr      = EkstVPILev.LevNr.

{syspara.i 50 15 18 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bDistribIPS = TRUE.

{syspara.i 5 1 1 iCL INT}

/* Legger opp mva postene */
RUN initTTMva.

/* Bygger liste over tillatte jamførenheter. */
FOR EACH JamforEnhet NO-LOCK:
  ASSIGN
    cJamforenhLst = cJamforenhLst + (IF cJamforenhLst = '' THEN '' ELSE ',') + TRIM(JamforEnhet.Jamforenhet).
END.

ASSIGN
    /*cJamforenhLst = 'stk,kg,l,m,m3,m2,100m,dos,pors,tabl,beh,vask,par,hg' */
    cColNamn = "Levnr,Varunr,EAN,Bestnr,Beskr,Strl,Enhet,Varugr,Inpris,Rabatt,Utpris,Rekpris,Moms,Förp,Link,Producent,Varumärke,KundNr"
    cEntryList = FILL(",",NUM-ENTRIES(cColNamn) - 1).

INPUT FROM VALUE(cExcelFilNavn).

/* Leser første linje fra filen - Overskriftsrad. */
IMPORT UNFORMATTED cStr.

/* Sjekker antall kolonner i filen. Avviser den hvis det er feil */
iNumEntries = NUM-ENTRIES(cStr,CHR(9)).
IF iNumEntries < 18 THEN 
DO:
    MESSAGE "Fel antal fält (" + STRING(iNumEntries) + ")" SKIP
            "Skall vara minst 18 kolonner." SKIP /* + STRING(NUM-ENTRIES(cColnamn))*/
            "Importstreng: " REPLACE(RIGHT-TRIM(
                                                cStr,CHR(9)
                                                ),CHR(9),';') SKIP 
            "Importfil:" cExcelFilNavn
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    INPUT CLOSE.
    RETURN.
END.

iRad = iRad + 1.
LESRAD:
REPEAT:
    /* Leser inn en rad. */
    IMPORT UNFORMATTED cStr.
  
    iRad = iRad + 1.
    cStr = TRIM(cStr).
    IF cStr = "" THEN NEXT.

    FIND tt_prisfil WHERE tt_prisfil.radnr = iRad NO-ERROR.
    /* Er recorden lest inn fra før og er uten endringer, går vi til neste. */
    /* Er den lest inn fra før, og endret, slettes den.                     */
    IF AVAIL tt_prisfil AND tt_prisfil.orgdata = cstr THEN
        NEXT.
    ELSE IF AVAIL tt_prisfil THEN
        DELETE tt_prisfil.
    /* Felt som er markert som endret på recorden tas alltid bort (2 eller flere gangers innlesning)*/
    FOR EACH TT_VareOrgInfo WHERE TT_VareOrgInfo.radnr = iRad:
        DELETE TT_VareOrgInfo.
    END.
    /* Samme gjelder feilmeldingene for raden. */
    FOR EACH tt_Error WHERE tt_error.radnr = iRad:
        DELETE tt_error.
    END.
    
    
    cError = cEntryList.

    ASSIGN 
        dbAssignedField = FALSE
        cEanCode        = ENTRY(3,cStr,CHR(9))
        /* Flagger om raden gjelder en ny, eller endring av en eksisterende artikkel */
        UpdateArtikkel  = getUpdateArtikkelStatus(cEanCode)
        NO-ERROR.

    
    /* Er det endring på artikkel, skal originalverdiene hentes fra databasen.  */
    /* Er det endring, må det også være endret i en av de kolonnene som  er satt i filteret, hvis filteret er aktivt. */ 
    IF NOT UpdateArtikkel THEN
         getFieldFromDB = FALSE. 
    ELSE getFieldFromDB = getFieldFromDb(ExcludeImportFields[2]). 


    /* Her hentes originalverdien på feltet fra databasen for hver kolonne. Denne legges i en variabel. */ 
    DO ii = 1 TO iNumEntries /*NUM-ENTRIES(cColnamn)*/:

      CASE ii:

        /* Field #1 "Levnr" import */ 
        WHEN 1 THEN 
        IF getFieldFromDB[ii] THEN dartbas_levnr = getArtBasLevNr(cEanCode).
        ELSE 
        DO:
            IF iLevNr = 0 THEN 
            DO:
              dartbas_levnr = INT(ENTRY(ii,cStr,CHR(9))) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN
                ASSIGN dartbas_levnr = ?
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i leverandørnr.'.
            END.
            ELSE 
              dArtBas_LevNr = iLevNr.
        END.
        
        /* Field #2 "Varunr" import */
        WHEN 2 THEN 
        IF getFieldFromDB[ii] THEN dartbas_artnr = STRING(getArtBasLevKod(cEanCode)).
        ELSE 
        DO:
            dartbas_artnr = (ENTRY(ii,cStr,CHR(9))) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN dartbas_artnr = ?
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i artikkelnr.'.
        END.
        
        /* strekkode hentes alltid fra fil */ 
        WHEN 3 THEN 
        DO:
            ASSIGN cOrgStrekkode_Kode = ENTRY(ii,cStr,CHR(9)).
            ASSIGN dstrekkode_kode    = DECI(ENTRY(ii,cStr,CHR(9))) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN dstrekkode_kode   = ?
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i strekkoden.'.
            IF dStrekkode_Kode = 0 THEN 
            DO:
                ASSIGN dstrekkode_kode   = ?
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' <Blank> strekkode.'.
            END.
            ELSE DO:
                ASSIGN cstrekkode_kode = STRING(dstrekkode_kode).
                IF LENGTH(STRING(dstrekkode_kode)) > 6 THEN
                    ASSIGN cstrekkode_kode = FILL("0",13 - LENGTH(cstrekkode_kode)) + cstrekkode_kode.
            END.
            /* Konvertering av PLU nummer */
            IF dstrekkode_kode <= 999999 AND cEDB-System <> '' AND 
              CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
              ImpKonv.EDB-System = cEDB-System AND 
              ImpKonv.Tabell     = 'PLU') THEN 
            DO:
              FIND FIRST ImpKonv NO-LOCK WHERE 
                ImpKonv.EDB-System = cEDB-System AND 
                ImpKonv.Tabell     = 'PLU' AND 
                ImpKonv.EksterntId = LEFT-TRIM(STRING(dstrekkode_kode),'0') NO-ERROR.
              IF NOT AVAILABLE ImpKonv THEN 
                  ASSIGN ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ingen mapping for PLU.'.
            END. 
        END.
        
        /* Bestnr */ 
        WHEN 4 THEN 
        IF getFieldFromDB[ii] THEN  ipris_bestnr = getStrekkodeBestillingsnummer(cstrekkode_kode).
        ELSE 
        DO:
            ipris_bestnr   = (ENTRY(ii,cStr,CHR(9))) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN ipris_bestnr   = ?
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i bestillingsnr.'.
        END.

        /* CHO - beskrivelse */ 
        WHEN 5 THEN 
        IF getFieldFromDB[ii] THEN cvare_varetekst = getArtBasBesk(cstrekkode_kode).
        ELSE 
        DO:
            cvare_varetekst = TRIM(TRIM(ENTRY(ii,cStr,CHR(9)),'"')) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN cvare_varetekst = ?
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i varetekst.'.
        END.
        
        /* TOM: Strl StrKonv.Storl – kobling til Strekkode.StrKode ligger i StrKonv.StrKode. Husk bibl_strfix.p  */ 
        WHEN 6 THEN 
        IF getFieldFromDB[ii] THEN dvare_mengde = getArtBasMengde(cstrekkode_kode).
        ELSE 
        DO:
            dvare_mengde = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN dvare_mengde = ?
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i jamførfaktor.'.
        END.
        
        /* CHO - JamførEnhet fra artbas */ 
        WHEN 7 THEN 
        IF getFieldFromDB[ii] THEN cvare_Cenhet = getArtBasSalgsEnhet(cstrekkode_kode).
        ELSE 
        DO:
            cvare_Cenhet  = TRIM(REPLACE(ENTRY(ii,cStr,CHR(9)),'"','')) NO-ERROR.
            IF cvare_Cenhet = 'St' THEN cVare_CEnhet = 'Stk'.
            
            IF ERROR-STATUS:ERROR THEN
                ASSIGN cvare_Cenhet  = ?
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i enhetstekst.'.

            IF cEDB-System <> '' AND 
              CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
              ImpKonv.EDB-System = cEDB-System AND 
              ImpKonv.Tabell     = 'Jamforenhet') THEN 
            DO: 
                 FIND FIRST ImpKonv NO-LOCK WHERE
                            ImpKonv.EDB-System = cEDB-System AND
                            ImpKonv.Tabell     = 'Jamforenhet' AND
                            ImpKonv.EksterntId = TRIM(cvare_Cenhet) NO-ERROR.
                 IF AVAILABLE ImpKonv THEN cvare_Cenhet = ImpKonv.InterntId.
                 ELSE ASSIGN ENTRY(ii,cError) = 'Kolonne Ukjent jamførenhet ' + TRIM(cvare_Cenhet) + '. Ingen MAPPING for ' + cEDB-System + ': ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn).
            END.
            ELSE IF LOOKUP (cvare_Cenhet , cJamforenhLst , ',') = 0 THEN
              DO: 
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ukjent enhetstekst.'.                       
              END.
        END.
        

        WHEN 8 THEN 
        IF getFieldFromDB[ii] THEN 
            ASSIGN 
                ivare_hgr = getArtBasVG(cstrekkode_kode)
                iVareGr = ivare_hgr.
        ELSE     
        DO:
            ivare_hgr    = INT(ENTRY(ii,cStr,CHR(9))) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN ivare_hgr    = ?
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i varegruppe.'.
            END.
            ASSIGN iVareGr = iVare_Hgr.
            IF cEDB-System <> '' AND 
              CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
              ImpKonv.EDB-System = cEDB-System AND 
              ImpKonv.Tabell     = 'VarGr') THEN 
            DO: 
                 FIND FIRST ImpKonv NO-LOCK WHERE
                            ImpKonv.EDB-System = cEDB-System AND
                            ImpKonv.Tabell     = 'VarGr' AND
                            ImpKonv.EksterntId = TRIM(STRING(ivare_hgr,'>>>>>9')) NO-ERROR.
                 IF NOT AVAILABLE ImpKonv THEN  
                    ASSIGN ENTRY(ii,cError) = 'Kolonne Ukjent varegruppe ' + TRIM(STRING(ivare_hgr,'>>>>>9')) + '. Ingen MAPPING for ' + cEDB-System + ': ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn).
                 ELSE 
                    ASSIGN iVareGr = INT(ImpKonv.InterntId) NO-ERROR.                                      
            END.
            ELSE DO:
              IF NOT CAN-FIND(VarGr WHERE VarGr.Vg = ivare_hgr) THEN  
                  ASSIGN ENTRY(ii,cError) = 'Kolonne Ukjent varegruppe ' + TRIM(STRING(ivare_hgr,'>>>>>9')) + '. Linje: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn).                  
            END.
        END.
        
        WHEN 9 THEN 
        IF getFieldFromDB[ii] THEN dpris_engrosn = getArtBasInnkjopspris(cstrekkode_kode).
        ELSE 
        DO:
            dpris_engrosn = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN dpris_engrosn = ?
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i innpris.'.
            ELSE IF dpris_engrosn = 0 THEN
                ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' O i innpris.'.
        END.
        WHEN 10 THEN DO:
            .
        END.
        
        
        WHEN 11 THEN 
        IF getFieldFromDB[ii] THEN  dpris_utprisn = getArtBasPris(cstrekkode_kode).
        ELSE 
        DO:
            dpris_utprisn = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN dpris_utprisn = ?
                       ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) +  ' Ugyldige tegn i utpris.'.
        END.


        WHEN 12 THEN 
        IF getFieldFromDB[ii] THEN dpris_veilpris = getArtBasAnbefaltPris(cstrekkode_kode).
        ELSE     
        DO:
            dpris_veilpris = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN dpris_veilpris = ? ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i veiledende pris.'.
        END.
        
        WHEN 13 THEN 
        IF getFieldFromDB[ii] THEN dvare_mva = getMVA(cstrekkode_kode).
        ELSE      
        DO:
            dvare_mva      = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN dvare_mva      = ? ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i mva%.'.
        END.

        WHEN 14 THEN
        IF getFieldFromDB[ii] THEN  dvare_antpkn = getArtBasAntIPakn(cstrekkode_kode).
        ELSE     
        DO:
            dvare_antpkn   = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN dvare_antpkn   = ? ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i antall i pkn.'.
        END.
        
        WHEN 15 THEN 
        IF getFieldFromDB[ii] THEN dvare_link = getArtBasLinkVareNr(cstrekkode_kode).
        ELSE 
        DO:
            cvare_Link   = ENTRY(ii,cStr,CHR(9)).
            dvare_link   = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO:
                ASSIGN dvare_link = 0.
                ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ugyldige tegn i liknr.'.
            END.
            IF cEDB-System <> '' AND dvare_link > 0 AND 
              CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
              ImpKonv.EDB-System = cEDB-System AND 
              ImpKonv.Tabell     = 'Pant') THEN 
            DO:
              FIND FIRST ImpKonv NO-LOCK WHERE 
                ImpKonv.EDB-System = cEDB-System AND 
                ImpKonv.Tabell     = 'Pant' AND 
                ImpKonv.EksterntId = cvare_Link NO-ERROR.
              IF NOT AVAILABLE ImpKonv THEN ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ingen mapping for link til pant.'.
              ELSE dvare_link = DEC(ImpKonv.InterntId).
            END.
            ELSE IF dvare_link > 0 AND NOT CAN-FIND(FIRST artbas WHERE Artbas.artikkelnr = dvare_link) THEN
               ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn) + ' Ukjent artikkelnr i linknr.'.
        END.
        
        WHEN 16 THEN 
        IF getFieldFromDB[ii] THEN cVare_Produsent = getProdusentBeskrivelse(cstrekkode_kode).
        ELSE     
        DO:
            cVare_Produsent = TRIM(TRIM(ENTRY(ii,cStr,CHR(9)),'"')) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                ASSIGN cVare_Produsent = ? ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn).
            IF NOT CAN-FIND(FIRST Produsent NO-LOCK WHERE Produsent.Beskrivelse = cVare_Produsent) THEN RUN opprett_Produsent.p (cVare_Produsent).
        END.
        
        WHEN 17 THEN 
        IF getFieldFromDB[ii] THEN cVare_Varemerke = getVaremerkeBeskrivelse(cstrekkode_kode).
        ELSE 
        DO:
            cVare_Varemerke = TRIM(TRIM(ENTRY(ii,cStr,CHR(9)),'"')) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN ASSIGN cVare_Varemerke = ? ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn).
            IF NOT CAN-FIND(FIRST Varemerke NO-LOCK WHERE Varemerke.Beskrivelse = cVare_Varemerke) THEN RUN opprett_Varemerke.p (cVare_Varemerke).
        END.

        WHEN 18 THEN 
        /*        
        IF getFieldFromDB[ii] THEN  iButikk_ButNr = getButikerButik(TRIM(ENTRY(ii,cStr,CHR(9)))).
        ELSE
        */ 
        DO:            
            iButikk_ButNr = INT(ENTRY(ii,cStr,CHR(9))) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
              ASSIGN 
              iButikk_ButNr = 0 
              ENTRY(ii,cError) = 'Kolonne: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn).
              
            IF cEDB-System <> '' AND 
              CAN-FIND(FIRST ImpKonv NO-LOCK WHERE 
              ImpKonv.EDB-System = cEDB-System AND 
              ImpKonv.Tabell     = 'Butiker') 
            THEN DO:
              IF iButikk_ButNr > 0 AND NOT CAN-FIND(ImpKonv WHERE
                  ImpKonv.EDB-System = cEDB-System AND
                  ImpKonv.Tabell     = 'Butiker' AND
                  ImpKonv.EksterntId = STRING(iButikk_ButNr)) THEN 
                  ASSIGN ENTRY(ii,cError) = 'Kolonne Ukjent kundenr. Ingen MAPPING: ' + STRING(ii) + ' ' + ENTRY(ii,cColNamn).
              ELSE DO: 
                  FIND ImpKonv WHERE
                      ImpKonv.EDB-System = cEDB-System AND
                      ImpKonv.Tabell     = 'Butiker' AND
                      ImpKonv.EksterntId = STRING(iButikk_ButNr) NO-ERROR.
                IF AVAILABLE ImpKonv THEN ASSIGN iButikk_ButNr = INT(ImpKonv.InterntId).
              END.
            END.                    
        END.
      END CASE.
    END.

    
    FIND tt_moms WHERE tt_moms.MomsProc = dvare_mva NO-ERROR.
    ivare_mvagr = IF AVAIL tt_moms THEN tt_moms.MomsKod ELSE ?.
    IF ivare_mvagr = ? THEN
        ENTRY(13,cError) = ENTRY(13,cColNamn).

    dOrigVareEAN = 0.
    dTandemEAN   = 0.
    IF AVAILABLE ArtBas THEN RELEASE ArtBas.
    
    dArtikkelNr = getStrekkodeArtikkelNr(cstrekkode_kode).
    FIND Artbas WHERE artbas.artikkelnr = dArtikkelnr NO-LOCK NO-ERROR. 

    
    /****************************************************************/
    /* UTGÅR PGA BUG, CHO ------------------------------------------ 
    /* Henter artikkelen ut fra bestillingsnummer. */
    FIND FIRST strekkode WHERE strekkode.Bestillingsnummer = STRING(ipris_bestnr) NO-LOCK NO-ERROR.
    
    IF NOT AVAIL strekkode THEN
        FIND strekkode WHERE strekkode.kode = cstrekkode_kode NO-LOCK NO-ERROR.
    IF NOT AVAIL strekkode THEN DO:
      /* Gjør ingenting */
    END.
    ELSE FIND ArtBas OF Strekkode NO-LOCK.
    ----------------------------------------------------------------*/


    /* - CHO endring */ 
    IF NOT getFieldFromDB[7] THEN
    DO:
        ivare_enhet = LOOKUP (cvare_Cenhet , cJamforenhLst , ','). 
        IF ivare_enhet = 0 OR ivare_enhet = ? THEN 
        DO:
            ivare_enhet = ?.
            ENTRY(7,cError) = ENTRY(7,cColNamn).
        END.
    END. 

    IF AVAILABLE VarGr THEN RELEASE vargr.
    FIND vargr WHERE vargr.vg = iVareGr NO-LOCK NO-ERROR.
    IF AVAIL vargr THEN DO:
            dhgrprof_brutto% = 100 - vargr.Kost_Proc.
    END.
    ELSE dhgrprof_brutto% = 0.
    IF iVareGr = 0 AND AVAIL artbas THEN
        iVareGr = artbas.vg.

    FIND Butiker NO-LOCK WHERE Butiker.Butik = iButikk_ButNr NO-ERROR.

    FIND FIRST Produsent NO-LOCK WHERE Produsent.Beskrivelse BEGINS cVare_Produsent NO-ERROR.
    FIND FIRST Varemerke NO-LOCK WHERE Varemerke.Beskrivelse BEGINS cVare_Varemerke NO-ERROR.
    CREATE tt_Prisfil.
    ASSIGN TT_Prisfil.Radnr           = iRad
           tt_Prisfil.lNy             = NOT AVAIL artbas
           tt_PrisFil.ArtikkelNr      = IF AVAILABLE ArtBas THEN ArtBas.ArtikkelNr ELSE 0
           tt_Prisfil.ctandem         = IF dTandemEAN <> 0 THEN STRING(dTandemEAN) ELSE ""
           /*  1 */ tt_Prisfil.artbas_levnr    = dartbas_levnr    
           /*  2 */ tt_Prisfil.artbas_artnr    = dartbas_artnr    
           /*  3 */ tt_Prisfil.strekkode_kode  = cOrgStrekkode_Kode
           /*  4 */ tt_Prisfil.pris_bestnr     = ipris_bestnr   
           /*  5 */ tt_Prisfil.vare_varetekst  = cvare_varetekst
           /*  6 */ tt_Prisfil.vare_mengde     = dvare_mengde   
           /*  7 */ tt_Prisfil.vare_enhet      = ivare_enhet    
                    tt_Prisfil.vare_Cenhet     = cvare_Cenhet  
                    tt_Prisfil.Salgsenhet      = IF AVAILABLE ArtBas THEN ArtBas.Salgsenhet ELSE 'Stk' 
           /*  8 */ tt_Prisfil.vare_hgr        = ivare_hgr      
           /*  9 */ tt_Prisfil.pris_engrosn    = dpris_engrosn  
                    tt_Prisfil.rabatt          = drabatt        
           /* 11 */ tt_Prisfil.pris_utprisn    = dpris_utprisn
           /* 12 */ tt_Prisfil.pris_veilpris   = IF dpris_veilpris = 0 THEN dpris_utprisn ELSE dpris_veilpris
           /* 13 */ tt_Prisfil.vare_mva        = dvare_mva      
                    tt_Prisfil.vare_mvagr      = ivare_mvagr  
           /* 14 */ tt_Prisfil.vare_antpkn     = IF dvare_antpkn = 0 AND AVAIL artbas THEN artbas.AntIPakn ELSE dvare_antpkn
           /* 15 */ tt_Prisfil.vare_link       = dvare_link
           /* 16 */ tt_PrisFil.Vare_Produsent  = TRIM(cVare_Produsent)
                    tt_Prisfil.ProdNr          = IF AVAILABLE Produsent THEN Produsent.ProdNr ELSE 0
           /* 17 */ tt_PrisFil.Vare_Varemerke  = cVare_VareMerke
                    tt_Prisfil.VmId            = IF AVAILABLE Varemerke THEN Varemerke.VmId ELSE 0
           /* 18 */ tt_PrisFil.Butikk_ButNr    = IF AVAILABLE Butiker THEN Butiker.Butik ELSE iButikk_ButNr
                    tt_PrisFil.ButNamn         = IF AVAILABLE Butiker THEN Butiker.ButNamn ELSE ''
                    tt_PrisFil.Butik           = IF AVAILABLE Butiker THEN Butiker.Butik ELSE 0
                    tt_PrisFil.Butik           = IF (bDistribIPS AND AVAILABLE Butiker) THEN Butiker.Butik ELSE tt_PrisFil.Butik
                    tt_Prisfil.hgrprof_brutto% = dhgrprof_brutto%
                    tt_Prisfil.orgdata         = cStr.
    IF dpris_utprisn = 0 AND dpris_engrosn > 0 AND AVAIL vargr AND vargr.Kost_Proc > 0 THEN DO:
        dpris_utprisn = dpris_engrosn * ( 100 / (100 - hgrprof_brutto%)) * (1 + dvare_mva / 100).
        IF dpris_utprisn = ? THEN
           dpris_utprisn = 0.
        ELSE DO:
            dOren = dpris_utprisn - TRUNC(dpris_utprisn,0).
            IF dOren <> 0 THEN DO:
                IF dOren < .5 THEN
                    dOren = .5.
                ELSE IF dOren < .9 THEN
                    dOren = .9.
                ELSE dOren = 1.
                dpris_utprisn = TRUNC(dpris_utprisn,0) + dOren.
            END.
            tt_prisfil.pris_utprisn = dpris_utprisn.
        END.
    END.
    IF dpris_engrosn > 0 THEN DO:
        ASSIGN dUtNetto = ROUND(dpris_utprisn / (1 + dvare_mva / 100),2)
               dTBkr    = dUtNetto - dpris_engrosn.
        ASSIGN tt_Prisfil.fil_tbproc = ROUND(dTBkr / dUtNetto * 100,2) NO-ERROR.
               tt_Prisfil.fil_tbproc = IF tt_Prisfil.fil_tbproc = ? THEN 0 ELSE tt_Prisfil.fil_tbproc.
    END.
    cErrorFelt = "".
    IF cError <> cEntryList THEN DO:
      DO ii = 1 TO NUM-ENTRIES(cError):
          IF ENTRY(ii,cError) <> "" THEN
              cErrorFelt = cErrorFelt + (IF cErrorFelt <> "" THEN " " ELSE "") + ENTRY(ii,cError).
      END.
      CREATE TT_Error.
      ASSIGN tt_Error.RadNr  = iRad
             tt_Error.cError = cErrorFelt.
      RELEASE TT_Error.
    END.
    ELSE DO:
      /* när vi inte har några fel på raden så skall vi jämföra mot db */
      IF NOT CAN-FIND(tt_error WHERE tt_error.radnr = iRad) THEN DO:
        IF tt_Prisfil.lNy = FALSE THEN DO:
          FIND strekkode WHERE strekkode.kode = cstrekkode_kode NO-LOCK NO-ERROR.
          IF NOT AVAIL strekkode THEN
              NEXT.
          FIND artbas OF strekkode NO-LOCK NO-ERROR.
          IF AVAIL ArtBas THEN
              FIND vargr OF artbas NO-LOCK NO-ERROR.
          FIND artpris WHERE artpris.artikkelnr = strekkode.Artikkelnr AND
                             artpris.profilnr   = 1 NO-LOCK NO-ERROR.
          IF NOT AVAIL artpris THEN
              NEXT.
          IF AVAIL Artbas THEN 
          DO:
            IF artbas.beskr <> cvare_varetekst THEN DO:
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"vare_varetekst",ENTRY(5,cColNamn),artbas.beskr).
            END.
            IF artbas.vg <> ivare_hgr THEN DO:
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"vare_hgr",ENTRY(8,cColNamn),STRING(artbas.vg)).
            END.
            IF ArtBas.Mengde <> dvare_mengde THEN DO:
                IF dvare_mengde = 0 THEN
                    ASSIGN tt_prisfil.vare_mengde = ArtBas.Mengde.
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"vare_mengde",ENTRY(6,cColNamn),STRING(artbas.mengde,">>,>>9.999")).
            END.               

            IF (cvare_Cenhet <> ? AND cvare_Cenhet <> '') AND 
               (artbas.jamforenhet <> cvare_Cenhet) THEN DO:
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"vare_enhet",ENTRY(7,cColNamn),STRING(artbas.jamforenhet)).
            END.
            IF artpris.InnkjopsPris[1] <> dpris_engrosn THEN DO:
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"pris_engrosn",ENTRY(9,cColNamn),STRING(artpris.InnkjopsPris[1],">>,>>9.99")).
            END.
            IF artpris.pris[1] <> dpris_utprisn THEN DO:
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"pris_utprisn",ENTRY(11,cColNamn),STRING(artpris.pris[1],">>,>>9.99")).
            END.
            IF artbas.AnbefaltPris <> dpris_veilpris THEN DO:
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"pris_veilpris",ENTRY(12,cColNamn),STRING(artbas.AnbefaltPris,">>,>>9.99")).
            END.
            IF AVAILABLE VarGr AND Vargr.MomsKod <> ivare_mvagr THEN DO:
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"vare_mvagr",ENTRY(13,cColNamn),STRING(Vargr.MomsKod)).
            END.
            IF ArtBas.LinkVareNr <> dvare_link THEN DO:
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"vare_link",ENTRY(15,cColNamn),STRING(ArtBas.LinkVareNr)).
            END.
            FIND FIRST Produsent NO-LOCK WHERE Produsent.Beskrivelse = tt_PrisFil.Vare_Produsent NO-ERROR.
            IF NOT AVAILABLE Produsent OR Produsent.ProdNr <> ArtBas.ProdNr THEN DO:
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"Vare_Produsent",ENTRY(16,cColNamn),(IF AVAILABLE Produsent THEN Produsent.Beskrivelse ELSE STRING(ArtBas.ProdNr))).
            END.
            FIND FIRST Varemerke NO-LOCK WHERE VareMerke.Beskrivelse = tt_PrisFil.Vare_Varemerke NO-ERROR.
            IF NOT AVAILABLE VareMerke OR VareMerke.VmId <> ArtBas.VmId THEN DO:
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"Vare_Varemerke",ENTRY(17,cColNamn),(IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE STRING(ArtBas.VmId))).
            END.
            IF NOT CAN-FIND(Butiker WHERE Butiker.Butik = tt_PrisFil.Butikk_ButNr) THEN DO:
                RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p(iRad,"Butikk_ButNr",ENTRY(18,cColNamn),STRING(tt_PrisFil.Butikk_ButNr)).                 
            END.
          END.
        END.
      END.
      IF CAN-FIND(FIRST TT_VareOrgInfo WHERE TT_VareOrgInfo.Radnr = iRad) THEN tt_Prisfil.cStatus = "J".
    END.
END.
INPUT CLOSE.

/* Bygger en kommaseparert liste med kolonnelabler på de kolonnene som skal importeres. */
c2Tekst = ''.
DO piLoop = 1 TO NUM-ENTRIES(cColNamn):
  IF ENTRY(piLoop,cColNamn) = 'EAN' THEN NEXT.
  
  IF NOT CAN-DO(ExcludeImportFields[1],ENTRY(piLoop,cColNamn)) THEN 
    c2Tekst = c2Tekst + 
             (IF c2Tekst <> '' THEN ',' ELSE '') + 
             ENTRY(piLoop,cColNamn).
END.
/*
MESSAGE 'ExcludeImportFields[1]' ExcludeImportFields[1] SKIP
'ExcludeImportFields[2]' ExcludeImportFields[2] SKIP
'getFieldFromDB' 
getFieldFromDB[ 1]
getFieldFromDB[ 2]
getFieldFromDB[ 3]
getFieldFromDB[ 4]
getFieldFromDB[ 5]
getFieldFromDB[ 6]
getFieldFromDB[ 7]
getFieldFromDB[ 8]
getFieldFromDB[ 9]
getFieldFromDB[10]
getFieldFromDB[11]
getFieldFromDB[12]
getFieldFromDB[13]
getFieldFromDB[14]
getFieldFromDB[15]
getFieldFromDB[16]
getFieldFromDB[17]
getFieldFromDB[18] SKIP
'cColNamn' cColNamn SKIP 
'c2tekst' c2Tekst
VIEW-AS ALERT-BOX.
*/

/* TN 31/8-13 Tar bort alle rader som ikke har endringer hvis filter er aktivert.       */
/* ExcludeImportFields[1] inneholder kommaseparert liste over de felt som IKKE skal med */
IF TRIM(ExcludeImportFields[1]) <> '' THEN 
FOR EACH tt_prisfil:
  
  obOk = FALSE.
  DO piLoop = 1 TO NUM-ENTRIES(c2Tekst):
    IF TRIM(ENTRY(piLoop,c2Tekst)) = '' THEN 
      NEXT.
    IF CAN-FIND(FIRST 
                TT_VareOrgInfo WHERE 
                TT_VareOrgInfo.Rad   = tt_prisfil.Radnr AND 
                TT_VareOrgInfo.cFelt = ENTRY(piLoop,c2Tekst)) THEN 
    DO:
      obOk = TRUE.
    END.
  END.
  
  /* Det er opprettet en post i TT_VAreorgInfo ffor hvert felt på recorden som er endret. */
  /* Er det ingen endringer på recorden, slettes hele linjen.                             */
  IF obOk = FALSE THEN 
  DO: 
    FOR EACH TT_VareOrgInfo WHERE 
             TT_VareOrgInfo.Rad = tt_prisfil.Radnr:
      DELETE TT_VareOrgInfo.          
    END. 
    DELETE tt_PrisFil.
  END.
END.
/*
OUTPUT TO value('tnc.csv').
FOR EACH TT_VareOrgInfo:
  PUT UNFORMATTED 
    TT_VareOrgInfo.Radnr ';'  
    TT_VareOrgInfo.cTTfelt ';'
    TT_VareOrgInfo.cFelt ';'  
    TT_VareOrgInfo.cVerdi ';'  
    SKIP .
END.
OUTPUT CLOSE.
*/
/* **********************  Internal Procedures  *********************** */

PROCEDURE initTTMva:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
    OPPRETT_MOMS:
    FOR EACH moms NO-LOCK.
        IF CAN-FIND(FIRST tt_Moms WHERE
                    tt_Moms.MomsKod = Moms.MomsKod) THEN 
                    LEAVE OPPRETT_MOMS.
        CREATE tt_moms.
        BUFFER-COPY moms TO tt_moms NO-ERROR.
        IF ERROR-STATUS:ERROR 
          THEN DELETE tt_Moms.
        ELSE RELEASE tt_moms.
    END. /* OPPRETT_MOMS */
END PROCEDURE.

