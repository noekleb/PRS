
/*------------------------------------------------------------------------
    File        : eksporter_kampanjevarer.p
    Purpose     : Ved å få eksportert disse varene, vil det være mulig å kunne evaluere disse og se hva som skal kjøres videre på kampanje og hva som eventuelt skal settes ytterliger ned.

    Syntax      : run eksporter_kampanjevarer.p (cKampIdLst)

    Description : Eksport av varene i de kampanjene som ligger i den mottatte listen med kampanjeid.

    Author(s)   : Tom nøkleby
    Created     : Tue Jan 31 10:31:18 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER cKampIdLst AS CHARACTER NO-UNDO.

DEFINE VARIABLE cUtskriftKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE iKampId AS INTEGER NO-UNDO.
DEFINE VARIABLE cbutLst AS CHARACTER NO-UNDO.

DEF VAR chExcelApplication        AS COM-HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttKamp
    FIELD LevKod       LIKE ArtBas.LevKod
    FIELD Kode         LIKE Strekkode.Kode
    FIELD Beskr        LIKE ArtBas.Beskr
    FIELD LevFargKod   LIKE ArtBas.LevFargKod
    FIELD Storl        LIKE StrKonv.Storl
    FIELD Varemerke    LIKE Varemerke.Beskrivelse
    FIELD AnbefaltPris LIKE ArtBas.AnbefaltPris /* Veil.pris */
    FIELD KatalogPris  LIKE ArtBas.KatalogPris  /* Katalogpris */
    FIELD Vg           LIKE ArtBas.Vg
    FIELD Sasong       LIKE ArtBas.Sasong
    FIELD ForhRab%     LIKE ArtBas.ForhRab%
    FIELD LevNr        LIKE ArtBas.LevNr
    FIELD ArtikkelNr   LIKE ArtBas.ArtikkelNr
    FIELD SistSolgt    AS DATE INITIAL ? 
    FIELD FraDato      AS DATE INITIAL ? EXTENT 10
    FIELD TilDato      AS DATE INITIAL ? EXTENT 10
    FIELD KampId       AS INTEGER FORMAT ">>>>>>>9" EXTENT 10
    FIELD AntSolgt     AS DECIMAL FORMAT '->>>,>>>,>>9' EXTENT 10
    FIELD KampRab%     AS DECIMAL FORMAT '->>9' EXTENT 10
    FIELD KampPris     AS DECIMAL FORMAT '->>>,>>>,>>9' EXTENT 10
    INDEX Modell LevNr LevKod Beskr Storl
    INDEX Artikkel ArtikkelNr
    .

DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION getExcelHandle RETURNS COM-HANDLE 
	(  ) FORWARD.


/* ***************************  Main Block  *************************** */
{syspara.i 1 1 8 cUtskriftKatalog}
ASSIGN 
    cUtskriftKatalog = RIGHT-TRIM(cUtskriftKatalog,'\')
    cUtskriftKatalog = cUtskriftKatalog + '\'
    .
    
{syspara.i 17 20 1 cButLst}    
IF cButLst = '' THEN 
    cButLst   = '15'.
    
ASSIGN     
    cExcelFil = 'kampanje_varer_' + REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.csv'
    .

IF TRIM(cKampIdLst) = '' THEN 
    RETURN.
ELSE 
   cKampIdLst = REPLACE(TRIM(cKampIdLst),'|',',').

/* Oppretter en record. pr. relevant strekkode. */
RUN byggTmpTbl.

/* Legger på salg som er gjort i en eller flere butikker. */
IF CAN-FIND(FIRST ttKamp) AND cButLst <> '' THEN 
    RUN setsalgTmpTbl.

IF CAN-FIND(FIRST ttKamp) THEN 
    RUN eksporterTmpTbl.

/* **********************  Internal Procedures  *********************** */

PROCEDURE byggTmpTbl:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO iLoop = 1 TO NUM-ENTRIES(cKampIdLst):
        iKampId = INT(ENTRY(iLoop,cKampIdLst)).
        
        FIND KampanjeHode NO-LOCK WHERE 
            KampanjeHode.KampanjeId = iKampId NO-ERROR.
        IF NOT AVAILABLE KampanjeHode THEN 
            NEXT.
            
        LINJE:
        FOR EACH Kampanjelinje OF KampanjeHode NO-LOCK:
            IF CAN-FIND(FIRST ttKamp WHERE 
                        ttKamp.ArtikkelNr = KampanjeLinje.ArtikkelNr) THEN 
                NEXT.
            
            FIND ArtBas NO-LOCK WHERE 
                ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.
            IF AVAILABLE ArtBas THEN 
                FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
                
            IF NOT AVAILABLE ArtBas OR NOT AVAILABLE Varemerke THEN 
                NEXT.
            
            /* Alle relevante størrelser på artikkelen skal legges opp.       */
            /* Om strekkoden er relevant, ligger det translogg poster på den. */
            STREKKODE:
            FOR EACH Strekkode OF ArtBas NO-LOCK,
                FIRST StrKonv OF Strekkode NO-LOCK /*,  
                FIRST TransLogg NO-LOCK WHERE 
                      TransLogg.ArtikkelNr = Strekkode.ArtikkelNr AND 
                      TransLogg.Storl      = StrKonv.Storl */:
            
                CREATE ttKamp.
                ASSIGN 
                    ttKamp.LevKod       = ArtBas.LevKod      
                    ttKamp.Kode         = Strekkode.Kode     
                    ttKamp.Beskr        = ArtBas.Beskr     
                    ttKamp.LevFargKod   = ArtBas.LevFargKod
                    ttKamp.Storl        = StrKonv.Storl
                    ttKamp.Varemerke    = Varemerke.Beskrivelse
                    ttKamp.AnbefaltPris = ArtBas.AnbefaltPris
                    ttKamp.KatalogPris  = ArtBas.KatalogPris
                    ttKamp.Vg           = ArtBas.Vg
                    ttKamp.Sasong       = ArtBas.Sasong
                    ttKamp.ForhRab%     = ArtBas.ForhRab%
                    ttKamp.LevNr        = ArtBas.LevNr
                    ttKamp.ArtikkelNr   = ArtBas.ArtikkelNr
                    .
            END. /* STREKKODE */
        END. /* LINJE */        
    END.    

END PROCEDURE.

PROCEDURE eksporterTmpTbl:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE chExcelApplication      AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkbook              AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorksheet             AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE cFil                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piLoop                  AS INTEGER NO-UNDO.
    DEFINE VARIABLE cHeading                AS CHARACTER INITIAL ',,,,,,,,,' NO-UNDO.
        
    OUTPUT STREAM Ut TO VALUE(cUtskriftKatalog + cExcelFil).
    PUT STREAM Ut UNFORMATTED
            'LevModellNr;'
            'EANnr;'   
            'VareTekst;'   
            'FargeKode;'   
            'Str1;'    
            'Varemerke;'   
            'VeilPris og  MarkedsPris;'    
            'LevPrisEngros og nettoForh;'  
            'VareGruppe;'  
            'Sesong;'  
            'forhRab%;'    
            'LevNr;'
            'SistSolgt;'
        .
    /* Logger for utlegg av heading */
    OVERSKRIFTER:
    FOR EACH ttKamp:
        DO piLoop = 1 TO 10:
            IF ttKamp.FraDato[piLoop] <> ? THEN
                ASSIGN  
                    ENTRY(piLoop,cHeading) = '1'
                    .
        END.
        IF cHeading = '1,1,1,1,1,1,1,1,1,1' THEN 
            LEAVE OVERSKRIFTER.
    END. /* OVERSKRIFTER */
    
    /* Legger ut ekstra kolonnelabler */
    DO piLoop = 1 TO 10:
        IF ENTRY(piLoop,cHeading) = '1' THEN 
        DO:
            PUT STREAM Ut UNFORMATTED 
                'KampanjeId;'
                'FraDato;'
                'TilDato;' 
                'KampRab%;'   
                'KampPris;'
                'AntSolgt;'     
            .
        END.
    END.        
    PUT STREAM Ut SKIP.
    
    FOR EACH ttKamp:
        PUT STREAM Ut UNFORMATTED
            ttKamp.LevKod ';'      
            ttKamp.Kode ';'        
            ttKamp.Beskr ';'       
            ttKamp.LevFargKod ';'  
            ttKamp.Storl ';'       
            ttKamp.Varemerke ';'   
            ttKamp.AnbefaltPris ';'
            ttKamp.KatalogPris ';' 
            ttKamp.Vg ';'          
            ttKamp.Sasong ';'      
            ttKamp.ForhRab% ';'    
            ttKamp.LevNr ';'
            ttKamp.SistSolgt ';'        
        .
        
        /* Legger ut Extenter. */
        EKSTENTER:
        DO piLoop = 1 TO 10:
            IF ttKamp.FraDato[piLoop] = ? THEN 
                NEXT.
            PUT STREAM Ut UNFORMATTED
                ttKamp.KampId[piLoop] ';'
                ttKamp.FraDato[piLoop] ';'
                ttKamp.TilDato[piLoop] ';' 
                ttKamp.KampRab%[piLoop] ';'   
                ttKamp.KampPris[piLoop] ';'
                ttKamp.AntSolgt[piLoop] ';'     
            .
        END. /* EKSTENTER */   
        PUT STREAM Ut SKIP.
                  
    END.
    OUTPUT STREAM Ut CLOSE.
    
    ASSIGN 
        FILE-INFO:FILE-NAME = SEARCH(cUtskriftKatalog + cExcelFil) 
        cFil = FILE-INFO:FULL-PATHNAME 
        .
    chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
    chExcelApplication:Workbooks:OpenText(cFil,2,,,,,TRUE).

    chExcelApplication:ScreenUpdating = FALSE.

    ASSIGN chWorkbook                            = chExcelApplication:WorkBooks:ITEM(1)
           chWorkSheet                           = chExcelApplication:Sheets:ITEM(1)
           chWorkSheet:NAME                      = "Kamp.artikler"

           chWorkSheet:Rows(1):FONT:Bold         = TRUE
           chWorkSheet:PageSetup:PrintTitleRows  = "$1:$1"
           chWorkSheet:Range("A:B"):NumberFormat = "##0"
           chWorkSheet:Range("G:H"):NumberFormat = "# ##0,00"
           chWorkSheet:Range("I:I"):NumberFormat = "##0"
           chWorkSheet:Range("C:C"):columnwidth  = chWorkSheet:Range("C:C"):columnwidth * 3
             .             
    chWorkSheet:Range("A2"):Select().
    ASSIGN chExcelApplication:ActiveWindow:FreezePanes = TRUE.

    chExcelApplication:ScreenUpdating = YES.
    chExcelApplication:VISIBLE = TRUE.
        
    RELEASE OBJECT chWorksheet NO-ERROR.
    RELEASE OBJECT chWorkbook NO-ERROR.
        
END PROCEDURE.

PROCEDURE setsalgTmpTbl:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE piButNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
    
    STORLOOP:
    FOR EACH ttKamp:
        LOOPEN:
        DO iLoop = 1 TO NUM-ENTRIES(cKampIdLst):
            iKampId = INT(ENTRY(iLoop,cKampIdLst)).
            IF iLoop > 10 THEN 
                LEAVE LOOPEN.
        
            FIND KampanjeHode NO-LOCK WHERE 
                KampanjeHode.KampanjeId = iKampId NO-ERROR.
            IF NOT AVAILABLE KampanjeHode THEN 
                NEXT.
            
            LINJE:
            FOR EACH Kampanjelinje OF KampanjeHode NO-LOCK WHERE
                KampanjeLinje.ArtikkelNr = ttKamp.ArtikkelNr: 
                    
                BUTIKKLOOP:    
                DO piLoop = 1 TO NUM-ENTRIES(cButLst):
                    pibutNr = INT(ENTRY(piLoop,cButLst)).
                    
                    /* Finner sist solgt dato. */
                    FIND LAST TransLogg NO-LOCK WHERE 
                        Translogg.ArtikkelNr = ttKamp.ArtikkelNr AND 
                        TransLogg.Storl      = ttKamp.Storl AND 
                        Translogg.Dato       > 01/01/2000 AND 
                        TransLogg.Tid        >= 0 AND 
                        TransLogg.Butik      = piButNr AND 
                        TransLogg.TTId       = 1 USE-INDEX OppslagStr NO-ERROR.
                    IF AVAILABLE TransLogg THEN 
                        DO:
                            IF ttKamp.SistSolg = ? THEN 
                                ttKamp.sistSolgt = TransLogg.Dato.
                            ELSE IF ttKamp.SistSolgt < TransLogg.Dato THEN 
                                ttKamp.sistSolgt = TransLogg.Dato.
                        END.
                    /* Setter fra - til dato, pris og rabatt. */
                    ASSIGN 
                        ttKamp.KampId[iLoop]   = iKampId
                        ttKamp.FraDato[iLoop]  = KampanjeHode.StartDato
                        ttKamp.TilDato[iLoop]  = KampanjeHode.SluttDato 
                        ttKamp.KampRab%[iLoop] = ROUND(((ttKamp.AnbefaltPris - KampanjeLinje.Pris[2]) / ttKamp.AnbefaltPris) * 100,2)   
                        ttKamp.KampPris[iLoop] = KampanjeLinje.Pris[2]
                        .        
    
                    /* Summerer opp salg. */
                    FOR EACH TransLogg NO-LOCK WHERE 
                        Translogg.ArtikkelNr = ttKamp.ArtikkelNr AND 
                        TransLogg.Storl      = ttKamp.Storl AND 
                        Translogg.Dato      >= ttKamp.FraDato[iLoop] AND 
                        Translogg.Dato      <= ttKamp.TilDato[iLoop] AND 
                        TransLogg.Tid        >= 0 AND 
                        TransLogg.Butik      = piButNr AND 
                        TransLogg.TTId       = 1:
                        
                        ASSIGN 
                            ttKamp.AntSolgt[iLoop] = ttKamp.AntSolgt[iLoop] + TransLogg.Antall.     
                    END.
                                     
                END. /* BUTIKKLOOP */             
        
            END. /* LINJE */
        END. /* LOOPEN */
    END. /* STORLOOP */
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION getExcelHandle RETURNS COM-HANDLE 
	(  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

    DEF VAR chActiveWin     AS COM-HANDLE NO-UNDO.
    DEF VAR chExcelInstance AS COM-HANDLE NO-UNDO.
    
    CREATE "Excel.Application" chExcelInstance NO-ERROR.
    IF VALID-HANDLE(chExcelApplication) THEN
      chExcelInstance:VISIBLE = FALSE.
    RETURN chExcelInstance.
		
END FUNCTION.

