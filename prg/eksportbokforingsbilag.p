&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksportbokforingsbilag.p
    Purpose     : Eksport av bokforingsbilag fra butikk til hk.
                  Eksporterer alle ikke tidligere eksporterte bokføringsbiag.
                  - Ved eksport settes eksportertdato
                  - endres et bokf.bilag etter at det er eksportert, nullstilles eksportdato igjen. 

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    
&ELSE
    
&ENDIF

DEFINE INPUT  PARAMETER dinpFraDato AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER dinpTilDato AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER ocRetur     AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

DEF VAR iAntLinjer          AS INT     NO-UNDO.
DEF VAR iAlle               AS INT     NO-UNDO.
DEF VAR bStream             AS LOG     NO-UNDO.
DEFINE VARIABLE lDec        AS DECIMAL NO-UNDO.
DEFINE VARIABLE bManuell    AS LOG     NO-UNDO.
DEFINE VARIABLE lTid        AS INTEGER NO-UNDO.
DEFINE VARIABLE cLinje      AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFraDato    AS DATE    NO-UNDO.
DEFINE VARIABLE dTilDato    AS DATE    NO-UNDO.
DEFINE VARIABLE pdLoopDato  AS DATE    NO-UNDO.

/* Filhåndtering */
DEF VAR cFilNavn   AS CHAR FORMAT "x(40)"     NO-UNDO.
DEFINE VARIABLE ctmpFilNavn AS CHARACTER NO-UNDO.
DEF VAR cKatalog   AS CHAR                    NO-UNDO.
DEF VAR cPrefix    AS CHAR                    NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.

DEFINE STREAM Inn.
DEF STREAM Ut.

{manedsrapport_tmptabell.i &NEW = "NEW" &SHARED = "SHARED"}

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
         HEIGHT             = 14.24
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


{syspara.i 1 1 61 cKatalog}
IF cKatalog = '' THEN 
DO:
  {syspara.i 1 1 51 cKatalog}
  IF cKatalog = '' THEN
    cKatalog = 'c:\home\lindbak\sendes'. 
END.

ASSIGN lTid = TIME.

IF dinpFraDato = ? OR dinpTilDato = ? THEN 
DO: 
  RUN SettFraTilDato.
  RUN bibl_logg.p ('eksporter_bokforingsbilag', 'eksportbokforingsbilag.p: AUTO WinCheduler Periode: ' + string(dinpFraDato) + ' - ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
END.
ELSE DO:
  ASSIGN 
    dFraDato = dinpFraDato
    dTilDato = dinpTilDato
    bManuell = TRUE. /* Flagger at eksporten kjøres manuelt og at det ikke skal sendes eMail. */
  RUN bibl_logg.p ('eksporter_bokforingsbilag', 'eksportbokforingsbilag.p: MANUELL Periode: ' + string(dinpFraDato) + ' - ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
END.

BUTIKKLOOP:
FOR EACH Butiker NO-LOCK WHERE
  Butiker.harButikksystem = TRUE AND
  Butiker.ApningsDato     <> ? AND
  Butiker.NedlagtDato     = ?: 

  /* Tømmer temp-tabell */
  FOR EACH tmpManedsrap:
      DELETE tmpManedsrap.
  END.
  
  /* Bygger temptabell. */
  RUN manedsrapport_bygg_tmptabell.p (Butiker.Butik,dFraDato,dTilDato).

  /* Legger ut data til fil. */
  RUN Eksporter (Butiker.Butik).

END. /* BUTIKKLOOP */

ocRetur = "OK," + STRING(iAntEksport) + cTekst.

lTid = TIME - lTid.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Eksporter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Eksporter Procedure 
PROCEDURE Eksporter :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iButikkNr AS INT  NO-UNDO.

DEFINE VARIABLE cFilNavn        AS CHAR NO-UNDO.
DEFINE VARIABLE iAnt            AS INT  NO-UNDO.
DEFINE VARIABLE cEAN            AS CHAR NO-UNDO.

ASSIGN
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cFilNavn    = cKatalog + '\' + 'POSB' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpPOSB' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

IF CAN-FIND(FIRST tmpManedsrap WHERE
    tmpManedsrap.ButikkNr = iButikkNr) THEN 
UTLEGG:
DO:  
    RUN bibl_logg.p ('eksporter_kreditsalg', 'eksportsalg.p Butikk: ' + string(Butiker.Butik)+ ' Fil: ' + string(cFilNavn) + STRING(Butiker.Butik)).
    
    OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + STRING(Butiker.Butik)).
    UTLEGG_AV_HEADING:
    DO:
        PUT STREAM Ut UNFORMATTED 
        /* A  */ "Butikknr.;"          
        /* B  */ "Dato;"              
        /* C  */ "Bokf.nr.;"                 
        /* D  */ "Opptalt Kasse;"        
        /* E  */ "Bank Pose;"               
        /* F  */ "Bank Kort;"              
        /* G  */ "Visa;"                   
        /* H  */ "Eurocard;"          
        /* I  */ "Amex;"                   
        /* J  */ "Diners;"           
        /* K  */ "Senter gavekort;"        
        /* L  */ "Diverse Kort;"       
        /* M  */ "Tilbake bet. Kunde;"                  
        /* N  */ "Beskrivelse;"      
        /* O  */ "Tilgode brukt egne;"       
        /* P  */ "Tilgode brukt andre;"     
        /* Q  */ "Gavekort brukt egne;"      
        /* R  */ "Gavekort brukt andre;"     
        /* S  */ "Tilgode ut;"             
        /* T  */ "Gavekort ut;"                   
        /* U  */ "Innbetalt Kunde;"      
        /* V  */ "Sum inn butikk;"        
        /* W  */ "Oms. Ekskl.kred;"             
        /* X  */ "Diff;"       
        /* Y  */ "Kreditsalg;" 
        /* Z  */ "Oms.MvaGrp1 Inkl.kred;"   
        /* AA */ "Oms.MvaGrp2 Inkl.kred;"   
        /* AB */ "Oms.MvaGrpDiv Inkl.kred;"   
        /* AC */ "Omsetningt Inkl.kred;"
        /* AD */ "Inn/Utbetalinger;"
        /* AE */ "Tot.Varekost;"
        /* AF */ "Varekost lagerstyrt"
        SKIP.   
        
    END. /* UTLEGG_AV_HEADING */
    
    UTLEGG_AV_DATA:
    FOR EACH tmpManedsrap WHERE 
        tmpManedsrap.ButikkNr = iButikkNr
        BREAK BY tmpManedsrap.ButikkNr
              BY tmpManedsrap.Dato:
        PUT STREAM Ut unformatted
            /* A  */ tmpManedsrap.ButikkNr ';'         
            /* B  */ tmpManedsrap.Dato ';'              
            /* C  */ tmpManedsrap.BokfNr ';'                 
            /* D  */ tmpManedsrap.Kontant ';'               
            /* E  */ tmpManedsrap.BankPose ';'               
            /* F  */ tmpManedsrap.BankKort ';'               
            /* G  */ tmpManedsrap.Visa ';'              
            /* H  */ tmpManedsrap.Eurocard ';'               
            /* I  */ tmpManedsrap.Amex ';'              
            /* J  */ tmpManedsrap.Diners ';'                 
            /* K  */ tmpManedsrap.SenterGavekort ';'        
            /* L  */ tmpManedsrap.DiverseKort ';'            
            /* M  */ tmpManedsrap.KontKjopKasse ';'     
            /* N  */ tmpManedsrap.Beskrivelse ';'            
            /* O  */ tmpManedsrap.TilgodeBruktEgne ';'       
            /* P  */ tmpManedsrap.TilgodeBruktAndre ';'      
            /* Q  */ tmpManedsrap.GavekortBruktEgne ';'      
            /* R  */ tmpManedsrap.GavekortBruktAndre ';'     
            /* S  */ tmpManedsrap.TilgodeUt ';'              
            /* T  */ tmpManedsrap.GavekortUt ';'             
            /* U  */ tmpManedsrap.InnbetaltKunde ';'      
            /* V  */ tmpManedsrap.SumInnbutikk ';'           
            /* W  */ tmpManedsrap.OmsetningEksKred ';'       
            /* X  */ tmpManedsrap.DiffKasse ';'        
            /* Y  */ tmpManedsrap.Kreditsalg ';'       
            /* Z */ tmpManedsrap.OmsetningMvaGrp1 ';'   
            /* AA */ tmpManedsrap.OmsetningMvaGrp2 ';'   
            /* AB */ tmpManedsrap.OmsetningMvaGrpDiv ';'   
            /* AC */ (tmpManedsrap.OmsetningMvaGrp1 + 
                      tmpManedsrap.OmsetningMvaGrp2 +  
                      tmpManedsrap.OmsetningMvaGrpDiv) ';'
            /* AD */ tmpManedsrap.InnUtbetaling ';'
            /* AE */ tmpManedsrap.TotVarekost ';'
            /* AF */ tmpManedsrap.LagerstyrtVarekost
              SKIP.             
    END. /* UTLEGG_AV_DATA */
    
    OUTPUT STREAM Ut CLOSE.
    /* Gir filen dens riktige navn og tar bort den temporære filen. */
    OS-COPY value(ctmpFilNavn + STRING(Butiker.Butik)) value(cFilNavn + STRING(Butiker.Butik)).
    IF SEARCH(cFilNavn + STRING(Butiker.Butik)) <> ? THEN
        OS-DELETE VALUE(ctmpFilNavn + STRING(Butiker.Butik)).
    
END. /* UTLEGG */
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SettFraTilDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettFraTilDato Procedure 
PROCEDURE SettFraTilDato :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE pdDato AS DATE NO-UNDO.

{syspara.i 210 202 1 pcTekst}
ASSIGN
  pdDato = DATE(pcTekst) NO-ERROR.
IF ERROR-STATUS:ERROR OR pcTekst = '' 
THEN ASSIGN pdDato = TODAY.
ELSE ASSIGN pdDato = pdDato + 1.

ASSIGN
  dFraDato    = pdDato - 60
  dTilDato    = (IF pdDato > TODAY THEN pdDato ELSE TODAY)
  dinpFraDato = dFraDato
  dinpTilDato = dTilDato
  .

DO TRANSACTION:
  {setsyspara.i 210 101 1 STRING(dTilDato)}
  IF AVAILABLE SysPara THEN 
    RELEASE SysPara.
END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

