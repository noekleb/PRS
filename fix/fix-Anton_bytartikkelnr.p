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

DEF VAR cInnFil AS CHAR NO-UNDO.
DEF VAR cUtFil  AS CHAR NO-UNDO.
def var iAntall as int no-undo.
def var cLinje  as char no-undo.

def var ix1ArtikkelNr as dec format ">>>>>>>>>>>>9" no-undo.
def var ix2ArtikkelNr as dec format ">>>>>>>>>>>>9" no-undo.

CURRENT-WINDOW:WIDTH = 250.

DEF STREAM Inn.
DEF STREAM Ut.

ASSIGN
    cInnFil = 'C:\appdir\AntonKonv\Utrekk av strekkoder fra Anton og match mot Sport1\Anton_Artikkler_sport1_Ulik_renset.csv'
    cUtFil  = 'C:\appdir\AntonKonv\Utrekk av strekkoder fra Anton og match mot Sport1\Anton_Artikkler_sport1_Ulik_renset_err.csv'
    .

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


input stream inn from value(cInnfil).
output stream Ut to value(cUtFil).

repeat:
  import stream Inn unformatted
    cLinje.
    
  assign
    ix1ArtikkelNr = dec(entry(1,cLinje,';'))
    ix2ArtikkelNr = dec(entry(8,cLinje,';'))
    iAntall = iAntall + 1
    .
    
      
  if iAntall modulo 100 = 0 then
    do:
      pause 0.
      display 
        iAntall
        ix1ArtikkelNr
        ix2ArtikkelNr
        entry(1,cLinje,';')
        entry(8,cLinje,';')
        with width 250 frame g.
    end.
    
  if not can-find(ArtBas where ArtBas.ArtikkelNr = ix1ArtikkelNr) then
  do:
    output to value("Anton_artikkelnr_mangler.txt") append.
    put unformatted 
      "MAngler: " ix2ArtikkelNr " "
      today " "
      string(time,"HH:MM:SS") skip.
    output close.
  end.    
  else if can-find(ArtBas where ArtBas.ArtikkelNr = ix2ArtikkelNr) then
  do:
    output to value("Anton_artikkelnr_finnes_fra_før.txt") append.
    put unformatted 
      "Kan ikke flyttes: " ix1ArtikkelNr " "
      ix2ArtikkelNr " "
      today " Finnes fra før."
      string(time,"HH:MM:SS") skip.
    output close.
  end.
  else do:
    output to value("Anton_artikkelnr_konv_fremdrift.txt") append.
    put unformatted 
      "Antall: " iAntall " "
      ix1ArtikkelNr " "
      ix2ArtikkelNr " "
      today " "
      string(time,"HH:MM:SS") skip.
    output close.
  
    /* Allt eller intet på en artikkel. */
    do transaction:
      run byttArtikkelNr (ix1ArtikkelNr, ix2ArtikkelNr).
    end.
  end.
    
end. /* Repeat */

output stream Ut close.
input stream inn close.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-byttArtikkelNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byttArtikkelNr Procedure 
PROCEDURE byttArtikkelNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input parameter iOldArtikkelNr as dec no-undo.
def input parameter iNewArtikkelNr as dec no-undo.

if not can-find(ArtBas where ArtBas.ArtikkelNr = iOldArtikkelNr) then
  do:
    output to value("Anton_Ukjente_artikkelnr.txt") append.
    put unformatted 
      iOldArtikkelNr " "
      today " "
      string(time,"HH:MM:SS").
    output close.
    return.
    
  end.
for each StrekKode where StrekKode.ArtikkelNr = iOldArtikkelNr:
    StrekKode.ArtikkelNr = iNewArtikkelNr.
end.

for each Individ where Individ.ArtikkelNr = iOldArtikkelNr:
    Individ.ArtikkelNr = iNewArtikkelNr.
end.

for each LevLager where LevLager.ArtikkelNr = iOldArtikkelNr:
    LevLager.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtBas where ArtBas.ArtikkelNr = iOldArtikkelNr:
    ArtBas.ArtikkelNr = iNewArtikkelNr.
end.

for each ReklamasjonsLinje where ReklamasjonsLinje.ArtikkelNr = iOldArtikkelNr:
    ReklamasjonsLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each ReklamasjonsLogg where ReklamasjonsLogg.ArtikkelNr = iOldArtikkelNr:
    ReklamasjonsLogg.ArtikkelNr = iNewArtikkelNr.
end.

for each Erstattningsvare where Erstattningsvare.ArtikkelNr = iOldArtikkelNr:
    Erstattningsvare.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtLag where ArtLag.ArtikkelNr = iOldArtikkelNr:
    ArtLag.ArtikkelNr = iNewArtikkelNr.
end.

for each PakkeLinje where PakkeLinje.ArtikkelNr = iOldArtikkelNr:
    PakkeLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtPris where ArtPris.ArtikkelNr = iOldArtikkelNr:
    ArtPris.ArtikkelNr = iNewArtikkelNr.
end.

for each Lager where Lager.ArtikkelNr = iOldArtikkelNr:
    Lager.ArtikkelNr = iNewArtikkelNr.
end.

for each LevPris where LevPris.ArtikkelNr = iOldArtikkelNr:
    LevPris.ArtikkelNr = iNewArtikkelNr.
end.

for each PrisKo where PrisKo.ArtikkelNr = iOldArtikkelNr:
    PrisKo.ArtikkelNr = iNewArtikkelNr.
end.

for each BestHode where BestHode.ArtikkelNr = iOldArtikkelNr:
    BestHode.ArtikkelNr = iNewArtikkelNr.
end.

for each BestPris where BestPris.ArtikkelNr = iOldArtikkelNr:
    BestPris.ArtikkelNr = iNewArtikkelNr.
end.

for each TransLogg where TransLogg.ArtikkelNr = iOldArtikkelNr:
    TransLogg.ArtikkelNr = iNewArtikkelNr.
end.

for each TelleLinje where TelleLinje.ArtikkelNr = iOldArtikkelNr:
    TelleLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each OvArt where OvArt.ArtikkelNr = iOldArtikkelNr:
    OvArt.ArtikkelNr = iNewArtikkelNr.
end.

for each OvLinje where OvLinje.ArtikkelNr = iOldArtikkelNr:
    OvLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each HPrisKo where HPrisKo.ArtikkelNr = iOldArtikkelNr:
    HPrisKo.ArtikkelNr = iNewArtikkelNr.
end.

for each KundeTrans where KundeTrans.ArtikkelNr = iOldArtikkelNr:
    KundeTrans.ArtikkelNr = iNewArtikkelNr.
end.

for each MedTrans where MedTrans.ArtikkelNr = iOldArtikkelNr:
    MedTrans.ArtikkelNr = iNewArtikkelNr.
end.

for each OvBuffer where OvBuffer.ArtikkelNr = iOldArtikkelNr:
    OvBuffer.ArtikkelNr = iNewArtikkelNr.
end.

for each AltLevBas where AltLevBas.ArtikkelNr = iOldArtikkelNr:
    AltLevBas.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtBestPkt where ArtBestPkt.ArtikkelNr = iOldArtikkelNr:
    ArtBestPkt.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtLok where ArtLok.ArtikkelNr = iOldArtikkelNr:
    ArtLok.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBokLinje where VareBokLinje.ArtikkelNr = iOldArtikkelNr:
    VareBokLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBehLinje where VareBehLinje.ArtikkelNr = iOldArtikkelNr:
    VareBehLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBehLinjeTrans where VareBehLinjeTrans.ArtikkelNr = iOldArtikkelNr:
    VareBehLinjeTrans.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBehPris where VareBehPris.ArtikkelNr = iOldArtikkelNr:
    VareBehPris.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBehBestHode where VareBehBestHode.ArtikkelNr = iOldArtikkelNr:
    VareBehBestHode.ArtikkelNr = iNewArtikkelNr.
end.

for each FakturaLinje where FakturaLinje.ArtikkelNr = iOldArtikkelNr:
    FakturaLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtSort where ArtSort.ArtikkelNr = iOldArtikkelNr:
    ArtSort.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBokTemaLinje where VareBokTemaLinje.ArtikkelNr = iOldArtikkelNr:
    VareBokTemaLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each KampanjeLinje where KampanjeLinje.ArtikkelNr = iOldArtikkelNr:
    KampanjeLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each PkSdlLinje where PkSdlLinje.ArtikkelNr = iOldArtikkelNr:
    PkSdlLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each PkSdlPris where PkSdlPris.ArtikkelNr = iOldArtikkelNr:
    PkSdlPris.ArtikkelNr = iNewArtikkelNr.
end.

for each VPIMottak where VPIMottak.ArtikkelNr = iOldArtikkelNr:
    VPIMottak.ArtikkelNr = iNewArtikkelNr.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

