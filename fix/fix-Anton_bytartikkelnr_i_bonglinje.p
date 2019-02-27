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
    cInnFil = 'C:\Appdir\Anton_Artikkler_sport1_Ulik_renset.csv'
    cUtFil  = 'C:\Appdir\Anton_Artikkler_sport1_Ulik_renset_bonglinje_err.csv'
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
  
  /* Disse artiklene er dubletter som vi ikke kunne endre artikkelnr på. de skla beholde sine transaksjoner. */  
  if can-find(ArtBAs where ArtBas.ArtikkelNr = ix1ArtikkelNr) then
    next.  
  
  /* Flytter alle translogg for artikkelen. Allt eller intet på artikkelen. */
  do transaction:
    run byttArtikkelNrBongLinje (ix1ArtikkelNr, ix2ArtikkelNr).
  end. 
  
  /* Logger fremdrift til fil. */
  FREMDRIFT:
  do:
    output to value("Anton_artikkelnr_konv_fremdrift_bonglinje.txt") append.
    put unformatted 
      "Antall: " iAntall " "
      ix1ArtikkelNr " "
      ix2ArtikkelNr " "
      today " "
      string(time,"HH:MM:SS") skip.
    output close.  
  end. /* FREMDRIFT */
    
end. /* Repeat */

output stream Ut close.
input stream inn close.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-byttArtikkelNrBongLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byttArtikkelNrBongLinje Procedure 
PROCEDURE byttArtikkelNrBongLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input parameter iOldArtikkelNr as dec no-undo.
def input parameter iNewArtikkelNr as dec no-undo.

if not can-find(ArtBas where ArtBas.ArtikkelNr = iNewArtikkelNr) then
    return.
  
find ArtBas no-lock where
  ArtBas.ArtikkelNr = iNewArtikkelNr no-error.  
for each BongLinje where
  BongLinje.ArtikkelNr = string(iOldArtikkelNr):
  
  assign
    BongLinje.ArtikkelNr = string(iNewArtikkelNr)
    BongLinje.VareGr     = ArtBas.Vg
    .
end.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

