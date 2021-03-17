DEF VAR piLinjeNr  AS INT  NO-UNDO.
DEF VAR cInnFilNavn AS CHAR NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cKode  AS CHAR NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.

DEF STREAM Ut.
DEF STREAM Inn.

  ASSIGN
      iButNr      = 406
      piLinjeNr   = 1
      cInnFilNavn = 'C:\ArkivDokument\Kunder\Stensland\Lagerfil.csv'
      cFilNavn    = 'C:\ArkivDokument\Kunder\Stensland\VARETRAN_StenslandLager.' + STRING(iButNr)
      .
      
INPUT STREAM Inn FROM VALUE(cInnFilNavn).      
OUTPUT STREAM Ut TO VALUE(cFilNavn).

LESERLINJER:
REPEAT:
  
  IMPORT STREAM Inn UNFORMATTED cLinje.
  ASSIGN cKode = ENTRY(1,cLinje,';') NO-ERROR.
  IF ERROR-STATUS:ERROR THEN NEXT.
  
  RUN bibl_chkean.p (INPUT-OUTPUT cKode).
  FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = cKode NO-ERROR.
  IF AVAILABLE Strekkode THEN
      FIND ArtBAs OF STrekkode NO-LOCK NO-ERROR.

  PUT STREAM Ut UNFORMATTED 
    STRING(iButNr)   ' ' 
    cKode    ' ' 
    TODAY    ' ' 
    TIME     ' ' 
    '0'      ' ' 
    '7'    
    ' "' + (IF AVAILABLE ArtBas THEN TRIM(ArtBas.Beskr) ELSE '') + '" '
    '"Overste"' ' ' 
    ENTRY(2,cLinje,';') ' ' 
    '0.00'    ' ' 
    '0.00'    ' '
    '0'    ' '
    '0' 
    SKIP.
END. /* LESERLINJER */

OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.
