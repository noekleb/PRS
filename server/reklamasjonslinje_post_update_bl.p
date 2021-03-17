/* To invoke: 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"PostUpdateProc","post_update.p").
   If there's no fieldmap (viewer) set the attribute on the browse object

/* To get the fields used in current update (comma-separated list): */
DEF VAR cFields AS CHAR NO-UNDO.
cFields = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE).

/* To get the corrensponding list of values used in current update (pipe-separated list): */
DEF VAR cValues AS CHAR NO-UNDO.
cValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE).

/* To get a spesific value from current update: */
fReklamasjonsnr = dec(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Reklamasjonsnr")).

-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR obOk            AS LOG  NO-UNDO.

ASSIGN
    ihBuffer:BUFFER-FIELD('ReklamVerdi'):BUFFER-VALUE    = (
                                                            (dec(ihBuffer:BUFFER-FIELD('Pris'):BUFFER-VALUE) - dec(ihBuffer:BUFFER-FIELD('RabKr'):BUFFER-VALUE)) * DEC(ihBuffer:BUFFER-FIELD('Antall'):BUFFER-VALUE)
                                                            ) +
                                                             dec(ihBuffer:BUFFER-FIELD('ReklamUtgifter'):BUFFER-VALUE) 

    ihBuffer:BUFFER-FIELD('ReklamTotal'):BUFFER-VALUE    = (dec(ihBuffer:BUFFER-FIELD('VVarekost'):BUFFER-VALUE)  * DEC(ihBuffer:BUFFER-FIELD('Antall'):BUFFER-VALUE)) +
                                                            dec(ihBuffer:BUFFER-FIELD('ReklamUtgifter'):BUFFER-VALUE)
    .

RUN reklamasjonslogg_recalc.p (STRING(ihBuffer:BUFFER-FIELD("Reklamasjonsnr"):BUFFER-VALUE),ihBuffer,'',OUTPUT ocValue,OUTPUT obOk).



