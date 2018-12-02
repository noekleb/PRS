/************************************************************
    Program:  listehode.p
    Created:  TN    7 Aug 99
Description:  Oppretter listehode.

       run listehode.p (wModus, wListeType, output wListerRecid).

Last change:  TN   18 Aug 99    9:58 pm
************************************************************/

DEF INPUT  PARAMETER  wOrdinaer    as CHAR  NO-UNDO.
DEF INPUT  PARAMETER  wModus       as CHAR  NO-UNDO.
DEF INPUT  PARAMETER  wListeType   as char  NO-UNDO.
DEF OUTPUT PARAMETER  wListerRecid as RECID NO-UNDO.

DEF VAR wListeNr as INT NO-UNDO.

DEF BUFFER bLister FOR Lister.

TRANSBLOKK:
do for bLister TRANSACTION:
  /* Oppstandelsen */
  if wModus = "NY" then
    do:
      /* Finner ledig listenummer */
      if wOrdinaer = "" then
        DO:
          find last bLister no-lock where
            bLister.ListeType = wListeType no-error.
          if available bLister then
            wListeNr = bLister.ListeNr + 1.
          else
            wListeNr = 1.
        END.
      /* Temporær liste med negativt nummer. */
      ELSE DO:
        find first bLister no-lock where
          bLister.ListeType = wListeType no-error.
        if available bLister then
          wListeNr = bLister.ListeNr - 1.
        else
          wListeNr = -1.
      END.

      create bLister.
      assign
        bLister.ListeType = wListeType
        bLister.ListeNr   = wListeNr
        bLister.Eier      = userid("dictdb")
        wListerRecid      = recid(bLister).
    end.

  release bLister.
end. /* TRANSBLOKK */

