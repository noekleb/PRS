DEF VAR fVarebehNr AS DEC  NO-UNDO FORMAT ">>>>>>>>>9".
DEF VAR iButNr     AS INT  NO-UNDO FORMAT ">>9".
DEF VAR bOk        AS LOG  NO-UNDO.
DEF VAR cReturn    AS CHAR NO-UNDO.
DEF VAR cUkeNr     AS CHAR NO-UNDO FORMAT "9999".

UPDATE fVarebehNr  LABEL "Messebok"
       iButNr      LABEL "Butikk"
       cUkeNr      LABEL "Ukenr (f.eks 5206)"
       WITH FRAME f1 SIDE-LABELS.

MESSAGE "Generer messeordre for butikk " iButNr "Messebok" fVarebehNr SKIP 
        "(kjedeleverte eller gjennomfakturerte varer)"
        VIEW-AS ALERT-BOX BUTTONS OK-CANCEL UPDATE bOk.

IF NOT bOk THEN RETURN.

FOR EACH VareBehLinje NO-LOCK
    WHERE VareBehLinje.VareBehNr = fVarebehNr
      AND (VareBehLinje.KjedeVare OR VareBehLinje.Gjennomfaktureres)
    :
  IF NOT CAN-FIND(FIRST VareBehLinjeTrans
                  WHERE VareBehLinjeTrans.VareBehNr  = fVarebehNr
                    AND VareBehLinjeTrans.ArtikkelNr = VareBehLinje.ArtikkelNr
                    AND VareBehLinjeTrans.ButikkNr   = iButNr) THEN DO:
    RUN varebehlinje_gentrans.p (STRING(fVarebehNr) + "," + STRING(iButNr) + "," + STRING(VareBehLinje.ArtikkelNr),
                                 ?,
                                 "",
                                 OUTPUT cReturn,
                                 OUTPUT bOk).
    FOR EACH VareBehLinjeTrans EXCLUSIVE-LOCK 
        WHERE VareBehLinjeTrans.VareBehNr  = fVarebehNr
          AND VareBehLinjeTrans.ButikkNr   = iButNr
          AND VareBehLinjeTrans.ArtikkelNr = VareBehLinje.ArtikkelNr
        :
      ASSIGN VareBehLinjeTrans.Levuke1              = INT("20" + SUBSTR(cUkeNr,3) + SUBSTR(cUkeNr,1,2))
             VareBehLinjeTrans.Bestilt1             = 1
             VarebehLinjeTrans.GodkjentBestilling   = YES
             VarebehLinjeTrans.RegistrertBestilling = YES
             .
    END.
  END.
END.

