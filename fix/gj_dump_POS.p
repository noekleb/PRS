
DEFINE STREAM artb.
DEFINE STREAM artp.
DEFINE STREAM artl.
DEFINE STREAM artls.
DEFINE STREAM strekk.

OUTPUT STREAM artb TO artbas.d.
OUTPUT STREAM artp TO artpris.d.
OUTPUT STREAM artl TO lager.d.
OUTPUT STREAM artls TO artlag.d.
OUTPUT STREAM strekk TO strekkode.d.

FOR EACH artbas WHERE artbas.vg = 10 AND lopnr >= 1.
    EXPORT STREAM artb artbas.
    FOR EACH artpris OF artbas.
        EXPORT STREAM artp artpris.
    END.
    FOR EACH lager OF artbas.
        EXPORT STREAM artl lager.
    END.
    FOR EACH artlag WHERE artlag.vg = artbas.vg AND artlag.lopnr = artbas.lopnr.
        EXPORT STREAM artls artlag.
    END.
    FOR EACH strekkode WHERE strekkode.artikkelnr = artbas.artikkelnr.
        EXPORT STREAM strekk strekkode.
    END.
END.
OUTPUT STREAM artb CLOSE.
OUTPUT STREAM artp CLOSE.
OUTPUT STREAM artl CLOSE.
OUTPUT STREAM artls CLOSE.
OUTPUT STREAM strekk CLOSE.

