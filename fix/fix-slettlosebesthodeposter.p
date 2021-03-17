
FOR EACH BestHode WHERE
    NOT CAN-FIND(ArtBas OF BestHode):

    PAUSE 0.
    DISPLAY BestHode.BestNR.

    FOR EACH BestLinje WHERE BestLinje.BestNr = BestHode.BestNr:
      DELETE BestLinje.
    END.
    FOR EACH BestStr WHERE BestStr.BestNr = BestHode.BestNr:
      DELETE BestStr.
    END.
    FOR EACH BestSort WHERE BestSort.BestNr = BestHode.BestNr:
       DELETE BestSort.
    END.
    FOR EACH BestPris WHERE BestPris.BestNr = BestHode.BestNr:
        DELETE BestPris.
    END.
    FOR EACH BestHLev WHERE BestHLev.BestNr = BestHode.BestNr:
        DELETE BestHLev.
    END.
    FOR EACH BestLevert WHERE BestLevert.BestNr = BestHode.BestNr:
        DELETE BestLevert.
    END.
    FOR EACH BestStr WHERE BestStr.BestNr = BestHode.BestNr:
        DELETE BestStr.
    END.
    FOR EACH BestKasse WHERE BestKasse.BestNr = BestHode.BestNr:
        DELETE BestKasse.
    END.
    FOR EACH FriButik WHERE FriButik.BestNr = BestHode.BestNr:
        DELETE FriButik.
    END.

DELETE BestHode.

END.
