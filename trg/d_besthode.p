TRIGGER PROCEDURE FOR DELETE OF BestHode.

FOR EACH BestHLev OF BestHode:
  DELETE BestHLev.
END.
FOR EACH BestKasse OF BestHode:
  DELETE BestKasse.
END.
FOR EACH BestLevert OF BestHode:
  DELETE BestLevert.
END.
FOR EACH BestLevIndivid OF BestHode:
  DELETE BestLevIndivid.
END.
FOR EACH BestLinje OF BestKasse:
  DELETE BestLinje.
END.
FOR EACH BestPris OF BestHode:
  DELETE BestPris.
END.
FOR EACH BestSort OF BestHode:
  DELETE BestSort.
END.
FOR EACH BestStr OF BestHode:
  DELETE BestStr.
END.
FOR EACH Fributik OF BestHode:
  DELETE FriButik.
END.

