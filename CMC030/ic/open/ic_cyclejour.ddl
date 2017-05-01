DEFINE RECORD CDD$TOP.IC.IC_CYCLEJOUR

        DESCRIPTION IS /*Cycle Counting Journal*/.

        IC_CYCLEJOUR_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Counting Date (YYYYMMDD) */
        COUNTDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = TRANSTYPE
        Description = Transaction type code */
        TRANSTYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element = PRIMREF
        Description = Primary reference */
        PRIMREF                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = SECREF
        Description = Secondary reference */
        SECREF                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = CROSSREF
        Description = Cross reference number */
        CROSSREF                DATATYPE IS TEXT SIZE IS 10.

        /* Element = SUBACCT
        Description = Sub account (job number) */
        SUBACCT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = STATIONMAN
        Description = Station man (operator) */
        STATIONMAN              DATATYPE IS TEXT SIZE IS 10.

        END IC_CYCLEJOUR_CDD STRUCTURE.

END IC_CYCLEJOUR.
