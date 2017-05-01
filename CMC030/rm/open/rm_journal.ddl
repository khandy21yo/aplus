DEFINE RECORD CDD$TOP.RM.RM_JOURNAL

        DESCRIPTION IS /*Restaurant Journal*/.

        RM_JOURNAL_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = First date from worksheet */
        STARTDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = TRANSTYPE
        Description = Transaction type code */
        TRANSTYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element = PRICETYPE
        Description = Price type */
        PRICETYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Station man */
        STATIONMAN              DATATYPE IS TEXT SIZE IS 10.

        /* Element = ACCOUNT
        Description = Expanse account number */
        EXPACCOUNT              DATATYPE IS TEXT SIZE IS 18.

        END RM_JOURNAL_CDD STRUCTURE.

END RM_JOURNAL.
