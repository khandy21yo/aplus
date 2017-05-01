DEFINE RECORD CDD$TOP.AD.AD_JOURNAL

        DESCRIPTION IS /*Depreciation Units Journal*/.

        AD_JOURNAL_CDD STRUCTURE.

        /* Element = DEP_OBJECT
        Description = Depreciation object */
        DEP_OBJECT              DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Date */
        ACTION_DATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element = STATIONMAN
        Description = Station man (operator) */
        STATIONMAN              DATATYPE IS TEXT SIZE IS 10.

        END AD_JOURNAL_CDD STRUCTURE.

END AD_JOURNAL.
