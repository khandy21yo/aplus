DEFINE RECORD CDD$TOP.BI.BI_DIAG

        DESCRIPTION IS /*Diagnosis Code Description*/.

        BI_DIAG_CDD STRUCTURE.

        /* Element = DIAGNOSIS
        Description = Diagnosis Code */
        DIAGNOSIS               DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        END BI_DIAG_CDD STRUCTURE.

END BI_DIAG.
