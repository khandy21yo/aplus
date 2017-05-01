DEFINE RECORD CDD$TOP.BI.BI_PATIENT

        DESCRIPTION IS /*Patient File*/.

        BI_PATIENT_CDD STRUCTURE.

        /* Element = INSURED
        Description = Insured */
        INSURED                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = PATIENT
        Description = Patient Number */
        PATIENT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = FAMRELAT
        Description = Family relation */
        FAMRELAT                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Insurance carrier */
        INSURANCE               DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Insurance Group Number */
        GROUPNO                 DATATYPE IS TEXT SIZE IS 10.

        END BI_PATIENT_CDD STRUCTURE.

END BI_PATIENT.
