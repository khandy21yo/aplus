DEFINE RECORD CDD$TOP.BI.BI_PATIENTEX

        DESCRIPTION IS /*Extra Information for Patients*/.

        BI_PATIENTEX_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Birth Date (YYYYMMDD) */
        BDAY                    DATATYPE IS TEXT SIZE IS 8.

        END BI_PATIENTEX_CDD STRUCTURE.

END BI_PATIENTEX.
