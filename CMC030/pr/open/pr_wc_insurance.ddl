DEFINE RECORD CDD$TOP.PR.PR_WC_INSURANCE

        DESCRIPTION IS /*Workman Comp Insurance Rate*/.

        PR_WC_INSURANCE_CDD STRUCTURE.

        /* Element =
        Description = */
        CODE                    DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Insurance type (liability ins, etc.) */
        INS_TYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        EFFDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Method (1-hour, 2-day) */
        METHOD                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Employee rate */
        EMPLE_RATE              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Employer rate */
        EMPLR_RATE              DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        MAXQHOURS               DATATYPE IS G_FLOATING.

        END PR_WC_INSURANCE_CDD STRUCTURE.

END PR_WC_INSURANCE.
