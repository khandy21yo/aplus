DEFINE RECORD CDD$TOP.BI.BI_BILLL

        DESCRIPTION IS /*Insurance Billing Journal Lines*/.

        BI_BILLL_CDD STRUCTURE.

        /* Element = INSURED
        Description = Insured number */
        INSURED                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = PATIENT
        Description = Patient Number */
        PATIENT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Service Date (YYYYMMDD) */
        SERVDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = CPT
        Description = Current Procedural Terminology Code */
        CPT                     DATATYPE IS TEXT SIZE IS 5.

        /* Element =
        Description = CPT Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element = DIAGNOSIS
        Description = Diagnosis Code */
        DIAGNOSIS               DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Service time (in hours) */
        LENTH                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Time Multiplier */
        MULTIPLIER              DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        END BI_BILLL_CDD STRUCTURE.

END BI_BILLL.
