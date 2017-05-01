DEFINE RECORD CDD$TOP.BI.BI_CPT

        DESCRIPTION IS /*Current Procedural Terminology Codes*/.

        BI_CPT_CDD STRUCTURE.

        /* Element = CPT
        Description = Current Procedural Terminology Code */
        CPT                     DATATYPE IS TEXT SIZE IS 5.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element = CPTTYPE
        Description = CPT Type */
        CPTTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = CATEGORY
        Description = CPT Category */
        CATEGORY                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Rate Flag (F,R,T) */
        RATEFLAG                DATATYPE IS TEXT SIZE IS 1.

        END BI_CPT_CDD STRUCTURE.

END BI_CPT.
