DEFINE RECORD CDD$TOP.BI.BI_RATETABLE

        DESCRIPTION IS /*CPT Time Rate Header*/.

        BI_RATETABLE_CDD STRUCTURE.

        /* Element = RATETABLE
        Description = CPT Time Rate Table */
        RATETABLE               DATATYPE IS TEXT SIZE IS 6.

        END BI_RATETABLE_CDD STRUCTURE.

END BI_RATETABLE.
