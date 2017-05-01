DEFINE RECORD CDD$TOP.UTL.UTL_SPELL

        DESCRIPTION IS /*Spelling Dictionary*/.

        UTL_SPELL_CDD STRUCTURE.

        /* Element =
        Description = One word */
        AWORD                   DATATYPE IS TEXT SIZE IS 16.

        END UTL_SPELL_CDD STRUCTURE.

END UTL_SPELL.
