DEFINE RECORD CDD$TOP.TV.TV_BREAK

        DESCRIPTION IS /*TV Break Master File*/.

        TV_BREAK_CDD STRUCTURE.

        /* Element =
        Description = Program number */
        PRGNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Relative Run time */
        RUN_TIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Break Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Break Type */
        BRKTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Break Length */
        LENGTH                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Comment */
        COMMENT                 DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Maximum number of commercials */
        MAXCOM                  DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Priority */
        PRIORITY                DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Match code */
        MATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Fill (usable for anything) */
        FILLER                  DATATYPE IS TEXT SIZE IS 2.

        END TV_BREAK_CDD STRUCTURE.

END TV_BREAK.
