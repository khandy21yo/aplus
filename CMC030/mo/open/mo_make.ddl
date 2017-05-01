DEFINE RECORD CDD$TOP.MO.MO_MAKE

        DESCRIPTION IS /*Make Master Table File*/.

        MO_MAKE_CDD STRUCTURE.

        /* Element =
        Description = Dealer Model of Make */
        MAKE                    DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Make Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Year for Make YYYY */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element = MTYPE
        Description = Type of Make */
        MTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Size of the Make */
        MSIZE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = CLASS
        Description = Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Cut Tubing in inches ie: xx x/x */
        TUBING                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Front Slant in Degrees ie: 3.5 */
        SLANT                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Overall Length of Cab in inches */
        OVERALL                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Indicator for Narrow Front (Y/N) */
        NFRONT                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Indicator for Narrow Back (Y/N) */
        NBACK                   DATATYPE IS TEXT SIZE IS 1.

        END MO_MAKE_CDD STRUCTURE.

END MO_MAKE.
