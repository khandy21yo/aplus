DEFINE RECORD CDD$TOP.AD.AD_ASSET

        DESCRIPTION IS /*Asset Description File*/.

        AD_ASSET_CDD STRUCTURE.

        /* Element = ASSET_NUM
        Description = Asset number */
        ASSET_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Asset description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Asset type */
        ASSET_TYPE              DATATYPE IS TEXT SIZE IS 2.

        /* Element = LOCATION
        Description = Location */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DEPT_NUM
        Description = Department number */
        DEPT_NUM                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Serial number */
        SERIAL_NUM              DATATYPE IS TEXT SIZE IS 20.

        /* Element = DATE
        Description = Service Date */
        SERVDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Initial cost */
        COST                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Salvage */
        SALVAGE                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Bonus, Section 179 */
        BONUS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Investment tax credit */
        ITC                     DATATYPE IS G_FLOATING.

        /* Element =
        Description = ITC Basis Reduction */
        ITCREDUCE               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Life in Units */
        UNITS                   DATATYPE IS G_FLOATING.

        END AD_ASSET_CDD STRUCTURE.

END AD_ASSET.
