/*
 * DSRF.H - Digital Standard Runoff Formatter
 */

/*
 * Define limits
 */
#define DSR_MAXREGISTER		20	/* Largest register number */
#define DSR_MAXTABSTOP		10	/* Maximum number of tab stops */

/*
 * Indexes into registers defining internal parameters
 */
#define DSR_LEFTMARGIN		1	/* Left Margin */
#define DSR_RIGHTMARGIN		2	/* Right Margin */
#define DSR_ATTRLONG		3	/* Long Term Attribute */
#define DSR_ATTRSHORT		4	/* Short term attribute (next char) */
#define DSR_INDENT		5	/* Spaces to indent current line */
#define DSR_LIST		6	/* List Level */
#define DSR_TABLE		7	/* Table Level */
#define DSR_NOTE		8	/* Note Level */
#define DSR_QUOTE		9	/* Quote Level */
#define DSR_FOOTNOTE		10	/* Footnote Level */
#define DSR_FILL		11	/* Fill Flag */
#define DSR_JUSTIFY		12	/* Justify Flag */
#define DSR_LITERAL		13	/* Literal Level */

/*
 * Attributes
 */
#define DSR_ATTRBOLD		1	/* Bold */
#define DSR_ATTRUNDERLINE	2	/* Underline */
#define DSR_ATTRITALIC		4	/* Italic */

/*
 * Special embeded character codes
 */
#define DSR_HARDSPACE		160	/* A Hard space */

