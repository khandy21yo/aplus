/*
 * File Layout for: TV.TV_CONTROL on 21-May-01
 *
 * Control File for TV Traffic
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_control_cdd
{
/* Element =
   Description = Program number */
	char prognum[10];
/* Element =
   Description = Weight for priority */
	double weight_priority;
/* Element =
   Description = Weight for spot length */
	double weight_length;
/* Element =
   Description = Weight for spot cost */
	double weight_cost;
/* Element =
   Description = Weight for spot randomness */
	double weight_random;
/* Element =
   Description = Badness for each priority point */
	double badness_priority;
/* Element =
   Description = Badness for each spot in break already */
	double badness_per_spot;
/* Element =
   Description = Badness for each confl. spot in break */
	double badness_conflict;
/* Element =
   Description = Badness if break match code matches */
	double badness_match;
/* Element =
   Description = Random badness amount */
	double badness_random;
/* Element =
   Description = Worst badness in break that can sched. */
	double badness_worst;
/* Element =
   Description = Default spot seperation */
	char default_spot_seper[6];
/* Element =
   Description = Default product seperation */
	char default_prod_seper[6];
/* Element =
   Description = Maximum spot/product seperation */
	char maximum_seper[6];
};
#pragma member_alignment restore
