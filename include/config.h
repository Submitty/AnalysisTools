#ifndef CONFIG_H
#define CONFIG_H

/*
 * Length used for character buffers holding paths.
 */
#ifndef STRING_LENGTH
#define STRING_LENGTH 1024
#endif

/*
 * The bounds of the hash function used in the winnowing process. This
 * value has a significant impact on both accuracy and performance: low
 * values lead to a high collision count, greatly reducing both accuracy
 * and performance, while high values drastically increase the runtime of
 * the comparison step.
 *
 * 128^2 seems to be a happy medium for hash size despite not matching up
 * cleanly to any specific byte-length of keys.  Allowing keys to take the
 * full two-byte range triples runtime with almost the same results, while
 * 256 * 16 (conveniently representable as three hexadecimal digits) has
 * terrible results alongside very poor performance due to the dynamic
 * allocation necessary on a collision.
 */
#ifndef HASH_BOUND
#define HASH_BOUND (128 * 128)
#endif

/*
 * The number of entries in the hash table used in the comparison step to
 * cache all file fingerprints. Each entry is potentially very large, so it
 * is preferable to keep this value as small as possible without
 * significantly impacting performance.
 */
#ifndef FINGERPRINT_CACHE_SIZE
#define FINGERPRINT_CACHE_SIZE 256
#endif

/*
 * The directory in which the system saves metadata for each execution.
 */
#ifndef WORKING_DIR
#define WORKING_DIR ".analysis_data"
#endif

/*
 * The name of the file within WORKING_DIR where cumulative totals of
 * fingerprint data are stored.
 */
#ifndef GLOBAL_FILE_NAME
#define GLOBAL_FILE_NAME "__global_fingerprint_count__"
#endif

/*
 * The threshold for a particular fingerprint to be deemed shared code or
 * instructor-provided code and ignored by the comparison step. With a
 * value of 0.6, for example, more than 60% of files must contain a
 * fingerprint for that fingerprint to be ignored.
 */
#ifndef SHARED_THRESHOLD
#define SHARED_THRESHOLD 0.8
#endif

/*
 * Boundary values used by the winnowing algorithm. All token sequences
 * longer than UPPER_BOUND tokens are guaranteed to be detected; all token
 * sequences shorter than LOWER_BOUND are guaranteed to not be detected.
 * Values in between the two values may or may not be detected. Optimal
 * values for these boundaries are likely dependent on the language, but
 * some sane defaults are provided here.
 */
#ifndef DEFAULT_UPPER_BOUND
#define DEFAULT_UPPER_BOUND 30
#endif
#ifndef DEFAULT_LOWER_BOUND
#define DEFAULT_LOWER_BOUND 15
#endif

#endif
